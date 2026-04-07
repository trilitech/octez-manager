(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** CLI commands for baker wallet operations. *)

open Cmdliner
open Octez_manager_lib
module Baker_ops = Octez_manager_ui.Baker_ops
module Baker_wallet_data = Octez_manager_ui.Baker_wallet_data

(* ── Helpers ───────────────────────────────────────────────── *)

(** Look up a baker service by instance name, validating it has baker role. *)
let with_baker_service ~instance f =
  match Service_registry.find ~instance with
  | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
  | Ok None ->
      Cli_helpers.cmdliner_error
        (Printf.sprintf "Unknown instance '%s'" instance)
  | Ok (Some svc) ->
      if String.equal svc.Service.role "baker" then f svc
      else
        Cli_helpers.cmdliner_error
          (Printf.sprintf
             "Instance '%s' is not a baker (role: %s)"
             instance
             svc.Service.role)

(** Resolve node endpoint and first delegate for a baker service. *)
let resolve_baker_context svc ~delegate_opt =
  let instance = svc.Service.instance in
  match
    Octez_manager_ui.Delegate_scheduler.get_baker_node_endpoint ~instance
  with
  | None ->
      Error
        (Printf.sprintf
           "No node endpoint configured for instance '%s'"
           instance)
  | Some endpoint -> (
      let delegates =
        Octez_manager_ui.Delegate_scheduler.get_baker_delegates ~instance
      in
      match delegate_opt with
      | Some pkh ->
          if List.mem pkh delegates then Ok (endpoint, pkh)
          else
            Error
              (Printf.sprintf
                 "Delegate '%s' not found in instance '%s'"
                 pkh
                 instance)
      | None -> (
          match delegates with
          | [] ->
              Error
                (Printf.sprintf
                   "No delegates configured for instance '%s'"
                   instance)
          | first :: _ -> Ok (endpoint, first)))

(* ── Common Arguments ──────────────────────────────────────── *)

let instance_arg =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INSTANCE")

let delegate_opt =
  let doc = "Target delegate public key hash (default: first delegate)." in
  Arg.(value & opt (some string) None & info ["delegate"] ~doc ~docv:"PKH")

let json_flag =
  let doc = "Output as JSON." in
  Arg.(value & flag & info ["json"] ~doc)

(* ── baker list ────────────────────────────────────────────── *)

let list_run json =
  let open Octez_manager_ui in
  (* load_service_states uses Capability.require which raises Failure when the
     service manager capability is not registered (e.g. on a clean install with
     no instances). Catch it and fall back to a direct registry read. *)
  let states = try Data.load_service_states () with Failure _ -> [] in
  let baker_states =
    List.filter
      (fun (st : Data.Service_state.t) ->
        String.equal st.service.Service.role "baker")
      states
  in
  (* When states is empty (no instances or capability unavailable), fall back
     to the service registry which requires no capability. *)
  let baker_states =
    if baker_states <> [] then baker_states
    else
      match Service_registry.list () with
      | Error _ -> []
      | Ok svcs ->
          List.filter_map
            (fun (svc : Service.t) ->
              if String.equal svc.role "baker" then
                Some
                  Data.Service_state.
                    {
                      service = svc;
                      enabled = None;
                      active = None;
                      status = Unknown "unavailable";
                      status_text = None;
                    }
              else None)
            svcs
  in
  if baker_states = [] then (
    Printf.printf
      "No baker instances found. Use 'octez-manager install-baker' to create \
       one.\n\
       %!" ;
    `Ok ())
  else if json then (
    let entries =
      List.map
        (fun (st : Data.Service_state.t) ->
          let svc = st.service in
          let delegates =
            Delegate_scheduler.get_baker_delegates ~instance:svc.instance
          in
          Printf.sprintf
            {|{"instance": "%s", "network": "%s", "delegates": [%s], "status": "%s"}|}
            svc.instance
            svc.network
            (String.concat
               ", "
               (List.map (fun d -> Printf.sprintf {|"%s"|} d) delegates))
            (Data.Service_state.status_label st))
        baker_states
    in
    Printf.printf "[%s]\n%!" (String.concat ", " entries) ;
    `Ok ())
  else (
    Printf.printf
      "%-20s %-12s %-30s %s\n"
      "INSTANCE"
      "NETWORK"
      "DELEGATES"
      "STATUS" ;
    List.iter
      (fun (st : Data.Service_state.t) ->
        let svc = st.service in
        let delegates =
          Delegate_scheduler.get_baker_delegates ~instance:svc.instance
        in
        let delegates_str = String.concat ", " delegates in
        Printf.printf
          "%-20s %-12s %-30s %s\n"
          svc.instance
          svc.network
          delegates_str
          (Data.Service_state.status_label st))
      baker_states ;
    `Ok ())

let list_cmd =
  let info = Cmd.info "list" ~doc:"List all baker instances" in
  Cmd.v info Term.(ret (const list_run $ json_flag))

(* ── baker <instance> status ───────────────────────────────── *)

let status_run instance delegate_opt json =
  with_baker_service ~instance (fun svc ->
      match resolve_baker_context svc ~delegate_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok (endpoint, pkh) -> (
          match
            Baker_wallet_data.fetch_wallet_data ~node_endpoint:endpoint ~pkh
          with
          | None ->
              Cli_helpers.cmdliner_error
                (Printf.sprintf
                   "Unable to fetch wallet data for %s (node may be \
                    unreachable)"
                   pkh)
          | Some data ->
              if json then (
                let unstake_finalizable =
                  List.map
                    (fun (r : Baker_wallet_data.finalizable_request) ->
                      Printf.sprintf
                        {|{"cycle": %d, "amount": "%s", "status": "finalizable"}|}
                        r.cycle
                        r.amount)
                    data.unstake_requests.finalizable
                in
                let unstake_unfinalizable =
                  List.map
                    (fun (r : Baker_wallet_data.unfinalizable_request) ->
                      Printf.sprintf
                        {|{"cycle": %d, "amount": "%s", "status": "frozen"}|}
                        r.cycle
                        r.amount)
                    data.unstake_requests.unfinalizable
                in
                let unstakes =
                  String.concat
                    ", "
                    (unstake_finalizable @ unstake_unfinalizable)
                in
                Printf.printf
                  {|{"instance": "%s", "network": "%s", "delegate": "%s", "spendable_balance": "%s", "staked_balance": "%s", "unstaked_frozen": "%s", "full_balance": "%s", "is_registered": %b, "deactivated": %b, "consensus_key": "%s", "unstake_requests": [%s]}|}
                  svc.instance
                  svc.network
                  pkh
                  data.spendable_balance
                  data.staked_balance
                  data.unstaked_frozen
                  data.full_balance
                  data.is_registered
                  data.deactivated
                  data.active_consensus_key
                  unstakes ;
                print_newline () ;
                `Ok ())
              else (
                Printf.printf "Baker: %s (%s)\n" svc.instance svc.network ;
                Printf.printf "Delegate: %s\n\n" pkh ;
                Printf.printf
                  "  Spendable balance:       %s\n"
                  (Baker_wallet_data.format_tez data.spendable_balance) ;
                Printf.printf
                  "  Staked balance:          %s\n"
                  (Baker_wallet_data.format_tez data.staked_balance) ;
                Printf.printf
                  "  Unstaked frozen:         %s\n"
                  (Baker_wallet_data.format_tez data.unstaked_frozen) ;
                Printf.printf
                  "  Full balance:            %s\n\n"
                  (Baker_wallet_data.format_tez data.full_balance) ;
                Printf.printf
                  "  Delegate status:         %s\n"
                  (if data.deactivated then "deactivated"
                   else if data.is_registered then "registered"
                   else "not registered") ;
                Printf.printf
                  "  Consensus key:           %s%s\n"
                  data.active_consensus_key
                  (if String.equal data.active_consensus_key pkh then
                     " (default)"
                   else "") ;
                (match data.staking_parameters with
                | Some params ->
                    Printf.printf "\n  Staking parameters:\n" ;
                    Printf.printf
                      "    Limit (staking/baking):  %s\n"
                      (Baker_wallet_data.format_staking_limit
                         params.limit_of_staking_over_baking) ;
                    Printf.printf
                      "    Edge (baking/staking):   %s\n"
                      (Baker_wallet_data.format_baking_edge
                         params.edge_of_baking_over_staking)
                | None -> ()) ;
                (match
                   data.unstake_requests.finalizable
                   @ List.map
                       (fun (r : Baker_wallet_data.unfinalizable_request) ->
                         ({cycle = r.cycle; amount = r.amount}
                           : Baker_wallet_data.finalizable_request))
                       data.unstake_requests.unfinalizable
                 with
                | [] -> ()
                | requests ->
                    Printf.printf "\n  Pending unstakes:\n" ;
                    List.iter
                      (fun (r : Baker_wallet_data.finalizable_request) ->
                        Printf.printf
                          "    Cycle %d:   %s\n"
                          r.cycle
                          (Baker_wallet_data.format_tez r.amount))
                      requests) ;
                print_newline () ;
                `Ok ())))

let status_cmd =
  let info = Cmd.info "status" ~doc:"Show wallet state for a baker instance" in
  Cmd.v
    info
    Term.(ret (const status_run $ instance_arg $ delegate_opt $ json_flag))

(* ── Common operation helpers ──────────────────────────────── *)

let yes_flag =
  let doc = "Skip confirmation prompt." in
  Arg.(value & flag & info ["yes"; "y"] ~doc)

let resolve_octez_client (svc : Service.t) =
  Filename.concat svc.app_bin_dir "octez-client"

let resolve_baker_base_dir (svc : Service.t) =
  match Node_env.read ~inst:svc.instance with
  | Error _ -> None
  | Ok pairs -> List.assoc_opt "OCTEZ_BAKER_BASE_DIR" pairs

let resolve_baker_password_file (svc : Service.t) =
  let rec find = function
    | ("-f" | "--password-filename") :: path :: _ when path <> "" -> Some path
    | _ :: rest -> find rest
    | [] -> None
  in
  match find svc.extra_args with
  | Some _ as result -> result
  | None -> (
      match Node_env.read ~inst:svc.instance with
      | Error _ -> None
      | Ok pairs -> (
          match List.assoc_opt "OCTEZ_BAKER_GLOBAL_ARGS" pairs with
          | None -> None
          | Some global_str -> find (String.split_on_char ' ' global_str)))

let run_operation ~instance ~svc ~endpoint ~pkh ~op ~json ~yes =
  let client_bin = resolve_octez_client svc in
  let base_dir = resolve_baker_base_dir svc in
  let password_file = resolve_baker_password_file svc in
  let description = Baker_ops.describe_operation op in
  let confirmed =
    if yes then true
    else (
      Printf.printf "Operation: %s\n" description ;
      Printf.printf "Delegate:  %s\n" pkh ;
      Printf.printf "Instance:  %s\n" instance ;
      Printf.printf "\n%!" ;
      Cli_helpers.prompt_yes_no "Proceed?" ~default:false)
  in
  if not confirmed then (
    Printf.printf "Cancelled.\n%!" ;
    `Ok ())
  else
    let result =
      Baker_ops.execute
        ~instance_name:instance
        ~octez_client_bin:client_bin
        ~endpoint
        ~base_dir
        ~password_file
        ~alias:pkh
        ~op
    in
    if result.success then (
      let hash = Option.value ~default:"(no hash)" result.op_hash in
      if json then Printf.printf {|{"success": true, "op_hash": "%s"}|} hash
      else Printf.printf "Operation submitted. Hash: %s\n" hash ;
      print_newline () ;
      `Ok ())
    else
      let err = Option.value ~default:"Unknown error" result.error in
      if json then (
        Printf.printf {|{"success": false, "error": "%s"}|} err ;
        print_newline () ;
        `Ok ())
      else Cli_helpers.cmdliner_error err

(* ── baker <instance> register ───────────────────────────── *)

let register_run instance delegate_opt json yes =
  with_baker_service ~instance (fun svc ->
      match resolve_baker_context svc ~delegate_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok (endpoint, pkh) ->
          run_operation
            ~instance
            ~svc
            ~endpoint
            ~pkh
            ~op:Baker_ops.Register
            ~json
            ~yes)

let register_cmd =
  let info = Cmd.info "register" ~doc:"Register delegate key" in
  Cmd.v
    info
    Term.(
      ret
        (const register_run $ instance_arg $ delegate_opt $ json_flag $ yes_flag))

(* ── baker <instance> stake <amount> ─────────────────────── *)

let amount_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"AMOUNT")

let stake_run instance amount delegate_opt json yes =
  with_baker_service ~instance (fun svc ->
      match resolve_baker_context svc ~delegate_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok (endpoint, pkh) ->
          run_operation
            ~instance
            ~svc
            ~endpoint
            ~pkh
            ~op:(Baker_ops.Stake {amount})
            ~json
            ~yes)

let stake_cmd =
  let info = Cmd.info "stake" ~doc:"Stake tez for a baker delegate" in
  Cmd.v
    info
    Term.(
      ret
        (const stake_run $ instance_arg $ amount_arg $ delegate_opt $ json_flag
       $ yes_flag))

(* ── baker <instance> unstake <amount|everything> ────────── *)

let unstake_run instance amount delegate_opt json yes =
  with_baker_service ~instance (fun svc ->
      match resolve_baker_context svc ~delegate_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok (endpoint, pkh) ->
          run_operation
            ~instance
            ~svc
            ~endpoint
            ~pkh
            ~op:(Baker_ops.Unstake {amount})
            ~json
            ~yes)

let unstake_cmd =
  let info = Cmd.info "unstake" ~doc:"Unstake tez (amount or \"everything\")" in
  Cmd.v
    info
    Term.(
      ret
        (const unstake_run $ instance_arg $ amount_arg $ delegate_opt
       $ json_flag $ yes_flag))

(* ── baker <instance> finalize-unstake ───────────────────── *)

let finalize_unstake_run instance delegate_opt json yes =
  with_baker_service ~instance (fun svc ->
      match resolve_baker_context svc ~delegate_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok (endpoint, pkh) ->
          run_operation
            ~instance
            ~svc
            ~endpoint
            ~pkh
            ~op:Baker_ops.Finalize_unstake
            ~json
            ~yes)

let finalize_unstake_cmd =
  let info =
    Cmd.info "finalize-unstake" ~doc:"Finalize pending unstake requests"
  in
  Cmd.v
    info
    Term.(
      ret
        (const finalize_unstake_run $ instance_arg $ delegate_opt $ json_flag
       $ yes_flag))

(* ── baker <instance> transfer <amount> <destination> ────── *)

let destination_arg =
  Arg.(required & pos 2 (some string) None & info [] ~docv:"DESTINATION")

let transfer_run instance amount destination delegate_opt json yes =
  with_baker_service ~instance (fun svc ->
      match resolve_baker_context svc ~delegate_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok (endpoint, pkh) ->
          run_operation
            ~instance
            ~svc
            ~endpoint
            ~pkh
            ~op:(Baker_ops.Transfer {amount; destination})
            ~json
            ~yes)

let transfer_cmd =
  let info = Cmd.info "transfer" ~doc:"Transfer tez to another address" in
  Cmd.v
    info
    Term.(
      ret
        (const transfer_run $ instance_arg $ amount_arg $ destination_arg
       $ delegate_opt $ json_flag $ yes_flag))

(* ── baker <instance> set-delegate-params ────────────────── *)

let limit_opt =
  let doc = "Limit of staking over baking (0-9)." in
  Arg.(value & opt (some int) None & info ["limit-of-staking-over-baking"] ~doc)

let edge_opt =
  let doc = "Edge of baking over staking (0-100)." in
  Arg.(value & opt (some int) None & info ["edge-of-baking-over-staking"] ~doc)

let set_delegate_params_run instance limit_opt edge_opt delegate_opt json yes =
  with_baker_service ~instance (fun svc ->
      match resolve_baker_context svc ~delegate_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok (endpoint, pkh) ->
          let limit = Option.value ~default:0 limit_opt in
          let edge = Option.value ~default:0 edge_opt in
          run_operation
            ~instance
            ~svc
            ~endpoint
            ~pkh
            ~op:(Baker_ops.Set_delegate_params {limit; edge})
            ~json
            ~yes)

let set_delegate_params_cmd =
  let info =
    Cmd.info "set-delegate-params" ~doc:"Set delegate staking parameters"
  in
  Cmd.v
    info
    Term.(
      ret
        (const set_delegate_params_run
        $ instance_arg $ limit_opt $ edge_opt $ delegate_opt $ json_flag
        $ yes_flag))

(* ── baker <instance> update-consensus-key <key> ─────────── *)

let key_arg = Arg.(required & pos 1 (some string) None & info [] ~docv:"KEY")

let update_consensus_key_run instance key delegate_opt json yes =
  with_baker_service ~instance (fun svc ->
      match resolve_baker_context svc ~delegate_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok (endpoint, pkh) ->
          run_operation
            ~instance
            ~svc
            ~endpoint
            ~pkh
            ~op:(Baker_ops.Update_consensus_key {key})
            ~json
            ~yes)

let update_consensus_key_cmd =
  let info =
    Cmd.info "update-consensus-key" ~doc:"Update baker consensus key"
  in
  Cmd.v
    info
    Term.(
      ret
        (const update_consensus_key_run
        $ instance_arg $ key_arg $ delegate_opt $ json_flag $ yes_flag))

(* ── baker <instance> vote <value> ───────────────────────── *)

let vote_value_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"VALUE")

let vote_run instance value delegate_opt json yes =
  with_baker_service ~instance (fun svc ->
      match resolve_baker_context svc ~delegate_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok (endpoint, pkh) -> (
          (* Fetch voting info to determine the right operation *)
          match Baker_wallet_data.fetch_voting_info ~node_endpoint:endpoint with
          | None ->
              Cli_helpers.cmdliner_error
                "Unable to fetch voting info (node may be unreachable)"
          | Some info -> (
              (* Check if already voted *)
              let already_voted =
                List.exists (fun (p, _) -> String.equal p pkh) info.ballots
              in
              if already_voted then
                Cli_helpers.cmdliner_error
                  "Delegate has already voted in this period"
              else
                match info.period_kind with
                | Baker_wallet_data.Proposal ->
                    (* value is a protocol hash to upvote *)
                    run_operation
                      ~instance
                      ~svc
                      ~endpoint
                      ~pkh
                      ~op:(Baker_ops.Submit_proposals {proposals = [value]})
                      ~json
                      ~yes
                | Baker_wallet_data.Exploration | Baker_wallet_data.Promotion
                  -> (
                    (* value is yay/nay/pass *)
                    let ballot =
                      match String.lowercase_ascii value with
                      | "yay" -> Some Baker_wallet_data.Yay
                      | "nay" -> Some Baker_wallet_data.Nay
                      | "pass" -> Some Baker_wallet_data.Pass
                      | _ -> None
                    in
                    match ballot with
                    | None ->
                        Cli_helpers.cmdliner_error
                          (Printf.sprintf
                             "Invalid ballot '%s' (expected: yay, nay, or pass)"
                             value)
                    | Some ballot ->
                        let proposal =
                          Option.value
                            ~default:"(unknown)"
                            info.current_proposal
                        in
                        run_operation
                          ~instance
                          ~svc
                          ~endpoint
                          ~pkh
                          ~op:(Baker_ops.Submit_ballot {proposal; ballot})
                          ~json
                          ~yes)
                | Baker_wallet_data.Cooldown | Baker_wallet_data.Adoption ->
                    if json then (
                      Printf.printf
                        {|{"success": false, "error": "No voting action available during %s period"}|}
                        (Baker_wallet_data.string_of_voting_period_kind
                           info.period_kind) ;
                      print_newline () ;
                      `Ok ())
                    else
                      Cli_helpers.cmdliner_error
                        (Printf.sprintf
                           "No voting action available during %s period"
                           (Baker_wallet_data.string_of_voting_period_kind
                              info.period_kind)))))

let vote_cmd =
  let info =
    Cmd.info
      "vote"
      ~doc:
        "Vote on governance (protocol hash during proposal period, \
         yay/nay/pass during exploration/promotion)"
  in
  Cmd.v
    info
    Term.(
      ret
        (const vote_run $ instance_arg $ vote_value_arg $ delegate_opt
       $ json_flag $ yes_flag))

(* ── Command group ─────────────────────────────────────────── *)

let baker_cmd =
  let doc = "Baker wallet operations" in
  let info = Cmd.info "baker" ~doc in
  Cmd.group
    info
    [
      list_cmd;
      status_cmd;
      register_cmd;
      stake_cmd;
      unstake_cmd;
      finalize_unstake_cmd;
      transfer_cmd;
      set_delegate_params_cmd;
      update_consensus_key_cmd;
      vote_cmd;
    ]
