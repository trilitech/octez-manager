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
  let states = Data.load_service_states () in
  let baker_states =
    List.filter
      (fun (st : Data.Service_state.t) ->
        String.equal st.service.Service.role "baker")
      states
  in
  if json then
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
    Printf.printf "[%s]\n%!" (String.concat ", " entries)
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
      baker_states) ;
  `Ok ()

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

(* ── Command group ─────────────────────────────────────────── *)

let baker_cmd =
  let doc = "Baker wallet operations" in
  let info = Cmd.info "baker" ~doc in
  Cmd.group info [list_cmd; status_cmd]
