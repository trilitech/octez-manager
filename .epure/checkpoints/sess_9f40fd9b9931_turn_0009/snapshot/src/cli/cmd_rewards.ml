(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** CLI commands for rewards & payouts. *)

open Cmdliner
open Octez_manager_lib
open Octez_manager_rewards

let ( let* ) = Result.bind

(* ── Helpers ───────────────────────────────────────────────── *)

(** All fields the payout pipeline needs about a baker, abstracted over
    whether the baker is registered as a managed service ({!Service_registry})
    or as a custom entry ({!Custom_baker_registry}). *)
type baker_context = {
  instance : string;
  baker_pkh : string;
  network : string;
  octez_client_bin : string;
  endpoint : string;
  base_dir : string option;
}

(** Read the first delegate PKH from a managed baker's environment config. *)
let baker_delegate_from_env (svc : Service.t) =
  match Node_env.read ~inst:svc.instance with
  | Error _ ->
      Error (Printf.sprintf "Cannot read config for baker '%s'." svc.instance)
  | Ok pairs -> (
      match List.assoc_opt "OCTEZ_BAKER_DELEGATES_CSV" pairs with
      | None | Some "" ->
          Error
            (Printf.sprintf
               "No delegates configured for baker '%s'."
               svc.instance)
      | Some csv -> (
          let delegates =
            String.split_on_char ',' csv
            |> List.map String.trim
            |> List.filter (fun s -> not (String.equal s ""))
          in
          match delegates with
          | [] ->
              Error
                (Printf.sprintf
                   "No delegates configured for baker '%s'."
                   svc.instance)
          | first :: _ -> Ok first))

(** Build a [baker_context] from a managed baker service. *)
let context_of_service (svc : Service.t) =
  let* baker_pkh = baker_delegate_from_env svc in
  let base_dir =
    match Node_env.read ~inst:svc.instance with
    | Error _ -> None
    | Ok pairs -> List.assoc_opt "OCTEZ_BAKER_BASE_DIR" pairs
  in
  Ok
    {
      instance = svc.instance;
      baker_pkh;
      network = svc.Service.network;
      octez_client_bin = Filename.concat svc.Service.app_bin_dir "octez-client";
      endpoint = Rpc_addr.to_endpoint svc.Service.rpc_addr;
      base_dir;
    }

(** Build a [baker_context] from a custom-baker registry entry.
    Normalizes the stored [host:port] endpoint into a URL via
    {!Rpc_addr.to_endpoint} so [octez-client --endpoint] accepts it. *)
let context_of_custom_entry (e : Custom_baker_registry.entry) =
  {
    instance = e.instance;
    baker_pkh = e.baker_pkh;
    network = e.network;
    octez_client_bin = e.octez_client_bin;
    endpoint = Rpc_addr.to_endpoint (Rpc_addr.of_string e.endpoint);
    base_dir = Some e.base_dir;
  }

(** List all baker instance names known to either registry, paired with their
    source for error messages. *)
let list_all_baker_instances () =
  let managed =
    match Service_registry.list () with
    | Ok svcs ->
        List.filter_map
          (fun (svc : Service.t) ->
            if String.equal svc.role "baker" then Some svc.instance else None)
          svcs
    | Error _ -> []
  in
  let custom =
    Custom_baker_registry.list ()
    |> List.map (fun (e : Custom_baker_registry.entry) -> e.instance)
  in
  managed @ custom

(** Resolve a baker instance to a {!baker_context}, looking in both
    {!Service_registry} and {!Custom_baker_registry}. Auto-infers when only
    one baker exists across both registries; otherwise [--baker] is required. *)
let rec resolve_baker_context baker_opt =
  match baker_opt with
  | Some instance -> (
      match Service_registry.find ~instance with
      | Ok (Some svc) when String.equal svc.Service.role "baker" ->
          context_of_service svc
      | Ok (Some svc) ->
          Error
            (Printf.sprintf
               "Instance '%s' is not a baker (role: %s)."
               instance
               svc.Service.role)
      | Ok None | Error _ -> (
          match Custom_baker_registry.find ~instance with
          | Some entry -> Ok (context_of_custom_entry entry)
          | None ->
              Error (Printf.sprintf "Baker instance '%s' not found." instance)))
  | None -> (
      match list_all_baker_instances () with
      | [] -> Error "No baker instances found."
      | [single] -> resolve_baker_context (Some single)
      | many ->
          Error
            (Printf.sprintf
               "Multiple bakers found. Use --baker <instance> to specify. \
                Available: %s"
               (String.concat ", " many)))

(** Load TzKT URL from payout config, falling back to default. *)
let tzkt_url_for ~instance ~baker_pkh =
  match Payout_config.load ~instance with
  | Ok c -> c.tzkt_url
  | Error _ -> (Payout_config.default ~baker_pkh).tzkt_url

(** Format a mutez amount as a short tez string with ꜩ suffix. *)
let format_tez_short mutez = Rewards.format_tez mutez ^ " \xEA\x9C\xA9"

(* ── Common arguments ──────────────────────────────────────── *)

let baker_arg =
  let doc =
    "Baker instance name. Auto-inferred when only one baker is registered."
  in
  Arg.(value & opt (some string) None & info ["baker"] ~doc ~docv:"INSTANCE")

let json_flag =
  let doc = "Output as JSON instead of human-readable table." in
  Arg.(value & flag & info ["json"] ~doc)

(* ── rewards status ────────────────────────────────────────── *)

let status_run baker_opt =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok ctx ->
      let instance = ctx.instance in
      let baker_pkh = ctx.baker_pkh in
      let network = ctx.network in
      let tzkt_url = tzkt_url_for ~instance ~baker_pkh in
      let current_cycle =
        match Cycle_data.fetch_current_cycle ~tzkt_url with
        | Ok c -> Some c
        | Error _ -> None
      in
      let paid_cycles = Payout_report.list_paid_cycles ~instance in
      let last_paid = match paid_cycles with c :: _ -> Some c | [] -> None in
      let recent =
        match
          Cycle_data.fetch_recent_cycles ~tzkt_url ~baker:baker_pkh ~limit:5
        with
        | Ok cycles -> cycles
        | Error _ -> []
      in
      let delegator_count =
        match recent with cr :: _ -> cr.Rewards.num_delegators | [] -> 0
      in
      let pending =
        match (current_cycle, last_paid) with
        | Some cur, Some lp ->
            let rec collect acc c =
              if c <= lp then List.rev acc else collect (c :: acc) (c - 1)
            in
            (* Pending = cycles after last_paid up to cur-1
                   (current cycle is still in progress) *)
            collect [] (cur - 1)
        | Some cur, None ->
            List.filter_map
              (fun (cr : Rewards.cycle_rewards) ->
                if cr.cycle < cur then Some cr.cycle else None)
              recent
        | _ -> []
      in
      Printf.printf "Baker: %s (%s)\n" instance baker_pkh ;
      Printf.printf "Network: %s\n" network ;
      (match current_cycle with
      | Some c -> Printf.printf "Current cycle: %d\n" c
      | None -> Printf.printf "Current cycle: unknown\n") ;
      (match last_paid with
      | Some c -> Printf.printf "Last paid cycle: %d\n" c
      | None -> Printf.printf "Last paid cycle: none\n") ;
      (match pending with
      | [] -> Printf.printf "Pending cycles: none\n"
      | _ ->
          Printf.printf
            "Pending cycles: %s\n"
            (String.concat ", " (List.map string_of_int pending))) ;
      Printf.printf "Delegators: %d\n" delegator_count ;
      `Ok ()

let status_cmd =
  let info =
    Cmd.info "status" ~doc:"Show current cycle and payout status for a baker."
  in
  Cmd.v info Term.(ret (const status_run $ baker_arg))

(* ── rewards generate ──────────────────────────────────────── *)

let render_blueprint_table (bp : Rewards.payout_blueprint) =
  Printf.printf "Cycle %d \xe2\x80\x94 %s\n" bp.cycle bp.baker ;
  Printf.printf "Network: %s\n\n" bp.network ;
  Printf.printf "Summary:\n" ;
  Printf.printf "  Earned rewards:    %s\n" (format_tez_short bp.earned_rewards) ;
  let distributable =
    List.fold_left
      (fun acc (r : Rewards.delegator_reward) ->
        match r.status with
        | Rewards.Eligible -> Int64.add acc r.net_reward
        | _ -> acc)
      0L
      bp.delegator_rewards
  in
  Printf.printf "  Distributable:     %s\n" (format_tez_short distributable) ;
  Printf.printf
    "  Baker bond income: %s\n"
    (format_tez_short bp.baker_bond_income) ;
  Printf.printf
    "  Baker fee income:  %s\n"
    (format_tez_short bp.baker_fee_income) ;
  Printf.printf
    "  Est. tx fees:      %s\n"
    (format_tez_short bp.estimated_tx_fees) ;
  Printf.printf
    "  Eligible / Total:  %d / %d\n\n"
    bp.eligible_delegators
    bp.total_delegators ;
  Printf.printf "Delegator Payouts:\n" ;
  Printf.printf
    "  %-36s %14s %12s %8s %14s  %s\n"
    "ADDRESS"
    "BALANCE"
    "REWARD"
    "FEE"
    "NET PAYOUT"
    "STATUS" ;
  List.iter
    (fun (r : Rewards.delegator_reward) ->
      Printf.printf
        "  %-36s %14s %12s %7.2f%% %14s  %s\n"
        r.recipient
        (Rewards.format_tez r.delegated_balance)
        (Rewards.format_tez r.gross_reward)
        (r.fee_rate *. 100.0)
        (format_tez_short r.net_reward)
        (Rewards.string_of_delegator_status r.status))
    bp.delegator_rewards

let blueprint_to_json (bp : Rewards.payout_blueprint) =
  let delegator_json (r : Rewards.delegator_reward) =
    `Assoc
      [
        ("address", `String r.delegator);
        ("recipient", `String r.recipient);
        ("balance", `String (Int64.to_string r.delegated_balance));
        ("gross_reward", `String (Int64.to_string r.gross_reward));
        ("fee_rate", `Float r.fee_rate);
        ("fee_amount", `String (Int64.to_string r.fee_amount));
        ("net_reward", `String (Int64.to_string r.net_reward));
        ("status", `String (Rewards.string_of_delegator_status r.status));
      ]
  in
  `Assoc
    [
      ("cycle", `Int bp.cycle);
      ("baker", `String bp.baker);
      ("network", `String bp.network);
      ("earned_rewards", `String (Int64.to_string bp.earned_rewards));
      ("baker_bond_income", `String (Int64.to_string bp.baker_bond_income));
      ("baker_fee_income", `String (Int64.to_string bp.baker_fee_income));
      ("estimated_tx_fees", `String (Int64.to_string bp.estimated_tx_fees));
      ("total_delegators", `Int bp.total_delegators);
      ("eligible_delegators", `Int bp.eligible_delegators);
      ("delegators", `List (List.map delegator_json bp.delegator_rewards));
    ]

let generate_run baker_opt cycle_opt json force =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok ctx -> (
      let baker_pkh = ctx.baker_pkh in
      let instance = ctx.instance in
      let network = ctx.network in
      let cycle =
        match cycle_opt with
        | Some c -> Some c
        | None -> (
            let tzkt_url = tzkt_url_for ~instance ~baker_pkh in
            (* Use the latest completed cycle (current - 1) *)
            match Cycle_data.fetch_current_cycle ~tzkt_url with
            | Ok cur -> Some (cur - 1)
            | Error _ -> None)
      in
      match cycle with
      | None ->
          Cli_helpers.cmdliner_error
            "Cannot determine current cycle. Use --cycle to specify."
      | Some cycle -> (
          match
            Payout_blueprint.generate
              ~instance
              ~baker:baker_pkh
              ~network
              ~cycle
              ~force
              ()
          with
          | Error msg ->
              if Payout_blueprint.is_already_paid ~instance ~cycle then (
                Printf.eprintf
                  "Warning: Cycle %d was already paid. Use --force to \
                   re-generate.\n"
                  cycle ;
                `Error (false, msg))
              else Cli_helpers.cmdliner_error msg
          | Ok bp ->
              if json then
                print_endline
                  (Yojson.Safe.pretty_to_string (blueprint_to_json bp))
              else render_blueprint_table bp ;
              `Ok ()))

let generate_cmd =
  let info =
    Cmd.info
      "generate"
      ~doc:"Calculate and display a payout preview for a specific cycle."
  in
  let cycle_arg =
    let doc = "Target cycle number (default: latest completed)." in
    Arg.(value & opt (some int) None & info ["cycle"] ~doc ~docv:"N")
  in
  let force_flag =
    let doc = "Re-generate even if the cycle was already paid." in
    Arg.(value & flag & info ["force"] ~doc)
  in
  Cmd.v
    info
    Term.(
      ret (const generate_run $ baker_arg $ cycle_arg $ json_flag $ force_flag))

(* ── rewards history ───────────────────────────────────────── *)

let history_run baker_opt cycles_count json =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok ctx -> (
      let baker_pkh = ctx.baker_pkh in
      let instance = ctx.instance in
      let tzkt_url = tzkt_url_for ~instance ~baker_pkh in
      match
        Cycle_data.fetch_recent_cycles
          ~tzkt_url
          ~baker:baker_pkh
          ~limit:cycles_count
      with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok cycles ->
          if json then
            let cycle_json (cr : Rewards.cycle_rewards) =
              let earned = Rewards.total_earned cr in
              let status =
                Payout_report.cycle_is_paid ~instance ~cycle:cr.cycle
              in
              let distributed, fee_income =
                if status then
                  match
                    Payout_report.read_summary_json ~instance ~cycle:cr.cycle
                  with
                  | Ok s ->
                      ( `String (Int64.to_string s.distributed_rewards),
                        `String (Int64.to_string s.fee_income) )
                  | Error _ -> (`Null, `Null)
                else (`Null, `Null)
              in
              `Assoc
                [
                  ("cycle", `Int cr.cycle);
                  ("earned", `String (Int64.to_string earned));
                  ("distributed", distributed);
                  ("fee_income", fee_income);
                  ("delegators", `Int cr.num_delegators);
                  ("status", `String (if status then "paid" else "unpaid"));
                ]
            in
            let json_out =
              `Assoc
                [
                  ("baker", `String baker_pkh);
                  ("instance", `String instance);
                  ("cycles", `List (List.map cycle_json cycles));
                ]
            in
            print_endline (Yojson.Safe.pretty_to_string json_out)
          else (
            Printf.printf "Baker: %s (%s)\n\n" instance baker_pkh ;
            Printf.printf
              "%-7s %-16s %-16s %-14s %-12s %s\n"
              "CYCLE"
              "EARNED"
              "DISTRIBUTED"
              "FEE INCOME"
              "DELEGATORS"
              "STATUS" ;
            List.iter
              (fun (cr : Rewards.cycle_rewards) ->
                let earned = Rewards.total_earned cr in
                let is_paid =
                  Payout_report.cycle_is_paid ~instance ~cycle:cr.cycle
                in
                let distributed, fee_income =
                  if is_paid then
                    match
                      Payout_report.read_summary_json ~instance ~cycle:cr.cycle
                    with
                    | Ok s ->
                        ( format_tez_short s.distributed_rewards,
                          format_tez_short s.fee_income )
                    | Error _ -> ("\xE2\x80\x94", "\xE2\x80\x94")
                  else ("\xE2\x80\x94", "\xE2\x80\x94")
                in
                let status_str = if is_paid then "paid" else "unpaid" in
                Printf.printf
                  "%-7d %-16s %-16s %-14s %-12d %s\n"
                  cr.cycle
                  (format_tez_short earned)
                  distributed
                  fee_income
                  cr.num_delegators
                  status_str)
              cycles) ;
          `Ok ())

let history_cmd =
  let info = Cmd.info "history" ~doc:"Show historical payout summaries." in
  let cycles_arg =
    let doc = "Number of recent cycles to show (default: 30)." in
    Arg.(value & opt int 30 & info ["cycles"] ~doc ~docv:"N")
  in
  Cmd.v info Term.(ret (const history_run $ baker_arg $ cycles_arg $ json_flag))

(* ── rewards pay ───────────────────────────────────────────── *)

let rec pay_run baker_opt cycle_opt dry_run confirm =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok bctx -> (
      let baker_pkh = bctx.baker_pkh in
      let instance = bctx.instance in
      let network = bctx.network in
      let cycle =
        match cycle_opt with
        | Some c -> Some c
        | None -> (
            let tzkt_url = tzkt_url_for ~instance ~baker_pkh in
            match Cycle_data.fetch_current_cycle ~tzkt_url with
            | Ok cur -> Some (cur - 1)
            | Error _ -> None)
      in
      match cycle with
      | None ->
          Cli_helpers.cmdliner_error
            "Cannot determine current cycle. Use --cycle to specify."
      | Some cycle -> (
          match
            Payout_blueprint.generate
              ~instance
              ~baker:baker_pkh
              ~network
              ~cycle
              ~force:dry_run
              ()
          with
          | Error msg -> Cli_helpers.cmdliner_error msg
          | Ok blueprint ->
              let config =
                match Payout_config.load ~instance with
                | Ok c -> c
                | Error _ -> Payout_config.default ~baker_pkh
              in
              let ctx : Payout_executor.context =
                {
                  octez_client_bin = bctx.octez_client_bin;
                  endpoint = bctx.endpoint;
                  base_dir = bctx.base_dir;
                  password_file = None;
                  payout_key_alias = config.payout_key_alias;
                  instance;
                }
              in
              (* Interactive confirmation unless --confirm is set *)
              if (not confirm) && not dry_run then begin
                let distributable =
                  List.fold_left
                    (fun acc (r : Rewards.delegator_reward) ->
                      match r.status with
                      | Rewards.Eligible -> Int64.add acc r.net_reward
                      | _ -> acc)
                    0L
                    blueprint.delegator_rewards
                in
                Printf.printf "=== PAYOUT CONFIRMATION ===\n" ;
                Printf.printf "Baker: %s (%s)\n" instance baker_pkh ;
                Printf.printf "Network: %s\n" (String.uppercase_ascii network) ;
                Printf.printf "Cycle: %d\n" cycle ;
                Printf.printf
                  "Total to distribute: %s\n"
                  (format_tez_short distributable) ;
                Printf.printf
                  "Eligible delegators: %d\n"
                  blueprint.eligible_delegators ;
                Printf.printf
                  "Estimated tx fees: %s\n"
                  (format_tez_short blueprint.estimated_tx_fees) ;
                Printf.printf "\n" ;
                if String.equal (String.lowercase_ascii network) "mainnet" then
                  Printf.printf "This action is IRREVERSIBLE on mainnet.\n" ;
                Printf.printf "Proceed? [y/N]: %!" ;
                let response = try input_line stdin with End_of_file -> "n" in
                let answer = String.lowercase_ascii (String.trim response) in
                if not (String.equal answer "y" || String.equal answer "yes")
                then begin
                  Printf.printf "Aborted.\n" ;
                  `Ok ()
                end
                else execute_pay ~ctx ~config ~blueprint ~dry_run ~cycle
              end
              else execute_pay ~ctx ~config ~blueprint ~dry_run ~cycle))

and execute_pay ~ctx ~config ~blueprint ~dry_run ~cycle =
  let mode_str = if dry_run then "Dry-run" else "Broadcasting" in
  match
    Payout_executor.execute
      ~ctx
      ~blueprint
      ~dry_run
      ~on_progress:(fun (p : Payout_executor.progress) ->
        if p.result.Rewards.success then
          Printf.printf
            "%s %d/%d: %s -> %s... done%s\n%!"
            mode_str
            p.current
            p.total
            p.delegator
            (String.sub
               p.result.recipient
               0
               (min 12 (String.length p.result.recipient)))
            (match p.result.op_hash with
            | Some h -> Printf.sprintf " (op: %s)" h
            | None -> "")
        else
          Printf.printf
            "%s %d/%d: %s... FAILED (%s)\n%!"
            mode_str
            p.current
            p.total
            p.delegator
            p.result.note)
      ~batch_size:config.Payout_config.sim_batch_size
      ()
  with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok (results, summary) ->
      let succeeded =
        List.filter (fun (r : Rewards.payout_result) -> r.success) results
      in
      let total = List.length results in
      let ok_count = List.length succeeded in
      Printf.printf
        "\n%s complete: %d/%d succeeded.\n"
        (if dry_run then "Dry-run" else "Payout")
        ok_count
        total ;
      if not dry_run then (
        Printf.printf
          "Reports saved to: %s\n"
          (Payout_report.report_dir ~instance:ctx.instance ~cycle) ;
        (* Send notifications *)
        if ok_count > 0 then
          let channels =
            match Payout_config.load ~instance:ctx.instance with
            | Ok c -> c.notifications
            | Error _ -> []
          in
          if channels <> [] then (
            Printf.printf "Sending notifications...\n%!" ;
            let notify_results =
              Payout_notifier.notify_all ~channels ~summary
            in
            List.iter
              (fun (name, result) ->
                match result with
                | Ok () -> Printf.printf "  %s: sent\n" name
                | Error msg -> Printf.printf "  %s: FAILED (%s)\n" name msg)
              notify_results)) ;
      if ok_count = total then `Ok ()
      else
        `Error
          ( false,
            Printf.sprintf
              "Partial success: %d/%d failed"
              (total - ok_count)
              total )

let pay_cmd =
  let info = Cmd.info "pay" ~doc:"Execute payout for a specific cycle." in
  let cycle_arg =
    let doc = "Target cycle number (default: latest completed)." in
    Arg.(value & opt (some int) None & info ["cycle"] ~doc ~docv:"N")
  in
  let dry_run_flag =
    let doc = "Simulate without broadcasting." in
    Arg.(value & flag & info ["dry-run"] ~doc)
  in
  let confirm_flag =
    let doc = "Skip interactive confirmation (for automation)." in
    Arg.(value & flag & info ["confirm"] ~doc)
  in
  Cmd.v
    info
    Term.(
      ret (const pay_run $ baker_arg $ cycle_arg $ dry_run_flag $ confirm_flag))

(* ── rewards config import ─────────────────────────────────── *)

let config_import_run baker_opt path =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok bctx -> (
      let baker_pkh = bctx.baker_pkh in
      let instance = bctx.instance in
      match Config_import.import_file ~baker_pkh path with
      | Error msg ->
          Printf.eprintf "Error: %s\n" msg ;
          `Error (false, msg)
      | Ok result ->
          (* Save imported config *)
          (match Payout_config.save ~instance result.config with
          | Ok () ->
              Printf.printf "Configuration imported successfully.\n" ;
              Printf.printf "Baker: %s (%s)\n" instance baker_pkh ;
              Printf.printf "Fields imported: %d\n" result.imported_fields
          | Error msg ->
              Printf.eprintf "Warning: failed to save config: %s\n" msg) ;
          if result.warnings <> [] then (
            Printf.printf "\nWarnings:\n" ;
            List.iter (fun w -> Printf.printf "  - %s\n" w) result.warnings) ;
          `Ok ())

let config_import_cmd =
  let info = Cmd.info "import" ~doc:"Import an external config.hjson file." in
  let path_arg =
    let doc = "Path to the external config.hjson file." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PATH")
  in
  Cmd.v info Term.(ret (const config_import_run $ baker_arg $ path_arg))

let config_cmd =
  let info = Cmd.info "config" ~doc:"Manage payout configuration." in
  Cmd.group info [config_import_cmd]

(* ── rewards notify test ──────────────────────────────────── *)

let notify_test_run baker_opt =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok bctx ->
      let instance = bctx.instance in
      let channels =
        match Payout_config.load ~instance with
        | Ok c -> c.notifications
        | Error _ -> []
      in
      if channels = [] then (
        Printf.printf "No notification channels configured for %s.\n" instance ;
        `Ok ())
      else (
        Printf.printf
          "Sending test notifications to %d channel(s)...\n%!"
          (List.length channels) ;
        let results = Payout_notifier.send_test ~channels in
        let all_ok = ref true in
        List.iter
          (fun (name, result) ->
            match result with
            | Ok () -> Printf.printf "  %s: sent\n" name
            | Error msg ->
                all_ok := false ;
                Printf.printf "  %s: FAILED (%s)\n" name msg)
          results ;
        if !all_ok then `Ok () else `Error (false, "Some notifications failed"))

let notify_test_cmd =
  let info =
    Cmd.info "test" ~doc:"Send a test notification to all configured channels."
  in
  Cmd.v info Term.(ret (const notify_test_run $ baker_arg))

let notify_cmd =
  let info = Cmd.info "notify" ~doc:"Manage payout notifications." in
  Cmd.group info [notify_test_cmd]

(* ── rewards continual start/stop/status/run ──────────────────── *)

let continual_run_run baker_opt =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok bctx -> (
      let baker_pkh = bctx.baker_pkh in
      let instance = bctx.instance in
      let config =
        match Payout_config.load ~instance with
        | Ok c -> c
        | Error _ -> Payout_config.default ~baker_pkh
      in
      let tzkt_url = config.tzkt_url in
      (* Fetch current cycle *)
      match Cycle_data.fetch_current_cycle ~tzkt_url with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok current_cycle ->
          let ctx : Payout_executor.context =
            {
              octez_client_bin = bctx.octez_client_bin;
              endpoint = bctx.endpoint;
              base_dir = bctx.base_dir;
              password_file = None;
              payout_key_alias = config.payout_key_alias;
              instance;
            }
          in
          let results =
            Payout_continual.pay_due_cycles
              ~ctx
              ~baker:baker_pkh
              ~network:bctx.network
              ~current_cycle
              ~interval:config.continual_interval
              ~offset:config.continual_offset
          in
          if List.length results = 0 then (
            Printf.printf "No cycles due for payout.\n" ;
            `Ok ())
          else (
            List.iter
              (fun (cycle, (paid_count, result)) ->
                match result with
                | Ok () ->
                    Printf.printf
                      "Cycle %d: paid %d delegators\n"
                      cycle
                      paid_count
                | Error msg ->
                    Printf.eprintf "Cycle %d: FAILED - %s\n" cycle msg)
              results ;
            let all_ok =
              List.for_all
                (fun (_, (_, result)) ->
                  match result with Ok () -> true | Error _ -> false)
                results
            in
            if all_ok then `Ok () else `Error (false, "Some payouts failed")))

let continual_start_run baker_opt interval offset =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok bctx -> (
      let baker_pkh = bctx.baker_pkh in
      let instance = bctx.instance in
      let config =
        match Payout_config.load ~instance with
        | Ok c -> c
        | Error _ -> Payout_config.default ~baker_pkh
      in
      let config =
        {
          config with
          continual_enabled = true;
          continual_interval = interval;
          continual_offset = offset;
        }
      in
      match Payout_config.validate config with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok () -> (
          match Payout_config.save ~instance config with
          | Error msg -> Cli_helpers.cmdliner_error msg
          | Ok () -> (
              (* Determine octez-manager binary path *)
              let octez_manager_bin =
                match Sys.executable_name with
                | "" -> "octez-manager"
                | path -> path
              in
              (* Get service user from the baker service *)
              let service_user =
                if Paths.is_root () then
                  Systemd.get_service_user ~role:"baker" ~instance
                else None
              in
              (* Write systemd units *)
              match
                let* () =
                  match
                    Systemd.write_payout_service
                      ~instance
                      ~octez_manager_bin
                      ~service_user
                      ()
                  with
                  | Ok () -> Ok ()
                  | Error (`Msg msg) ->
                      Error
                        (Printf.sprintf
                           "Failed to write payout service: %s"
                           msg)
                in
                let* () =
                  match Systemd.write_payout_timer ~instance () with
                  | Ok () -> Ok ()
                  | Error (`Msg msg) ->
                      Error
                        (Printf.sprintf "Failed to write payout timer: %s" msg)
                in
                Ok ()
              with
              | Error msg -> Cli_helpers.cmdliner_error msg
              | Ok () ->
                  (* Enable and start the timer *)
                  (match Systemd.enable_payout_timer ~instance with
                  | Error (`Msg msg) ->
                      Printf.eprintf
                        "Warning: failed to enable payout timer: %s\n"
                        msg
                  | Ok () -> Printf.printf "Payout timer enabled and started.\n") ;
                  Printf.printf "Continual mode enabled for %s.\n" instance ;
                  Printf.printf "  Interval: every %d cycle(s)\n" interval ;
                  if offset > 0 then Printf.printf "  Offset: %d\n" offset ;
                  `Ok ())))

let continual_stop_run baker_opt =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok bctx -> (
      let baker_pkh = bctx.baker_pkh in
      let instance = bctx.instance in
      let config =
        match Payout_config.load ~instance with
        | Ok c -> c
        | Error _ -> Payout_config.default ~baker_pkh
      in
      let config = {config with continual_enabled = false} in
      match Payout_config.save ~instance config with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok () ->
          (* Disable and stop the timer *)
          (match Systemd.disable_payout_timer ~instance with
          | Error (`Msg msg) ->
              Printf.eprintf "Warning: failed to disable payout timer: %s\n" msg
          | Ok () -> Printf.printf "Payout timer disabled and stopped.\n") ;
          Printf.printf "Continual mode disabled for %s.\n" instance ;
          `Ok ())

let continual_status_run baker_opt =
  match resolve_baker_context baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok bctx ->
      let baker_pkh = bctx.baker_pkh in
      let instance = bctx.instance in
      let config =
        match Payout_config.load ~instance with
        | Ok c -> c
        | Error _ -> Payout_config.default ~baker_pkh
      in
      Printf.printf "Baker: %s (%s)\n" instance baker_pkh ;
      Printf.printf
        "Continual mode: %s\n"
        (if config.continual_enabled then "enabled" else "disabled") ;
      Printf.printf "Interval: every %d cycle(s)\n" config.continual_interval ;
      if config.continual_offset > 0 then
        Printf.printf "Offset: %d\n" config.continual_offset ;
      (* Show timer status *)
      let timer_active = Systemd.is_payout_timer_active ~instance in
      Printf.printf "Timer active: %s\n" (if timer_active then "yes" else "no") ;
      (match Systemd.payout_timer_status ~instance with
      | Some status -> Printf.printf "\nTimer status:\n%s\n" status
      | None -> ()) ;
      `Ok ()

let continual_start_cmd =
  let info =
    Cmd.info
      "start"
      ~doc:
        "Enable continual payouts. Automatically pays due cycles when the \
         scheduler detects cycle transitions."
  in
  let interval_arg =
    let doc = "Pay every N cycles (default: 1 = every cycle)." in
    Arg.(value & opt int 1 & info ["interval"] ~doc ~docv:"N")
  in
  let offset_arg =
    let doc = "Cycle offset within the interval (default: 0)." in
    Arg.(value & opt int 0 & info ["offset"] ~doc ~docv:"N")
  in
  Cmd.v
    info
    Term.(
      ret (const continual_start_run $ baker_arg $ interval_arg $ offset_arg))

let continual_stop_cmd =
  let info = Cmd.info "stop" ~doc:"Disable continual payouts." in
  Cmd.v info Term.(ret (const continual_stop_run $ baker_arg))

let continual_status_cmd =
  let info = Cmd.info "status" ~doc:"Show continual mode status." in
  Cmd.v info Term.(ret (const continual_status_run $ baker_arg))

let continual_run_cmd =
  let info =
    Cmd.info
      "run"
      ~doc:
        "Execute a single payout check for due cycles. Designed for systemd \
         timer automation."
  in
  Cmd.v info Term.(ret (const continual_run_run $ baker_arg))

let continual_cmd =
  let info =
    Cmd.info "continual" ~doc:"Manage continual (automatic) payouts."
  in
  Cmd.group
    info
    [
      continual_start_cmd;
      continual_stop_cmd;
      continual_status_cmd;
      continual_run_cmd;
    ]

(* ── rewards command group ─────────────────────────────────── *)

let rewards_cmd =
  let info = Cmd.info "rewards" ~doc:"Manage baker rewards and payouts." in
  Cmd.group
    info
    [
      status_cmd;
      generate_cmd;
      history_cmd;
      pay_cmd;
      config_cmd;
      notify_cmd;
      continual_cmd;
    ]
