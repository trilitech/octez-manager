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

(* ── Indexer logging setup ─────────────────────────────────── *)

(** Normalize a JSON value to a canonical string for comparison.
    Strips string-vs-int encoding differences (e.g. ["120262"] vs [120262])
    and ignores extra object fields not present on the other side. *)
let rec normalize_json_value = function
  | `String s -> (
      match Int64.of_string_opt s with
      | Some n -> Int64.to_string n
      | None -> Printf.sprintf "%S" s)
  | `Int n -> string_of_int n
  | `Intlit s -> s
  | `Float f -> string_of_float f
  | `Bool b -> string_of_bool b
  | `Null -> "null"
  | `Assoc fields ->
      (* Sort fields and normalize values for stable comparison *)
      let sorted =
        List.sort (fun (a, _) (b, _) -> String.compare a b) fields
      in
      let parts =
        List.map
          (fun (k, v) -> Printf.sprintf "%s:%s" k (normalize_json_value v))
          sorted
      in
      "{" ^ String.concat "," parts ^ "}"
  | `List items ->
      let parts = List.map normalize_json_value items in
      "[" ^ String.concat "," parts ^ "]"

(** Compare two JSON arrays element-wise, ignoring extra fields in objects
    that only appear on one side.  Returns [true] if semantically equal. *)
let json_arrays_equal a b =
  let normalize_obj fields =
    List.sort (fun (a, _) (b, _) -> String.compare a b) fields
  in
  let rec values_equal v1 v2 =
    match (v1, v2) with
    | `Assoc f1, `Assoc f2 ->
        (* Only compare fields present on both sides *)
        let f1 = normalize_obj f1 in
        let f2 = normalize_obj f2 in
        let shared_keys =
          List.filter_map
            (fun (k, _) ->
              if List.mem_assoc k f2 then Some k else None)
            f1
        in
        List.for_all
          (fun k ->
            let v1 = List.assoc k f1 in
            let v2 = List.assoc k f2 in
            values_equal v1 v2)
          shared_keys
    | `List l1, `List l2 ->
        List.length l1 = List.length l2
        && List.for_all2 values_equal l1 l2
    | _ ->
        String.equal (normalize_json_value v1) (normalize_json_value v2)
  in
  values_equal a b

(** Pretty-print a JSON value for display. *)
let display_json_value = function
  | `String s -> s
  | v -> Yojson.Safe.to_string v

(** Fields read by [Cycle_data.fetch_current_cycle]. *)
let head_fields = ["cycle"]

(** Fields read by [Cycle_data.parse_cycle_rewards]. *)
let split_fields =
  [
    "cycle";
    "delegatorsCount";
    "delegators";
    "stakingBalance";
    "delegatedBalance";
    "ownStakedBalance";
    "ownDelegatedBalance";
    "externalStakedBalance";
    "externalDelegatedBalance";
    "blockFees";
  ]
  @ List.concat_map
      (fun prefix ->
        List.map
          (fun suffix -> prefix ^ suffix)
          ["Delegated"; "StakedOwn"; "StakedEdge"; "StakedShared"])
      [
        "blockRewards";
        "attestationRewards";
        "dalAttestationRewards";
        "vdfRevelationRewards";
        "nonceRevelationRewards";
      ]

(** Select the relevant field set for [path]. *)
let relevant_fields_for path =
  if String.equal path "/v1/head" then Some head_fields
  else if
    (* /v1/rewards/split/... or /v1/rewards/bakers/... *)
    let prefix = "/v1/rewards/" in
    String.length path >= String.length prefix
    && String.equal (String.sub path 0 (String.length prefix)) prefix
  then Some split_fields
  else None

(** Compare two JSON objects on the fields that the rewards code actually
    reads.  Returns [(key, custom_val, tzkt_val)] for fields that differ. *)
let json_field_diffs path custom_body tzkt_body =
  let parse s =
    try
      match Yojson.Safe.from_string s with
      | `Assoc fields -> Some fields
      | _ -> None
    with _ -> None
  in
  match (parse custom_body, parse tzkt_body) with
  | Some custom_fields, Some tzkt_fields ->
      let keys =
        match relevant_fields_for path with
        | Some ks -> ks
        | None -> List.map fst custom_fields
      in
      List.filter_map
        (fun key ->
          match (List.assoc_opt key custom_fields, List.assoc_opt key tzkt_fields)
          with
          | Some cv, Some tv ->
              if json_arrays_equal cv tv then None
              else Some (key, display_json_value cv, display_json_value tv)
          | _ -> None)
        keys
  | _ -> []

let setup_indexer_logging () =
  Indexer.set_log_info (fun msg -> Printf.eprintf "%s\n%!" msg) ;
  Indexer.set_log_warn (fun msg -> Printf.eprintf "Warning: %s\n%!" msg) ;
  Indexer.set_on_divergence (fun path custom_body tzkt_body ->
      let diffs = json_field_diffs path custom_body tzkt_body in
      match diffs with
      | [] ->
          `Use_custom
      | fields ->
          Printf.eprintf "Warning: indexer divergence on %s\n%!" path ;
          List.iter
            (fun (key, cv, tv) ->
              Printf.eprintf "  %s: custom=%s  tzkt=%s\n%!" key cv tv)
            fields ;
          Printf.eprintf "  -> using public TzKT response\n%!" ;
          `Use_tzkt)

(* ── Helpers ───────────────────────────────────────────────── *)

(** List all baker instances from the service registry. *)
let list_baker_services () =
  match Service_registry.list () with
  | Error (`Msg msg) -> Error msg
  | Ok services ->
      Ok
        (List.filter
           (fun (svc : Service.t) -> String.equal svc.role "baker")
           services)

(** Resolve baker instance: auto-infer when single baker exists,
    require [--baker] when multiple bakers are registered. *)
let resolve_baker baker_opt =
  match baker_opt with
  | Some instance -> (
      match Service_registry.find ~instance with
      | Error (`Msg msg) -> Error msg
      | Ok None ->
          Error (Printf.sprintf "Baker instance '%s' not found." instance)
      | Ok (Some svc) ->
          if String.equal svc.Service.role "baker" then Ok svc
          else
            Error
              (Printf.sprintf
                 "Instance '%s' is not a baker (role: %s)."
                 instance
                 svc.Service.role))
  | None -> (
      match list_baker_services () with
      | Error msg -> Error msg
      | Ok [] -> Error "No baker instances found in service registry."
      | Ok [svc] -> Ok svc
      | Ok bakers ->
          let names = List.map (fun (svc : Service.t) -> svc.instance) bakers in
          Error
            (Printf.sprintf
               "Multiple bakers found. Use --baker <instance> to specify. \
                Available: %s"
               (String.concat ", " names)))

(** Read the first delegate PKH from a baker's environment config. *)
let baker_delegate (svc : Service.t) =
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

(** Load preferred TzKT base URL from payout config, if available. *)
let preferred_base_for ~network ~instance =
  match Payout_config.load ~instance with
  | Ok c -> Some (Payout_config.effective_tzkt_url ~network c)
  | Error _ -> None

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
  setup_indexer_logging () ;
  match resolve_baker baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok svc -> (
      match baker_delegate svc with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok baker_pkh ->
          let instance = svc.instance in
          let network = svc.network in
          let preferred_base = preferred_base_for ~network ~instance in
          let current_cycle =
            match Cycle_data.fetch_current_cycle ~network ~preferred_base with
            | Ok c -> Some c
            | Error _ -> None
          in
          let paid_cycles = Payout_report.list_paid_cycles ~instance in
          let last_paid =
            match paid_cycles with c :: _ -> Some c | [] -> None
          in
          let recent =
            match
              Cycle_data.fetch_recent_cycles
                ~network
                ~preferred_base
                ~baker:baker_pkh
                ~limit:5
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
          `Ok ())

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
  setup_indexer_logging () ;
  match resolve_baker baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok svc -> (
      match baker_delegate svc with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok baker_pkh -> (
          let instance = svc.instance in
          let network = svc.network in
          let cycle =
            match cycle_opt with
            | Some c -> Some c
            | None -> (
                let preferred_base = preferred_base_for ~network ~instance in
                (* Use the latest completed cycle (current - 1) *)
                match
                  Cycle_data.fetch_current_cycle ~network ~preferred_base
                with
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
                  `Ok ())))

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
  setup_indexer_logging () ;
  match resolve_baker baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok svc -> (
      match baker_delegate svc with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok baker_pkh -> (
          let instance = svc.instance in
          let network = svc.network in
          let preferred_base = preferred_base_for ~network ~instance in
          match
            Cycle_data.fetch_recent_cycles
              ~network
              ~preferred_base
              ~baker:baker_pkh
              ~limit:cycles_count
          with
          | Error msg -> Cli_helpers.cmdliner_error msg
          | Ok cycles ->
              if json then
                let cycle_json (cr : Rewards.cycle_rewards) =
                  let earned =
                    List.fold_left
                      Int64.add
                      0L
                      [
                        cr.block_rewards;
                        cr.attestation_rewards;
                        cr.other_rewards;
                        cr.block_fees;
                      ]
                  in
                  let status =
                    Payout_report.cycle_is_paid ~instance ~cycle:cr.cycle
                  in
                  let distributed, fee_income =
                    if status then
                      match
                        Payout_report.read_summary_json
                          ~instance
                          ~cycle:cr.cycle
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
                    let earned =
                      List.fold_left
                        Int64.add
                        0L
                        [
                          cr.block_rewards;
                          cr.attestation_rewards;
                          cr.other_rewards;
                          cr.block_fees;
                        ]
                    in
                    let is_paid =
                      Payout_report.cycle_is_paid ~instance ~cycle:cr.cycle
                    in
                    let distributed, fee_income =
                      if is_paid then
                        match
                          Payout_report.read_summary_json
                            ~instance
                            ~cycle:cr.cycle
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
              `Ok ()))

let history_cmd =
  let info = Cmd.info "history" ~doc:"Show historical payout summaries." in
  let cycles_arg =
    let doc = "Number of recent cycles to show (default: 30)." in
    Arg.(value & opt int 30 & info ["cycles"] ~doc ~docv:"N")
  in
  Cmd.v info Term.(ret (const history_run $ baker_arg $ cycles_arg $ json_flag))

(* ── Shared executor context builder ──────────────────────── *)

let build_executor_ctx ~(svc : Service.t) ~(config : Payout_config.t) =
  let octez_client_bin =
    Filename.concat svc.Service.app_bin_dir "octez-client"
  in
  let endpoint = Rpc_addr.to_endpoint svc.Service.rpc_addr in
  let base_dir =
    match Node_env.read ~inst:svc.instance with
    | Ok pairs -> (
        match List.assoc_opt "OCTEZ_CLIENT_BASE_DIR" pairs with
        | Some d -> Some d
        | None -> List.assoc_opt "OCTEZ_BAKER_BASE_DIR" pairs)
    | Error _ -> None
  in
  let ctx : Payout_executor.context =
    {
      octez_client_bin;
      endpoint;
      base_dir;
      password_file = None;
      payout_key_alias = config.payout_key_alias;
      instance = svc.instance;
    }
  in
  ctx

(* ── rewards pay ───────────────────────────────────────────── *)

let rec pay_run baker_opt cycle_opt dry_run confirm compare =
  setup_indexer_logging () ;
  if compare then Indexer.set_debug_mode true ;
  match resolve_baker baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok svc -> (
      match baker_delegate svc with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok baker_pkh -> (
          let instance = svc.instance in
          let network = svc.network in
          let cycle =
            match cycle_opt with
            | Some c -> Some c
            | None -> (
                let preferred_base = preferred_base_for ~network ~instance in
                match
                  Cycle_data.fetch_current_cycle ~network ~preferred_base
                with
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
                    | Error _ -> Payout_config.default ~network ~baker_pkh ()
                  in
                  let ctx = build_executor_ctx ~svc ~config in
                  (* For dry-run, show the full blueprint first *)
                  if dry_run then render_blueprint_table blueprint ;
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
                    Printf.printf
                      "Network: %s\n"
                      (String.uppercase_ascii network) ;
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
                    if String.equal (String.lowercase_ascii network) "mainnet"
                    then
                      Printf.printf "This action is IRREVERSIBLE on mainnet.\n" ;
                    Printf.printf "Proceed? [y/N]: %!" ;
                    let response =
                      try input_line stdin with End_of_file -> "n"
                    in
                    let answer =
                      String.lowercase_ascii (String.trim response)
                    in
                    if not (String.equal answer "y" || String.equal answer "yes")
                    then begin
                      Printf.printf "Aborted.\n" ;
                      `Ok ()
                    end
                    else execute_pay ~ctx ~config ~blueprint ~dry_run ~cycle
                  end
                  else execute_pay ~ctx ~config ~blueprint ~dry_run ~cycle)))

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
      (* Show octez-client simulation output for dry-run *)
      if dry_run then begin
        let sim_output =
          List.filter_map
            (fun (r : Rewards.payout_result) ->
              if r.success && not (String.equal r.note "")
                 && not (String.equal r.note "dry-run")
              then Some r.note
              else None)
            results
        in
        match sim_output with
        | first :: _ ->
            Printf.printf "\n=== Simulated operations ===\n%s\n" first
        | [] -> ()
      end ;
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
  let compare_flag =
    let doc =
      "Compare custom indexer results with public TzKT and log divergences."
    in
    Arg.(value & flag & info ["compare"; "compare-indexers"] ~doc)
  in
  Cmd.v
    info
    Term.(
      ret
        (const pay_run $ baker_arg $ cycle_arg $ dry_run_flag $ confirm_flag
       $ compare_flag))

(* ── rewards config import ─────────────────────────────────── *)

let config_import_run baker_opt path =
  setup_indexer_logging () ;
  match resolve_baker baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok svc -> (
      match baker_delegate svc with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok baker_pkh -> (
          match Config_import.import_file ~baker_pkh path with
          | Error msg ->
              Printf.eprintf "Error: %s\n" msg ;
              `Error (false, msg)
          | Ok result ->
              let instance = svc.Service.instance in
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
              `Ok ()))

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
  setup_indexer_logging () ;
  match resolve_baker baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok svc ->
      let instance = svc.Service.instance in
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

(* ── rewards continual tick ────────────────────────────────── *)

(** Tick a single baker: check delay state, compute due cycles, pay. *)
let tick_one_baker (svc : Service.t) =
  let instance = svc.instance in
  match baker_delegate svc with
  | Error msg ->
      Printf.eprintf "Error [%s]: %s\n%!" instance msg
  | Ok baker_pkh -> (
      let config =
        match Payout_config.load ~instance with
        | Ok c -> c
        | Error _ ->
            Payout_config.default ~network:svc.network ~baker_pkh ()
      in
      if not config.continual_enabled then
        Printf.printf "[%s] continual mode disabled, skipping.\n%!" instance
      else
        let network = svc.network in
        let preferred_base = preferred_base_for ~network ~instance in
        match Cycle_data.fetch_current_cycle ~network ~preferred_base with
        | Error msg ->
            Printf.eprintf
              "Error [%s]: cannot fetch current cycle: %s\n%!"
              instance
              msg
        | Ok current_cycle ->
            let due =
              Payout_continual.cycles_due
                ~instance
                ~current_cycle
                ~interval:config.continual_interval
                ~offset:config.continual_offset
            in
            if due = [] then
              Printf.printf
                "[%s] no cycles due (current: %d).\n%!"
                instance
                current_cycle
            else (
              (* Check delay state *)
              let now = Unix.gettimeofday () in
              match Payout_continual.read_delay_until ~instance with
              | Some until when Float.compare until now > 0 ->
                  let remaining = until -. now in
                  Printf.printf
                    "[%s] delay active, %.0fs remaining.\n%!"
                    instance
                    remaining
              | Some _ ->
                  (* Delay expired — clear and pay *)
                  Payout_continual.clear_delay_until ~instance ;
                  Printf.printf
                    "[%s] delay expired, paying cycles: %s\n%!"
                    instance
                    (String.concat
                       ", "
                       (List.map string_of_int due)) ;
                  let ctx = build_executor_ctx ~svc ~config in
                  let results =
                    Payout_continual.pay_due_cycles
                      ~ctx
                      ~baker:baker_pkh
                      ~network
                      ~current_cycle
                      ~interval:config.continual_interval
                      ~offset:config.continual_offset
                  in
                  List.iter
                    (fun (cycle, result) ->
                      match result with
                      | Ok () ->
                          Printf.printf
                            "[%s] cycle %d: paid successfully.\n%!"
                            instance
                            cycle
                      | Error msg ->
                          Printf.eprintf
                            "[%s] cycle %d: FAILED (%s)\n%!"
                            instance
                            cycle
                            msg)
                    results
              | None ->
                  (* No delay file — write one with random delay *)
                  let min_blocks =
                    Float.of_int (max 1 config.min_delay_blocks)
                  in
                  let max_blocks =
                    Float.of_int (max 1 config.max_delay_blocks)
                  in
                  let block_time = 10.0 in
                  let delay_secs =
                    min_blocks *. block_time
                    +. Random.float
                         ((max_blocks -. min_blocks) *. block_time)
                  in
                  let until = now +. delay_secs in
                  Payout_continual.write_delay_until ~instance until ;
                  Printf.printf
                    "[%s] cycles due: %s — delay set for %.0fs.\n%!"
                    instance
                    (String.concat
                       ", "
                       (List.map string_of_int due))
                    delay_secs))

let tick_run baker_opt =
  setup_indexer_logging () ;
  Random.self_init () ;
  match baker_opt with
  | Some _ -> (
      match resolve_baker baker_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok svc ->
          tick_one_baker svc ;
          `Ok ())
  | None -> (
      match list_baker_services () with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok [] ->
          Printf.printf "No baker instances found.\n" ;
          `Ok ()
      | Ok bakers ->
          List.iter tick_one_baker bakers ;
          `Ok ())

let tick_cmd =
  let info =
    Cmd.info
      "tick"
      ~doc:
        "Run one continual payout tick. Checks due cycles, manages delay, \
         and pays when ready. Designed for cron/systemd timer invocation."
  in
  Cmd.v info Term.(ret (const tick_run $ baker_arg))

(* ── Systemd timer helpers ─────────────────────────────────── *)

let timer_unit_name = "octez-manager-continual"

let timer_unit_path () =
  if Paths.is_root () then
    Printf.sprintf "/etc/systemd/system/%s.timer" timer_unit_name
  else
    Filename.concat
      (Filename.concat (Paths.xdg_config_home ()) "systemd/user")
      (Printf.sprintf "%s.timer" timer_unit_name)

let timer_is_active () =
  let cmd = Systemd.systemctl_cmd () in
  match
    Cmd_runner.run_silent
      (cmd @ ["is-active"; "--quiet"; timer_unit_name ^ ".timer"])
  with
  | Ok () -> true
  | Error _ -> false

let timer_is_installed () = Sys.file_exists (timer_unit_path ())

let service_unit_path () =
  if Paths.is_root () then
    Printf.sprintf "/etc/systemd/system/%s.service" timer_unit_name
  else
    Filename.concat
      (Filename.concat (Paths.xdg_config_home ()) "systemd/user")
      (Printf.sprintf "%s.service" timer_unit_name)

let resolve_exe_path () =
  match Paths.which "octez-manager" with
  | Some path -> path
  | None -> (
      try Unix.readlink (Printf.sprintf "/proc/%d/exe" (Unix.getpid ()))
      with _ -> Sys.argv.(0))

let generate_service_unit () =
  let exe = resolve_exe_path () in
  Printf.sprintf
    {|[Unit]
Description=octez-manager continual payout tick

[Service]
Type=oneshot
ExecStart=%s rewards continual tick
|}
    exe

let generate_timer_unit () =
  {|[Unit]
Description=Periodic continual payout tick

[Timer]
OnBootSec=2min
OnUnitActiveSec=5min
RandomizedDelaySec=30s
Persistent=true

[Install]
WantedBy=timers.target
|}

let systemctl_cmd () =
  if Paths.is_root () then ["systemctl"] else ["systemctl"; "--user"]

let rec mkdir_p path =
  if Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path) ;
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write_unit_file path content =
  mkdir_p (Filename.dirname path) ;
  let oc = open_out path in
  output_string oc content ;
  close_out oc

let install_timer () =
  let svc_path = service_unit_path () in
  let tmr_path = timer_unit_path () in
  write_unit_file svc_path (generate_service_unit ()) ;
  Printf.printf "Wrote %s\n" svc_path ;
  write_unit_file tmr_path (generate_timer_unit ()) ;
  Printf.printf "Wrote %s\n" tmr_path ;
  (match Cmd_runner.run ~quiet:true (systemctl_cmd () @ ["daemon-reload"]) with
  | Ok () -> ()
  | Error (`Msg msg) ->
      Printf.eprintf "Warning: daemon-reload failed: %s\n" msg) ;
  let timer_name = timer_unit_name ^ ".timer" in
  match
    Cmd_runner.run ~quiet:true (systemctl_cmd () @ ["enable"; "--now"; timer_name])
  with
  | Ok () ->
      Printf.printf "Timer %s enabled and started.\n" timer_name ;
      Ok ()
  | Error (`Msg msg) -> Error msg

let uninstall_timer () =
  let svc_path = service_unit_path () in
  let tmr_path = timer_unit_path () in
  let timer_name = timer_unit_name ^ ".timer" in
  (match
     Cmd_runner.run
       ~quiet:true
       (systemctl_cmd () @ ["disable"; "--now"; timer_name])
   with
  | Ok () ->
      Printf.printf "Timer %s disabled and stopped.\n" timer_name
  | Error (`Msg msg) ->
      Printf.eprintf "Warning: disable failed: %s\n" msg) ;
  if Sys.file_exists svc_path then (
    Sys.remove svc_path ;
    Printf.printf "Removed %s\n" svc_path) ;
  if Sys.file_exists tmr_path then (
    Sys.remove tmr_path ;
    Printf.printf "Removed %s\n" tmr_path) ;
  (match Cmd_runner.run ~quiet:true (systemctl_cmd () @ ["daemon-reload"]) with
  | Ok () -> ()
  | Error (`Msg msg) ->
      Printf.eprintf "Warning: daemon-reload failed: %s\n" msg)

(* ── rewards continual start/stop/status ──────────────────── *)

let continual_start_run baker_opt interval offset =
  setup_indexer_logging () ;
  match resolve_baker baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok svc -> (
      match baker_delegate svc with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok baker_pkh -> (
          let instance = svc.Service.instance in
          let config =
            match Payout_config.load ~instance with
            | Ok c -> c
            | Error _ -> Payout_config.default ~network:svc.Service.network ~baker_pkh ()
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
              | Ok () ->
                  Payout_continual.enable ~instance ;
                  Printf.printf "Continual mode enabled for %s.\n" instance ;
                  Printf.printf "  Interval: every %d cycle(s)\n" interval ;
                  if offset > 0 then Printf.printf "  Offset: %d\n" offset ;
                  if not (timer_is_active ()) then (
                    Printf.printf "\nInstalling systemd timer...\n" ;
                    match install_timer () with
                    | Ok () -> `Ok ()
                    | Error msg ->
                        Printf.eprintf
                          "Warning: timer install failed: %s\n" msg ;
                        Printf.eprintf
                          "  Payouts will only run while the TUI is open.\n" ;
                        `Ok ())
                  else (
                    Printf.printf "\nSystemd timer already active.\n" ;
                    `Ok ()))))

let continual_stop_run baker_opt =
  setup_indexer_logging () ;
  match resolve_baker baker_opt with
  | Error msg -> Cli_helpers.cmdliner_error msg
  | Ok svc -> (
      match baker_delegate svc with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok baker_pkh -> (
          let instance = svc.instance in
          let config =
            match Payout_config.load ~instance with
            | Ok c -> c
            | Error _ -> Payout_config.default ~network:svc.Service.network ~baker_pkh ()
          in
          let config = {config with continual_enabled = false} in
          match Payout_config.save ~instance config with
          | Error msg -> Cli_helpers.cmdliner_error msg
          | Ok () ->
              Payout_continual.disable ~instance ;
              Printf.printf "Continual mode disabled for %s.\n" instance ;
              (* Uninstall timer if no other baker has continual enabled *)
              let others_enabled =
                match list_baker_services () with
                | Ok bakers ->
                    List.exists
                      (fun (b : Service.t) ->
                        (not (String.equal b.instance instance))
                        && match Payout_config.load ~instance:b.instance with
                           | Ok c -> c.continual_enabled
                           | Error _ -> false)
                      bakers
                | Error _ -> false
              in
              if (not others_enabled) && timer_is_installed () then (
                Printf.printf "\nNo bakers have continual mode enabled.\n" ;
                Printf.printf "Removing systemd timer...\n" ;
                uninstall_timer ()) ;
              `Ok ()))

let continual_status_run baker_opt =
  setup_indexer_logging () ;
  let show_baker_status (svc : Service.t) =
    match baker_delegate svc with
    | Error msg ->
        Printf.eprintf "Error [%s]: %s\n" svc.instance msg
    | Ok baker_pkh ->
        let instance = svc.instance in
        let config =
          match Payout_config.load ~instance with
          | Ok c -> c
          | Error _ -> Payout_config.default ~network:svc.Service.network ~baker_pkh ()
        in
        Printf.printf "Baker: %s (%s)\n" instance baker_pkh ;
        Printf.printf
          "  Continual mode: %s\n"
          (if config.continual_enabled then "enabled" else "disabled") ;
        Printf.printf
          "  Interval: every %d cycle(s)\n"
          config.continual_interval ;
        if config.continual_offset > 0 then
          Printf.printf "  Offset: %d\n" config.continual_offset ;
        Printf.printf
          "  Delay: %d-%d blocks\n"
          config.min_delay_blocks
          config.max_delay_blocks ;
        (match Payout_continual.read_delay_until ~instance with
        | Some until ->
            let now = Unix.gettimeofday () in
            if Float.compare until now > 0 then
              Printf.printf
                "  Pending delay: %.0fs remaining\n"
                (until -. now)
            else
              Printf.printf "  Pending delay: expired (will pay on next tick)\n"
        | None ->
            Printf.printf "  Pending delay: none\n") ;
        Printf.printf "\n"
  in
  match baker_opt with
  | Some _ -> (
      match resolve_baker baker_opt with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok svc ->
          show_baker_status svc ;
          `Ok ())
  | None -> (
      match list_baker_services () with
      | Error msg -> Cli_helpers.cmdliner_error msg
      | Ok [] ->
          Printf.printf "No baker instances found.\n" ;
          `Ok ()
      | Ok bakers ->
          List.iter show_baker_status bakers ;
          `Ok ())

let continual_start_cmd =
  let info =
    Cmd.info
      "start"
      ~doc:
        "Enable continual payouts and install the systemd timer. \
         Automatically pays due cycles every 5 minutes."
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
  let info =
    Cmd.info
      "stop"
      ~doc:
        "Disable continual payouts. Removes the systemd timer when no \
         bakers have continual mode enabled."
  in
  Cmd.v info Term.(ret (const continual_stop_run $ baker_arg))

let continual_status_cmd =
  let info = Cmd.info "status" ~doc:"Show continual mode and timer status." in
  Cmd.v info Term.(ret (const continual_status_run $ baker_arg))

let continual_cmd =
  let info =
    Cmd.info "continual" ~doc:"Manage continual (automatic) payouts."
  in
  Cmd.group info [continual_start_cmd; continual_stop_cmd; continual_status_cmd; tick_cmd]

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
