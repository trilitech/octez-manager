(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Continual payout mode: automatic payouts when new cycles complete. *)

(* ── Cycle matching ──────────────────────────────────────── *)

(** Pure trigger check: is this cycle on an interval boundary? *)
let is_trigger_cycle ~current_cycle ~interval ~offset =
  interval <= 1 || (current_cycle - offset) mod interval = 0

(** Pure cycle collection: given a predicate for "is paid", return due cycles.
    Only looks back [interval] cycles from [current_cycle]. *)
let collect_due_cycles ~current_cycle ~interval ~is_paid =
  let due = ref [] in
  let check_from = max 0 (current_cycle - interval) in
  for c = check_from to current_cycle - 1 do
    if not (is_paid c) then due := c :: !due
  done ;
  List.rev !due

let cycles_due ~instance ~current_cycle ~interval ~offset =
  (* Only trigger on interval boundaries *)
  if not (is_trigger_cycle ~current_cycle ~interval ~offset) then []
  else
    collect_due_cycles ~current_cycle ~interval ~is_paid:(fun c ->
        Payout_report.cycle_is_paid ~instance ~cycle:c)

(* ── Execute due cycles ──────────────────────────────────── *)

let pay_due_cycles ~ctx ~baker ~network ~current_cycle ~interval ~offset =
  let instance = ctx.Payout_executor.instance in
  let config =
    match Payout_config.load ~instance with
    | Ok c -> c
    | Error _ -> Payout_config.default ~baker_pkh:baker
  in
  let due = cycles_due ~instance ~current_cycle ~interval ~offset in
  match due with
  | [] -> []
  | [single_cycle] ->
      (* Single cycle: use existing path for backward compatibility *)
      let result =
        match
          Payout_blueprint.generate
            ~instance
            ~baker
            ~network
            ~cycle:single_cycle
            ~force:false
            ()
        with
        | Error msg -> (0, Error msg)
        | Ok blueprint -> (
            match
              Payout_executor.execute
                ~ctx
                ~blueprint
                ~batch_size:config.sim_batch_size
                ()
            with
            | Error msg -> (0, Error msg)
            | Ok (_results, summary) ->
                (* Send notifications *)
                let channels =
                  match Payout_config.load ~instance with
                  | Ok c -> c.notifications
                  | Error _ -> []
                in
                if channels <> [] then
                  ignore (Payout_notifier.notify_all ~channels ~summary) ;
                let paid_count = summary.Rewards.paid_delegators in
                (paid_count, Ok ()))
      in
      [(single_cycle, result)]
  | multiple_cycles -> (
      (* Multiple cycles: aggregate payouts *)
      (* Step 1: Generate blueprints for all cycles *)
      let blueprints_result =
        List.fold_left
          (fun acc cycle ->
            match acc with
            | Error _ as e -> e
            | Ok bps -> (
                match
                  Payout_blueprint.generate
                    ~instance
                    ~baker
                    ~network
                    ~cycle
                    ~force:false
                    ()
                with
                | Error msg -> Error (cycle, msg)
                | Ok bp -> Ok ((cycle, bp) :: bps)))
          (Ok [])
          multiple_cycles
      in
      match blueprints_result with
      | Error (failed_cycle, msg) ->
          (* If blueprint generation fails, mark all cycles as failed *)
          List.map
            (fun cycle ->
              if cycle = failed_cycle then (cycle, (0, Error msg))
              else
                ( cycle,
                  ( 0,
                    Error
                      (Printf.sprintf
                         "Skipped due to failure in cycle %d"
                         failed_cycle) ) ))
            multiple_cycles
      | Ok blueprints_with_cycles -> (
          let blueprints_with_cycles : (int * Rewards.payout_blueprint) list =
            List.rev blueprints_with_cycles
          in
          (* Step 2: Collect payouts from each blueprint *)
          let all_payouts =
            List.map
              (fun (_cycle, bp) -> Payout_executor.collect_payouts bp)
              blueprints_with_cycles
          in
          (* Step 3: Merge payouts *)
          let merged_payouts = Payout_executor.merge_payouts all_payouts in
          (* Step 4: Execute merged payouts *)
          match
            Payout_executor.execute_merged
              ~ctx
              ~payouts:merged_payouts
              ~batch_size:config.sim_batch_size
              ()
          with
          | Error msg ->
              (* All cycles failed *)
              List.map (fun cycle -> (cycle, (0, Error msg))) multiple_cycles
          | Ok merged_results ->
              (* Step 5: Write per-cycle reports *)
              (* Build a map of (delegator, recipient) -> result *)
              let module PairMap = Map.Make (struct
                type t = string * string

                let compare (d1, r1) (d2, r2) =
                  match String.compare d1 d2 with
                  | 0 -> String.compare r1 r2
                  | c -> c
              end) in
              let result_map =
                List.fold_left
                  (fun map (r : Rewards.payout_result) ->
                    PairMap.add (r.delegator, r.recipient) r map)
                  PairMap.empty
                  merged_results
              in
              (* For each cycle, create per-cycle reports *)
              let per_cycle_results =
                List.map
                  (fun (cycle, blueprint) ->
                    let cycle_payouts =
                      Payout_executor.collect_payouts blueprint
                    in
                    (* Map each cycle payout to its execution result *)
                    let cycle_payout_results =
                      List.map
                        (fun (delegator, recipient, cycle_amount) ->
                          match
                            PairMap.find_opt (delegator, recipient) result_map
                          with
                          | Some merged_result ->
                              (* Use the merged result but with cycle amount *)
                              {merged_result with Rewards.amount = cycle_amount}
                          | None ->
                              (* Should not happen, but handle gracefully *)
                              {
                                Rewards.delegator;
                                recipient;
                                amount = cycle_amount;
                                op_hash = None;
                                success = false;
                                note = "missing from merged execution";
                              })
                        cycle_payouts
                    in
                    let report_dir =
                      Payout_report.report_dir ~instance ~cycle
                    in
                    (* Ensure report directory exists *)
                    let rec ensure_dir path =
                      if Sys.file_exists path then ()
                      else (
                        ensure_dir (Filename.dirname path) ;
                        try Unix.mkdir path 0o755
                        with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
                    in
                    ensure_dir report_dir ;
                    let lock_path = Filename.concat report_dir ".lock" in
                    File_ops.with_file_lock lock_path (fun () ->
                        (* Write per-cycle reports *)
                        let succeeded =
                          List.filter
                            (fun (r : Rewards.payout_result) -> r.success)
                            cycle_payout_results
                        in
                        let distributed =
                          List.fold_left
                            (fun acc (r : Rewards.payout_result) ->
                              if r.success then Int64.add acc r.amount else acc)
                            0L
                            cycle_payout_results
                        in
                        let summary : Rewards.cycle_summary =
                          {
                            cycle;
                            delegators = blueprint.total_delegators;
                            paid_delegators = List.length succeeded;
                            own_staked_balance = 0L;
                            own_delegated_balance = 0L;
                            external_staked_balance = 0L;
                            external_delegated_balance = 0L;
                            earned_rewards = blueprint.earned_rewards;
                            earned_block_fees = blueprint.earned_block_fees;
                            distributed_rewards = distributed;
                            bond_income = blueprint.baker_bond_income;
                            fee_income = blueprint.baker_fee_income;
                            tx_fees_paid = blueprint.estimated_tx_fees;
                            timestamp =
                              (let tm = Unix.gmtime (Unix.gettimeofday ()) in
                               Printf.sprintf
                                 "%04d-%02d-%02dT%02d:%02d:%02dZ"
                                 (tm.Unix.tm_year + 1900)
                                 (tm.Unix.tm_mon + 1)
                                 tm.Unix.tm_mday
                                 tm.Unix.tm_hour
                                 tm.Unix.tm_min
                                 tm.Unix.tm_sec);
                          }
                        in
                        let invalid =
                          List.filter
                            (fun (r : Rewards.delegator_reward) ->
                              match r.status with
                              | Rewards.Eligible -> false
                              | _ -> true)
                            blueprint.delegator_rewards
                        in
                        let _ =
                          Payout_report.write_payouts_csv
                            ~dir:report_dir
                            ~baker:blueprint.baker
                            ~cycle
                            cycle_payout_results
                        in
                        let _ =
                          Payout_report.write_invalid_csv
                            ~dir:report_dir
                            ~baker:blueprint.baker
                            ~cycle
                            invalid
                        in
                        let _ =
                          Payout_report.write_summary_json
                            ~dir:report_dir
                            summary
                        in
                        (cycle, (List.length succeeded, Ok ()))))
                  blueprints_with_cycles
              in
              (* Step 6: Send one combined notification *)
              let channels =
                match Payout_config.load ~instance with
                | Ok c -> c.notifications
                | Error _ -> []
              in
              if channels <> [] then begin
                (* Use the first cycle's summary for notification *)
                match blueprints_with_cycles with
                | (first_cycle, first_blueprint) :: _ ->
                    let total_distributed =
                      List.fold_left
                        (fun acc (r : Rewards.payout_result) ->
                          if r.success then Int64.add acc r.amount else acc)
                        0L
                        merged_results
                    in
                    let total_paid =
                      List.filter
                        (fun (r : Rewards.payout_result) -> r.success)
                        merged_results
                      |> List.length
                    in
                    let combined_summary : Rewards.cycle_summary =
                      {
                        cycle = first_cycle;
                        delegators = first_blueprint.total_delegators;
                        paid_delegators = total_paid;
                        own_staked_balance = 0L;
                        own_delegated_balance = 0L;
                        external_staked_balance = 0L;
                        external_delegated_balance = 0L;
                        earned_rewards =
                          List.fold_left
                            (fun acc (_c, (bp : Rewards.payout_blueprint)) ->
                              Int64.add acc bp.earned_rewards)
                            0L
                            blueprints_with_cycles;
                        earned_block_fees =
                          List.fold_left
                            (fun acc (_c, (bp : Rewards.payout_blueprint)) ->
                              Int64.add acc bp.earned_block_fees)
                            0L
                            blueprints_with_cycles;
                        distributed_rewards = total_distributed;
                        bond_income =
                          List.fold_left
                            (fun acc (_c, (bp : Rewards.payout_blueprint)) ->
                              Int64.add acc bp.baker_bond_income)
                            0L
                            blueprints_with_cycles;
                        fee_income =
                          List.fold_left
                            (fun acc (_c, (bp : Rewards.payout_blueprint)) ->
                              Int64.add acc bp.baker_fee_income)
                            0L
                            blueprints_with_cycles;
                        tx_fees_paid =
                          List.fold_left
                            (fun acc (_c, (bp : Rewards.payout_blueprint)) ->
                              Int64.add acc bp.estimated_tx_fees)
                            0L
                            blueprints_with_cycles;
                        timestamp =
                          (let tm = Unix.gmtime (Unix.gettimeofday ()) in
                           Printf.sprintf
                             "%04d-%02d-%02dT%02d:%02d:%02dZ"
                             (tm.Unix.tm_year + 1900)
                             (tm.Unix.tm_mon + 1)
                             tm.Unix.tm_mday
                             tm.Unix.tm_hour
                             tm.Unix.tm_min
                             tm.Unix.tm_sec);
                      }
                    in
                    ignore
                      (Payout_notifier.notify_all
                         ~channels
                         ~summary:combined_summary)
                | [] -> ()
              end ;
              per_cycle_results))

module Internal_for_tests = struct
  let is_trigger_cycle = is_trigger_cycle

  let collect_due_cycles ~current_cycle ~is_paid =
    collect_due_cycles ~current_cycle ~interval:20 ~is_paid
end
