(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Payout executor: broadcasts transfer operations for a payout blueprint. *)

type context = {
  octez_client_bin : string;
  endpoint : string;
  base_dir : string option;
  password_file : string option;
  payout_key_alias : string;
  instance : string;
}

type progress = {
  current : int;
  total : int;
  delegator : string;
  result : Rewards.payout_result;
}

(* ── Helpers ───────────────────────────────────────────────── *)

let base_argv ctx =
  [ctx.octez_client_bin]
  @ (match ctx.base_dir with Some d -> ["--base-dir"; d] | None -> [])
  @ (match ctx.password_file with
    | Some f -> ["--password-filename"; f]
    | None -> [])
  @ ["--endpoint"; ctx.endpoint]

(* ── Output parsing ────────────────────────────────────────── *)

let extract_op_hash output =
  let lines = String.split_on_char '\n' output in
  let rec find = function
    | [] -> None
    | line :: rest -> (
        let trimmed = String.trim line in
        if String.length trimmed > 2 && trimmed.[0] = 'o' && trimmed.[1] = 'o'
        then Some trimmed
        else
          match String.split_on_char '\'' trimmed with
          | _ :: hash :: _
            when String.length hash > 2 && hash.[0] = 'o' && hash.[1] = 'o' ->
              Some hash
          | _ -> find rest)
  in
  find lines

(* ── Wallet balance ────────────────────────────────────────── *)

let fetch_wallet_balance ~ctx =
  let argv =
    [ctx.octez_client_bin]
    @ (match ctx.base_dir with Some d -> ["--base-dir"; d] | None -> [])
    @ [
        "--endpoint"; ctx.endpoint; "get"; "balance"; "for"; ctx.payout_key_alias;
      ]
  in
  match Cmd_runner.run_out_with_timeout ~timeout:30.0 argv with
  | Error (`Msg msg) -> Error msg
  | Ok output -> (
      (* Output is like "1234.567890 ꜩ" *)
      let trimmed = String.trim output in
      let parts = String.split_on_char ' ' trimmed in
      match parts with
      | tez_str :: _ -> (
          match float_of_string_opt tez_str with
          | Some tez -> Ok (Int64.of_float (tez *. 1_000_000.0))
          | None -> Error (Printf.sprintf "Cannot parse balance: %s" trimmed))
      | [] -> Error "Empty balance output")

(* ── Build payout list ─────────────────────────────────────── *)

let collect_payouts (bp : Rewards.payout_blueprint) =
  let delegator_payouts =
    List.filter_map
      (fun (r : Rewards.delegator_reward) ->
        match r.status with
        | Rewards.Eligible when Int64.compare r.net_reward 0L > 0 ->
            Some (r.delegator, r.recipient, r.net_reward)
        | _ -> None)
      bp.delegator_rewards
  in
  let bond_payouts =
    List.filter_map
      (fun (addr, amount) ->
        if Int64.compare amount 0L > 0 then Some (addr, addr, amount) else None)
      bp.bond_payouts
  in
  let fee_payouts =
    List.filter_map
      (fun (addr, amount) ->
        if Int64.compare amount 0L > 0 then Some (addr, addr, amount) else None)
      bp.fee_payouts
  in
  delegator_payouts @ bond_payouts @ fee_payouts

(* ── Batch transfer support ───────────────────────────────── *)

let write_batch_file payouts =
  let json =
    `List
      (List.map
         (fun (_delegator, recipient, amount) ->
           `Assoc
             [
               ("destination", `String recipient);
               ("amount", `String (Rewards.tez_of_mutez amount));
             ])
         payouts)
  in
  let path = Filename.temp_file "om-payout-" ".json" in
  Yojson.Safe.to_file path json ;
  path

let build_batch_cmd ~ctx ~batch_file ~batch_len ~dry_run =
  let burn_cap = Printf.sprintf "%.2f" (Float.of_int batch_len *. 0.28) in
  let cmd =
    base_argv ctx
    @ [
        "multiple";
        "transfers";
        "from";
        ctx.payout_key_alias;
        "using";
        batch_file;
        "--burn-cap";
        burn_cap;
      ]
  in
  if dry_run then cmd @ ["--dry-run"] else cmd

let execute_batch ~ctx ~payouts ~dry_run =
  let batch_file = write_batch_file payouts in
  let batch_len = List.length payouts in
  let argv = build_batch_cmd ~ctx ~batch_file ~batch_len ~dry_run in
  let cmd_result = Cmd_runner.run_out_with_timeout ~timeout:300.0 argv in
  (try Sys.remove batch_file with Sys_error _ -> ()) ;
  match cmd_result with
  | Ok output ->
      let op_hash = if dry_run then None else extract_op_hash output in
      List.map
        (fun (delegator, recipient, amount) ->
          {
            Rewards.delegator;
            recipient;
            amount;
            op_hash;
            success = true;
            note = (if dry_run then "dry-run" else "ok");
          })
        payouts
  | Error (`Msg err) ->
      List.map
        (fun (delegator, recipient, amount) ->
          {
            Rewards.delegator;
            recipient;
            amount;
            op_hash = None;
            success = false;
            note = err;
          })
        payouts

let rec take n lst =
  if n <= 0 then []
  else match lst with [] -> [] | x :: rest -> x :: take (n - 1) rest

let rec drop n lst =
  if n <= 0 then lst
  else match lst with [] -> [] | _ :: rest -> drop (n - 1) rest

let chunk_list ~size lst =
  let rec aux acc remaining =
    match remaining with
    | [] -> List.rev acc
    | _ -> aux (take size remaining :: acc) (drop size remaining)
  in
  aux [] lst

(* ── Main execution ────────────────────────────────────────── *)

let execute ~ctx ~(blueprint : Rewards.payout_blueprint) ?(dry_run = false)
    ?on_progress ?(batch_size = 80) () =
  (* Early guard: payout key must be configured *)
  if String.length (String.trim ctx.payout_key_alias) = 0 then
    Error "payout_key_alias is not configured"
  else
    let cycle = blueprint.cycle in
    let instance = ctx.instance in
    (* Check double-payment *)
    if (not dry_run) && Payout_report.cycle_is_paid ~instance ~cycle then
      Error (Printf.sprintf "Cycle %d has already been paid." cycle)
    else
      let report_dir =
        if dry_run then Payout_report.dry_report_dir ~instance ~cycle
        else Payout_report.report_dir ~instance ~cycle
      in
      File_ops.mkdir_p report_dir ;
      let lock_path = Filename.concat report_dir ".lock" in
      File_ops.with_file_lock lock_path (fun () ->
          let payouts = collect_payouts blueprint in
          let total = List.length payouts in
          let chunks = chunk_list ~size:batch_size payouts in
          let results = ref [] in
          let processed = ref 0 in
          let consecutive_failures = ref 0 in
          let aborted = ref false in
          List.iter
            (fun chunk ->
              if not !aborted then begin
                let batch_results =
                  execute_batch ~ctx ~payouts:chunk ~dry_run
                in
                let batch_ok =
                  List.exists
                    (fun (r : Rewards.payout_result) -> r.success)
                    batch_results
                in
                if batch_ok then consecutive_failures := 0
                else consecutive_failures := !consecutive_failures + 1 ;
                (* Fire per-transfer progress callbacks *)
                List.iter
                  (fun result ->
                    processed := !processed + 1 ;
                    results := result :: !results ;
                    match on_progress with
                    | Some cb ->
                        cb
                          {
                            current = !processed;
                            total;
                            delegator = result.Rewards.delegator;
                            result;
                          }
                    | None -> ())
                  batch_results ;
                (* Abort after 2 consecutive failed batches *)
                if !consecutive_failures >= 2 then aborted := true
              end)
            chunks ;
          (* Mark remaining payouts as aborted if we stopped early *)
          if !aborted then begin
            let remaining_count = total - !processed in
            if remaining_count > 0 then begin
              let remaining_payouts = drop !processed payouts in
              List.iter
                (fun (delegator, recipient, amount) ->
                  processed := !processed + 1 ;
                  let result =
                    {
                      Rewards.delegator;
                      recipient;
                      amount;
                      op_hash = None;
                      success = false;
                      note = "aborted: consecutive batch failures";
                    }
                  in
                  results := result :: !results ;
                  match on_progress with
                  | Some cb ->
                      cb {current = !processed; total; delegator; result}
                  | None -> ())
                remaining_payouts
            end
          end ;
          let results = List.rev !results in
          let succeeded =
            List.filter (fun (r : Rewards.payout_result) -> r.success) results
          in
          let distributed =
            List.fold_left
              (fun acc (r : Rewards.payout_result) ->
                if r.success then Int64.add acc r.amount else acc)
              0L
              results
          in
          let tx_fees_paid = blueprint.estimated_tx_fees in
          let summary : Rewards.cycle_summary =
            {
              cycle;
              delegators = blueprint.total_delegators;
              paid_delegators = List.length succeeded;
              own_staked_balance = blueprint.own_staked_balance;
              own_delegated_balance = blueprint.own_delegated_balance;
              external_staked_balance = blueprint.external_staked_balance;
              external_delegated_balance = blueprint.external_delegated_balance;
              earned_rewards = blueprint.earned_rewards;
              earned_block_fees = blueprint.earned_block_fees;
              distributed_rewards = distributed;
              bond_income = blueprint.baker_bond_income;
              fee_income = blueprint.baker_fee_income;
              tx_fees_paid;
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
          (* Write reports *)
          let invalid =
            List.filter
              (fun (r : Rewards.delegator_reward) ->
                match r.status with Rewards.Eligible -> false | _ -> true)
              blueprint.delegator_rewards
          in
          let _ =
            Payout_report.write_payouts_csv
              ~dir:report_dir
              ~baker:blueprint.baker
              ~cycle
              results
          in
          let _ =
            Payout_report.write_invalid_csv
              ~dir:report_dir
              ~baker:blueprint.baker
              ~cycle
              invalid
          in
          let _ = Payout_report.write_summary_json ~dir:report_dir summary in
          Ok (results, summary))

module Internal_for_tests = struct
  let extract_op_hash = extract_op_hash
end
