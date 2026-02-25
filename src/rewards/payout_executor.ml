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

(* ── Command building ──────────────────────────────────────── *)

let build_transfer_cmd ~ctx ~amount_mutez ~destination ~dry_run =
  let amount_tez =
    Printf.sprintf "%Ld" amount_mutez |> fun s ->
    let len = String.length s in
    if len <= 6 then "0." ^ String.make (6 - len) '0' ^ s
    else String.sub s 0 (len - 6) ^ "." ^ String.sub s (len - 6) 6
  in
  let base =
    [ctx.octez_client_bin]
    @ (match ctx.base_dir with Some d -> ["--base-dir"; d] | None -> [])
    @ (match ctx.password_file with
      | Some f -> ["--password-filename"; f]
      | None -> [])
    @ ["--endpoint"; ctx.endpoint]
  in
  let cmd =
    base
    @ [
        "transfer";
        amount_tez;
        "from";
        ctx.payout_key_alias;
        "to";
        destination;
        "--burn-cap";
        "0.257";
      ]
  in
  if dry_run then cmd @ ["--dry-run"] else cmd

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

(* ── Single transfer execution ─────────────────────────────── *)

let execute_transfer ~ctx ~delegator ~recipient ~amount ~dry_run =
  let argv =
    build_transfer_cmd ~ctx ~amount_mutez:amount ~destination:recipient ~dry_run
  in
  match Cmd_runner.run_out_with_timeout ~timeout:120.0 argv with
  | Ok output ->
      let op_hash = if dry_run then None else extract_op_hash output in
      {
        Rewards.delegator;
        recipient;
        amount;
        op_hash;
        success = true;
        note = (if dry_run then "dry-run" else "ok");
      }
  | Error (`Msg err) ->
      {
        Rewards.delegator;
        recipient;
        amount;
        op_hash = None;
        success = false;
        note = err;
      }

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

(* ── Main execution ────────────────────────────────────────── *)

let execute ~ctx ~(blueprint : Rewards.payout_blueprint) ?(dry_run = false)
    ?on_progress () =
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
        let payouts = collect_payouts blueprint in
        let total = List.length payouts in
        let results =
          List.mapi
            (fun i (delegator, recipient, amount) ->
              let result =
                execute_transfer ~ctx ~delegator ~recipient ~amount ~dry_run
              in
              (match on_progress with
              | Some cb -> cb {current = i + 1; total; delegator; result}
              | None -> ()) ;
              result)
            payouts
        in
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
            own_staked_balance = 0L;
            own_delegated_balance = 0L;
            external_staked_balance = 0L;
            external_delegated_balance = 0L;
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
