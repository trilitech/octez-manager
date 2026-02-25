(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

let rec mkdir_p path =
  if Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path) ;
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let report_dir ~instance ~cycle =
  let base = Payout_config.rewards_dir ~instance in
  Filename.concat (Filename.concat base "reports") (string_of_int cycle)

let dry_report_dir ~instance ~cycle =
  let base = Payout_config.rewards_dir ~instance in
  Filename.concat
    (Filename.concat (Filename.concat base "reports") "dry")
    (string_of_int cycle)

(* standard CSV header *)
let csv_header =
  "id,baker,timestamp,cycle,kind,op_kind,contract,token_id,fa_alias,fa_decimals,delegator,delegator_balance,staked_balance,recipient,amount,fee_rate,fee,tx_fee,op_hash,success,note"

let escape_csv_field s =
  if String.contains s ',' || String.contains s '"' then
    "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""
  else s

let write_payouts_csv ~dir ~baker ~cycle results =
  try
    mkdir_p dir ;
    let path = Filename.concat dir "payouts.csv" in
    Out_channel.with_open_text path (fun oc ->
        output_string oc csv_header ;
        output_char oc '\n' ;
        List.iteri
          (fun i (r : Rewards.payout_result) ->
            let timestamp =
              let t = Unix.gettimeofday () in
              let tm = Unix.gmtime t in
              Printf.sprintf
                "%04d-%02d-%02dT%02d:%02d:%02dZ"
                (1900 + tm.tm_year)
                (tm.tm_mon + 1)
                tm.tm_mday
                tm.tm_hour
                tm.tm_min
                tm.tm_sec
            in
            let op_hash = match r.op_hash with Some h -> h | None -> "" in
            let line =
              Printf.sprintf
                "%d,%s,%s,%d,delegator,transaction,,,,,%s,%s,,%s,%Ld,,,,%s,%b,%s"
                (i + 1)
                baker
                timestamp
                cycle
                r.delegator
                (Int64.to_string r.amount)
                r.recipient
                r.amount
                op_hash
                r.success
                (escape_csv_field r.note)
            in
            output_string oc line ;
            output_char oc '\n')
          results) ;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let write_invalid_csv ~dir ~baker ~cycle rewards =
  try
    mkdir_p dir ;
    let path = Filename.concat dir "invalid.csv" in
    Out_channel.with_open_text path (fun oc ->
        output_string oc csv_header ;
        output_char oc '\n' ;
        List.iteri
          (fun i (r : Rewards.delegator_reward) ->
            let line =
              Printf.sprintf
                "%d,%s,,%d,delegator,transaction,,,,,%s,%Ld,%Ld,%s,%Ld,%.6f,%Ld,,,,,%s"
                (i + 1)
                baker
                cycle
                r.delegator
                r.delegated_balance
                r.staked_balance
                r.recipient
                r.net_reward
                r.fee_rate
                r.fee_amount
                (Rewards.string_of_delegator_status r.status)
            in
            output_string oc line ;
            output_char oc '\n')
          rewards) ;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let summary_to_json (s : Rewards.cycle_summary) =
  `Assoc
    [
      ("cycle", `Int s.cycle);
      ("delegators", `Int s.delegators);
      ("paid_delegators", `Int s.paid_delegators);
      ("own_staked_balance", `String (Int64.to_string s.own_staked_balance));
      ( "own_delegated_balance",
        `String (Int64.to_string s.own_delegated_balance) );
      ( "external_staked_balance",
        `String (Int64.to_string s.external_staked_balance) );
      ( "external_delegated_balance",
        `String (Int64.to_string s.external_delegated_balance) );
      ("earned_rewards", `String (Int64.to_string s.earned_rewards));
      ("earned_block_fees", `String (Int64.to_string s.earned_block_fees));
      ("distributed_rewards", `String (Int64.to_string s.distributed_rewards));
      ("bond_income", `String (Int64.to_string s.bond_income));
      ("fee_income", `String (Int64.to_string s.fee_income));
      ("tx_fees_paid", `String (Int64.to_string s.tx_fees_paid));
      ("timestamp", `String s.timestamp);
    ]

let write_summary_json ~dir summary =
  try
    mkdir_p dir ;
    let path = Filename.concat dir "summary.json" in
    let json = summary_to_json summary in
    let content = Yojson.Safe.pretty_to_string ~std:true json in
    Out_channel.with_open_text path (fun oc ->
        output_string oc content ;
        output_char oc '\n') ;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let int64_of_json_field json field =
  let open Yojson.Safe.Util in
  match member field json with
  | `String s -> Int64.of_string s
  | `Int i -> Int64.of_int i
  | `Intlit s -> Int64.of_string s
  | _ -> 0L

let summary_of_json json =
  let open Yojson.Safe.Util in
  {
    Rewards.cycle = member "cycle" json |> to_int;
    delegators = member "delegators" json |> to_int;
    paid_delegators = member "paid_delegators" json |> to_int;
    own_staked_balance = int64_of_json_field json "own_staked_balance";
    own_delegated_balance = int64_of_json_field json "own_delegated_balance";
    external_staked_balance = int64_of_json_field json "external_staked_balance";
    external_delegated_balance =
      int64_of_json_field json "external_delegated_balance";
    earned_rewards = int64_of_json_field json "earned_rewards";
    earned_block_fees = int64_of_json_field json "earned_block_fees";
    distributed_rewards = int64_of_json_field json "distributed_rewards";
    bond_income = int64_of_json_field json "bond_income";
    fee_income = int64_of_json_field json "fee_income";
    tx_fees_paid = int64_of_json_field json "tx_fees_paid";
    timestamp = member "timestamp" json |> to_string;
  }

let read_summary_json ~instance ~cycle =
  let dir = report_dir ~instance ~cycle in
  let path = Filename.concat dir "summary.json" in
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "no summary for cycle %d" cycle)
  else
    try
      let content = In_channel.with_open_text path In_channel.input_all in
      let json = Yojson.Safe.from_string content in
      Ok (summary_of_json json)
    with
    | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
    | exn -> Error (Printexc.to_string exn)

let cycle_is_paid ~instance ~cycle =
  let dir = report_dir ~instance ~cycle in
  let path = Filename.concat dir "summary.json" in
  Sys.file_exists path

let list_paid_cycles ~instance =
  let base = Payout_config.rewards_dir ~instance in
  let reports_dir = Filename.concat base "reports" in
  if not (Sys.file_exists reports_dir) then []
  else
    try
      Sys.readdir reports_dir |> Array.to_list
      |> List.filter_map (fun name ->
          match int_of_string name with
          | cycle ->
              let summary =
                Filename.concat
                  (Filename.concat reports_dir name)
                  "summary.json"
              in
              if Sys.file_exists summary then Some cycle else None
          | exception _ -> None)
      |> List.sort (fun a b -> Int.compare b a)
    with _ -> []
