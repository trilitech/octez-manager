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

(* Replace newlines and tabs with spaces so a single CSV field cannot break
   the row layout. CSV technically supports newlines inside quoted fields,
   but several of our consumers (and shell tooling) parse line-by-line. *)
let sanitize_csv_field s =
  String.map (fun c -> match c with '\n' | '\r' | '\t' -> ' ' | _ -> c) s

let escape_csv_field s =
  let s = sanitize_csv_field s in
  if String.contains s ',' || String.contains s '"' then
    "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""
  else s

let write_payouts_csv ~dir ~baker ~cycle results =
  try
    mkdir_p dir ;
    let path = Filename.concat dir "payouts.csv" in
    let oc = open_out path in
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
      results ;
    close_out oc ;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let write_invalid_csv ~dir ~baker ~cycle rewards =
  try
    mkdir_p dir ;
    let path = Filename.concat dir "invalid.csv" in
    let oc = open_out path in
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
      rewards ;
    close_out oc ;
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
    let oc = open_out path in
    output_string oc content ;
    output_char oc '\n' ;
    close_out oc ;
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
      let ic = open_in path in
      let content = In_channel.input_all ic in
      close_in ic ;
      let json = Yojson.Safe.from_string content in
      Ok (summary_of_json json)
    with
    | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
    | exn -> Error (Printexc.to_string exn)

(* Parse the whole file into logical CSV records. Newlines inside double-quoted
   fields are kept as part of the field — required to recover reports written
   by older builds that did not sanitize multi-line octez-client errors. *)
let parse_csv_content content =
  let n = String.length content in
  let records = ref [] in
  let current_fields = ref [] in
  let buf = Buffer.create 64 in
  let in_quote = ref false in
  let push_field () =
    current_fields := Buffer.contents buf :: !current_fields ;
    Buffer.clear buf
  in
  let push_record () =
    push_field () ;
    records := List.rev !current_fields :: !records ;
    current_fields := []
  in
  let i = ref 0 in
  while !i < n do
    let c = content.[!i] in
    if !in_quote then
      if c = '"' then
        if !i + 1 < n && content.[!i + 1] = '"' then begin
          Buffer.add_char buf '"' ;
          incr i
        end
        else in_quote := false
      else Buffer.add_char buf c
    else if c = '"' && Buffer.length buf = 0 then in_quote := true
    else if c = ',' then push_field ()
    else if c = '\n' || c = '\r' then begin
      if c = '\r' && !i + 1 < n && content.[!i + 1] = '\n' then incr i ;
      if List.length !current_fields > 0 || Buffer.length buf > 0 then
        push_record ()
    end
    else Buffer.add_char buf c ;
    incr i
  done ;
  if List.length !current_fields > 0 || Buffer.length buf > 0 then
    push_record () ;
  List.rev !records

let read_csv_rows path =
  let ic = open_in path in
  let content = In_channel.input_all ic in
  close_in ic ;
  match parse_csv_content content with _header :: rest -> rest | [] -> []

let nth_or_empty fields i =
  match List.nth_opt fields i with Some s -> s | None -> ""

let int64_or_zero s = try Int64.of_string s with _ -> 0L

let parse_delegator_status = function
  | "eligible" -> Rewards.Eligible
  | "below min payout" -> Rewards.Below_minimum_payout
  | "below min balance" -> Rewards.Below_minimum_balance
  | "ignored" -> Rewards.Ignored
  | "emptied" -> Rewards.Emptied
  | _ -> Rewards.Override_excluded

let read_payouts_csv ~instance ~cycle =
  let dir = report_dir ~instance ~cycle in
  let path = Filename.concat dir "payouts.csv" in
  if not (Sys.file_exists path) then Ok []
  else
    try
      let rows = read_csv_rows path in
      let parse fields =
        let op_hash_str = nth_or_empty fields 18 in
        {
          Rewards.delegator = nth_or_empty fields 10;
          recipient = nth_or_empty fields 13;
          amount = int64_or_zero (nth_or_empty fields 14);
          op_hash =
            (if String.length op_hash_str = 0 then None else Some op_hash_str);
          success = String.equal (nth_or_empty fields 19) "true";
          note = nth_or_empty fields 20;
        }
      in
      Ok (List.map parse rows)
    with exn -> Error (Printexc.to_string exn)

let read_invalid_csv ~instance ~cycle =
  let dir = report_dir ~instance ~cycle in
  let path = Filename.concat dir "invalid.csv" in
  if not (Sys.file_exists path) then Ok []
  else
    try
      let rows = read_csv_rows path in
      let parse fields =
        let status_str = match List.rev fields with s :: _ -> s | [] -> "" in
        {
          Rewards.delegator = nth_or_empty fields 10;
          delegated_balance = int64_or_zero (nth_or_empty fields 11);
          staked_balance = int64_or_zero (nth_or_empty fields 12);
          gross_reward = 0L;
          fee_rate =
            (try Float.of_string (nth_or_empty fields 15) with _ -> 0.0);
          fee_amount = int64_or_zero (nth_or_empty fields 16);
          net_reward = int64_or_zero (nth_or_empty fields 14);
          recipient = nth_or_empty fields 13;
          status = parse_delegator_status status_str;
        }
      in
      Ok (List.map parse rows)
    with exn -> Error (Printexc.to_string exn)

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

module Internal_for_tests = struct
  let parse_csv_content = parse_csv_content
end
