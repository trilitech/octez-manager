(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker wallet operations via octez-client.

    Builds and executes octez-client commands for delegate operations:
    register, stake, unstake, finalize unstake, transfer,
    set delegate parameters, update consensus key, and governance voting. *)

(* ── Types ─────────────────────────────────────────────────── *)

type wallet_operation =
  | Register
  | Stake of {amount : string}
  | Unstake of {amount : string}
  | Finalize_unstake
  | Transfer of {amount : string; destination : string}
  | Set_delegate_params of {limit : int; edge : int}
  | Update_consensus_key of {key : string}
  | Submit_proposals of {proposals : string list}
  | Submit_ballot of {proposal : string; ballot : Baker_wallet_data.ballot_vote}

type operation_result = {
  success : bool;
  op_hash : string option;
  error : string option;
}

(* ── Command Building ──────────────────────────────────────── *)

let build_command ~octez_client_bin ~endpoint ~base_dir ~password_file ~alias
    ~op =
  let base =
    [octez_client_bin]
    @ (match base_dir with Some d -> ["--base-dir"; d] | None -> [])
    @ (match password_file with
      | Some f -> ["--password-filename"; f]
      | None -> [])
    @ ["--endpoint"; endpoint]
  in
  let cmd =
    match op with
    | Register -> ["register"; "key"; alias; "as"; "delegate"]
    | Stake {amount} -> ["stake"; amount; "for"; alias]
    | Unstake {amount} -> ["unstake"; amount; "for"; alias]
    | Finalize_unstake -> ["finalize"; "unstake"; "for"; alias]
    | Transfer {amount; destination} ->
        ["transfer"; amount; "from"; alias; "to"; destination]
    | Set_delegate_params {limit; edge} ->
        [
          "set";
          "delegate";
          "parameters";
          "for";
          alias;
          "--limit-of-staking-over-baking";
          string_of_int limit;
          "--edge-of-baking-over-staking";
          string_of_int edge;
        ]
    | Update_consensus_key {key} ->
        ["update"; "consensus"; "key"; "for"; alias; "to"; key]
    | Submit_proposals {proposals} ->
        ["submit"; "proposals"; "for"; alias] @ proposals
    | Submit_ballot {proposal; ballot} ->
        [
          "submit";
          "ballot";
          "for";
          alias;
          proposal;
          Baker_wallet_data.string_of_ballot_vote ballot;
        ]
  in
  base @ cmd @ ["--burn-cap"; "1"]

(* ── Output Parsing ────────────────────────────────────────── *)

let extract_op_hash output =
  (* octez-client prints the operation hash on a line like:
     "Operation hash is 'oo...'" or just the hash on a line *)
  let lines = String.split_on_char '\n' output in
  let rec find = function
    | [] -> None
    | line :: rest -> (
        let trimmed = String.trim line in
        if String.length trimmed > 2 && trimmed.[0] = 'o' && trimmed.[1] = 'o'
        then Some trimmed
        else
          match String.split_on_char '\'' trimmed with
          | _ :: hash :: _ when String.length hash > 2 && hash.[0] = 'o' ->
              Some hash
          | _ -> find rest)
  in
  find lines

let extract_fee_estimate output =
  (* Look for fee information in dry-run output.
     Typical line: "  Fee to the baker: ꜩ0.001234" or
     "Estimated gas: ... Fee: 0.001234" *)
  let lines = String.split_on_char '\n' output in
  let rec find = function
    | [] -> None
    | line :: rest ->
        let trimmed = String.trim line in
        if
          (try
             let _ = Str.search_forward (Str.regexp_string "Fee") trimmed 0 in
             true
           with Not_found -> false)
          ||
            try
              let _ = Str.search_forward (Str.regexp_string "fee") trimmed 0 in
              true
            with Not_found -> false
        then
          (* Extract the tez amount after ꜩ or the last number *)
          let parts = String.split_on_char ' ' trimmed in
          let rec find_amount = function
            | [] -> None
            | part :: rest_parts -> (
                let cleaned =
                  String.to_seq part
                  |> Seq.filter (fun c ->
                      (c >= '0' && c <= '9') || c = '.' || c = '-')
                  |> String.of_seq
                in
                match float_of_string_opt cleaned with
                | Some _ when String.length cleaned > 0 -> Some cleaned
                | _ -> find_amount rest_parts)
          in
          match find_amount (List.rev parts) with
          | Some fee -> Some fee
          | None -> find rest
        else find rest
  in
  find lines

(* ── Execution ─────────────────────────────────────────────── *)

let execute ~instance_name:_ ~octez_client_bin ~endpoint ~base_dir
    ~password_file ~alias ~op =
  let argv =
    build_command
      ~octez_client_bin
      ~endpoint
      ~base_dir
      ~password_file
      ~alias
      ~op
  in
  match Cmd_runner.run_out_with_timeout ~timeout:100.0 argv with
  | Ok output -> (
      match extract_op_hash output with
      | Some hash -> {success = true; op_hash = Some hash; error = None}
      | None ->
          {
            success = true;
            op_hash = None;
            error = Some "Operation succeeded but no hash found in output";
          })
  | Error (`Msg err) -> {success = false; op_hash = None; error = Some err}

let estimate_fee ~instance_name:_ ~octez_client_bin ~endpoint ~base_dir
    ~password_file ~alias ~op =
  let argv =
    build_command
      ~octez_client_bin
      ~endpoint
      ~base_dir
      ~password_file
      ~alias
      ~op
    @ ["--dry-run"]
  in
  match Cmd_runner.run_out_with_timeout ~timeout:30.0 argv with
  | Ok output -> (
      match extract_fee_estimate output with
      | Some fee -> Ok fee
      | None -> Ok "~0.001")
  | Error (`Msg err) -> Error err

(* ── Helpers ───────────────────────────────────────────────── *)

let describe_operation = function
  | Register -> "Register as Delegate"
  | Stake {amount} -> Printf.sprintf "Stake %s ꜩ" amount
  | Unstake {amount} ->
      if String.equal amount "everything" then "Unstake everything"
      else Printf.sprintf "Unstake %s ꜩ" amount
  | Finalize_unstake -> "Finalize Unstake"
  | Transfer {amount; destination} ->
      Printf.sprintf "Transfer %s ꜩ to %s" amount destination
  | Set_delegate_params {limit; edge} ->
      Printf.sprintf
        "Set Delegate Parameters (limit: %s, edge: %s)"
        (Baker_wallet_data.format_staking_limit limit)
        (Baker_wallet_data.format_baking_edge edge)
  | Update_consensus_key {key} ->
      Printf.sprintf "Update Consensus Key to %s" key
  | Submit_proposals {proposals} ->
      Printf.sprintf
        "Submit Proposal%s: %s"
        (if List.length proposals > 1 then "s" else "")
        (String.concat ", " proposals)
  | Submit_ballot {proposal; ballot} ->
      Printf.sprintf
        "Submit Ballot: %s for %s"
        (Baker_wallet_data.string_of_ballot_vote ballot)
        proposal
