(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Notification dispatch after payout completion. *)

(* ── Template rendering ──────────────────────────────────── *)

let format_tez mutez =
  let s = Printf.sprintf "%Ld" mutez in
  let len = String.length s in
  if len <= 6 then "0." ^ String.make (6 - len) '0' ^ s
  else String.sub s 0 (len - 6) ^ "." ^ String.sub s (len - 6) 6

let render_template ~template ~(summary : Rewards.cycle_summary) =
  let replacements =
    [
      ("<Cycle>", string_of_int summary.cycle);
      ("<Delegators>", string_of_int summary.paid_delegators);
      ("<TotalPaid>", format_tez summary.distributed_rewards);
      ("<DistributedRewards>", format_tez summary.distributed_rewards);
      ("<BakerFee>", format_tez summary.fee_income);
      ("<TxFees>", format_tez summary.tx_fees_paid);
      ("<Timestamp>", summary.timestamp);
    ]
  in
  List.fold_left
    (fun acc (placeholder, value) ->
      let replace s =
        match String.split_on_char '<' s with
        | [_] -> s
        | _ ->
            (* Simple replacement — find and replace *)
            let plen = String.length placeholder in
            let slen = String.length s in
            let buf = Buffer.create slen in
            let i = ref 0 in
            while !i <= slen - plen do
              if String.sub s !i plen = placeholder then (
                Buffer.add_string buf value ;
                i := !i + plen)
              else (
                Buffer.add_char buf s.[!i] ;
                incr i)
            done ;
            while !i < slen do
              Buffer.add_char buf s.[!i] ;
              incr i
            done ;
            Buffer.contents buf
      in
      replace acc)
    template
    replacements

(* ── Channel name helpers ────────────────────────────────── *)

let channel_name = function
  | Rewards.Discord _ -> "discord"
  | Rewards.Telegram _ -> "telegram"
  | Rewards.Webhook _ -> "webhook"
  | Rewards.External _ -> "external"

(* ── Escape JSON string value ────────────────────────────── *)

let escape_json_string s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s ;
  Buffer.contents buf

(* ── Channel dispatch ────────────────────────────────────── *)

let send_discord ~webhook_url ~message =
  let payload =
    Printf.sprintf {|{"content":"%s"}|} (escape_json_string message)
  in
  let argv =
    [
      "curl";
      "-fsSL";
      "--max-time";
      "30";
      "-X";
      "POST";
      "-H";
      "Content-Type: application/json";
      "-d";
      payload;
      webhook_url;
    ]
  in
  match Cmd_runner.run_out_with_timeout ~timeout:30.0 argv with
  | Ok _ -> Ok ()
  | Error (`Msg msg) -> Error msg

let send_telegram ~api_token ~receivers ~message =
  let errors = ref [] in
  List.iter
    (fun chat_id ->
      let url =
        Printf.sprintf "https://api.telegram.org/bot%s/sendMessage" api_token
      in
      let payload =
        Printf.sprintf
          {|{"chat_id":%d,"text":"%s","parse_mode":"Markdown"}|}
          chat_id
          (escape_json_string message)
      in
      let argv =
        [
          "curl";
          "-fsSL";
          "--max-time";
          "30";
          "-X";
          "POST";
          "-H";
          "Content-Type: application/json";
          "-d";
          payload;
          url;
        ]
      in
      match Cmd_runner.run_out_with_timeout ~timeout:30.0 argv with
      | Ok _ -> ()
      | Error (`Msg msg) ->
          errors := Printf.sprintf "chat_id %d: %s" chat_id msg :: !errors)
    receivers ;
  if !errors = [] then Ok () else Error (String.concat "; " (List.rev !errors))

let send_webhook ~url ~auth ~message =
  let payload =
    Printf.sprintf {|{"message":"%s"}|} (escape_json_string message)
  in
  let auth_headers =
    match auth with
    | Rewards.No_auth -> []
    | Rewards.Bearer token ->
        ["-H"; Printf.sprintf "Authorization: Bearer %s" token]
  in
  let argv =
    [
      "curl";
      "-fsSL";
      "--max-time";
      "30";
      "-X";
      "POST";
      "-H";
      "Content-Type: application/json";
    ]
    @ auth_headers @ ["-d"; payload; url]
  in
  match Cmd_runner.run_out_with_timeout ~timeout:30.0 argv with
  | Ok _ -> Ok ()
  | Error (`Msg msg) -> Error msg

let send_external ~path ~args ~message =
  let argv = (path :: args) @ [message] in
  match Cmd_runner.run_out_with_timeout ~timeout:60.0 argv with
  | Ok _ -> Ok ()
  | Error (`Msg msg) -> Error msg

(* ── Public API ──────────────────────────────────────────── *)

let send ~channel ~(summary : Rewards.cycle_summary) =
  match channel with
  | Rewards.Discord {webhook_url; message_template; _} ->
      let template =
        if String.equal message_template "" then
          "Cycle <Cycle>: paid <Delegators> delegators, distributed \
           <TotalPaid> XTZ"
        else message_template
      in
      let message = render_template ~template ~summary in
      send_discord ~webhook_url ~message
  | Rewards.Telegram {api_token; receivers; message_template} ->
      let template =
        if String.equal message_template "" then
          "Cycle <Cycle>: paid <Delegators> delegators, distributed \
           <TotalPaid> XTZ"
        else message_template
      in
      let message = render_template ~template ~summary in
      send_telegram ~api_token ~receivers ~message
  | Rewards.Webhook {url; auth} ->
      let message =
        render_template
          ~template:
            "Cycle <Cycle>: paid <Delegators> delegators, distributed \
             <TotalPaid> XTZ"
          ~summary
      in
      send_webhook ~url ~auth ~message
  | Rewards.External {path; args} ->
      let message =
        render_template
          ~template:
            "Cycle <Cycle>: paid <Delegators> delegators, distributed \
             <TotalPaid> XTZ"
          ~summary
      in
      send_external ~path ~args ~message

let notify_all ~channels ~summary =
  List.map
    (fun channel ->
      let name = channel_name channel in
      let result = send ~channel ~summary in
      (name, result))
    channels

let test_summary : Rewards.cycle_summary =
  {
    cycle = 999;
    delegators = 50;
    paid_delegators = 48;
    own_staked_balance = 100_000_000_000L;
    own_delegated_balance = 0L;
    external_staked_balance = 0L;
    external_delegated_balance = 500_000_000_000L;
    earned_rewards = 10_000_000L;
    earned_block_fees = 500_000L;
    distributed_rewards = 9_500_000L;
    bond_income = 0L;
    fee_income = 500_000L;
    tx_fees_paid = 25_000L;
    timestamp = "2026-01-01T00:00:00Z";
  }

let send_test ~channels = notify_all ~channels ~summary:test_summary
