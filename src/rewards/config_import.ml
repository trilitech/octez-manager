(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Import external [config.hjson] into an octez-manager payout configuration. *)

open Yojson.Safe.Util

type import_result = {
  config : Payout_config.t;
  warnings : string list;
  imported_fields : int;
}

(* ── Helpers ──────────────────────────────────────────────── *)

let tez_to_mutez f = Int64.of_float (f *. 1_000_000.0)

let opt_float json field =
  match member field json with `Float f -> Some f | _ -> None

let opt_int json field =
  match member field json with `Int i -> Some i | _ -> None

let opt_bool json field =
  match member field json with `Bool b -> Some b | _ -> None

let opt_string json field =
  match member field json with `String s -> Some s | _ -> None

let string_list json field =
  match member field json with
  | `List items ->
      List.filter_map
        (fun item -> match item with `String s -> Some s | _ -> None)
        items
  | _ -> []

(* ── Section importers ───────────────────────────────────── *)

let import_payouts ~config ~json ~warnings ~count =
  let payouts = member "payouts" json in
  if payouts = `Null then (config, warnings, count)
  else
    let config =
      match opt_string payouts "payout_mode" with
      | Some "ideal" -> {config with Payout_config.payout_mode = Rewards.Ideal}
      | Some "actual" ->
          {config with Payout_config.payout_mode = Rewards.Actual}
      | _ -> config
    in
    let count = ref count in
    let config =
      match opt_float payouts "fee" with
      | Some f ->
          incr count ;
          {config with baker_fee = f}
      | None -> config
    in
    let config =
      match opt_bool payouts "baker_pays_transaction_fee" with
      | Some b ->
          incr count ;
          {config with baker_pays_tx_fee = b}
      | None -> config
    in
    let config =
      match opt_bool payouts "baker_pays_allocation_fee" with
      | Some b ->
          incr count ;
          {config with baker_pays_alloc_fee = b}
      | None -> config
    in
    let config =
      match opt_float payouts "minimum_payout_amount" with
      | Some f ->
          incr count ;
          {config with min_payout = tez_to_mutez f}
      | None -> config
    in
    let config =
      match opt_int payouts "transaction_gas_limit_buffer" with
      | Some i ->
          incr count ;
          {config with gas_buffer = i}
      | None -> config
    in
    let config =
      match opt_int payouts "kt_transaction_gas_limit_buffer" with
      | Some i ->
          incr count ;
          {config with kt_gas_buffer = i}
      | None -> config
    in
    let config =
      match opt_int payouts "transaction_deserialization_gas_buffer" with
      | Some i ->
          incr count ;
          {config with deser_gas_buffer = i}
      | None -> config
    in
    let config =
      match opt_int payouts "transaction_fee_buffer" with
      | Some i ->
          incr count ;
          {config with fee_buffer = i}
      | None -> config
    in
    let config =
      match opt_int payouts "kt_transaction_fee_buffer" with
      | Some i ->
          incr count ;
          {config with kt_fee_buffer = i}
      | None -> config
    in
    let config =
      match opt_int payouts "simulation_batch_size" with
      | Some i ->
          incr count ;
          {config with sim_batch_size = i}
      | None -> config
    in
    let config =
      match opt_int payouts "minimum_delay_blocks" with
      | Some i ->
          incr count ;
          {config with min_delay_blocks = i}
      | None -> config
    in
    let config =
      match opt_int payouts "maximum_delay_blocks" with
      | Some i ->
          incr count ;
          {config with max_delay_blocks = i}
      | None -> config
    in
    let warnings =
      match opt_string payouts "wallet_mode" with
      | Some mode when not (String.equal mode "local-private-key") ->
          Printf.sprintf "Unsupported wallet_mode: %s (using octez-client)" mode
          :: warnings
      | _ -> warnings
    in
    (config, warnings, !count)

let import_delegators ~config ~json ~warnings ~count =
  let delegators = member "delegators" json in
  if delegators = `Null then (config, warnings, count)
  else
    let count = ref count in
    let reqs = member "requirements" delegators in
    let config =
      if reqs = `Null then config
      else
        let config =
          match opt_float reqs "minimum_balance" with
          | Some f ->
              incr count ;
              {config with Payout_config.min_balance = tez_to_mutez f}
          | None -> config
        in
        let config =
          match opt_string reqs "below_minimum_reward_destination" with
          | Some "everyone" ->
              incr count ;
              {config with below_min_dest = Rewards.Redistribute}
          | Some "none" ->
              incr count ;
              {config with below_min_dest = Rewards.Baker_keeps}
          | _ -> config
        in
        config
    in
    let prefilter = string_list delegators "prefilter" in
    let config =
      if prefilter <> [] then (
        incr count ;
        {config with whitelist = prefilter})
      else config
    in
    let ignore_list = string_list delegators "ignore" in
    let config =
      if ignore_list <> [] then (
        incr count ;
        {config with blacklist = ignore_list})
      else config
    in
    (* Delegator overrides *)
    let overrides_json = member "overrides" delegators in
    let overrides =
      match overrides_json with
      | `Assoc entries ->
          List.filter_map
            (fun (addr, v) ->
              let ovr : Rewards.delegator_override =
                {
                  redirect_to = opt_string v "recipient";
                  custom_fee = opt_float v "fee";
                  custom_min_balance =
                    Option.map tez_to_mutez (opt_float v "minimum_balance");
                  max_balance_cap =
                    Option.map tez_to_mutez (opt_float v "maximum_balance");
                  baker_pays_tx_fee = opt_bool v "baker_pays_transaction_fee";
                  baker_pays_alloc_fee = opt_bool v "baker_pays_allocation_fee";
                }
              in
              Some (addr, ovr))
            entries
      | _ -> []
    in
    (* Fee overrides expand into delegator_overrides *)
    let fee_overrides_json = member "fee_overrides" delegators in
    let fee_overrides =
      match fee_overrides_json with
      | `Assoc entries ->
          List.filter_map
            (fun (addr, v) ->
              match v with
              | `Float f ->
                  Some
                    ( addr,
                      {
                        Rewards.redirect_to = None;
                        custom_fee = Some f;
                        custom_min_balance = None;
                        max_balance_cap = None;
                        baker_pays_tx_fee = None;
                        baker_pays_alloc_fee = None;
                      } )
              | _ -> None)
            entries
      | _ -> []
    in
    let all_overrides =
      let merged = overrides @ fee_overrides in
      if merged <> [] then incr count ;
      merged
    in
    let config = {config with delegator_overrides = all_overrides} in
    (config, warnings, !count)

let import_income_recipients ~config ~json ~warnings ~count =
  let income = member "income_recipients" json in
  if income = `Null then (config, warnings, count)
  else
    let count = ref count in
    let parse_share_map field =
      match member field income with
      | `Assoc entries ->
          List.filter_map
            (fun (addr, v) ->
              match v with `Float f -> Some (addr, f) | _ -> None)
            entries
      | _ -> []
    in
    let bonds = parse_share_map "bonds" in
    let config =
      if bonds <> [] then (
        incr count ;
        {config with Payout_config.bond_recipients = bonds})
      else config
    in
    let fees = parse_share_map "fees" in
    let config =
      if fees <> [] then (
        incr count ;
        {config with fee_recipients = fees})
      else config
    in
    let warnings =
      match opt_float income "donate" with
      | Some rate when rate > 0.0 ->
          Printf.sprintf
            "Donation rate %.2f%% imported as warning (not fully supported)"
            (rate *. 100.0)
          :: warnings
      | _ -> warnings
    in
    (config, warnings, !count)

let import_network ~config ~json ~warnings ~count =
  let network = member "network" json in
  if network = `Null then (config, warnings, count)
  else
    let count = ref count in
    let config =
      match opt_string network "tzkt_url" with
      | Some url ->
          incr count ;
          {config with Payout_config.tzkt_url = url}
      | None -> config
    in
    let config =
      match opt_string network "explorer" with
      | Some url ->
          incr count ;
          {config with explorer_url = url}
      | None -> config
    in
    let config =
      match opt_bool network "ignore_kt" with
      | Some b ->
          incr count ;
          {config with ignore_contracts = b}
      | None -> config
    in
    let rpc_pool = string_list network "rpc_pool" in
    let config =
      if rpc_pool <> [] then (
        incr count ;
        {config with rpc_fallback_pool = rpc_pool})
      else config
    in
    (config, warnings, !count)

let import_overdelegation ~config ~json ~_warnings ~count =
  let overdelegation = member "overdelegation" json in
  if overdelegation = `Null then (config, count)
  else
    let count = ref count in
    let config =
      match opt_bool overdelegation "protect" with
      | Some b ->
          incr count ;
          {config with Payout_config.overdelegation_protect = b}
      | None -> config
    in
    (config, !count)

let check_unsupported ~json ~warnings =
  let warnings =
    match member "extensions" json with
    | `Null | `List [] -> warnings
    | `List exts ->
        Printf.sprintf
          "%d extension(s) skipped (not supported)"
          (List.length exts)
        :: warnings
    | `Assoc exts ->
        Printf.sprintf
          "%d extension(s) skipped (not supported)"
          (List.length exts)
        :: warnings
    | _ -> warnings
  in
  let notifications = member "notifications" json in
  let warnings =
    match notifications with
    | `Null -> warnings
    | `List items ->
        List.fold_left
          (fun ws item ->
            match opt_string item "type" with
            | Some "twitter" -> "Twitter notifications not supported" :: ws
            | Some "bluesky" -> "Bluesky notifications not supported" :: ws
            | _ -> ws)
          warnings
          items
    | _ -> warnings
  in
  warnings

(* ── Notification import ─────────────────────────────────── *)

let import_notifications ~json =
  match member "notifications" json with
  | `List items ->
      List.filter_map
        (fun item ->
          match opt_string item "type" with
          | Some "discord" -> (
              match opt_string item "webhook_url" with
              | Some url ->
                  let template =
                    Option.value
                      ~default:""
                      (opt_string item "message_template")
                  in
                  let admin =
                    Option.value ~default:false (opt_bool item "admin")
                  in
                  Some
                    (Rewards.Discord
                       {webhook_url = url; message_template = template; admin})
              | None -> None)
          | Some "telegram" -> (
              match (opt_string item "api_token", member "receivers" item) with
              | Some token, `List receivers_json ->
                  let receivers =
                    List.filter_map
                      (fun r -> match r with `Int i -> Some i | _ -> None)
                      receivers_json
                  in
                  let template =
                    Option.value
                      ~default:""
                      (opt_string item "message_template")
                  in
                  Some
                    (Rewards.Telegram
                       {
                         api_token = token;
                         receivers;
                         message_template = template;
                       })
              | _ -> None)
          | Some "webhook" -> (
              match opt_string item "url" with
              | Some url ->
                  let auth =
                    match opt_string item "auth_type" with
                    | Some "bearer" -> (
                        match opt_string item "token" with
                        | Some t -> Rewards.Bearer t
                        | None -> Rewards.No_auth)
                    | _ -> Rewards.No_auth
                  in
                  Some (Rewards.Webhook {url; auth})
              | None -> None)
          | Some "external" -> (
              match opt_string item "path" with
              | Some path ->
                  let args = string_list item "args" in
                  Some (Rewards.External {path; args})
              | None -> None)
          | _ -> None)
        items
  | _ -> []

(* ── Main import ─────────────────────────────────────────── *)

let import_from_json ~baker_pkh json =
  let config = Payout_config.default ~baker_pkh in
  let warnings = [] in
  let count = 0 in
  (* Check config version *)
  let warnings =
    match opt_int json "config_version" with
    | Some v when v <> 0 ->
        Printf.sprintf "Unknown config_version: %d (expected 0)" v :: warnings
    | _ -> warnings
  in
  let config, warnings, count = import_payouts ~config ~json ~warnings ~count in
  let config, warnings, count =
    import_delegators ~config ~json ~warnings ~count
  in
  let config, warnings, count =
    import_income_recipients ~config ~json ~warnings ~count
  in
  let config, warnings, count = import_network ~config ~json ~warnings ~count in
  let config, count =
    import_overdelegation ~config ~json ~_warnings:warnings ~count
  in
  let warnings = check_unsupported ~json ~warnings in
  let notifications = import_notifications ~json in
  let config =
    if notifications <> [] then {config with notifications} else config
  in
  Ok {config; warnings = List.rev warnings; imported_fields = count}

let import_string ~baker_pkh input =
  match Hjson_parser.parse input with
  | Error msg -> Error msg
  | Ok json -> import_from_json ~baker_pkh json

let import_file ~baker_pkh path =
  match Hjson_parser.parse_file path with
  | Error msg -> Error msg
  | Ok json -> import_from_json ~baker_pkh json
