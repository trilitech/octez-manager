(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type t = {
  version : int;
  baker_pkh : string;
  payout_key_alias : string;
  payout_mode : Rewards.payout_mode;
  baker_fee : float;
  min_payout : Int64.t;
  min_balance : Int64.t;
  below_min_dest : Rewards.below_min_destination;
  overdelegation_protect : bool;
  baker_pays_tx_fee : bool;
  baker_pays_alloc_fee : bool;
  ignore_contracts : bool;
  gas_buffer : int;
  kt_gas_buffer : int;
  deser_gas_buffer : int;
  fee_buffer : int;
  kt_fee_buffer : int;
  sim_batch_size : int;
  min_delay_blocks : int;
  max_delay_blocks : int;
  whitelist : string list;
  blacklist : string list;
  delegator_overrides : (string * Rewards.delegator_override) list;
  bond_recipients : (string * float) list;
  fee_recipients : (string * float) list;
  rpc_fallback_pool : string list;
  tzkt_url : string;
  explorer_url : string;
  notifications : Rewards.notification_channel list;
  continual_enabled : bool;
  continual_interval : int;
  continual_offset : int;
}

let default ~baker_pkh =
  {
    version = 1;
    baker_pkh;
    payout_key_alias = baker_pkh;
    payout_mode = Rewards.Actual;
    baker_fee = 0.05;
    min_payout = 0L;
    min_balance = 0L;
    below_min_dest = Rewards.Baker_keeps;
    overdelegation_protect = true;
    baker_pays_tx_fee = false;
    baker_pays_alloc_fee = false;
    ignore_contracts = false;
    gas_buffer = 200;
    kt_gas_buffer = 2500;
    deser_gas_buffer = 5;
    fee_buffer = 2;
    kt_fee_buffer = 20;
    sim_batch_size = 80;
    min_delay_blocks = 1;
    max_delay_blocks = 15;
    whitelist = [];
    blacklist = [];
    delegator_overrides = [];
    bond_recipients = [];
    fee_recipients = [];
    rpc_fallback_pool = [];
    tzkt_url = "https://api.tzkt.io";
    explorer_url = "https://tzkt.io";
    notifications = [];
    continual_enabled = false;
    continual_interval = 1;
    continual_offset = 0;
  }

let tzkt_base_url_for_network network =
  if String.equal network "mainnet" then "https://api.tzkt.io"
  else Printf.sprintf "https://api.%s.tzkt.io" network

(* Validation *)

let is_valid_tz_address s =
  let len = String.length s in
  len = 36
  && (String.starts_with ~prefix:"tz1" s
     || String.starts_with ~prefix:"tz2" s
     || String.starts_with ~prefix:"tz3" s
     || String.starts_with ~prefix:"tz4" s)

let is_valid_baker_pkh s = is_valid_tz_address s

let is_valid_address s =
  is_valid_tz_address s
  || (String.length s = 36 && String.starts_with ~prefix:"KT1" s)

let validate_share_map label shares =
  let sum = List.fold_left (fun acc (_, share) -> acc +. share) 0.0 shares in
  if sum > 1.0 then
    Error (Printf.sprintf "%s shares sum to %.4f, must be <= 1.0" label sum)
  else
    let invalid =
      List.filter (fun (_, share) -> share < 0.0 || share > 1.0) shares
    in
    match invalid with
    | [] -> (
        let bad_addrs =
          List.filter (fun (addr, _) -> not (is_valid_address addr)) shares
        in
        match bad_addrs with
        | [] -> Ok ()
        | (addr, _) :: _ ->
            Error (Printf.sprintf "invalid address in %s: %s" label addr))
    | (_, v) :: _ ->
        Error (Printf.sprintf "%s share %.4f out of range [0.0, 1.0]" label v)

let validate t =
  if t.version <> 1 then
    Error (Printf.sprintf "unsupported version: %d" t.version)
  else if String.length (String.trim t.payout_key_alias) = 0 then
    Error "payout_key_alias must not be empty"
  else if t.baker_fee < 0.0 || t.baker_fee > 1.0 then
    Error (Printf.sprintf "baker_fee %.4f out of range [0.0, 1.0]" t.baker_fee)
  else if t.min_payout < 0L then Error "min_payout must be >= 0"
  else if t.min_balance < 0L then Error "min_balance must be >= 0"
  else if t.gas_buffer <= 0 then Error "gas_buffer must be > 0"
  else if t.kt_gas_buffer <= 0 then Error "kt_gas_buffer must be > 0"
  else if t.deser_gas_buffer <= 0 then Error "deser_gas_buffer must be > 0"
  else if t.fee_buffer <= 0 then Error "fee_buffer must be > 0"
  else if t.kt_fee_buffer <= 0 then Error "kt_fee_buffer must be > 0"
  else if t.sim_batch_size <= 0 then Error "sim_batch_size must be > 0"
  else if t.min_delay_blocks < 0 then Error "min_delay_blocks must be >= 0"
  else if t.max_delay_blocks < t.min_delay_blocks then
    Error "max_delay_blocks must be >= min_delay_blocks"
  else if t.continual_interval < 1 then Error "continual_interval must be >= 1"
  else if t.continual_offset < 0 || t.continual_offset >= t.continual_interval
  then Error "continual_offset must be in [0, continual_interval)"
  else
    let bad_wl = List.filter (fun a -> not (is_valid_address a)) t.whitelist in
    match bad_wl with
    | addr :: _ -> Error (Printf.sprintf "invalid whitelist address: %s" addr)
    | [] -> (
        let bad_bl =
          List.filter (fun a -> not (is_valid_address a)) t.blacklist
        in
        match bad_bl with
        | addr :: _ ->
            Error (Printf.sprintf "invalid blacklist address: %s" addr)
        | [] -> (
            match validate_share_map "bond_recipients" t.bond_recipients with
            | Error _ as e -> e
            | Ok () -> (
                match validate_share_map "fee_recipients" t.fee_recipients with
                | Error _ as e -> e
                | Ok () -> Ok ())))

(* JSON serialization *)

let string_of_below_min_dest = function
  | Rewards.Baker_keeps -> "baker_keeps"
  | Rewards.Redistribute -> "redistribute"

let below_min_dest_of_string = function
  | "baker_keeps" -> Ok Rewards.Baker_keeps
  | "redistribute" -> Ok Rewards.Redistribute
  | s -> Error (Printf.sprintf "unknown below_min_dest: %s" s)

let webhook_auth_to_json = function
  | Rewards.No_auth -> `String "none"
  | Rewards.Bearer token ->
      `Assoc [("type", `String "bearer"); ("token", `String token)]

let webhook_auth_of_json = function
  | `String "none" -> Ok Rewards.No_auth
  | `Assoc _ as j -> (
      match Yojson.Safe.Util.member "type" j with
      | `String "bearer" ->
          let token =
            Yojson.Safe.Util.member "token" j |> Yojson.Safe.Util.to_string
          in
          Ok (Rewards.Bearer token)
      | _ -> Error "unknown webhook auth type")
  | _ -> Error "invalid webhook auth"

let notification_to_json = function
  | Rewards.Discord {webhook_url; message_template; admin} ->
      `Assoc
        [
          ("type", `String "discord");
          ("webhook_url", `String webhook_url);
          ("message_template", `String message_template);
          ("admin", `Bool admin);
        ]
  | Rewards.Telegram {api_token; receivers; message_template} ->
      `Assoc
        [
          ("type", `String "telegram");
          ("api_token", `String api_token);
          ("receivers", `List (List.map (fun r -> `Int r) receivers));
          ("message_template", `String message_template);
        ]
  | Rewards.Webhook {url; auth} ->
      `Assoc
        [
          ("type", `String "webhook");
          ("url", `String url);
          ("auth", webhook_auth_to_json auth);
        ]
  | Rewards.External {path; args} ->
      `Assoc
        [
          ("type", `String "external");
          ("path", `String path);
          ("args", `List (List.map (fun a -> `String a) args));
        ]

let notification_of_json json =
  let open Yojson.Safe.Util in
  match member "type" json |> to_string with
  | "discord" ->
      Ok
        (Rewards.Discord
           {
             webhook_url = member "webhook_url" json |> to_string;
             message_template =
               member "message_template" json
               |> to_string_option |> Option.value ~default:"";
             admin =
               member "admin" json |> to_bool_option
               |> Option.value ~default:false;
           })
  | "telegram" ->
      Ok
        (Rewards.Telegram
           {
             api_token = member "api_token" json |> to_string;
             receivers = member "receivers" json |> to_list |> List.map to_int;
             message_template =
               member "message_template" json
               |> to_string_option |> Option.value ~default:"";
           })
  | "webhook" -> (
      match webhook_auth_of_json (member "auth" json) with
      | Ok auth ->
          Ok (Rewards.Webhook {url = member "url" json |> to_string; auth})
      | Error _ as e -> e)
  | "external" ->
      Ok
        (Rewards.External
           {
             path = member "path" json |> to_string;
             args = member "args" json |> to_list |> List.map to_string;
           })
  | t -> Error (Printf.sprintf "unknown notification type: %s" t)
  | exception _ -> Error "missing or invalid notification type"

let delegator_override_to_json (ov : Rewards.delegator_override) =
  let add_opt key f v acc =
    match v with None -> acc | Some x -> (key, f x) :: acc
  in
  let fields =
    []
    |> add_opt "redirect_to" (fun s -> `String s) ov.redirect_to
    |> add_opt "custom_fee" (fun f -> `Float f) ov.custom_fee
    |> add_opt
         "custom_min_balance"
         (fun i -> `String (Int64.to_string i))
         ov.custom_min_balance
    |> add_opt
         "max_balance_cap"
         (fun i -> `String (Int64.to_string i))
         ov.max_balance_cap
    |> add_opt "baker_pays_tx_fee" (fun b -> `Bool b) ov.baker_pays_tx_fee
    |> add_opt "baker_pays_alloc_fee" (fun b -> `Bool b) ov.baker_pays_alloc_fee
  in
  `Assoc fields

let delegator_override_of_json json =
  let open Yojson.Safe.Util in
  {
    Rewards.redirect_to = member "redirect_to" json |> to_string_option;
    custom_fee = member "custom_fee" json |> to_float_option;
    custom_min_balance =
      (match member "custom_min_balance" json with
      | `String s -> ( try Some (Int64.of_string s) with _ -> None)
      | _ -> None);
    max_balance_cap =
      (match member "max_balance_cap" json with
      | `String s -> ( try Some (Int64.of_string s) with _ -> None)
      | _ -> None);
    baker_pays_tx_fee = member "baker_pays_tx_fee" json |> to_bool_option;
    baker_pays_alloc_fee = member "baker_pays_alloc_fee" json |> to_bool_option;
  }

let to_json t =
  `Assoc
    [
      ("version", `Int t.version);
      ("baker_pkh", `String t.baker_pkh);
      ("payout_key_alias", `String t.payout_key_alias);
      ("payout_mode", `String (Rewards.string_of_payout_mode t.payout_mode));
      ("baker_fee", `Float t.baker_fee);
      ("min_payout", `String (Int64.to_string t.min_payout));
      ("min_balance", `String (Int64.to_string t.min_balance));
      ("below_min_dest", `String (string_of_below_min_dest t.below_min_dest));
      ("overdelegation_protect", `Bool t.overdelegation_protect);
      ("baker_pays_tx_fee", `Bool t.baker_pays_tx_fee);
      ("baker_pays_alloc_fee", `Bool t.baker_pays_alloc_fee);
      ("ignore_contracts", `Bool t.ignore_contracts);
      ("gas_buffer", `Int t.gas_buffer);
      ("kt_gas_buffer", `Int t.kt_gas_buffer);
      ("deser_gas_buffer", `Int t.deser_gas_buffer);
      ("fee_buffer", `Int t.fee_buffer);
      ("kt_fee_buffer", `Int t.kt_fee_buffer);
      ("sim_batch_size", `Int t.sim_batch_size);
      ("min_delay_blocks", `Int t.min_delay_blocks);
      ("max_delay_blocks", `Int t.max_delay_blocks);
      ("whitelist", `List (List.map (fun s -> `String s) t.whitelist));
      ("blacklist", `List (List.map (fun s -> `String s) t.blacklist));
      ( "delegator_overrides",
        `Assoc
          (List.map
             (fun (addr, ov) -> (addr, delegator_override_to_json ov))
             t.delegator_overrides) );
      ( "bond_recipients",
        `Assoc
          (List.map
             (fun (addr, share) -> (addr, `Float share))
             t.bond_recipients) );
      ( "fee_recipients",
        `Assoc
          (List.map
             (fun (addr, share) -> (addr, `Float share))
             t.fee_recipients) );
      ( "rpc_fallback_pool",
        `List (List.map (fun s -> `String s) t.rpc_fallback_pool) );
      ("tzkt_url", `String t.tzkt_url);
      ("explorer_url", `String t.explorer_url);
      ("notifications", `List (List.map notification_to_json t.notifications));
      ("continual_enabled", `Bool t.continual_enabled);
      ("continual_interval", `Int t.continual_interval);
      ("continual_offset", `Int t.continual_offset);
    ]

let of_json json =
  try
    let open Yojson.Safe.Util in
    let int64_of_json j =
      match j with
      | `String s -> Int64.of_string s
      | `Int i -> Int64.of_int i
      | `Intlit s -> Int64.of_string s
      | _ -> failwith "expected int64"
    in
    let version = member "version" json |> to_int in
    let payout_mode_str = member "payout_mode" json |> to_string in
    let payout_mode =
      match Rewards.payout_mode_of_string payout_mode_str with
      | Some m -> m
      | None ->
          failwith (Printf.sprintf "unknown payout_mode: %s" payout_mode_str)
    in
    let below_min_str = member "below_min_dest" json |> to_string in
    let below_min_dest =
      match below_min_dest_of_string below_min_str with
      | Ok d -> d
      | Error msg -> failwith msg
    in
    let delegator_overrides =
      match member "delegator_overrides" json with
      | `Assoc pairs ->
          List.map (fun (addr, v) -> (addr, delegator_override_of_json v)) pairs
      | _ -> []
    in
    let share_map_of_json j =
      match j with
      | `Assoc pairs -> List.map (fun (addr, v) -> (addr, to_float v)) pairs
      | _ -> []
    in
    let notifications =
      match member "notifications" json with
      | `List items ->
          List.filter_map
            (fun j ->
              match notification_of_json j with
              | Ok n -> Some n
              | Error _ -> None)
            items
      | _ -> []
    in
    Ok
      {
        version;
        baker_pkh = member "baker_pkh" json |> to_string;
        payout_key_alias = member "payout_key_alias" json |> to_string;
        payout_mode;
        baker_fee = member "baker_fee" json |> to_float;
        min_payout = member "min_payout" json |> int64_of_json;
        min_balance = member "min_balance" json |> int64_of_json;
        below_min_dest;
        overdelegation_protect = member "overdelegation_protect" json |> to_bool;
        baker_pays_tx_fee = member "baker_pays_tx_fee" json |> to_bool;
        baker_pays_alloc_fee = member "baker_pays_alloc_fee" json |> to_bool;
        ignore_contracts = member "ignore_contracts" json |> to_bool;
        gas_buffer = member "gas_buffer" json |> to_int;
        kt_gas_buffer = member "kt_gas_buffer" json |> to_int;
        deser_gas_buffer = member "deser_gas_buffer" json |> to_int;
        fee_buffer = member "fee_buffer" json |> to_int;
        kt_fee_buffer = member "kt_fee_buffer" json |> to_int;
        sim_batch_size = member "sim_batch_size" json |> to_int;
        min_delay_blocks = member "min_delay_blocks" json |> to_int;
        max_delay_blocks = member "max_delay_blocks" json |> to_int;
        whitelist = member "whitelist" json |> to_list |> List.map to_string;
        blacklist = member "blacklist" json |> to_list |> List.map to_string;
        delegator_overrides;
        bond_recipients = share_map_of_json (member "bond_recipients" json);
        fee_recipients = share_map_of_json (member "fee_recipients" json);
        rpc_fallback_pool =
          member "rpc_fallback_pool" json |> to_list |> List.map to_string;
        tzkt_url = member "tzkt_url" json |> to_string;
        explorer_url = member "explorer_url" json |> to_string;
        notifications;
        continual_enabled =
          (try member "continual_enabled" json |> to_bool with _ -> false);
        continual_interval =
          (let v =
             try member "continual_interval" json |> to_int with _ -> 1
           in
           if v >= 1 then v else 1);
        continual_offset =
          (try member "continual_offset" json |> to_int with _ -> 0);
      }
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg
  | Failure msg -> Error msg
  | exn -> Error (Printexc.to_string exn)

(* Persistence *)

let rewards_dir ~instance =
  Filename.concat (Filename.concat (Paths.registry_root ()) "rewards") instance

let config_path ~instance =
  Filename.concat (rewards_dir ~instance) "config.json"

let rec mkdir_p path =
  if Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path) ;
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let exists ~instance = Sys.file_exists (config_path ~instance)

let load ~instance =
  let path = config_path ~instance in
  if not (Sys.file_exists path) then
    Error (Printf.sprintf "config not found: %s" path)
  else
    try
      let ic = open_in path in
      let content = In_channel.input_all ic in
      close_in ic ;
      let json = Yojson.Safe.from_string content in
      of_json json
    with
    | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
    | exn -> Error (Printexc.to_string exn)

let save ~instance t =
  try
    let dir = rewards_dir ~instance in
    mkdir_p dir ;
    let json = to_json t in
    let content = Yojson.Safe.pretty_to_string ~std:true json in
    let path = config_path ~instance in
    let oc = open_out path in
    output_string oc content ;
    output_char oc '\n' ;
    close_out oc ;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let delete ~instance =
  try
    let path = config_path ~instance in
    if Sys.file_exists path then Sys.remove path ;
    let dir = rewards_dir ~instance in
    (if Sys.file_exists dir && Sys.is_directory dir then
       match Sys.readdir dir with [||] -> Unix.rmdir dir | _ -> ()) ;
    Ok ()
  with exn -> Error (Printexc.to_string exn)
