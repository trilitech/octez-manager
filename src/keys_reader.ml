(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** {1 Basic key info} *)

type key_info = {
  name : string;
  value : string; (* public key hash: tz1/tz2/tz3/tz4... *)
}

let key_info_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let value = json |> member "value" |> to_string in
    Ok {name; value}
  with
  | Type_error (msg, _) -> Error (`Msg msg)
  | Undefined (msg, _) -> Error (`Msg msg)

let read_public_key_hashes ~base_dir =
  let path = Filename.concat base_dir "public_key_hashs" in
  if not (Sys.file_exists path) then Ok []
  else
    try
      let json = Yojson.Safe.from_file path in
      match json with
      | `List entries ->
          let results = List.map key_info_of_yojson entries in
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | Ok key :: rest -> collect (key :: acc) rest
            | Error msg :: _ -> Error msg
          in
          collect [] results
      | _ -> Error (`Msg "Invalid public_key_hashs format: expected JSON array")
    with
    | Sys_error msg -> Error (`Msg msg)
    | Yojson.Json_error msg -> Error (`Msg msg)

(** {1 Enriched key metadata} *)

type key_kind = Unencrypted | Encrypted | Ledger of string | Remote of string

type key_metadata = {
  alias : string;
  pkh : string;
  public_key : string option;
  key_kind : key_kind;
  has_secret_key : bool;
}

(** Parse the URI scheme prefix from a key locator string.

    octez-client stores locators as [scheme:payload]:
    - [unencrypted:edsk...] / [unencrypted:edpk...]
    - [encrypted:edesk...]
    - [ledger://animal-words/curve/path]
    - [tcp://host:port/tz1...] *)
let key_kind_of_locator locator =
  if
    String.length locator >= 10
    && String.equal (String.sub locator 0 10) "encrypted:"
  then Encrypted
  else if
    String.length locator >= 9
    && String.equal (String.sub locator 0 9) "ledger:///"
  then
    let payload = String.sub locator 9 (String.length locator - 9) in
    Ledger payload
  else if
    String.length locator >= 9
    && String.equal (String.sub locator 0 9) "ledger://"
  then
    let payload = String.sub locator 9 (String.length locator - 9) in
    Ledger payload
  else if
    String.length locator >= 6 && String.equal (String.sub locator 0 6) "tcp://"
  then
    let payload = String.sub locator 6 (String.length locator - 6) in
    Remote payload
  else Unencrypted

(** Read a JSON array file with [{name; value}] entries into an alias→value
    map. Returns an empty map if the file doesn't exist. *)
let read_name_value_file ~base_dir filename =
  let path = Filename.concat base_dir filename in
  if not (Sys.file_exists path) then Ok []
  else
    try
      let json = Yojson.Safe.from_file path in
      match json with
      | `List entries ->
          let parse_entry json =
            let open Yojson.Safe.Util in
            try
              let name = json |> member "name" |> to_string in
              let value = json |> member "value" in
              Ok (name, value)
            with
            | Type_error (msg, _) -> Error (`Msg msg)
            | Undefined (msg, _) -> Error (`Msg msg)
          in
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | Ok entry :: rest -> collect (entry :: acc) rest
            | Error msg :: _ -> Error msg
          in
          collect [] (List.map parse_entry entries)
      | _ ->
          Error
            (`Msg
               (Printf.sprintf
                  "Invalid %s format: expected JSON array"
                  filename))
    with
    | Sys_error msg -> Error (`Msg msg)
    | Yojson.Json_error msg -> Error (`Msg msg)

(** Extract public key from a public_keys entry value.

    The value can be either:
    - A string (legacy format): the key locator directly
    - An object [{locator; key}]: contains both locator URI and the public key *)
let extract_public_key_info value =
  let open Yojson.Safe.Util in
  match value with
  | `String locator -> (key_kind_of_locator locator, None)
  | `Assoc _ ->
      let locator =
        value |> member "locator" |> to_string_option
        |> Option.value ~default:""
      in
      let key = value |> member "key" |> to_string_option in
      (key_kind_of_locator locator, key)
  | _ -> (Unencrypted, None)

(** Extract key_kind from a secret_keys entry value.

    The value is a string containing the scheme:payload locator. *)
let extract_secret_key_kind value =
  match value with
  | `String locator -> key_kind_of_locator locator
  | _ -> Unencrypted

let read_keys_full ~base_dir =
  match read_public_key_hashes ~base_dir with
  | Error _ as err -> err
  | Ok hashes -> (
      (* Read public_keys and secret_keys — errors are non-fatal, we just
         lose enrichment for those files *)
      let public_keys =
        match read_name_value_file ~base_dir "public_keys" with
        | Ok entries -> entries
        | Error _ -> []
      in
      let secret_keys =
        match read_name_value_file ~base_dir "secret_keys" with
        | Ok entries -> entries
        | Error _ -> []
      in
      let find_by_alias alias entries =
        List.find_opt (fun (name, _) -> String.equal name alias) entries
        |> Option.map snd
      in
      let build_metadata {name = alias; value = pkh} =
        let public_key_info =
          find_by_alias alias public_keys |> Option.map extract_public_key_info
        in
        let secret_key_info =
          find_by_alias alias secret_keys |> Option.map extract_secret_key_kind
        in
        let has_secret_key = Option.is_some secret_key_info in
        let public_key =
          match public_key_info with Some (_, key) -> key | None -> None
        in
        (* key_kind priority: secret_keys > public_keys > default *)
        let key_kind =
          match secret_key_info with
          | Some kind -> kind
          | None -> (
              match public_key_info with
              | Some (kind, _) -> kind
              | None -> Unencrypted)
        in
        {alias; pkh; public_key; key_kind; has_secret_key}
      in
      try Ok (List.map build_metadata hashes)
      with exn ->
        Error
          (`Msg
             (Printf.sprintf
                "Failed to enrich key metadata: %s"
                (Printexc.to_string exn))))

module For_tests = struct
  let key_info_of_yojson = key_info_of_yojson

  let key_kind_of_locator = key_kind_of_locator
end
