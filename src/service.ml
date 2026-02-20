(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type t = {
  instance : string;
  role : string;
  network : string;
  history_mode : History_mode.t;
  data_dir : string;
  rpc_addr : Rpc_addr.t;
  net_addr : string;
  service_user : string;
  app_bin_dir : string;
  bin_source : Binary_registry.bin_source option;
  created_at : string;
  logging_mode : Logging_mode.t;
  snapshot_auto : bool;
  snapshot_uri : string option;
  snapshot_network_slug : string option;
  snapshot_no_check : bool;
  extra_args : string list;
  depends_on : string option;
  dependents : string list;
  signer_mode : Signer_types.signer_mode option;
      (** Remote signer configuration for bakers (None = Local_keys for backward compat) *)
  signer_uri : string option;  (** Resolved URI for display/metrics *)
  group : string option;  (** Group this service belongs to, if any *)
}

let make ~instance ~role ~network ~history_mode ~data_dir ~rpc_addr ~net_addr
    ~service_user ~app_bin_dir ?bin_source ~logging_mode
    ?(snapshot_auto = false) ?(snapshot_uri = None)
    ?(snapshot_network_slug = None) ?(snapshot_no_check = false)
    ?(extra_args = []) ?(depends_on = None) ?(dependents = [])
    ?(signer_mode = None) ?(signer_uri = None) ?(group = None) () =
  {
    instance;
    role;
    network;
    history_mode;
    data_dir;
    rpc_addr;
    net_addr;
    service_user;
    app_bin_dir;
    bin_source;
    created_at = String_utils.now ();
    logging_mode;
    snapshot_auto;
    snapshot_uri;
    snapshot_network_slug;
    snapshot_no_check;
    extra_args;
    depends_on;
    dependents;
    signer_mode;
    signer_uri;
    group;
  }

let get_bin_source t =
  match t.bin_source with
  | Some bs -> bs
  | None -> (
      (* For backward compatibility with services created before bin_source field:
         Detect if app_bin_dir matches a managed version or registered directory *)
      let binaries_dir = Binary_registry.binaries_dir () in
      (* Check if this is a managed version path *)
      if String.starts_with ~prefix:binaries_dir t.app_bin_dir then
        let basename = Filename.basename t.app_bin_dir in
        (* Managed versions are stored as "vX.Y" *)
        if String.starts_with ~prefix:"v" basename then
          let version = String.sub basename 1 (String.length basename - 1) in
          Binary_registry.Managed_octez_version version
        else Binary_registry.Raw_path t.app_bin_dir
      else
        (* Check if this matches a registered directory *)
        match Binary_registry.load_registered_dirs () with
        | Ok dirs -> (
            match
              List.find_opt
                (fun (ld : Binary_registry.registered_dir) ->
                  ld.path = t.app_bin_dir)
                dirs
            with
            | Some ld -> Binary_registry.Registered_alias ld.alias
            | None -> Binary_registry.Raw_path t.app_bin_dir)
        | Error _ -> Binary_registry.Raw_path t.app_bin_dir)

let logging_mode_to_yojson = Logging_mode.to_yojson

let logging_mode_of_yojson json =
  Logging_mode.of_yojson json |> Result.map_error (fun msg -> `Msg msg)

let signer_mode_to_yojson = function
  | Signer_types.Local_keys -> `Assoc [("type", `String "local_keys")]
  | Signer_types.Remote_signer {instance; uri} ->
      `Assoc
        [
          ("type", `String "remote_signer");
          ("instance", match instance with Some i -> `String i | None -> `Null);
          ("uri", `String uri);
        ]

let signer_mode_of_yojson json =
  let open Yojson.Safe.Util in
  try
    match json |> member "type" |> to_string with
    | "local_keys" -> Ok Signer_types.Local_keys
    | "remote_signer" ->
        let instance =
          match json |> member "instance" with
          | `String s when s <> "" -> Some s
          | _ -> None
        in
        let uri = json |> member "uri" |> to_string in
        Ok (Signer_types.Remote_signer {instance; uri})
    | _ -> Error (`Msg "Invalid signer_mode type")
  with Yojson.Json_error msg -> Error (`Msg msg)

let to_yojson t =
  let base =
    [
      ("instance", `String t.instance);
      ("role", `String t.role);
      ("network", `String t.network);
      ("history_mode", `String (History_mode.to_string t.history_mode));
      ("data_dir", `String t.data_dir);
      ("rpc_addr", Rpc_addr.to_yojson t.rpc_addr);
      ("net_addr", `String t.net_addr);
      ("service_user", `String t.service_user);
      ("app_bin_dir", `String t.app_bin_dir);
      ("created_at", `String t.created_at);
      ("logging_mode", logging_mode_to_yojson t.logging_mode);
      ("snapshot_auto", `Bool t.snapshot_auto);
      ( "snapshot_uri",
        match t.snapshot_uri with Some s -> `String s | None -> `Null );
      ( "snapshot_network_slug",
        match t.snapshot_network_slug with Some s -> `String s | None -> `Null
      );
      ("snapshot_no_check", `Bool t.snapshot_no_check);
      ("extra_args", `List (List.map (fun s -> `String s) t.extra_args));
      ( "depends_on",
        match t.depends_on with Some s -> `String s | None -> `Null );
      ("dependents", `List (List.map (fun s -> `String s) t.dependents));
      ( "signer_mode",
        match t.signer_mode with
        | Some sm -> signer_mode_to_yojson sm
        | None -> `Null );
      ( "signer_uri",
        match t.signer_uri with Some s -> `String s | None -> `Null );
      ("group", match t.group with Some s -> `String s | None -> `Null);
    ]
  in
  (* Add bin_source if present *)
  let fields =
    match t.bin_source with
    | Some bs -> ("bin_source", Binary_registry.bin_source_to_yojson bs) :: base
    | None -> base
  in
  `Assoc fields

let of_yojson json =
  let open Yojson.Safe.Util in
  try
    let instance = json |> member "instance" |> to_string in
    let role = json |> member "role" |> to_string in
    let network = json |> member "network" |> to_string in
    let history_mode =
      match json |> member "history_mode" with
      | `Null -> Ok History_mode.default
      | `String s -> History_mode.of_string s
      | _ -> Error (`Msg "Invalid history_mode field")
    in
    let data_dir = json |> member "data_dir" |> to_string in
    let rpc_addr =
      json |> member "rpc_addr" |> to_string |> Rpc_addr.of_string
    in
    let net_addr = json |> member "net_addr" |> to_string in
    let service_user = json |> member "service_user" |> to_string in
    let app_bin_dir = json |> member "app_bin_dir" |> to_string in
    let bin_source =
      match json |> member "bin_source" with
      | `Null -> None
      | bs_json -> (
          match Binary_registry.bin_source_of_yojson bs_json with
          | Ok bs -> Some bs
          | Error _ -> None (* Fall back to legacy mode on parse error *))
    in
    let created_at = json |> member "created_at" |> to_string in
    let snapshot_auto =
      match json |> member "snapshot_auto" with `Bool b -> b | _ -> false
    in
    let snapshot_uri =
      match json |> member "snapshot_uri" with
      | `String s when s <> "" -> Some s
      | _ -> None
    in
    let snapshot_network_slug =
      match json |> member "snapshot_network_slug" with
      | `String s when s <> "" -> Some s
      | _ -> None
    in
    let snapshot_no_check =
      match json |> member "snapshot_no_check" with `Bool b -> b | _ -> false
    in
    let extra_args =
      match json |> member "extra_args" with
      | `List l -> List.map to_string l
      | _ -> []
    in
    let depends_on =
      match json |> member "depends_on" with
      | `String s when s <> "" -> Some s
      | _ -> None
    in
    let dependents =
      match json |> member "dependents" with
      | `List l -> List.map to_string l
      | _ -> []
    in
    let signer_mode =
      match json |> member "signer_mode" with
      | `Null -> None
      | sm_json -> (
          match signer_mode_of_yojson sm_json with
          | Ok sm -> Some sm
          | Error _ ->
              (* Backward compat: treat as Local_keys on parse error *)
              Some Signer_types.Local_keys)
    in
    let signer_uri =
      match json |> member "signer_uri" with
      | `String s when s <> "" -> Some s
      | _ -> None
    in
    let group =
      match json |> member "group" with
      | `String s when s <> "" -> Some s
      | _ -> None
    in
    match history_mode with
    | Error _ as err -> err
    | Ok history_mode -> (
        match logging_mode_of_yojson (json |> member "logging_mode") with
        | Ok logging_mode ->
            Ok
              {
                instance;
                role;
                network;
                history_mode;
                data_dir;
                rpc_addr;
                net_addr;
                service_user;
                app_bin_dir;
                bin_source;
                created_at;
                logging_mode;
                snapshot_auto;
                snapshot_uri;
                snapshot_network_slug;
                snapshot_no_check;
                extra_args;
                depends_on;
                dependents;
                signer_mode;
                signer_uri;
                group;
              }
        | Error _ as err -> err)
  with Yojson.Json_error msg -> Error (`Msg msg)
