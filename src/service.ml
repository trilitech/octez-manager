(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type t = {
  instance : string;
  role : string;
  network : string;
  history_mode : History_mode.t;
  data_dir : string;
  rpc_addr : string;
  net_addr : string;
  service_user : string;
  app_bin_dir : string;
  created_at : string;
  logging_mode : Logging_mode.t;
  snapshot_auto : bool;
  snapshot_uri : string option;
  snapshot_network_slug : string option;
  snapshot_no_check : bool;
  extra_args : string list;
  depends_on : string option;
}

let now () =
  let tm = Unix.time () |> Unix.localtime in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let make ~instance ~role ~network ~history_mode ~data_dir ~rpc_addr ~net_addr
    ~service_user ~app_bin_dir ~logging_mode ?(snapshot_auto = false)
    ?(snapshot_uri = None) ?(snapshot_network_slug = None)
    ?(snapshot_no_check = false) ?(extra_args = []) ?(depends_on = None) () =
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
    created_at = now ();
    logging_mode;
    snapshot_auto;
    snapshot_uri;
    snapshot_network_slug;
    snapshot_no_check;
    extra_args;
    depends_on;
  }

let logging_mode_to_yojson = Logging_mode.to_yojson

let logging_mode_of_yojson json =
  Logging_mode.of_yojson json |> Result.map_error (fun msg -> `Msg msg)

let to_yojson t =
  `Assoc
    [
      ("instance", `String t.instance);
      ("role", `String t.role);
      ("network", `String t.network);
      ("history_mode", `String (History_mode.to_string t.history_mode));
      ("data_dir", `String t.data_dir);
      ("rpc_addr", `String t.rpc_addr);
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
    ]

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
    let rpc_addr = json |> member "rpc_addr" |> to_string in
    let net_addr = json |> member "net_addr" |> to_string in
    let service_user = json |> member "service_user" |> to_string in
    let app_bin_dir = json |> member "app_bin_dir" |> to_string in
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
                created_at;
                logging_mode;
                snapshot_auto;
                snapshot_uri;
                snapshot_network_slug;
                snapshot_no_check;
                extra_args;
                depends_on;
              }
        | Error _ as err -> err)
  with Yojson.Json_error msg -> Error (`Msg msg)
