(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type t = {
  name : string;
  network : string;
  bin_source : Binary_registry.bin_source;
  service_user : string;
  app_bin_dir : string;
  created_at : string;
  sandbox : bool;
}

let make ~name ~network ~bin_source ~service_user ~app_bin_dir
    ?(sandbox = false) () =
  {
    name;
    network;
    bin_source;
    service_user;
    app_bin_dir;
    created_at = String_utils.now ();
    sandbox;
  }

let to_yojson t =
  `Assoc
    ([
       ("name", `String t.name);
       ("network", `String t.network);
       ("bin_source", Binary_registry.bin_source_to_yojson t.bin_source);
       ("service_user", `String t.service_user);
       ("app_bin_dir", `String t.app_bin_dir);
       ("created_at", `String t.created_at);
     ]
    @ if t.sandbox then [("sandbox", `Bool true)] else [])

let of_yojson json : (t, [> `Msg of string]) result =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let network = json |> member "network" |> to_string in
    let bin_source_json = json |> member "bin_source" in
    let service_user = json |> member "service_user" |> to_string in
    let app_bin_dir = json |> member "app_bin_dir" |> to_string in
    let created_at = json |> member "created_at" |> to_string in
    let sandbox =
      match json |> member "sandbox" with `Bool b -> b | _ -> false
    in
    match Binary_registry.bin_source_of_yojson bin_source_json with
    | Ok bin_source ->
        Ok
          {
            name;
            network;
            bin_source;
            service_user;
            app_bin_dir;
            created_at;
            sandbox;
          }
    | Error (`Msg msg) -> Error (`Msg msg)
  with
  | Yojson.Json_error msg -> Error (`Msg msg)
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (`Msg msg)
