(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type t = {
  name : string;
  service_user : string;
  app_bin_dir : string;
  created_at : string;
  sandbox : bool;
}

let make ~name ~service_user ~app_bin_dir ?(sandbox = false) () =
  {name; service_user; app_bin_dir; created_at = String_utils.now (); sandbox}

let to_yojson t =
  `Assoc
    ([
       ("name", `String t.name);
       ("service_user", `String t.service_user);
       ("app_bin_dir", `String t.app_bin_dir);
       ("created_at", `String t.created_at);
     ]
    @ if t.sandbox then [("sandbox", `Bool true)] else [])

let of_yojson json : (t, [> `Msg of string]) result =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    (* Legacy fields — read and discard for backwards-compatible deserialization *)
    let _network = json |> member "network" in
    let _bin_source = json |> member "bin_source" in
    let service_user = json |> member "service_user" |> to_string in
    let app_bin_dir = json |> member "app_bin_dir" |> to_string in
    let created_at = json |> member "created_at" |> to_string in
    let sandbox =
      match json |> member "sandbox" with `Bool b -> b | _ -> false
    in
    Ok {name; service_user; app_bin_dir; created_at; sandbox}
  with
  | Yojson.Json_error msg -> Error (`Msg msg)
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (`Msg msg)
