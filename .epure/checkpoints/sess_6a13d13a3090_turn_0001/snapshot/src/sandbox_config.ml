(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type t = {
  group_name : string;
  network : string;
  bin_source : Binary_registry.bin_source;
  created_at : string;
}

let make ~group_name ~network ~bin_source () =
  {group_name; network; bin_source; created_at = String_utils.now ()}

let to_yojson t =
  `Assoc
    [
      ("group_name", `String t.group_name);
      ("network", `String t.network);
      ("bin_source", Binary_registry.bin_source_to_yojson t.bin_source);
      ("created_at", `String t.created_at);
    ]

let of_yojson json : (t, [> `Msg of string]) result =
  let open Yojson.Safe.Util in
  try
    let group_name = json |> member "group_name" |> to_string in
    let network = json |> member "network" |> to_string in
    let bin_source_json = json |> member "bin_source" in
    let created_at = json |> member "created_at" |> to_string in
    match Binary_registry.bin_source_of_yojson bin_source_json with
    | Ok bin_source -> Ok {group_name; network; bin_source; created_at}
    | Error (`Msg msg) -> Error (`Msg msg)
  with
  | Yojson.Json_error msg -> Error (`Msg msg)
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (`Msg msg)
