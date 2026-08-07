(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type entry = {base_dir : string; pkh : string; alias : string}

let cache : entry list ref = ref []

let cache_lock = Mutex.create ()

let aliases_path () =
  Filename.concat (Paths.registry_root ()) "key_aliases.json"

let save_to_disk () =
  try
    let dir = Paths.registry_root () in
    (if not (Sys.file_exists dir) then try Sys.mkdir dir 0o755 with _ -> ()) ;
    let json =
      `List
        (List.map
           (fun e ->
             `Assoc
               [
                 ("base_dir", `String e.base_dir);
                 ("pkh", `String e.pkh);
                 ("alias", `String e.alias);
               ])
           !cache)
    in
    let path = aliases_path () in
    let oc = open_out path in
    Fun.protect ~finally:(fun () -> close_out oc) @@ fun () ->
    output_string oc (Yojson.Safe.to_string json)
  with _ -> ()

let matches ~base_dir ~pkh e =
  String.equal e.base_dir base_dir && String.equal e.pkh pkh

let get ~base_dir ~pkh =
  Mutex.protect cache_lock (fun () ->
      List.find_opt (matches ~base_dir ~pkh) !cache
      |> Option.map (fun e -> e.alias))

let set ~base_dir ~pkh ~alias =
  Mutex.protect cache_lock (fun () ->
      let filtered =
        List.filter (fun e -> not (matches ~base_dir ~pkh e)) !cache
      in
      cache := {base_dir; pkh; alias} :: filtered ;
      save_to_disk ())

let remove ~base_dir ~pkh =
  Mutex.protect cache_lock (fun () ->
      cache := List.filter (fun e -> not (matches ~base_dir ~pkh e)) !cache ;
      save_to_disk ())

let load () =
  let path = aliases_path () in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let content =
        Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
        let n = in_channel_length ic in
        let buf = Bytes.create n in
        really_input ic buf 0 n ;
        Bytes.to_string buf
      in
      let json = Yojson.Safe.from_string content in
      match json with
      | `List entries ->
          let parsed =
            List.filter_map
              (fun entry ->
                let open Yojson.Safe.Util in
                try
                  let base_dir = entry |> member "base_dir" |> to_string in
                  let pkh = entry |> member "pkh" |> to_string in
                  let alias = entry |> member "alias" |> to_string in
                  Some {base_dir; pkh; alias}
                with _ -> None)
              entries
          in
          Mutex.protect cache_lock (fun () -> cache := parsed)
      | _ -> ()
    with _ -> ()
