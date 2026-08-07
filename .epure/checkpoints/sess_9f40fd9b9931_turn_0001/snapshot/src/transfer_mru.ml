(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type entry = {pkh : string; alias : string option; last_used_at : float}

let max_entries = 50

let cache : entry list ref = ref []

let cache_lock = Mutex.create ()

let get () = Mutex.protect cache_lock (fun () -> !cache)

(** Path to disk persistence file. *)
let mru_path () =
  let dir = Paths.registry_root () in
  Filename.concat dir "transfer_mru.json"

(** Save the current cache to disk. Best-effort. *)
let save_to_disk () =
  try
    let dir = Paths.registry_root () in
    (if not (Sys.file_exists dir) then try Sys.mkdir dir 0o755 with _ -> ()) ;
    let entries = !cache in
    let json =
      `List
        (List.map
           (fun e ->
             `Assoc
               ([
                  ("pkh", `String e.pkh); ("last_used_at", `Float e.last_used_at);
                ]
               @
               match e.alias with
               | Some a -> [("alias", `String a)]
               | None -> []))
           entries)
    in
    let path = mru_path () in
    let oc = open_out path in
    Fun.protect ~finally:(fun () -> close_out oc) @@ fun () ->
    output_string oc (Yojson.Safe.to_string json)
  with _ -> ()

let add ~pkh ?alias () =
  Mutex.protect cache_lock (fun () ->
      let entry = {pkh; alias; last_used_at = Unix.gettimeofday ()} in
      let filtered =
        List.filter (fun e -> not (String.equal e.pkh pkh)) !cache
      in
      let updated = entry :: filtered in
      let capped =
        if List.length updated > max_entries then
          List.filteri (fun i _ -> i < max_entries) updated
        else updated
      in
      cache := capped ;
      save_to_disk ())

let remove ~pkh =
  Mutex.protect cache_lock (fun () ->
      cache := List.filter (fun e -> not (String.equal e.pkh pkh)) !cache ;
      save_to_disk ())

let load () =
  let path = mru_path () in
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
                  let pkh = entry |> member "pkh" |> to_string in
                  let alias = entry |> member "alias" |> to_string_option in
                  let last_used_at =
                    entry |> member "last_used_at" |> to_float_option
                    |> Option.value ~default:0.0
                  in
                  Some {pkh; alias; last_used_at}
                with _ -> None)
              entries
          in
          Mutex.protect cache_lock (fun () -> cache := parsed)
      | _ -> ()
    with _ -> ()
