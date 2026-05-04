(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Per-network alias caches: network → (pkh → alias) *)
let caches : (string, (string, string) Hashtbl.t) Hashtbl.t = Hashtbl.create 7

let cache_lock = Mutex.create ()

(** Refresh interval: 6 hours *)
let refresh_interval = 6.0 *. 3600.0

(** Per-network last refresh timestamps *)
let last_refresh : (string, float) Hashtbl.t = Hashtbl.create 7

let last_refresh_lock = Mutex.create ()

let aliases_path =
  "/v1/accounts?type=delegate&select=address,alias&limit=10000&alias.null=false"

let find ~network ~pkh =
  Mutex.protect cache_lock (fun () ->
      match Hashtbl.find_opt caches network with
      | None -> None
      | Some tbl -> Hashtbl.find_opt tbl pkh)

(** Parse tzkt API response: [{address, alias}, ...] *)
let parse_aliases_json json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `List entries ->
        let tbl = Hashtbl.create (List.length entries) in
        List.iter
          (fun entry ->
            let open Yojson.Safe.Util in
            let address = entry |> member "address" |> to_string_option in
            let alias = entry |> member "alias" |> to_string_option in
            match (address, alias) with
            | Some addr, Some name -> Hashtbl.replace tbl addr name
            | _ -> ())
          entries ;
        Some tbl
    | _ -> None
  with _ -> None

(** Path to disk cache file for a network. *)
let cache_path ~network =
  let dir = Paths.registry_root () in
  Filename.concat dir (Printf.sprintf "tzkt_aliases_%s.json" network)

(** Save aliases to disk for offline access. *)
let save_to_disk ~network tbl =
  try
    let dir = Paths.registry_root () in
    (if not (Sys.file_exists dir) then try Sys.mkdir dir 0o755 with _ -> ()) ;
    let entries =
      Hashtbl.fold
        (fun addr name acc ->
          `Assoc [("address", `String addr); ("alias", `String name)] :: acc)
        tbl
        []
    in
    let json = `List entries in
    let path = cache_path ~network in
    let oc = open_out path in
    Fun.protect ~finally:(fun () -> close_out oc) @@ fun () ->
    output_string oc (Yojson.Safe.to_string json)
  with _ ->
    (* Disk persistence is best-effort *)
    ()

(** Load aliases from disk cache. *)
let load_from_disk ~network =
  let path = cache_path ~network in
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
      parse_aliases_json content
    with _ -> None
  else None

let refresh ~network =
  let result = Indexer.fetch ~network aliases_path in
  match result with
  | Ok body -> (
      match parse_aliases_json body with
      | Some tbl ->
          Mutex.protect cache_lock (fun () ->
              Hashtbl.replace caches network tbl) ;
          Mutex.protect last_refresh_lock (fun () ->
              Hashtbl.replace last_refresh network (Unix.gettimeofday ())) ;
          save_to_disk ~network tbl
      | None -> ())
  | Error _ -> (
      (* On API failure, try loading from disk if we have no cache *)
      let has_cache =
        Mutex.protect cache_lock (fun () -> Hashtbl.mem caches network)
      in
      if not has_cache then
        match load_from_disk ~network with
        | Some tbl ->
            Mutex.protect cache_lock (fun () ->
                Hashtbl.replace caches network tbl)
        | None -> ())

(** Check if a network's cache needs refreshing. *)
let needs_refresh ~network =
  Mutex.protect last_refresh_lock (fun () ->
      match Hashtbl.find_opt last_refresh network with
      | None -> true
      | Some ts -> Unix.gettimeofday () -. ts > refresh_interval)

let load ~network =
  match load_from_disk ~network with
  | Some tbl ->
      Mutex.protect cache_lock (fun () -> Hashtbl.replace caches network tbl)
  | None -> ()
