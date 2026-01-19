(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** DAL node health status polling and caching. *)

open Octez_manager_lib

type status = Up | Down | Degraded | Unknown

type check = {name : string; status : status}

type t = {status : status; checks : check list; last_fetch : float}

let status_of_string = function
  | "up" | "ok" -> Up
  | "down" | "ko" | "no" -> Down
  | "degraded" -> Degraded
  | _ -> Unknown

let status_to_string = function
  | Up -> "up"
  | Down -> "down"
  | Degraded -> "degraded"
  | Unknown -> "?"

(** Cache indexed by instance name *)
let cache : (string, t) Hashtbl.t = Hashtbl.create 17

let lock = Mutex.create ()

let with_lock f =
  Mutex.lock lock ;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) f

(** Fetch health from DAL node RPC *)
let fetch ~rpc_endpoint =
  let url = rpc_endpoint ^ "/health" in
  match
    Common.run_out
      ["curl"; "-sf"; "--connect-timeout"; "2"; "--max-time"; "5"; url]
  with
  | Error _ -> None
  | Ok json -> (
      try
        let j = Yojson.Safe.from_string json in
        let open Yojson.Safe.Util in
        let status =
          match member "status" j with
          | `String s -> status_of_string (String.lowercase_ascii s)
          | _ -> Unknown
        in
        let checks =
          match member "checks" j with
          | `List items ->
              List.filter_map
                (fun item ->
                  let name =
                    match member "name" item with `String s -> s | _ -> ""
                  in
                  let st =
                    match member "status" item with
                    | `String s -> status_of_string (String.lowercase_ascii s)
                    | _ -> Unknown
                  in
                  if name = "" then None else Some {name; status = st})
                items
          | _ -> []
        in
        Some {status; checks; last_fetch = Unix.gettimeofday ()}
      with _ -> None)

(** Get cached health for an instance *)
let get ~instance = with_lock (fun () -> Hashtbl.find_opt cache instance)

(** Set health for an instance *)
let set ~instance data =
  with_lock (fun () -> Hashtbl.replace cache instance data)

(** Clear cache for a specific instance *)
let clear_instance ~instance =
  with_lock (fun () -> Hashtbl.remove cache instance)

(** Clear all cache *)
let clear () = with_lock (fun () -> Hashtbl.clear cache)
