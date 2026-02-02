(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(* Cache entry with timestamp *)
type cache_entry = { body : string; timestamp : float }

(* In-memory cache: (rpc_addr, path) -> entry *)
let cache : (string * string, cache_entry) Hashtbl.t = Hashtbl.create 97

let cache_lock = Mutex.create ()

(* Tool availability cache *)
let curl_available = ref None

let wget_available = ref None

let check_lock = Mutex.create ()

let has_curl () =
  Mutex.lock check_lock ;
  let result =
    match !curl_available with
    | Some v -> v
    | None ->
        let available =
          match Common.which "curl" with Some _ -> true | None -> false
        in
        curl_available := Some available ;
        available
  in
  Mutex.unlock check_lock ;
  result

let has_wget () =
  Mutex.lock check_lock ;
  let result =
    match !wget_available with
    | Some v -> v
    | None ->
        let available =
          match Common.which "wget" with Some _ -> true | None -> false
        in
        wget_available := Some available ;
        available
  in
  Mutex.unlock check_lock ;
  result

let build_url ~rpc_addr ~path =
  let base =
    if
      String.starts_with ~prefix:"http://" rpc_addr
      || String.starts_with ~prefix:"https://" rpc_addr
    then rpc_addr
    else "http://" ^ rpc_addr
  in
  let path =
    if String.length path > 0 && path.[0] = '/' then path else "/" ^ path
  in
  base ^ path

let get ~rpc_addr ~path ?(timeout = 2.0) () =
  let url = build_url ~rpc_addr ~path in
  let timeout_str = Printf.sprintf "%.1f" timeout in
  let result =
    if has_curl () then
      let cmd =
        [
          "curl";
          "-sf";
          "-m";
          timeout_str;
          "--connect-timeout";
          "0.8";
          url;
        ]
      in
      Common.run_out_silent cmd
    else if has_wget () then
      let timeout_int = int_of_float (timeout +. 0.5) in
      let cmd =
        [
          "wget";
          "-qO-";
          "--timeout=" ^ string_of_int timeout_int;
          "--tries=1";
          url;
        ]
      in
      Common.run_out_silent cmd
    else Error (`Msg "No HTTP client available (curl or wget required)")
  in
  match result with Ok body -> Ok body | Error (`Msg e) -> Error e

let get_cached ~rpc_addr ~path ~ttl =
  Mutex.lock cache_lock ;
  let result =
    match Hashtbl.find_opt cache (rpc_addr, path) with
    | Some { body; timestamp } when Unix.gettimeofday () -. timestamp < ttl ->
        Some body
    | Some _ ->
        Hashtbl.remove cache (rpc_addr, path) ;
        None
    | None -> None
  in
  Mutex.unlock cache_lock ;
  result

let cache_put ~rpc_addr ~path ~body =
  Mutex.lock cache_lock ;
  Hashtbl.replace cache (rpc_addr, path)
    { body; timestamp = Unix.gettimeofday () } ;
  Mutex.unlock cache_lock

let clear_cache () =
  Mutex.lock cache_lock ;
  Hashtbl.clear cache ;
  Mutex.unlock cache_lock
