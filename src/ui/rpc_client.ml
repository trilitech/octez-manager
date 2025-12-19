(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module Bg = Background_runner

(* Helper to compute endpoint URL for a service. *)
let endpoint_of (s : Service.t) =
  if String.starts_with ~prefix:"http" s.rpc_addr then s.rpc_addr
  else "http://" ^ s.rpc_addr

let octez_client_bin (s : Service.t) =
  Filename.concat s.app_bin_dir "octez-client"

let rpc_get (s : Service.t) path =
  let argv =
    [octez_client_bin s; "--endpoint"; endpoint_of s; "rpc"; "get"; path]
  in
  let cmd_s = Common.cmd_to_string argv ^ " 2>/dev/null" in
  Common.run_out ["/bin/sh"; "-lc"; cmd_s]

let curl_available () = Sys.command "command -v curl >/dev/null 2>&1" = 0

let wget_available () = Sys.command "command -v wget >/dev/null 2>&1" = 0

(* Cache curl availability to avoid a shell probe on each request. *)
let has_curl_cached = lazy (curl_available ())

let rec try_fetch_methods last_err = function
  | [] -> (
      match last_err with
      | Some e -> Error e
      | None -> Error "no HTTP methods available")
  | m :: rest -> (
      match m () with
      | None -> try_fetch_methods last_err rest
      | Some (Ok _ as ok) -> ok
      | Some (Error e) -> try_fetch_methods (Some e) rest)

(* Simple concurrency limiter for external HTTP commands (curl/wget/rpc_get). *)
let max_concurrent_requests =
  match Sys.getenv_opt "OCTEZ_RPC_MAX_CONCURRENT" with
  | Some s -> ( try int_of_string s with _ -> 2)
  | None -> 2

let current_requests = ref 0

let request_mutex = Mutex.create ()

let request_cond = Condition.create ()

let with_request_slot f =
  Mutex.lock request_mutex ;
  try
    while !current_requests >= max_concurrent_requests do
      Condition.wait request_cond request_mutex
    done ;
    incr current_requests ;
    Mutex.unlock request_mutex ;
    let res =
      try f ()
      with e ->
        Mutex.lock request_mutex ;
        decr current_requests ;
        Condition.signal request_cond ;
        Mutex.unlock request_mutex ;
        raise e
    in
    Mutex.lock request_mutex ;
    decr current_requests ;
    Condition.signal request_cond ;
    Mutex.unlock request_mutex ;
    res
  with e ->
    Mutex.unlock request_mutex ;
    raise e

let absolutize_url (s : Service.t) (path : string) : string =
  let base = endpoint_of s in
  if String.length path > 0 && path.[0] = '/' then base ^ path
  else base ^ "/" ^ path

let http_fetch_methods ~url ~rpc_path (s : Service.t) :
    (unit -> (string, string) result option) list =
  let via_curl () =
    if Lazy.force has_curl_cached then
      Some
        (with_request_slot (fun () ->
             match
               Common.run_out
                 ["curl"; "-sfm"; "2"; "--connect-timeout"; "0.8"; url]
             with
             | Ok s -> Ok s
             | Error (`Msg m) -> Error m))
    else None
  in
  let via_wget () =
    if wget_available () then
      Some
        (with_request_slot (fun () ->
             match
               Common.run_out ["wget"; "-qO-"; "--timeout=1"; "--tries=1"; url]
             with
             | Ok s -> Ok s
             | Error (`Msg m) -> Error m))
    else None
  in
  let via_rpc_get () =
    Some
      (with_request_slot (fun () ->
           match rpc_get s rpc_path with
           | Ok s -> Ok s
           | Error (`Msg m) -> Error m))
  in
  [via_curl; via_wget; via_rpc_get]

let http_get_string (s : Service.t) (path : string) =
  let url = absolutize_url s path in
  let rpc_path =
    if String.length path > 0 && path.[0] = '/' then path else "/" ^ path
  in
  try_fetch_methods None (http_fetch_methods ~url ~rpc_path s)

let http_get_url (s : Service.t) (path : string) =
  let url =
    if String.starts_with ~prefix:"http" path then path
    else absolutize_url s path
  in
  let rpc_path =
    if String.length path > 0 && path.[0] = '/' then path else "/" ^ path
  in
  try_fetch_methods None (http_fetch_methods ~url ~rpc_path s)

let chain_id_cache : (string, string option) Hashtbl.t = Hashtbl.create 31

let head_level_cache : (string, int option) Hashtbl.t = Hashtbl.create 31

let bootstrapped_cache : (string, bool option) Hashtbl.t = Hashtbl.create 31

let version_cache : (string, string option) Hashtbl.t = Hashtbl.create 31

let last_refresh_chain_id : (string, float) Hashtbl.t = Hashtbl.create 31

let last_refresh_head : (string, float) Hashtbl.t = Hashtbl.create 31

let last_refresh_boot : (string, float) Hashtbl.t = Hashtbl.create 31

let last_refresh_version : (string, float) Hashtbl.t = Hashtbl.create 31

let last_error : (string, string) Hashtbl.t = Hashtbl.create 31

let refresh_period_head = 3.5

let refresh_period_boot = 5.5

let refresh_period_version = 3600.0

let refresh_period_chain = 3600.0

let cache_lock = Mutex.create ()

let set_error (s : Service.t) msg =
  Mutex.lock cache_lock ;
  Hashtbl.replace last_error s.rpc_addr msg ;
  Mutex.unlock cache_lock

let clear_error (s : Service.t) =
  Mutex.lock cache_lock ;
  Hashtbl.remove last_error s.rpc_addr ;
  Mutex.unlock cache_lock

let rpc_last_error (s : Service.t) =
  Mutex.lock cache_lock ;
  let v = Hashtbl.find_opt last_error s.rpc_addr in
  Mutex.unlock cache_lock ;
  v

let rpc_head_header (s : Service.t) : int option =
  let now = Unix.gettimeofday () in
  let fresh =
    Mutex.lock cache_lock ;
    let res =
      match Hashtbl.find_opt last_refresh_head s.rpc_addr with
      | Some t when now -. t < refresh_period_head -> true
      | _ -> false
    in
    Mutex.unlock cache_lock ;
    res
  in
  if fresh then (
    Mutex.lock cache_lock ;
    let res = Hashtbl.find_opt head_level_cache s.rpc_addr |> Option.join in
    Mutex.unlock cache_lock ;
    res)
  else
    let v =
      match http_get_string s "/chains/main/blocks/head/header" with
      | Ok out -> (
          clear_error s ;
          try
            let j = Yojson.Safe.from_string out in
            let open Yojson.Safe.Util in
            Some (to_int (member "level" j))
          with _ -> None)
      | Error m ->
          set_error s m ;
          None
    in
    Mutex.lock cache_lock ;
    Hashtbl.replace head_level_cache s.rpc_addr v ;
    Hashtbl.replace last_refresh_head s.rpc_addr now ;
    Mutex.unlock cache_lock ;
    v

let rpc_head_header_cached (s : Service.t) : int option =
  Mutex.lock cache_lock ;
  let v = Hashtbl.find_opt head_level_cache s.rpc_addr in
  Mutex.unlock cache_lock ;
  match v with Some v -> v | None -> None

let rpc_chain_id (s : Service.t) : string option =
  let now = Unix.gettimeofday () in
  let fresh =
    Mutex.lock cache_lock ;
    let res =
      match Hashtbl.find_opt last_refresh_chain_id s.rpc_addr with
      | Some t when now -. t < refresh_period_chain -> true
      | _ -> false
    in
    Mutex.unlock cache_lock ;
    res
  in
  if fresh then (
    Mutex.lock cache_lock ;
    let res = Hashtbl.find_opt chain_id_cache s.rpc_addr |> Option.join in
    Mutex.unlock cache_lock ;
    res)
  else
    let v =
      match http_get_string s "/chains/main/chain_id" with
      | Ok out -> (
          clear_error s ;
          try
            let j = Yojson.Safe.from_string out in
            let open Yojson.Safe.Util in
            Some (to_string j)
          with _ -> None)
      | Error m ->
          set_error s m ;
          None
    in
    Mutex.lock cache_lock ;
    Hashtbl.replace chain_id_cache s.rpc_addr v ;
    Hashtbl.replace last_refresh_chain_id s.rpc_addr now ;
    Mutex.unlock cache_lock ;
    v

let rpc_chain_id_cached (s : Service.t) : string option =
  Mutex.lock cache_lock ;
  let v = Hashtbl.find_opt chain_id_cache s.rpc_addr in
  Mutex.unlock cache_lock ;
  match v with Some v -> v | None -> None

let rpc_protocol (s : Service.t) : string option =
  match http_get_string s "/chains/main/blocks/head/metadata" with
  | Ok out -> (
      clear_error s ;
      try
        let j = Yojson.Safe.from_string out in
        let open Yojson.Safe.Util in
        match member "protocol" j with
        | `String p -> Some p
        | _ -> (
            match member "next_protocol" j with
            | `String p -> Some p
            | _ -> None)
      with _ -> None)
  | Error m ->
      set_error s m ;
      None

let rpc_is_bootstrapped (s : Service.t) : bool option =
  let now = Unix.gettimeofday () in
  let fresh =
    Mutex.lock cache_lock ;
    let res =
      match Hashtbl.find_opt last_refresh_boot s.rpc_addr with
      | Some t when now -. t < refresh_period_boot -> true
      | _ -> false
    in
    Mutex.unlock cache_lock ;
    res
  in
  if fresh then (
    Mutex.lock cache_lock ;
    let res = Hashtbl.find_opt bootstrapped_cache s.rpc_addr |> Option.join in
    Mutex.unlock cache_lock ;
    res)
  else
    let v =
      match http_get_string s "/chains/main/is_bootstrapped" with
      | Ok out -> (
          clear_error s ;
          try
            let j = Yojson.Safe.from_string out in
            let open Yojson.Safe.Util in
            match member "bootstrapped" j with
            | `Bool b -> Some b
            | _ -> (
                match member "sync_state" j with
                | `String s when String.lowercase_ascii s = "synced" ->
                    Some true
                | _ -> None)
          with _ -> None)
      | Error m ->
          set_error s m ;
          None
    in
    Mutex.lock cache_lock ;
    Hashtbl.replace bootstrapped_cache s.rpc_addr v ;
    Hashtbl.replace last_refresh_boot s.rpc_addr now ;
    Mutex.unlock cache_lock ;
    v

let node_version (s : Service.t) : string option =
  let now = Unix.gettimeofday () in
  let key = s.Service.instance in
  let fresh =
    Mutex.lock cache_lock ;
    let res =
      match Hashtbl.find_opt last_refresh_version key with
      | Some t when now -. t < refresh_period_version -> true
      | _ -> false
    in
    Mutex.unlock cache_lock ;
    res
  in
  if fresh then (
    Mutex.lock cache_lock ;
    let res = Hashtbl.find_opt version_cache key |> Option.join in
    Mutex.unlock cache_lock ;
    res)
  else
    let bin = Filename.concat s.app_bin_dir "octez-node" in
    (* Use shell to avoid SIGPIPE issues - octez-node doesn't handle broken pipes gracefully *)
    let cmd =
      Printf.sprintf "timeout 2s %s --version 2>/dev/null" (Common.sh_quote bin)
    in
    let v =
      match Common.run_out ["sh"; "-c"; cmd] with
      | Ok out ->
          clear_error s ;
          Some (String.trim out)
      | Error (`Msg m) ->
          set_error s m ;
          None
    in
    Mutex.lock cache_lock ;
    Hashtbl.replace version_cache key v ;
    Hashtbl.replace last_refresh_version key now ;
    Mutex.unlock cache_lock ;
    v

let rpc_is_bootstrapped_cached (s : Service.t) : bool option =
  Mutex.lock cache_lock ;
  let v = Hashtbl.find_opt bootstrapped_cache s.rpc_addr in
  Mutex.unlock cache_lock ;
  match v with Some v -> v | None -> None

(* Head monitor stream: keep a single connection per node to reduce socket
  churn. We stream /monitor/heads/main and push level/protocol/chain updates
  to a callback. *)

type monitor_handle = {stop : unit -> unit; alive : unit -> bool}

let start_head_monitor (s : Service.t) ~on_head ~on_disconnect : monitor_handle
    =
  let stopped = ref false in
  let running = ref true in
  let ic_ref = ref None in
  let url = absolutize_url s "/monitor/heads/main" in
  let cmd = Printf.sprintf "curl -sN --connect-timeout 2 --no-buffer %s" url in
  let run () =
    let ic = Unix.open_process_in cmd in
    ic_ref := Some ic ;
    let rec loop () =
      if !stopped then ()
      else
        match input_line ic with
        | exception End_of_file -> ()
        | exception Sys_error _ -> () (* Channel closed by stop() *)
        | line ->
            (try
               let j = Yojson.Safe.from_string line in
               let open Yojson.Safe.Util in
               let level =
                 match member "level" j with `Int l -> Some l | _ -> None
               in
               let proto =
                 match member "protocol" j with
                 | `String p -> Some p
                 | _ -> None
               in
               let chain_id =
                 match member "chain_id" j with
                 | `String c -> Some c
                 | _ -> None
               in
               on_head ~level ~proto ~chain_id
             with _ -> ()) ;
            loop ()
    in
    loop () ;
    ic_ref := None ;
    (try ignore (Unix.close_process_in ic) with _ -> ()) ;
    running := false ;
    on_disconnect ()
  in
  Bg.submit_blocking run ;
  let stop () =
    stopped := true ;
    (* Close the channel to unblock input_line and kill the curl process *)
    (match !ic_ref with
    | Some ic -> ( try close_in_noerr ic with _ -> ())
    | None -> ()) ;
    ()
  in
  let alive () = !running in
  {stop; alive}
