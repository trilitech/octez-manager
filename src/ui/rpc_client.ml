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

(* RPC caches with per-key TTL *)
let head_level_cache =
  Cache.create_safe_keyed ~name:"rpc_head_level" ~ttl:3.5 ()

let bootstrapped_cache =
  Cache.create_safe_keyed ~name:"rpc_bootstrapped" ~ttl:5.5 ()

let chain_id_cache = Cache.create_safe_keyed ~name:"rpc_chain_id" ~ttl:3600.0 ()

let version_cache = Cache.create_safe_keyed ~name:"rpc_version" ~ttl:3600.0 ()

let last_error_cache = Cache.create_safe_keyed ~name:"rpc_errors" ~ttl:60.0 ()

let set_error (s : Service.t) msg =
  Cache.set_safe_keyed last_error_cache s.rpc_addr (Some msg)

let clear_error (s : Service.t) =
  Cache.remove_safe_keyed last_error_cache s.rpc_addr

let rpc_last_error (s : Service.t) =
  Cache.get_safe_keyed_cached last_error_cache s.rpc_addr |> Option.join

let rpc_head_header (s : Service.t) : int option =
  Cache.get_safe_keyed head_level_cache s.rpc_addr ~fetch:(fun () ->
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
          None)

let rpc_head_header_cached (s : Service.t) : int option =
  Cache.get_safe_keyed_cached head_level_cache s.rpc_addr |> Option.join

let rpc_chain_id (s : Service.t) : string option =
  Cache.get_safe_keyed chain_id_cache s.rpc_addr ~fetch:(fun () ->
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
          None)

let rpc_chain_id_cached (s : Service.t) : string option =
  Cache.get_safe_keyed_cached chain_id_cache s.rpc_addr |> Option.join

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
  Cache.get_safe_keyed bootstrapped_cache s.rpc_addr ~fetch:(fun () ->
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
                | `String ss when String.lowercase_ascii ss = "synced" ->
                    Some true
                | _ -> None)
          with _ -> None)
      | Error m ->
          set_error s m ;
          None)

let node_version (s : Service.t) : string option =
  let key = s.Service.instance in
  Cache.get_safe_keyed version_cache key ~fetch:(fun () ->
      let bin = Filename.concat s.app_bin_dir "octez-node" in
      match Common.run_out ["timeout"; "2s"; bin; "--version"] with
      | Ok out ->
          clear_error s ;
          Some (String.trim out)
      | Error (`Msg m) ->
          set_error s m ;
          None)

let rpc_is_bootstrapped_cached (s : Service.t) : bool option =
  Cache.get_safe_keyed_cached bootstrapped_cache s.rpc_addr |> Option.join

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
  (* Use --max-time to ensure curl doesn't hang forever *)
  let cmd =
    Printf.sprintf
      "curl -sN --connect-timeout 2 --max-time 86400 --no-buffer %s"
      url
  in
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
  (* Head monitors run in dedicated domains instead of the shared background
     worker pool. Each monitor blocks forever (streaming curl -sN), so using
     Bg.submit_blocking would permanently consume a worker and starve user
     operations (stop/restart) when the number of nodes >= num_workers. *)
  ignore (Domain.spawn run) ;
  let stop () =
    stopped := true ;
    (* Just set the flag - don't try to close the channel or wait.
       The background worker will exit on next input_line timeout/error
       and clean up the process. Since we're exiting anyway, orphaned
       curl processes will be cleaned up by the OS. *)
    ()
  in
  let alive () = !running in
  {stop; alive}
