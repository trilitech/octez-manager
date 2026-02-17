(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

let log msg = Cmd_runner.append_debug_log ("RPC_CLIENT " ^ msg)

(* Helper to compute endpoint URL for a service. *)
let endpoint_of (s : Service.t) = Rpc_addr.to_endpoint s.rpc_addr

let octez_client_bin (s : Service.t) =
  Filename.concat s.app_bin_dir "octez-client"

let rpc_get (s : Service.t) path =
  let argv =
    [octez_client_bin s; "--endpoint"; endpoint_of s; "rpc"; "get"; path]
  in
  let cmd_s = Cmd_runner.cmd_to_string argv ^ " 2>/dev/null" in
  log (Printf.sprintf "octez-client rpc get: %s (instance=%s)" cmd_s s.instance) ;
  let result = Cmd_runner.run_out ["/bin/sh"; "-lc"; cmd_s] in
  (match result with
  | Ok body ->
      log
        (Printf.sprintf
           "octez-client rpc get OK: %d bytes"
           (String.length body))
  | Error (`Msg m) -> log (Printf.sprintf "octez-client rpc get FAILED: %s" m)) ;
  result

(* Cache tool availability to avoid shell probes on each request. *)
let has_curl_cached = lazy (Sys.command "command -v curl >/dev/null 2>&1" = 0)

let has_wget_cached = lazy (Sys.command "command -v wget >/dev/null 2>&1" = 0)

let curl_available () = Lazy.force has_curl_cached

let wget_available () = Lazy.force has_wget_cached

let rec try_fetch_methods last_err = function
  | [] -> (
      log
        (Printf.sprintf
           "try_fetch_methods: all methods exhausted, last_err=%s"
           (match last_err with Some e -> e | None -> "<none>")) ;
      match last_err with
      | Some e -> Error e
      | None -> Error "no HTTP methods available")
  | m :: rest -> (
      match m () with
      | None ->
          log "try_fetch_methods: method not available, trying next" ;
          try_fetch_methods last_err rest
      | Some (Ok _ as ok) ->
          log "try_fetch_methods: method succeeded" ;
          ok
      | Some (Error e) ->
          log (Printf.sprintf "try_fetch_methods: method failed: %s" e) ;
          try_fetch_methods (Some e) rest)

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

let via_curl ~url () =
  if curl_available () then (
    log (Printf.sprintf "via_curl: url=%s" url) ;
    Some
      (with_request_slot (fun () ->
           match
             Cmd_runner.run_out
               ["curl"; "-sfm"; "2"; "--connect-timeout"; "0.8"; url]
           with
           | Ok s -> Ok s
           | Error (`Msg m) -> Error m)))
  else None

let via_wget ~url () =
  if wget_available () then (
    log (Printf.sprintf "via_wget: url=%s" url) ;
    Some
      (with_request_slot (fun () ->
           match
             Cmd_runner.run_out
               ["wget"; "-qO-"; "--timeout=1"; "--tries=1"; url]
           with
           | Ok s -> Ok s
           | Error (`Msg m) -> Error m)))
  else None

let via_rpc_get ~rpc_path (s : Service.t) () =
  log (Printf.sprintf "via_rpc_get: path=%s instance=%s" rpc_path s.instance) ;
  Some
    (with_request_slot (fun () ->
         match rpc_get s rpc_path with
         | Ok s -> Ok s
         | Error (`Msg m) -> Error m))

let http_fetch_methods ~url ~rpc_path (s : Service.t) :
    (unit -> (string, string) result option) list =
  [via_curl ~url; via_wget ~url; via_rpc_get ~rpc_path s]

let http_get_string (s : Service.t) (path : string) =
  let url = absolutize_url s path in
  let rpc_path =
    if String.length path > 0 && path.[0] = '/' then path else "/" ^ path
  in
  log
    (Printf.sprintf
       "http_get_string: path=%s url=%s instance=%s"
       path
       url
       s.instance) ;
  let result = try_fetch_methods None (http_fetch_methods ~url ~rpc_path s) in
  (match result with
  | Ok body ->
      log
        (Printf.sprintf
           "http_get_string: DONE OK (%d bytes)"
           (String.length body))
  | Error msg -> log (Printf.sprintf "http_get_string: DONE ERROR: %s" msg)) ;
  result

let http_get_url (s : Service.t) (path : string) =
  let url =
    if String.starts_with ~prefix:"http" path then path
    else absolutize_url s path
  in
  let rpc_path =
    if String.length path > 0 && path.[0] = '/' then path else "/" ^ path
  in
  log
    (Printf.sprintf
       "http_get_url: path=%s url=%s instance=%s"
       path
       url
       s.instance) ;
  let result = try_fetch_methods None (http_fetch_methods ~url ~rpc_path s) in
  (match result with
  | Ok body ->
      log
        (Printf.sprintf "http_get_url: DONE OK (%d bytes)" (String.length body))
  | Error msg -> log (Printf.sprintf "http_get_url: DONE ERROR: %s" msg)) ;
  result

(* RPC caches with per-key TTL *)
let head_level_cache =
  Cache.create_safe_keyed ~name:"rpc_head_level" ~ttl:3.5 ()

let bootstrapped_cache =
  Cache.create_safe_keyed ~name:"rpc_bootstrapped" ~ttl:5.5 ()

let chain_id_cache = Cache.create_safe_keyed ~name:"rpc_chain_id" ~ttl:3600.0 ()

let version_cache = Cache.create_safe_keyed ~name:"rpc_version" ~ttl:3600.0 ()

let last_error_cache = Cache.create_safe_keyed ~name:"rpc_errors" ~ttl:60.0 ()

let set_error (s : Service.t) msg =
  Cache.set_safe_keyed
    last_error_cache
    (Rpc_addr.to_string s.rpc_addr)
    (Some msg)

let clear_error (s : Service.t) =
  Cache.remove_safe_keyed last_error_cache (Rpc_addr.to_string s.rpc_addr)

let rpc_last_error (s : Service.t) =
  Cache.get_safe_keyed_cached last_error_cache (Rpc_addr.to_string s.rpc_addr)
  |> Option.join

let rpc_head_header (s : Service.t) : int option =
  Cache.get_safe_keyed
    head_level_cache
    (Rpc_addr.to_string s.rpc_addr)
    ~fetch:(fun () ->
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
  Cache.get_safe_keyed_cached head_level_cache (Rpc_addr.to_string s.rpc_addr)
  |> Option.join

let rpc_chain_id (s : Service.t) : string option =
  Cache.get_safe_keyed
    chain_id_cache
    (Rpc_addr.to_string s.rpc_addr)
    ~fetch:(fun () ->
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
  Cache.get_safe_keyed_cached chain_id_cache (Rpc_addr.to_string s.rpc_addr)
  |> Option.join

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
  Cache.get_safe_keyed
    bootstrapped_cache
    (Rpc_addr.to_string s.rpc_addr)
    ~fetch:(fun () ->
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
      match Cmd_runner.run_out ["timeout"; "2s"; bin; "--version"] with
      | Ok out ->
          clear_error s ;
          Some (String.trim out)
      | Error (`Msg m) ->
          set_error s m ;
          None)

let rpc_is_bootstrapped_cached (s : Service.t) : bool option =
  Cache.get_safe_keyed_cached bootstrapped_cache (Rpc_addr.to_string s.rpc_addr)
  |> Option.join

(* Head monitor stream: keep a single connection per node to reduce socket
  churn. We stream /monitor/heads/main and push level/protocol/chain updates
  to a callback. *)

type monitor_handle = {stop : unit -> unit; alive : unit -> bool}

let parse_head_line line ~on_head =
  try
    let j = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let level = match member "level" j with `Int l -> Some l | _ -> None in
    let proto =
      match member "protocol" j with `String p -> Some p | _ -> None
    in
    let chain_id =
      match member "chain_id" j with `String c -> Some c | _ -> None
    in
    on_head ~level ~proto ~chain_id
  with _ -> ()

(* Generic RPC stream: open a long-lived curl connection and feed each line
   to the caller. Works for any streaming endpoint (monitor/*, etc.). *)

let run_rpc_stream_eio (Eio_process.Mgr mgr) ~stopped ~url ~on_line =
  Eio.Switch.run @@ fun sw ->
  let argv =
    [
      "curl";
      "-sN";
      "--connect-timeout";
      "2";
      "--max-time";
      "86400";
      "--no-buffer";
      url;
    ]
  in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let proc = Eio.Process.spawn ~sw mgr ~stdout:stdout_w argv in
  Eio.Flow.close stdout_w ;
  let reader =
    Eio.Buf_read.of_flow
      ~max_size:(10 * 1024 * 1024)
      (stdout_r :> _ Eio.Flow.source)
  in
  let rec loop () =
    match
      Eio.Fiber.first
        (fun () ->
          match Eio.Buf_read.line reader with
          | line -> `Line line
          | exception End_of_file -> `Eof)
        (fun () ->
          (* Periodically check the stop flag so we don't block forever
             waiting for the next JSON line from the streaming endpoint. *)
          let rec wait () =
            if Atomic.get stopped then `Stopped
            else (
              Eio_unix.sleep 0.5 ;
              wait ())
          in
          wait ())
    with
    | `Line line ->
        on_line line ;
        loop ()
    | `Eof -> ()
    | `Stopped -> ( try Eio.Process.signal proc Sys.sigterm with _ -> ())
  in
  loop () ;
  try ignore (Eio.Process.await proc) with _ -> ()

let run_rpc_stream_blocking ~stopped ~cmd ~on_line =
  let ic =
    (Unix.open_process_in
       cmd [@allow_forbidden "blocking I/O in background domain"])
  in
  let rec loop () =
    if Atomic.get stopped then ()
    else
      match input_line ic with
      | exception End_of_file -> ()
      | exception Sys_error _ -> ()
      | line ->
          on_line line ;
          loop ()
  in
  loop () ;
  try
    ignore
      ((Unix.close_process_in
       [@allow_forbidden "blocking I/O in background domain"])
         ic)
  with _ -> ()

let start_rpc_stream (s : Service.t) ~path ~on_line ~on_disconnect :
    monitor_handle =
  let stopped = Atomic.make false in
  let running = Atomic.make true in
  let url = absolutize_url s path in
  let run () =
    (match Eio_process.get_process_mgr () with
    | Some mgr -> run_rpc_stream_eio mgr ~stopped ~url ~on_line
    | None ->
        let cmd =
          Printf.sprintf
            "curl -sN --connect-timeout 2 --max-time 86400 --no-buffer %s"
            url
        in
        run_rpc_stream_blocking ~stopped ~cmd ~on_line) ;
    Atomic.set running false ;
    on_disconnect ()
  in
  Domain_pool.submit (fun () -> try run () with _ -> ()) ;
  let stop () = Atomic.set stopped true in
  let alive () = Atomic.get running in
  {stop; alive}

let start_head_monitor (s : Service.t) ~on_head ~on_disconnect : monitor_handle
    =
  start_rpc_stream
    s
    ~path:"/monitor/heads/main"
    ~on_line:(fun line -> parse_head_line line ~on_head)
    ~on_disconnect

module For_tests = struct
  let try_fetch_methods = try_fetch_methods

  let octez_client_bin = octez_client_bin

  let with_request_slot = with_request_slot
end
