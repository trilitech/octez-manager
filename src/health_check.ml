(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Health checks for Octez services.

    Provides functions to wait for services to be ready by polling their
    RPC endpoints. Used during sequential restart to ensure services are
    fully operational before starting dependents. *)

type check_result = {ready : bool; elapsed : float; message : string}

(** Default timeouts in seconds *)
let default_node_timeout = 120.0

let default_dal_timeout = 30.0

let default_poll_interval = 2.0

(** Make an HTTP GET request and return the response body *)
let http_get ~timeout_secs url =
  let tmp = Filename.temp_file "health_check" ".out" in
  let cmd =
    Printf.sprintf
      "curl -sf --connect-timeout 2 --max-time %.0f %s > %s 2>/dev/null"
      timeout_secs
      (Filename.quote url)
      (Filename.quote tmp)
  in
  let result =
    match Sys.command cmd with
    | 0 -> (
        try
          let ic = open_in tmp in
          let body = really_input_string ic (in_channel_length ic) in
          close_in ic ;
          Ok (String.trim body)
        with _ -> Error "Failed to read response")
    | _ -> Error "Request failed"
  in
  (try Sys.remove tmp with _ -> ()) ;
  result

(** Check if node RPC is responding *)
let check_node_rpc ~host ~port =
  let url = Printf.sprintf "http://%s:%d/version" host port in
  match http_get ~timeout_secs:5.0 url with Ok _ -> true | Error _ -> false

(** Check if DAL node RPC is responding *)
let check_dal_rpc ~host ~port =
  (* DAL node has /health endpoint *)
  let url = Printf.sprintf "http://%s:%d/health" host port in
  match http_get ~timeout_secs:5.0 url with
  | Ok _ -> true
  | Error _ -> (
      (* Fallback to /version *)
      let url = Printf.sprintf "http://%s:%d/version" host port in
      match http_get ~timeout_secs:5.0 url with
      | Ok _ -> true
      | Error _ -> false)

(** Parse host:port address *)
let parse_addr addr =
  match String.rindex_opt addr ':' with
  | Some idx -> (
      let host = String.sub addr 0 idx in
      let port_str = String.sub addr (idx + 1) (String.length addr - idx - 1) in
      match int_of_string_opt port_str with
      | Some port -> Some (host, port)
      | None -> None)
  | None -> None

(** Wait for node RPC to be ready.
    @param rpc_addr The RPC address (host:port)
    @param timeout Maximum time to wait in seconds
    @param on_progress Optional callback called during polling with elapsed time
    @return check_result with ready status and timing info *)
let wait_for_node_rpc ?(timeout = default_node_timeout)
    ?(poll_interval = default_poll_interval) ?on_progress ~rpc_addr () =
  match parse_addr rpc_addr with
  | None ->
      {ready = false; elapsed = 0.0; message = "Invalid RPC address format"}
  | Some (host, port) ->
      let start = Unix.gettimeofday () in
      let rec poll () =
        let elapsed = Unix.gettimeofday () -. start in
        if elapsed >= timeout then
          {ready = false; elapsed; message = "Timeout waiting for RPC"}
        else if check_node_rpc ~host ~port then
          {
            ready = true;
            elapsed;
            message = Printf.sprintf "RPC ready at %s" rpc_addr;
          }
        else (
          Option.iter (fun f -> f elapsed) on_progress ;
          Unix.sleepf poll_interval ;
          poll ())
      in
      poll ()

(** Wait for DAL node RPC to be ready.
    @param rpc_addr The RPC address (host:port)
    @param timeout Maximum time to wait in seconds
    @param on_progress Optional callback called during polling with elapsed time
    @return check_result with ready status and timing info *)
let wait_for_dal_rpc ?(timeout = default_dal_timeout)
    ?(poll_interval = default_poll_interval) ?on_progress ~rpc_addr () =
  match parse_addr rpc_addr with
  | None ->
      {ready = false; elapsed = 0.0; message = "Invalid RPC address format"}
  | Some (host, port) ->
      let start = Unix.gettimeofday () in
      let rec poll () =
        let elapsed = Unix.gettimeofday () -. start in
        if elapsed >= timeout then
          {ready = false; elapsed; message = "Timeout waiting for DAL RPC"}
        else if check_dal_rpc ~host ~port then
          {
            ready = true;
            elapsed;
            message = Printf.sprintf "DAL RPC ready at %s" rpc_addr;
          }
        else (
          Option.iter (fun f -> f elapsed) on_progress ;
          Unix.sleepf poll_interval ;
          poll ())
      in
      poll ()

(** Wait for a service to be ready based on its role.
    @param svc The service to check
    @param timeout Maximum time to wait
    @param on_progress Optional progress callback
    @return check_result *)
let wait_for_service ?(timeout = default_node_timeout) ?on_progress
    (svc : Service.t) =
  match svc.role with
  | "node" -> wait_for_node_rpc ~timeout ?on_progress ~rpc_addr:svc.rpc_addr ()
  | "dal-node" | "dal" ->
      wait_for_dal_rpc
        ~timeout:(min timeout default_dal_timeout)
        ?on_progress
        ~rpc_addr:svc.rpc_addr
        ()
  | _ ->
      (* Baker, accuser don't have RPC to check - consider them ready immediately *)
      {ready = true; elapsed = 0.0; message = "No RPC to check"}
