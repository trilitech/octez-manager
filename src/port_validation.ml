(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Port validation utilities shared between CLI and UI. *)

let parse_host_port (s : string) : (string * int) option =
  match String.split_on_char ':' s with
  | [host; port] -> (
      try
        let p = int_of_string (String.trim port) in
        let h = String.trim host in
        if p > 0 && p < 65536 && h <> "" then Some (h, p) else None
      with _ -> None)
  | _ -> None

let parse_port addr =
  match String.split_on_char ':' addr with
  | [_; port_str] -> (
      try Some (int_of_string (String.trim port_str)) with _ -> None)
  | _ -> None

(** Cache for service ports to avoid repeated I/O during form validation.
    Short TTL since services can be added/removed. *)
let service_ports_cache : ((int * string) list * (int * string) list) option ref
    =
  ref None

let service_ports_cache_time = ref 0.0

let service_ports_cache_ttl = 2.0 (* seconds *)

let fetch_service_ports () =
  match Service_registry.list () with
  | Error _ -> ([], [])
  | Ok services ->
      (* Filter services that have RPC/P2P ports: nodes and DAL nodes *)
      let with_ports =
        services
        |> List.filter (fun (s : Service.t) ->
            s.role = "node" || s.role = "dal-node" || s.role = "dal")
      in
      let rpc_ports =
        with_ports
        |> List.filter_map (fun s ->
            parse_port s.Service.rpc_addr
            |> Option.map (fun p -> (p, s.Service.instance)))
      in
      let p2p_ports =
        with_ports
        |> List.filter_map (fun s ->
            parse_port s.Service.net_addr
            |> Option.map (fun p -> (p, s.Service.instance)))
      in
      (rpc_ports, p2p_ports)

(** Collect RPC and P2P ports from registered services (nodes and DAL nodes),
    optionally excluding a specific instance.
    Returns (port, instance_name) pairs. Uses cache to avoid I/O during typing. *)
let ports_from_services ?(exclude_instance : string option) () =
  let now = Unix.gettimeofday () in
  let rpc_ports, p2p_ports =
    match !service_ports_cache with
    | Some cached
      when now -. !service_ports_cache_time < service_ports_cache_ttl ->
        cached
    | _ ->
        let result = fetch_service_ports () in
        service_ports_cache := Some result ;
        service_ports_cache_time := now ;
        result
  in
  (* Filter out excluded instance *)
  match exclude_instance with
  | None -> (rpc_ports, p2p_ports)
  | Some inst ->
      let filter_inst = List.filter (fun (_, i) -> i <> inst) in
      (filter_inst rpc_ports, filter_inst p2p_ports)

(** Check if a port is owned by a specific instance (used for edit mode). *)
let port_owned_by_instance ~instance port =
  match Service_registry.find ~instance with
  | Ok (Some svc)
    when svc.Service.role = "node"
         || svc.Service.role = "dal-node"
         || svc.Service.role = "dal" ->
      parse_port svc.Service.rpc_addr = Some port
      || parse_port svc.Service.net_addr = Some port
  | _ -> false

(** Extract process name from ss output line.
    Example: "users:(("nginx",pid=1234,fd=5))" -> Some "nginx" *)
let extract_process_name line =
  (* Look for pattern: users:(("process_name", ... *)
  let re = Str.regexp {|users:(("\([^"]+\)"|} in
  try
    if Str.search_forward re line 0 >= 0 then Some (Str.matched_group 1 line)
    else None
  with Not_found -> None

(** Cache for port process checks, keyed by port number.
    Re-check every 1 second while port is being edited. *)
let port_process_cache : (int, string option option * float) Hashtbl.t =
  Hashtbl.create 16

let port_process_cache_ttl = 1.0 (* seconds *)

(** Clear the port process cache. Call when form opens to get fresh data. *)
let clear_port_process_cache () = Hashtbl.clear port_process_cache

(** Check if a port is in use by any running process.
    Returns Some process_name if in use, None otherwise.
    Uses ss (with -p for process info) or falls back to netstat.
    Results are cached per port with 1s TTL. *)
let get_port_process port =
  let now = Unix.gettimeofday () in
  match Hashtbl.find_opt port_process_cache port with
  | Some (cached, time) when now -. time < port_process_cache_ttl -> cached
  | _ ->
      let port_str = string_of_int port in
      (* Try ss with process info first (-p requires root or same user) *)
      let ss_cmd =
        Printf.sprintf "ss -tulnp 2>/dev/null | grep -w ':%s'" port_str
      in
      let try_cmd cmd =
        try
          let ic = Unix.open_process_in cmd in
          let output = In_channel.input_all ic in
          let _ = Unix.close_process_in ic in
          if String.trim output <> "" then Some output else None
        with _ -> None
      in
      let result =
        match try_cmd ss_cmd with
        | Some output -> (
            match extract_process_name output with
            | Some proc -> Some (Some proc)
            | None -> Some None (* Port in use but can't get process name *))
        | None -> (
            (* Fall back to basic ss/netstat without process info *)
            let ss_basic =
              Printf.sprintf "ss -tuln 2>/dev/null | grep -w ':%s'" port_str
            in
            let netstat_cmd =
              Printf.sprintf
                "netstat -tuln 2>/dev/null | grep -w ':%s'"
                port_str
            in
            match try_cmd ss_basic with
            | Some _ -> Some None
            | None -> (
                match try_cmd netstat_cmd with
                | Some _ -> Some None
                | None -> None))
      in
      Hashtbl.replace port_process_cache port (result, now) ;
      result

(** Check if a port is in use by any running process. *)
let is_port_in_use port = Option.is_some (get_port_process port)

type validation_error =
  | Invalid_format of string
  | Port_out_of_range
  | Used_by_other_instance of int * string
  | Port_in_use of int * string option

let pp_error = function
  | Invalid_format example ->
      Printf.sprintf "Must be host:port (e.g., %s)" example
  | Port_out_of_range -> "Port must be between 1024 and 65535"
  | Used_by_other_instance (port, instance) ->
      Printf.sprintf "Port %d is used by instance '%s'" port instance
  | Port_in_use (port, Some proc) ->
      Printf.sprintf "Port %d is used by process '%s'" port proc
  | Port_in_use (port, None) -> Printf.sprintf "Port %d is in use" port

(** Validate a port address.
    @param addr The address in host:port format
    @param exclude_instance Optional instance to exclude from "in use" checks
    @param example Example format to show in error messages
    @return Ok () if valid, Error with reason if not *)
let validate_addr ~addr ?exclude_instance ~example () =
  match parse_host_port addr with
  | None -> Error (Invalid_format example)
  | Some (_host, port) -> (
      if port < 1024 || port > 65535 then Error Port_out_of_range
      else
        let rpc_ports, p2p_ports = ports_from_services ?exclude_instance () in
        let all_ports = rpc_ports @ p2p_ports in
        let find_instance p =
          List.find_opt (fun (pt, _) -> pt = p) all_ports |> Option.map snd
        in
        match find_instance port with
        | Some instance -> Error (Used_by_other_instance (port, instance))
        | None -> (
            let owned_by_self =
              match exclude_instance with
              | Some inst -> port_owned_by_instance ~instance:inst port
              | None -> false
            in
            if owned_by_self then Ok ()
            else
              match get_port_process port with
              | Some proc_opt -> Error (Port_in_use (port, proc_opt))
              | None -> Ok ()))

(** Validate an RPC address. *)
let validate_rpc_addr ?exclude_instance addr =
  validate_addr ~addr ?exclude_instance ~example:"127.0.0.1:8732" ()

(** Validate a P2P address. *)
let validate_p2p_addr ?exclude_instance addr =
  validate_addr ~addr ?exclude_instance ~example:"0.0.0.0:9732" ()
