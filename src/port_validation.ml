(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
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

(** Collect RPC and P2P ports from registered node services, optionally
    excluding a specific instance. *)
let ports_from_services ?(exclude_instance : string option) () =
  match Service_registry.list () with
  | Error _ -> ([], [])
  | Ok services ->
      let dominated =
        services
        |> List.filter (fun (s : Service.t) ->
            s.role = "node" && Some s.instance <> exclude_instance)
      in
      let rpc_ports =
        dominated |> List.filter_map (fun s -> parse_port s.Service.rpc_addr)
      in
      let p2p_ports =
        dominated |> List.filter_map (fun s -> parse_port s.Service.net_addr)
      in
      (rpc_ports, p2p_ports)

(** Check if a port is owned by a specific instance (used for edit mode). *)
let port_owned_by_instance ~instance port =
  match Service_registry.find ~instance with
  | Ok (Some svc) when svc.Service.role = "node" ->
      parse_port svc.Service.rpc_addr = Some port
      || parse_port svc.Service.net_addr = Some port
  | _ -> false

(** Check if a port is in use by any running process.
    Uses netstat/ss to check. *)
let is_port_in_use port =
  (* Try ss first, fall back to netstat *)
  let check_with cmd =
    try
      let ic = Unix.open_process_in cmd in
      let output = In_channel.input_all ic in
      let _ = Unix.close_process_in ic in
      String.contains output ':'
      && String.contains output (Char.chr (48 + (port mod 10)))
    with _ -> false
  in
  let port_str = string_of_int port in
  let ss_cmd = Printf.sprintf "ss -tuln 2>/dev/null | grep -w ':%s'" port_str in
  let netstat_cmd =
    Printf.sprintf "netstat -tuln 2>/dev/null | grep -w ':%s'" port_str
  in
  check_with ss_cmd || check_with netstat_cmd

type validation_error =
  | Invalid_format of string
  | Port_out_of_range
  | Used_by_other_instance of int
  | Port_in_use of int

let pp_error = function
  | Invalid_format example ->
      Printf.sprintf "Must be host:port (e.g., %s)" example
  | Port_out_of_range -> "Port must be between 1024 and 65535"
  | Used_by_other_instance port ->
      Printf.sprintf "Port %d is used by another Octez instance" port
  | Port_in_use port -> Printf.sprintf "Port %d is in use" port

(** Validate a port address.
    @param addr The address in host:port format
    @param exclude_instance Optional instance to exclude from "in use" checks
    @param example Example format to show in error messages
    @return Ok () if valid, Error with reason if not *)
let validate_addr ~addr ?exclude_instance ~example () =
  match parse_host_port addr with
  | None -> Error (Invalid_format example)
  | Some (_host, port) ->
      if port < 1024 || port > 65535 then Error Port_out_of_range
      else
        let rpc_ports, p2p_ports = ports_from_services ?exclude_instance () in
        if List.mem port rpc_ports || List.mem port p2p_ports then
          Error (Used_by_other_instance port)
        else
          let owned_by_self =
            match exclude_instance with
            | Some inst -> port_owned_by_instance ~instance:inst port
            | None -> false
          in
          if is_port_in_use port && not owned_by_self then
            Error (Port_in_use port)
          else Ok ()

(** Validate an RPC address. *)
let validate_rpc_addr ?exclude_instance addr =
  validate_addr ~addr ?exclude_instance ~example:"127.0.0.1:8732" ()

(** Validate a P2P address. *)
let validate_p2p_addr ?exclude_instance addr =
  validate_addr ~addr ?exclude_instance ~example:"0.0.0.0:9732" ()
