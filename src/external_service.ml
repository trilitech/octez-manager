(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** {1 Role Detection} *)

type role = Node | Baker | Accuser | Dal_node | Unknown of string

let role_to_string = function
  | Node -> "node"
  | Baker -> "baker"
  | Accuser -> "accuser"
  | Dal_node -> "dal-node"
  | Unknown s -> s

let role_of_string = function
  | "node" -> Node
  | "baker" -> Baker
  | "accuser" -> Accuser
  | "dal-node" | "dal" -> Dal_node
  | s -> Unknown s

let role_of_binary_name ?subcommand binary_name =
  let name = Filename.basename binary_name |> String.lowercase_ascii in
  (* Check for DAL subcommand first - octez-baker can run DAL node *)
  match subcommand with
  | Some "dal" -> Dal_node
  | _ ->
      if String.starts_with ~prefix:"octez-node" name then Node
      else if
        String.starts_with ~prefix:"octez-baker" name
        || String.starts_with ~prefix:"tezos-baker" name
      then Baker
      else if
        String.starts_with ~prefix:"octez-accuser" name
        || String.starts_with ~prefix:"tezos-accuser" name
      then Accuser
      else if String.starts_with ~prefix:"octez-dal-node" name then Dal_node
      else Unknown name

(** {1 Confidence Tracking} *)

type confidence = Detected | Inferred | Permission_denied | Unknown

let confidence_to_string = function
  | Detected -> "detected"
  | Inferred -> "inferred"
  | Permission_denied -> "permission-denied"
  | Unknown -> "unknown"

type 'a field = {value : 'a option; confidence : confidence; source : string}

let unknown () = {value = None; confidence = Unknown; source = "unknown"}

let detected ~source value = {value = Some value; confidence = Detected; source}

let inferred ~source value = {value = Some value; confidence = Inferred; source}

let permission_denied ~source =
  {value = None; confidence = Permission_denied; source}

let is_known field =
  match field.confidence with Detected | Inferred -> true | _ -> false

let value_or ~default field =
  match field.value with Some v -> v | None -> default

(** {1 Systemd Unit State} *)

type unit_state = {
  active_state : string;
  sub_state : string;
  enabled : bool option;
}

(** {1 Detected Configuration} *)

type detected_config = {
  (* Always available from systemd *)
  unit_name : string;
  unit_file_path : string option;
  exec_start : string;
  unit_state : unit_state;
  (* From systemd show properties *)
  user : string option;
  group : string option;
  working_dir : string option;
  environment_files : string list;
  (* Inferred from binary/args/env/config *)
  role : role field;
  binary_path : string field;
  binary_version : string field;
  data_dir : string field;
  rpc_addr : string field;
  net_addr : string field;
  network : string field;
  history_mode : string field;
  (* Baker/Accuser specific *)
  node_endpoint : string field;
  base_dir : string field;
  delegates : string list field;
  (* DAL specific *)
  dal_endpoint : string field;
  (* Logging *)
  daily_logs_dir : string option;
      (** Path to daily_logs directory if it exists *)
  (* Unparsed *)
  extra_args : string list;
  parse_warnings : string list;
}

(** {1 External Service} *)

type t = {config : detected_config; suggested_instance_name : string}

(** {1 Status} *)

type status =
  | Running
  | Stopped
  | Disabled
  | Failed of string
  | Unknown of string

let status_of_unit_state state =
  match (state.active_state, state.sub_state, state.enabled) with
  | "active", "running", _ -> Running
  | "inactive", "dead", Some false -> Disabled
  | "inactive", _, _ -> Stopped
  | "failed", sub, _ -> Failed sub
  | active, sub, _ -> Unknown (active ^ "/" ^ sub)

let status_label = function
  | Running -> "running"
  | Stopped -> "stopped"
  | Disabled -> "disabled"
  | Failed msg -> "failed (" ^ msg ^ ")"
  | Unknown msg -> "unknown (" ^ msg ^ ")"

(** {1 Constructors} *)

let empty_config ~unit_name ~exec_start ~unit_state =
  {
    unit_name;
    unit_file_path = None;
    exec_start;
    unit_state;
    user = None;
    group = None;
    working_dir = None;
    environment_files = [];
    role = unknown ();
    binary_path = unknown ();
    binary_version = unknown ();
    data_dir = unknown ();
    rpc_addr = unknown ();
    net_addr = unknown ();
    network = unknown ();
    history_mode = unknown ();
    node_endpoint = unknown ();
    base_dir = unknown ();
    delegates = unknown ();
    dal_endpoint = unknown ();
    daily_logs_dir = None;
    extra_args = [];
    parse_warnings = [];
  }

let suggest_instance_name ~unit_name =
  let name =
    if String.ends_with ~suffix:".service" unit_name then
      String.sub unit_name 0 (String.length unit_name - 8)
    else unit_name
  in
  (* Remove common prefixes to keep names short *)
  if String.starts_with ~prefix:"octez-" name then
    String.sub name 6 (String.length name - 6)
  else if String.starts_with ~prefix:"tezos-" name then
    String.sub name 6 (String.length name - 6)
  else name

(** {1 Inspection} *)

let unknown_field_count config =
  let count_unknown f = if f.confidence = Unknown then 1 else 0 in
  count_unknown config.role
  + count_unknown config.binary_path
  + count_unknown config.data_dir
  + count_unknown config.rpc_addr
  + count_unknown config.network

let unknown_field_names config =
  let check name field acc =
    if field.confidence = Unknown then name :: acc else acc
  in
  [] |> check "role" config.role
  |> check "binary_path" config.binary_path
  |> check "data_dir" config.data_dir
  |> check "rpc_addr" config.rpc_addr
  |> check "network" config.network
  |> List.rev

let permission_denied_fields config =
  let check name field acc =
    if field.confidence = Permission_denied then name :: acc else acc
  in
  [] |> check "role" config.role
  |> check "binary_path" config.binary_path
  |> check "data_dir" config.data_dir
  |> check "rpc_addr" config.rpc_addr
  |> check "network" config.network
  |> List.rev
