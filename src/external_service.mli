(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** External (non-octez-manager-managed) service detection types.

    These types represent Octez services discovered on the system that were
    not installed by octez-manager. They support confidence tracking to
    distinguish detected values from inferred or unknown ones. *)

(** {1 Role Detection} *)

(** Service role detected from binary name. *)
type role = Node | Baker | Accuser | Dal_node | Unknown of string

val role_to_string : role -> string

val role_of_string : string -> role

(** Map binary name to role.
    @param subcommand Optional subcommand (e.g., "dal" for octez-baker run dal)
    @param binary_name e.g., "octez-node", "octez-baker-PsParisC" *)
val role_of_binary_name : ?subcommand:string -> string -> role

(** {1 Confidence Tracking} *)

(** Confidence level for configuration values.
    - [Detected]: Directly observed (e.g., from ExecStart argument)
    - [Inferred]: Derived from other data (e.g., network from chain_id)
    - [Permission_denied]: Could not read due to permissions
    - [Unknown]: Could not determine *)
type confidence = Detected | Inferred | Permission_denied | Unknown

val confidence_to_string : confidence -> string

(** A configuration field with confidence tracking.
    @param value The detected/inferred value, if any
    @param confidence How confident we are in this value
    @param source Human-readable description of where this came from *)
type 'a field = {value : 'a option; confidence : confidence; source : string}

(** Create a field with unknown value. *)
val unknown : unit -> 'a field

(** Create a field with a detected value.
    @param source Description of source, e.g., "ExecStart --data-dir" *)
val detected : source:string -> 'a -> 'a field

(** Create a field with an inferred value.
    @param source Description of source, e.g., "config.json network field" *)
val inferred : source:string -> 'a -> 'a field

(** Create a field indicating permission was denied.
    @param source What we tried to read, e.g., "/etc/octez/node.env" *)
val permission_denied : source:string -> 'a field

(** Check if a field has a known value (Detected or Inferred). *)
val is_known : 'a field -> bool

(** Get field value or a default. *)
val value_or : default:'a -> 'a field -> 'a

(** {1 Systemd Unit State} *)

(** State of a systemd unit. *)
type unit_state = {
  active_state : string;  (** "active", "inactive", "failed" *)
  sub_state : string;  (** "running", "dead", "exited", etc. *)
  enabled : bool option;  (** Enabled to start on boot *)
}

(** {1 Detected Configuration} *)

(** Configuration parsed from an external service. *)
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

(** A detected external service with metadata. *)
type t = {config : detected_config; suggested_instance_name : string}

(** {1 Status} *)

(** Display status, similar to Data.Service_state.status. *)
type status =
  | Running
  | Stopped
  | Disabled
  | Failed of string
  | Unknown of string

val status_of_unit_state : unit_state -> status

val status_label : status -> string

(** {1 Constructors} *)

(** Create an empty detected_config with only required fields. *)
val empty_config :
  unit_name:string ->
  exec_start:string ->
  unit_state:unit_state ->
  detected_config

(** Derive a suggested instance name from a unit name.
    E.g., "my-tezos-node.service" -> "my-tezos-node" *)
val suggest_instance_name : unit_name:string -> string

(** {1 Inspection} *)

(** Count how many required fields are unknown. *)
val unknown_field_count : detected_config -> int

(** List names of fields that are unknown (for UI display). *)
val unknown_field_names : detected_config -> string list

(** List names of fields with permission denied. *)
val permission_denied_fields : detected_config -> string list

(** {1 Dependency Resolution} *)

(** Get list of services this one depends on via endpoint matching.
    Matches node_endpoint and dal_endpoint against RPC addresses.
    @param external_svc The service to analyze
    @param all_services List of all external services to match against
    @return List of (unit_name, role_str) tuples *)
val get_dependencies : t -> t list -> (string * string) list

(** Get list of services that depend on this one (reverse lookup).
    @param external_svc The service to analyze
    @param all_services List of all external services to check
    @return List of (unit_name, role_str) tuples that depend on this *)
val get_dependents : t -> t list -> (string * string) list
