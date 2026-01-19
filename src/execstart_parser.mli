(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Parsing of systemd ExecStart command lines.

    This module extracts configuration from ExecStart commands for
    octez services. It handles various formats including direct
    binary invocations, shell scripts, and environment variable
    expansions. *)

(** {1 Types} *)

(** Parsed command line arguments and flags *)
type parsed_args = {
  binary_path : string option;
  subcommand : string option;
      (** Subcommand after binary, e.g., "run", "dal" *)
  data_dir : string option;
  base_dir : string option;
  rpc_addr : string option;
  net_addr : string option;
  endpoint : string option;
  history_mode : string option;
  network : string option;
  extra_args : string list;
  warnings : string list;
}

(** {1 Parsing} *)

(** Parse an ExecStart command line.
    
    Handles common patterns:
    - Direct: [/usr/bin/octez-node run --data-dir /path ...]
    - Shell: [/bin/sh -c 'octez-node run --data-dir /path ...']
    - Variables: [${APP_BIN_DIR}/octez-node run --data-dir=${DATA_DIR}]
    
    Returns parsed arguments with detected values and warnings for
    unparseable constructs. *)
val parse : string -> parsed_args

(** {1 Helpers} *)

(** Extract the binary path from ExecStart.
    Returns the first octez/tezos binary path found. *)
val extract_binary_path : string -> string option

(** Check if a string is a shell script invocation.
    Returns true for patterns like "/bin/sh -c ..." or "sh -lc ...". *)
val is_shell_script : string -> bool

(** Strip shell wrapper and extract inner command.
    For [/bin/sh -c 'command'], returns ['command'].
    Returns original string if not a shell wrapper. *)
val unwrap_shell : string -> string
