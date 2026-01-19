(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Parsing of shell environment files.

    This module reads and parses environment files (key=value format)
    used by systemd EnvironmentFile directive. *)

(** {1 Parsing} *)

(** Parse an environment file from disk.
    
    Handles:
    - KEY=value format
    - Comments (#)
    - Empty lines
    - Quoted values ("value" or 'value')
    - Multi-line values (basic support)
    
    @return Association list of key-value pairs, or error *)
val parse_file : string -> ((string * string) list, string) result

(** Parse environment content from a string.
    Same format as parse_file but operates on string content. *)
val parse_string : string -> (string * string) list

(** {1 Variable Expansion} *)

(** Expand variables in a string using environment.
    Supports ${VAR} and $VAR syntax.
    
    @param env Association list of variable name to value
    @param str String potentially containing variables
    @return String with variables expanded, or original if var not found *)
val expand_vars : env:(string * string) list -> string -> string
