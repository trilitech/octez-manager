(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure helper functions for shell completion script generation. *)

(** Escape a string for use inside a zsh single-quoted context. *)
val escape_zsh_single : string -> string

(** Escape a string for use inside a zsh [[DESCRIPTION]] bracket.
    Escapes ['\''], [':'], ['\['], and ['\]']. *)
val escape_zsh_description : string -> string

(** [true] if [name] is a valid CLI command or subcommand name
    (alphanumeric, [-], or [_] only). Rejects wrapped-line continuation
    tokens like [--flag=VALUE] that appear in cmdliner help output. *)
val is_valid_cmd_name : string -> bool

(** Shell-quote a string for safe interpolation into a [sh -c] command.
    Wraps in single quotes and escapes embedded single quotes as ['\\'']. *)
val quote_shell : string -> string
