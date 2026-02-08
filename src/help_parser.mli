(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shared helper for parsing CLI --help output.

    Parses the output of octez-node, octez-baker, and cmdliner-based
    --help text into structured option and command entries. *)

(** Classification of the expected value for a CLI option argument. *)
type value_kind =
  | Addr_port  (** Address:port pair (e.g. [127.0.0.1:8732]). *)
  | Port  (** Port number. *)
  | Addr  (** IP address or hostname. *)
  | File  (** File path. *)
  | Dir  (** Directory path. *)
  | Path  (** Generic filesystem path. *)
  | Number  (** Integer. *)
  | Float  (** Floating-point number. *)
  | Text  (** Free-form text. *)

(** Whether a CLI option is a boolean toggle or takes a typed value. *)
type arg_kind = Toggle | Value of value_kind

(** A parsed CLI option with flag names, optional placeholder, doc string,
    and classified argument kind. *)
type option_entry = {
  names : string list;
  arg : string option;
  doc : string;
  kind : arg_kind;
}

(** A parsed CLI subcommand with its name and documentation. *)
type command_entry = {name : string; doc : string}

(** {2 String Helpers} *)

(** [contains ~needle haystack] returns [true] if [needle] occurs in
    [haystack]. *)
val contains : needle:string -> string -> bool

(** Trim whitespace and return [Some s] if non-empty, [None] otherwise. *)
val trim_nonempty : string -> string option

(** Return the first long-form ([--]-prefixed) name, or the first name. *)
val primary_name : string list -> string

(** Filter to long-form names only; return all if none are long-form. *)
val display_names : string list -> string list

(** {2 Argument Classification} *)

(** Heuristically classify an option's argument kind by scanning the
    placeholder, doc string, and flag names for keywords. *)
val classify_arg_kind :
  names:string list -> arg:string option -> doc:string -> arg_kind

(** {2 Spec/Doc Splitting} *)

(** Split a help line into (spec, doc) using tab or double-space gap. *)
val split_spec_doc_default : string -> string * string

(** Like {!split_spec_doc_default} but falls back to [: ] for baker output. *)
val split_spec_doc_baker : string -> string * string

(** Strip trailing punctuation from a placeholder string. *)
val clean_placeholder : string -> string

(** Strip trailing punctuation from an option name string. *)
val clean_name : string -> string

(** {2 Spec Parsing} *)

(** Parse an option specifier into flag names and optional placeholder. *)
val parse_spec : string -> string list * string option

(** Split bracket syntax like [--flag\[=VAL\]] into [Some ("--flag", "=VAL")]. *)
val split_bracket_arg : string -> (string * string) option

(** Parse a cmdliner-style option specifier (handles bracket and [=] syntax). *)
val parse_spec_cmdliner : string -> string list * string option

(** {2 Line Detection} *)

(** [true] if the line looks like a node help option definition. *)
val is_option_line_node : string -> bool

(** [true] if the line looks like a baker help option definition. *)
val is_option_line_baker : string -> bool

(** [true] if the line looks like a cmdliner option definition. *)
val is_option_line_cmdliner : string -> bool

(** {2 Parsing Pipelines} *)

(** Remove ANSI escape sequences from a string. *)
val strip_ansi : string -> string

(** Generic help parser parameterized by line detection, splitting,
    and spec parsing strategies. *)
val parse_help_with :
  is_option_line:(string -> bool) ->
  split_spec_doc:(string -> string * string) ->
  parse_spec:(string -> string list * string option) ->
  string ->
  option_entry list

(** Parse [octez-node --help] output into option entries. *)
val parse_help_node : string -> option_entry list

(** Parse [octez-baker --help] output into option entries. *)
val parse_help_baker : string -> option_entry list

(** {2 Section Extraction} *)

(** [true] if the line is an all-caps section header (e.g. ["OPTIONS"]). *)
val is_section_header : string -> bool

(** Extract lines belonging to a named section from help output lines. *)
val extract_section_lines : header:string -> string list -> string list

(** Parse the OPTIONS and COMMON OPTIONS sections of cmdliner help output. *)
val parse_cmdliner_options : string -> option_entry list

(** [true] if the line looks like a subcommand usage line. *)
val looks_like_command_line : string -> bool

(** Parse the COMMANDS section of cmdliner help output. *)
val parse_cmdliner_commands : string -> command_entry list

(** {2 Baker Global Options} *)

(** [true] if the line starts with ["Global options"]. *)
val is_baker_global_section_header : string -> bool

(** Extract lines belonging to the baker global options section. *)
val extract_baker_global_section : string list -> string list

(** Parse the baker global options section into option entries. *)
val parse_baker_global_options : string -> option_entry list

(** Parse baker global options and return a flat list of flag names. *)
val extract_baker_global_option_names : string -> string list

(** Classify an argument as [`Global] or [`Command] based on known global
    option names. *)
val classify_arg : global_options:string list -> string -> [> `Global | `Command]

(** Partition extra CLI arguments into [(global_args, command_args)]. *)
val split_extra_args :
  global_options:string list -> string list -> string list * string list
