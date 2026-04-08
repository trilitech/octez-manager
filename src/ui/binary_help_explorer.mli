(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Modal explorer for octez binary --help options.

    Parses --help output and displays options in a selectable grid,
    allowing users to toggle and set values for extra CLI arguments. *)

open Octez_manager_lib

(** A selectable row in the options grid, holding one CLI option with its
    current value and selection state. *)
type row = {
  opt : Help_parser.option_entry;
  mutable value : string option;
  mutable selected : bool;
}

(** {2 Modal Openers} *)

(** Open the help explorer modal for [octez-node run] flags.
    Parses the binary's --help output and filters out options already
    managed by the install form (e.g. [--data-dir], [--rpc-addr]).
    @param app_bin_dir directory containing the octez-node binary
    @param initial_args previously selected extra arguments to pre-populate
    @param on_apply callback invoked with the list of selected flag tokens *)
val open_node_run_help :
  app_bin_dir:string ->
  initial_args:string ->
  on_apply:(string list -> unit) ->
  unit

(** Open the help explorer modal for [octez-baker run] flags.
    @param app_bin_dir directory containing the octez-baker binary
    @param mode [`Local] for local-node mode, [`Remote] for remote-node mode
    @param initial_args previously selected extra arguments to pre-populate
    @param on_apply callback invoked with the list of selected flag tokens *)
val open_baker_run_help :
  app_bin_dir:string ->
  mode:[`Local | `Remote] ->
  initial_args:string ->
  on_apply:(string list -> unit) ->
  unit

(** Open the help explorer modal for [octez-accuser run] flags.
    @param app_bin_dir directory containing the octez-accuser binary
    @param initial_args previously selected extra arguments to pre-populate
    @param on_apply callback invoked with the list of selected flag tokens *)
val open_accuser_run_help :
  app_bin_dir:string ->
  initial_args:string ->
  on_apply:(string list -> unit) ->
  unit

(** Open the help explorer modal for [octez-dal-node run] flags.
    @param app_bin_dir directory containing the octez-dal-node binary
    @param initial_args previously selected extra arguments to pre-populate
    @param on_apply callback invoked with the list of selected flag tokens *)
val open_dal_run_help :
  app_bin_dir:string ->
  initial_args:string ->
  on_apply:(string list -> unit) ->
  unit

(** Open the help explorer modal for [octez-index run] flags.
    @param app_bin_dir directory containing the octez-index binary
    @param initial_args previously selected extra arguments to pre-populate
    @param on_apply callback invoked with the list of selected flag tokens *)
val open_index_run_help :
  app_bin_dir:string ->
  initial_args:string ->
  on_apply:(string list -> unit) ->
  unit

(** {2 Utility Functions} *)

(** Render an option value for display: [None] becomes [""], [Some v] becomes [v]. *)
val render_value : string option -> string

(** Truncate a string to [max_len] characters, appending an ellipsis if needed. *)
val truncate : max_len:int -> string -> string

(** Return the display label for an option entry (comma-separated flag names). *)
val option_label : Help_parser.option_entry -> string

(** Build a list of CLI tokens from all selected rows.
    Each selected flag produces its name, plus its value if set. *)
val format_tokens : row list -> string list

(** Check whether a flag [name] matches an [excluded] prefix string.
    Returns [true] if [name] equals or starts with the prefix. *)
val name_matches_excluded : string -> string -> bool

(** Check whether an option entry should be excluded based on a list
    of excluded flag name prefixes. *)
val is_excluded_option :
  Help_parser.option_entry -> excluded:string list -> bool

(** Flag prefixes excluded from the node help explorer
    (flags already managed by the install form). *)
val excluded_node_options : string list

(** Flag prefixes excluded from the baker help explorer. *)
val excluded_baker_options : string list

(** Flag prefixes excluded from the accuser help explorer. *)
val excluded_accuser_options : string list

(** Flag prefixes excluded from the DAL node help explorer. *)
val excluded_dal_options : string list

(** Flag prefixes excluded from the index help explorer. *)
val excluded_index_options : string list

(** Check whether an option entry matches a specific flag string. *)
val option_matches_flag : Help_parser.option_entry -> string -> bool

(** Initialize rows from a list of option entries, pre-selecting those
    that appear in the [initial_args] string. *)
val init_rows_from_args : Help_parser.option_entry list -> string -> row list

(** Generate short and long markdown help-hint text for a row.
    Returns [(short_hint, long_hint)] where either may be [None]. *)
val option_hint_markdown : row -> string option * string option

(** {2 Testing Interface} *)

module For_tests : sig
  (** Parse [octez-node --help] output into option entries. *)
  val parse_help : string -> Help_parser.option_entry list

  (** Parse [octez-baker --help] output into option entries. *)
  val parse_baker_help : string -> Help_parser.option_entry list

  (** Convert an {!Help_parser.arg_kind} to its string representation. *)
  val arg_kind_to_string : Help_parser.arg_kind -> string

  (** Parse an initial-args string into [(flag, value option)] pairs. *)
  val parse_initial_args : string -> (string * string option) list

  (** @see {!truncate} *)
  val truncate : max_len:int -> string -> string

  (** Wrap text to a given [width], returning a list of wrapped lines. *)
  val wrap_text : width:int -> string -> string list

  (** @see {!option_label} *)
  val option_label : Help_parser.option_entry -> string

  (** @see {!render_value} *)
  val render_value : string option -> string

  (** @see {!format_tokens} *)
  val format_tokens : row list -> string list

  (** @see {!name_matches_excluded} *)
  val name_matches_excluded : string -> string -> bool

  (** @see {!is_excluded_option} *)
  val is_excluded_option :
    Help_parser.option_entry -> excluded:string list -> bool

  (** @see {!excluded_node_options} *)
  val excluded_node_options : string list

  (** @see {!excluded_baker_options} *)
  val excluded_baker_options : string list

  (** Create a row from an option entry with default (unselected, no value) state. *)
  val make_row : Help_parser.option_entry -> row

  (** Create a pre-selected row from an option entry with the given value. *)
  val make_row_selected : Help_parser.option_entry -> string option -> row
end
