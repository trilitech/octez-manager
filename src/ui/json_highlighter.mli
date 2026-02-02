(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** JSON syntax highlighting for TUI display.

    Pretty-prints JSON with color syntax highlighting using Miaou Widgets. *)

(** Highlighting options. *)
type options = {
  indent : int;  (** Spaces per indentation level (default: 2) *)
  max_depth : int;  (** Max nesting depth before ellipsis (default: 20) *)
  colors : bool;  (** Enable color highlighting (default: true) *)
}

(** Default options. *)
val default_options : options

(** Pretty-print JSON string with syntax highlighting.

    @param options Formatting options
    @param json_string Raw JSON string
    @return Formatted, colored output or error message *)
val highlight : ?options:options -> string -> (string, string) result

(** Strip ANSI color codes from highlighted output.
    Used when saving to file.

    @param colored_text Text with ANSI codes
    @return Plain text *)
val strip_colors : string -> string
