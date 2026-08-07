(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Themed formatting for UI code.

    All text displayed in the UI should use these formatters to ensure
    visibility in both light and dark modes.

    Usage:
    {[
      open Ui_fmt

      (* Basic themed text - most common *)
      text "Hello %s" name
      text "Count: %d" n

      (* Styled variants *)
      muted "(%d items)" count
      bold "Selected: %s" item
      error "Failed: %s" msg
      success "Done!"
    ]}
*)

(** {1 Themed formatters} *)

val text : ('a, Format.formatter, unit, string) format4 -> 'a

val muted : ('a, Format.formatter, unit, string) format4 -> 'a

val primary : ('a, Format.formatter, unit, string) format4 -> 'a

val secondary : ('a, Format.formatter, unit, string) format4 -> 'a

val bold : ('a, Format.formatter, unit, string) format4 -> 'a

val success : ('a, Format.formatter, unit, string) format4 -> 'a

val error : ('a, Format.formatter, unit, string) format4 -> 'a

val warning : ('a, Format.formatter, unit, string) format4 -> 'a

val info : ('a, Format.formatter, unit, string) format4 -> 'a

val accent : ('a, Format.formatter, unit, string) format4 -> 'a

(** {1 Raw formatting} *)

val raw : ('a, Format.formatter, unit, string) format4 -> 'a

(** {1 Style printers for %a} *)

val pp_text : Format.formatter -> string -> unit

val pp_muted : Format.formatter -> string -> unit

val pp_primary : Format.formatter -> string -> unit

val pp_secondary : Format.formatter -> string -> unit

val pp_bold : Format.formatter -> string -> unit

val pp_success : Format.formatter -> string -> unit

val pp_error : Format.formatter -> string -> unit

val pp_warning : Format.formatter -> string -> unit

val pp_info : Format.formatter -> string -> unit

val pp_accent : Format.formatter -> string -> unit
