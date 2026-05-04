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

module W = Miaou_widgets_display.Widgets

(** {1 Themed formatters}

    Each function formats a string and applies the corresponding theme style. *)

(** Default themed text - use for normal content *)
let text fmt = Format.kasprintf W.themed_text fmt

(** Muted/dimmed text - use for secondary info, hints *)
let muted fmt = Format.kasprintf W.themed_muted fmt

(** Primary color - use for titles, important labels *)
let primary fmt = Format.kasprintf W.themed_primary fmt

(** Secondary color - use for subtitles *)
let secondary fmt = Format.kasprintf W.themed_secondary fmt

(** Bold/emphasized - use for selected items, emphasis *)
let bold fmt = Format.kasprintf W.themed_emphasis fmt

(** Success (green) - use for running, ok status *)
let success fmt = Format.kasprintf W.themed_success fmt

(** Error (red) - use for errors, stopped status *)
let error fmt = Format.kasprintf W.themed_error fmt

(** Warning (orange/yellow) - use for warnings *)
let warning fmt = Format.kasprintf W.themed_warning fmt

(** Info (cyan/blue) - use for informational messages *)
let info fmt = Format.kasprintf W.themed_info fmt

(** Accent color - use for links, actions *)
let accent fmt = Format.kasprintf W.themed_accent fmt

(** {1 Raw formatting}

    For building intermediate strings that will be styled later,
    or for combining multiple styled parts. *)

(** Format without styling - use when combining styled parts *)
let raw fmt = Format.asprintf fmt

(** {1 Style printers for %a}

    Use with [raw] when you need mixed styles in one string:
    {[
      raw "%a: %a" pp_primary "Key" pp_muted "value"
    ]} *)

let pp_text ppf s = Format.pp_print_string ppf (W.themed_text s)

let pp_muted ppf s = Format.pp_print_string ppf (W.themed_muted s)

let pp_primary ppf s = Format.pp_print_string ppf (W.themed_primary s)

let pp_secondary ppf s = Format.pp_print_string ppf (W.themed_secondary s)

let pp_bold ppf s = Format.pp_print_string ppf (W.themed_emphasis s)

let pp_success ppf s = Format.pp_print_string ppf (W.themed_success s)

let pp_error ppf s = Format.pp_print_string ppf (W.themed_error s)

let pp_warning ppf s = Format.pp_print_string ppf (W.themed_warning s)

let pp_info ppf s = Format.pp_print_string ppf (W.themed_info s)

let pp_accent ppf s = Format.pp_print_string ppf (W.themed_accent s)
