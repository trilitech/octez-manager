(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Themed text formatting helpers.

    Use these instead of raw Printf.sprintf in UI code to ensure all text
    is properly themed and visible in both light and dark modes. *)

module Widgets = Miaou_widgets_display.Widgets

(** Format a string with themed text (default foreground color).
    Use this for normal body text. *)
let text fmt = Printf.ksprintf Widgets.themed_text fmt

(** Format a string with themed muted text (dimmed/secondary).
    Use this for hints, secondary info, timestamps, etc. *)
let muted fmt = Printf.ksprintf Widgets.themed_muted fmt

(** Format a string with themed primary color (usually blue/accent).
    Use this for titles, important labels. *)
let primary fmt = Printf.ksprintf Widgets.themed_primary fmt

(** Format a string with themed secondary color.
    Use this for subtitles, less important labels. *)
let secondary fmt = Printf.ksprintf Widgets.themed_secondary fmt

(** Format a string with themed emphasis (bold text).
    Use this for emphasized content, selected items. *)
let emphasis fmt = Printf.ksprintf Widgets.themed_emphasis fmt

(** Format a string with themed success color (green).
    Use this for success messages, running status. *)
let success fmt = Printf.ksprintf Widgets.themed_success fmt

(** Format a string with themed error color (red).
    Use this for error messages, failed status. *)
let error fmt = Printf.ksprintf Widgets.themed_error fmt

(** Format a string with themed warning color (orange/yellow).
    Use this for warnings, caution messages. *)
let warning fmt = Printf.ksprintf Widgets.themed_warning fmt

(** Format a string with themed info color (cyan/blue).
    Use this for informational messages. *)
let info fmt = Printf.ksprintf Widgets.themed_info fmt

(** Format a string with themed accent color.
    Use this for highlighted items, links. *)
let accent fmt = Printf.ksprintf Widgets.themed_accent fmt

(** Concatenate themed text fragments.
    Example: [concat [text "Name: "; emphasis "%s" name; muted " (id: %d)" id]] *)
let concat = String.concat ""

(** Join themed text fragments with a separator.
    Example: [join " · " [text "a"; text "b"; text "c"]] *)
let join sep = String.concat sep

(** Create a line with multiple styled parts.
    Example: [line [|text "Label"; muted "value"|]] *)
let line parts = String.concat "" (Array.to_list parts)
