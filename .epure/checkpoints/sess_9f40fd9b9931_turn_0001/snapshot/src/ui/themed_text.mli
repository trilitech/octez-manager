(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Themed text formatting helpers.

    Use these instead of raw [Printf.sprintf] in UI code to ensure all text
    is properly themed and visible in both light and dark modes.

    {2 Basic usage}

    {[
      open Themed_text

      (* Instead of: *)
      Printf.sprintf "Name: %s" name

      (* Use: *)
      text "Name: %s" name

      (* For styled text: *)
      primary "Title: %s" title
      muted "(%d items)" count
      error "Failed: %s" msg
    ]}

    {2 Combining styled fragments}

    {[
      concat [
        text "Status: ";
        success "running";
        muted " (pid %d)" pid
      ]
    ]} *)

(** {1 Themed formatters} *)

(** Format with themed text color (default foreground). *)
val text : ('a, unit, string) format -> 'a

(** Format with themed muted color (dimmed/secondary). *)
val muted : ('a, unit, string) format -> 'a

(** Format with themed primary color (titles, important labels). *)
val primary : ('a, unit, string) format -> 'a

(** Format with themed secondary color (subtitles). *)
val secondary : ('a, unit, string) format -> 'a

(** Format with themed emphasis (bold). *)
val emphasis : ('a, unit, string) format -> 'a

(** Format with themed success color (green). *)
val success : ('a, unit, string) format -> 'a

(** Format with themed error color (red). *)
val error : ('a, unit, string) format -> 'a

(** Format with themed warning color (orange/yellow). *)
val warning : ('a, unit, string) format -> 'a

(** Format with themed info color (cyan/blue). *)
val info : ('a, unit, string) format -> 'a

(** Format with themed accent color. *)
val accent : ('a, unit, string) format -> 'a

(** {1 Combining fragments} *)

(** Concatenate themed text fragments. *)
val concat : string list -> string

(** Join themed text fragments with a separator. *)
val join : string -> string list -> string

(** Create a line from multiple styled parts. *)
val line : string array -> string
