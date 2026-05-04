(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Display utilities for rewards page tables.

    Provides display-width-aware padding that correctly handles:
    - Multi-byte UTF-8 characters (ꜩ, —, ▸)
    - ANSI escape codes from themed_* functions (zero display width)
*)

(** Strip ANSI escape sequences from a string. *)
val strip_ansi : string -> string

(** Count display width of a string (Unicode scalar values, not bytes).

    This approximates display width by counting Unicode code points.
    Most characters (including ꜩ, —, ▸) are 1 display column. *)
val display_width : string -> int

(** Pad string to target display width (right-aligned).

    Accounts for ANSI codes and multi-byte UTF-8 characters.
    @param width Target display width in columns
    @param s String to pad (may contain ANSI codes) *)
val pad_right : int -> string -> string

(** Pad string to target display width (left-aligned).

    Accounts for ANSI codes and multi-byte UTF-8 characters.
    @param width Target display width in columns
    @param s String to pad (may contain ANSI codes) *)
val pad_left : int -> string -> string
