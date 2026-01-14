(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* SDL-specific palette helpers. For now we reuse the base rendering but
   keep this entrypoint separate to allow future richer SDL visuals. *)

let fg_primary = Palette.fg_primary

let fg_secondary = Palette.fg_secondary

let fg_muted = Palette.fg_muted

let fg_stealth = Palette.fg_stealth

let fg_slate = Palette.fg_slate

let fg_steel = Palette.fg_steel

let fg_success = Palette.fg_success

let fg_error = Palette.fg_error

let selection_bg = Palette.selection_bg

let selection_fg = Palette.selection_fg

let purple_gradient_line = Palette.purple_gradient_line
