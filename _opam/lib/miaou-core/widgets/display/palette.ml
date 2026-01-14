(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* SPDX-License-Identifier: MIT *)

type gradient_dir = Miaou_interfaces.Palette.gradient_dir =
  | Up
  | Right
  | DownRight

let p () = Miaou_interfaces.Palette.require ()

let fg_primary s = (p ()).fg_primary s

let bg_primary s = (p ()).bg_primary s

let fg_stealth s = (p ()).fg_stealth s

let bg_stealth s = (p ()).bg_stealth s

let fg_slate s = (p ()).fg_slate s

let bg_slate s = (p ()).bg_slate s

let fg_steel s = (p ()).fg_steel s

let bg_steel s = (p ()).bg_steel s

let fg_white s = (p ()).fg_white s

let bg_white s = (p ()).bg_white s

let purple_gradient s = (p ()).purple_gradient s

let purple_gradient_at dir ~total_visible ~start_pos s =
  (p ()).purple_gradient_at dir ~total_visible ~start_pos s

let purple_gradient_line dir s = (p ()).purple_gradient_line dir s

let fg_success s = (p ()).fg_success s

let fg_error s = (p ()).fg_error s

(* Generic semantic aliases so the Palette adapter also exposes
	application-agnostic names. These mirror the helpers added to
	`Miaou_widgets_display.Widgets`. *)
let fg_secondary s = (p ()).fg_secondary s

let fg_muted s = (p ()).fg_muted s

let selection_bg s = (p ()).selection_bg s

let selection_fg s = (p ()).selection_fg s

let fixed_region_bg s = (p ()).fixed_region_bg s

let header_bg s = (p ()).header_bg s
