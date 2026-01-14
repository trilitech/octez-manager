(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type gradient_dir = Up | Right | DownRight

type t = {
  (* Brand-specific names removed; use generic aliases below. *)
  (* New, generic semantic aliases (non-breaking additions). *)
  fg_primary : string -> string;
  fg_secondary : string -> string;
  fg_muted : string -> string;
  bg_primary : string -> string;
  fg_stealth : string -> string;
  bg_stealth : string -> string;
  fg_slate : string -> string;
  bg_slate : string -> string;
  fg_steel : string -> string;
  bg_steel : string -> string;
  fg_white : string -> string;
  bg_white : string -> string;
  purple_gradient : string -> string;
  purple_gradient_at :
    gradient_dir -> total_visible:int -> start_pos:int -> string -> string;
  purple_gradient_line : gradient_dir -> string -> string;
  fg_success : string -> string;
  fg_error : string -> string;
  selection_bg : string -> string;
  selection_fg : string -> string;
  fixed_region_bg : string -> string;
  header_bg : string -> string;
}

val set : t -> unit

val get : unit -> t option

val require : unit -> t
