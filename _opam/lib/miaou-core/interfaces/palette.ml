(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type gradient_dir = Up | Right | DownRight

type t = {
  (* Brand-specific names removed; use generic aliases below. *)
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

let id s = s

let default : t =
  {
    fg_primary = id;
    fg_secondary = id;
    fg_muted = id;
    bg_primary = id;
    fg_stealth = id;
    bg_stealth = id;
    fg_slate = id;
    bg_slate = id;
    fg_steel = id;
    bg_steel = id;
    fg_white = id;
    bg_white = id;
    purple_gradient = id;
    purple_gradient_at = (fun _dir ~total_visible:_ ~start_pos:_ s -> s);
    purple_gradient_line = (fun _dir s -> s);
    fg_success = id;
    fg_error = id;
    selection_bg = id;
    selection_fg = id;
    fixed_region_bg = id;
    header_bg = id;
  }

let current : t option ref = ref (Some default)

let set t = current := Some t

let get () = !current

let require () =
  match !current with
  | Some t -> t
  | None -> failwith "Miaou Palette not registered"
