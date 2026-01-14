(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type ansi_state = {fg : Sdl_colors.color; bg : Sdl_colors.color}

type segment = {text : string; fg : Sdl_colors.color; bg : Sdl_colors.color}

val parse_ansi_segments : default:ansi_state -> string -> segment list

val strip_ansi_to_text : default:ansi_state -> string -> string
