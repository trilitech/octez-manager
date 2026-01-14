(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** SDL color utilities and ANSI escape sequence parsing. *)

module Sdl = Tsdl.Sdl

type color = {r : int; g : int; b : int; a : int}

type ansi_state = {fg : color; bg : color}

(** Convert internal color representation to SDL color. *)
val color_to_sdl : color -> Sdl.color

(** Convert 256-color palette index to RGB color. *)
val color256 : int -> color

(** Parse ANSI escape sequences into styled text segments. *)
val parse_ansi_segments :
  default:ansi_state -> string -> (ansi_state * string) list

(** Remove ANSI escape sequences, returning plain text. *)
val strip_ansi_to_text : default:ansi_state -> string -> string
