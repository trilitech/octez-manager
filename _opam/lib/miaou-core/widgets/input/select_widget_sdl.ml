(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* SDL-specific render entrypoint in its own module to keep backend-specific
   rendering split from the terminal implementation. *)

let render (w : 'a Select_widget.t) ~focus =
  Select_widget.render_for_backend `Sdl w ~focus
