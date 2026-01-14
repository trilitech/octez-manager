(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(** A small two-pane layout container.

    The Pane_layout module provides a simple container that arranges two
    string-rendered child widgets side-by-side. It exposes an API to create
    a pane with optional width ratio and to render it into a single string
    representing the combined layout. *)

type t

val create : ?left_ratio:float -> left:string -> right:string -> unit -> t

val set_left : t -> string -> t

val set_right : t -> string -> t

val render : t -> int -> string

val split_lines : string -> string list
