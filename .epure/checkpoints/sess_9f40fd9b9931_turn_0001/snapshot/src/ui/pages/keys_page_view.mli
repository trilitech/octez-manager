(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the keys page. No Eio calls. *)

(** A group of keys from one base directory *)
type dir_group = {
  base_dir : string;
  keys : Octez_manager_lib.Keys_reader.key_info list;
  error : string option;
}

(** Page state *)
type state = {
  groups : dir_group list;
  selected : int;
  total_keys : int;
}

val view : state -> focus:bool -> size:LTerm_geom.size -> string
