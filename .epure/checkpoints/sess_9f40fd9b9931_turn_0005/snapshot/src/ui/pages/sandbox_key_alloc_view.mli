(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the sandbox key allocation page.
    No Eio calls. All inputs are pre-computed by the page module. *)

type edit_field = EKeys | EPct

type baker_row = {
  instance : string;
  keys : int;
  is_new : bool;
  to_delete : bool;
}

type state = {
  group_name : string;
  rows : baker_row list;
  total_delegates : int;
  delegate_balances : (float array * float) option;
  cursor : int;
  editing : (int * edit_field * string) option;
  user_modified : bool;
}

val key_hint_pairs : (string * string) list

(** Render the full key allocation page view.

    [toast] is a pre-rendered toast string (may be empty). *)
val view : state -> toast:string -> focus:bool -> size:LTerm_geom.size -> string
