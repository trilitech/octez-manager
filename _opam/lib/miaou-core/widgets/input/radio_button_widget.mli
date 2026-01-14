(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type t

val create : ?label:string -> ?selected:bool -> ?disabled:bool -> unit -> t

val open_centered :
  ?label:string -> ?selected:bool -> ?disabled:bool -> unit -> t

val render : t -> focus:bool -> string

(** Space/Enter selects this radio button. *)
val handle_key : t -> key:string -> t

val is_selected : t -> bool

val set_selected : t -> bool -> t

val is_cancelled : t -> bool

val reset_cancelled : t -> t

(** Usage:
    {[
      let r = create ~label:"Mainnet" () in
      let r = handle_key r ~key:"Enter" in
      render r ~focus:true
    ]}
    Keys: Enter/Space selects; Esc sets [cancelled]. Callers enforce exclusivity across a group. *)
