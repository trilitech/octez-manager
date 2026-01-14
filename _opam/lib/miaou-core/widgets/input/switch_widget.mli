(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type t

val create : ?label:string -> ?on:bool -> ?disabled:bool -> unit -> t

val open_centered : ?label:string -> ?on:bool -> ?disabled:bool -> unit -> t

val render : t -> focus:bool -> string

(** Space/Enter toggles the switch. *)
val handle_key : t -> key:string -> t

val is_on : t -> bool

val set_on : t -> bool -> t

val is_cancelled : t -> bool

val reset_cancelled : t -> t

(** Usage:
    {[
      let s = create ~label:"Auto-update" () in
      let s = handle_key s ~key:"Enter" in
      render s ~focus:true
    ]}
    Keys: Enter/Space toggles; Esc sets [cancelled]. *)
