(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type t

(** Create a checkbox widget state.
    @param label Optional label to render to the right of the box
    @param checked_ Initial boolean state (default false)
    @param disabled Ignore key handling when true (default false)
*)
val create : ?label:string -> ?checked_:bool -> ?disabled:bool -> unit -> t

(** Alias for create; kept for API symmetry with other widgets. *)
val open_centered :
  ?label:string -> ?checked_:bool -> ?disabled:bool -> unit -> t

(** Render the checkbox to a string suitable for terminal output. *)
val render : t -> focus:bool -> string

(** Handle a key press. Space/Enter toggles the checkbox. Esc marks it cancelled. *)
val handle_key : t -> key:string -> t

(** Current boolean value. *)
val is_checked : t -> bool

val set_checked : t -> bool -> t

val is_cancelled : t -> bool

val reset_cancelled : t -> t

(** Usage:
    {[
      let c = create ~label:"Enable" () in
      let c = handle_key c ~key:"Space" in
      render c ~focus:true
    ]}
    Keys: Enter/Space toggles; Esc sets [cancelled]. *)
