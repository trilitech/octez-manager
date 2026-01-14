(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type tab = private {id : string; label : string}

val tab : id:string -> label:string -> tab

type t = private {tabs : tab list; selected : int}

val make : tab list -> t

val id : tab -> string

val label : tab -> string

val current : t -> tab option

val move : t -> [`Left | `Right | `First | `Last] -> t

val select : t -> id:string -> t

val handle_event :
  ?bubble_unhandled:bool -> t -> key:string -> t * [`Handled | `Bubble]

val handle_key : t -> key:string -> t

val render : t -> focus:bool -> string

(** Usage:
    {[
      let tabs =
        Tabs_widget.(make [tab ~id:"one" ~label:"One"; tab ~id:"two" ~label:"Two"])
      in
      let tabs = Tabs_widget.handle_key tabs ~key:"Right" in
      Tabs_widget.render tabs ~focus:true
    ]}
    Keys: Left/Right/Home/End move selection; callers act on [current]. *)
