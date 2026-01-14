(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type target =
  | Internal of string  (** page id or route name *)
  | External of string  (** URL *)

(** Abstract type for a link widget. *)
type t

val create : label:string -> target:target -> on_navigate:(target -> unit) -> t

val render : t -> focus:bool -> string

val handle_event :
  ?bubble_unhandled:bool -> t -> key:string -> t * [`Handled | `Bubble]

val handle_key : t -> key:string -> t * bool

(** Usage:
    {[
      let link =
        Link_widget.create ~label:"Home" ~target:(Internal "home")
          ~on_navigate:(fun _ -> ())
      in
      let link, fired = Link_widget.handle_key link ~key:"Enter" in
      ignore fired ;
      Link_widget.render link ~focus:true
    ]}
    Keys: Enter/Space triggers [on_navigate]. *)
