(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type crumb = private {
  id : string;
  label : string;
  on_enter : (unit -> unit) option;
}

type t = private {crumbs : crumb list; selected : int}

val crumb :
  id:string -> label:string -> ?on_enter:(unit -> unit) -> unit -> crumb

val make : crumb list -> t

val current : t -> crumb option

val id : crumb -> string

val label : crumb -> string

val move : t -> [`Left | `Right | `First | `Last] -> t

val select : t -> id:string -> t

val handle_event :
  ?bubble_unhandled:bool -> t -> key:string -> t * [`Handled | `Bubble]

val handle_key : t -> key:string -> t * [`Handled | `Ignored]

val render : t -> focus:bool -> string

(** Usage:
    {[
      let crumbs =
        Breadcrumbs_widget.
          [
            crumb ~id:"root" ~label:"Root" ();
            crumb ~id:"child" ~label:"Child" ();
          ]
        |> Breadcrumbs_widget.make
      in
      let crumbs, _ = Breadcrumbs_widget.handle_key crumbs ~key:"Right" in
      Breadcrumbs_widget.render crumbs ~focus:true
    ]}
    Keys: Left/Right/Home/End move selection; Enter triggers [on_enter] if set. *)
