(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Simple card container with optional title/footer. Pure renderer. *)

type t = {
  title : string option;
  body : string;
  footer : string option;
  accent : int option;
}

val create :
  ?title:string -> ?footer:string -> ?accent:int -> body:string -> unit -> t

val with_body : t -> string -> t

(* Render a card constrained to [cols] columns. *)
val render : t -> cols:int -> string
