(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Accuser installation form using field bundles *)

(** Page identifier for registration *)
val name : string

(** The page module implementing accuser installation *)
val page : Miaou.Core.Registry.page

(** Register the accuser installation page in the Miaou registry *)
val register : unit -> unit

module For_tests : sig
  val initial_base_dir : string
end
