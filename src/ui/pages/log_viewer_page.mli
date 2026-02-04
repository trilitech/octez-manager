(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

val name : string

module Page : Miaou.Core.Tui_page.PAGE_SIG

val register : unit -> unit

(** Functions exposed for testing. *)
module For_tests : sig
  (** Convert log source to display label. *)
  val source_label : Octez_manager_lib.Log_viewer.log_source -> string
end
