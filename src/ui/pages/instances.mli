(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Page name for the page registry. *)
val name : string

(** Register this page with the global page registry. *)
val register : unit -> unit

module Page : Miaou.Core.Tui_page.PAGE_SIG

(** Functions exposed for testing. *)
module For_tests : sig
  (** Move selection up or down by [delta] steps.
      Handles menu items, separator skipping, single-column linear navigation,
      and multi-column column-constrained navigation. *)
  val move_selection : Instances_state.state -> int -> Instances_state.state
end
