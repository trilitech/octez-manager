(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* This module file intentionally contains no definitions. The page signature
  is declared in the corresponding mli file `tui_page.mli`. Keeping this
  .ml file minimal prevents duplicate module-type definitions and helps the
  build stay consistent. *)

[@@@warning "-32-34-37-69"]

type 'state key_binding_desc = {
  key : string;
  action : 'state Navigation.t -> 'state Navigation.t;
  help : string;
  display_only : bool;
}

type 'state key_binding = 'state key_binding_desc

module type PAGE_SIG = sig
  (** The page's own state type (no next_page field needed). *)
  type state

  type msg

  (** Key binding description.
      [display_only] lets you show reserved keys (e.g., "?") in the footer without
      expecting them to be dispatched to [action]. *)
  type key_binding = state key_binding_desc

  (** Wrapped state with navigation support. *)
  type pstate = state Navigation.t

  val init : unit -> pstate

  val update : pstate -> msg -> pstate

  val view : pstate -> focus:bool -> size:LTerm_geom.size -> string

  (* Driver-callable helpers, keeping msg abstract *)
  val move : pstate -> int -> pstate

  val refresh : pstate -> pstate

  val service_select : pstate -> int -> pstate

  val service_cycle : pstate -> int -> pstate

  val back : pstate -> pstate

  val keymap : pstate -> key_binding list

  val handled_keys : unit -> Keys.t list

  (* When a modal is active, pages can handle raw key strings here *)
  val handle_modal_key : pstate -> string -> size:LTerm_geom.size -> pstate

  (* Generic key handler - includes Enter, Esc, and all other keys *)
  val handle_key : pstate -> string -> size:LTerm_geom.size -> pstate

  (* Return true if the page currently has an active modal overlay that
     should consume most input. When true, the driver will avoid routing
     navigation keys (Up/Down/Left/Right/NextPage) to the background page. *)
  val has_modal : pstate -> bool
end
