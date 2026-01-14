(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
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

  (** Wrapped state with navigation support.
      Pages use [Navigation.goto], [Navigation.back], [Navigation.quit]
      to navigate, and [Navigation.update] to modify inner state. *)
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

  (* Unified keymap: pure description for stack-based dispatcher and footer/help
     display. Footer hints are auto-generated from this keymap; pages should not
     render their own keymap footer. Reserved keys like "?" are intercepted by
     the driver but can still appear here with [display_only = true] so they show
     up in the footer/help overlay without being dispatched. *)
  val keymap : pstate -> key_binding list

  (* Declare which keys this page handles (for conflict detection).
     Pages should list all keys they handle, using Keys.t variants.
     This enables compile-time checking; it is not used for footer rendering. *)
  val handled_keys : unit -> Keys.t list

  (* When a modal is active, pages can handle raw key strings here (e.g., "a", "Backspace", "Left"). *)
  val handle_modal_key : pstate -> string -> size:LTerm_geom.size -> pstate

  (* Generic key handler - includes Enter, Esc, and all other keys.
     Use Navigation.goto/back/quit for navigation. *)
  val handle_key : pstate -> string -> size:LTerm_geom.size -> pstate

  (* Return true if the page currently has an active modal overlay that
     should consume most input. When true, the driver should avoid routing
     navigation keys (Up/Down/Left/Right/NextPage) to the background page. *)
  val has_modal : pstate -> bool
end
