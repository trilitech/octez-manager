(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Main application shell with 7-tab navigation.

    Manages tab state for Instances, Wallets, Binaries, RPCs, Sandboxes,
    Diagnostics, and Topology. Each tab preserves its inner state across tab
    switches within a single session. Sub-page navigations (forms, detail pages)
    propagate to the runner as [SwitchTo] events.

    Tab switching:
    - Number keys [1]-[7] switch directly to a named tab
    - Pressing the current tab's number triggers the tab's primary action
      (e.g., pressing [1] on the Instances tab opens the create dropdown)
    - [Context.set_pending_tab] triggers a switch on the next [refresh] cycle
    - Navigation to a registered tab page name is intercepted and converted
      to a tab switch rather than a page push *)

(** Page name for the global page registry. *)
val name : string

(** Register the shell page in the global registry. *)
val register : unit -> unit

(**/**)

(** PAGE_SIG module for use with the headless driver in tests. *)
module Page : Miaou.Core.Tui_page.PAGE_SIG

(** Internal functions exposed for testing. *)
module Internal_for_tests : sig
  type state

  type pstate = state Miaou.Core.Navigation.t

  val apply_sub_nav :
    shell_ps:pstate ->
    shell_s:state ->
    Miaou.Core.Navigation.nav option ->
    pstate

  val make_state : ?on_hidden_page:string option -> unit -> state

  val get_on_hidden_page : state -> string option

  val get_state : pstate -> state
end

(**/**)
