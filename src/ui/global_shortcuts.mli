(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Global keyboard shortcuts that work across all pages.

    This module centralizes the handling of application-wide keyboard shortcuts
    like '?' for Help, 'm' for Menu, etc.

    The key handling priority is:
    1. Modal/Dialog (if active) - handles ALL keys
    2. Global shortcuts (this module) - cross-page navigation
    3. Page-specific keys - per-page actions
*)

(** Keys reserved for global shortcuts. Pages should not use these for
    page-specific actions. *)
val reserved_keys : string list

(** Check if a key is reserved for global use.
    Example: [is_reserved "?"] returns [true] *)
val is_reserved : string -> bool

(** Outcome of global key handling *)
type outcome =
  | Handled  (** Key was a global shortcut and was handled *)
  | NotGlobal  (** Key is not a global shortcut, page should handle it *)

(** Handle global shortcuts.

    Returns [Handled] if the key triggered a global action (like opening Settings),
    or [NotGlobal] if the page should handle the key itself.

    Reserved global shortcuts:
    - ['?'] - Show Help modal
    - ['m'] - Show Menu modal

    Example usage in a page:
    {[
      let handle_key state key ~size =
        if Miaou.Core.Modal_manager.has_active () then
          (* Modal is open, it handles the key *)
          Miaou.Core.Modal_manager.handle_key key;
          state
        else
          (* No modal, try global shortcuts first *)
          match Global_shortcuts.handle key with
          | Global_shortcuts.Handled -> state
          | Global_shortcuts.NotGlobal ->
              (* Handle page-specific keys *)
              match key with
              | "c" -> create_instance state
              | "Tab" -> toggle_view state
              | "f" -> cycle_filter state
              | _ -> state
    ]}
*)
val handle : string -> outcome

(** Emit a warning if a page tries to use a reserved key.
    Useful during development to catch conflicts.

    Example:
    {[
      Global_shortcuts.warn_if_reserved "?" "quick_search"
      (* WARNING: Key '?' is reserved for global shortcuts but is being used for 'quick_search' *)
    ]}
*)
val warn_if_reserved : string -> string -> unit
