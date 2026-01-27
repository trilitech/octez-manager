(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Global keyboard shortcuts that work across all pages *)

(** List of keys reserved for global shortcuts. Pages should avoid using these
    for page-specific actions. *)
let reserved_keys = ["s"; "?"; "m"; "Esc"; "q"]

(** Check if a key is reserved for global use *)
let is_reserved key = List.mem key reserved_keys

(** Outcome of global key handling *)
type outcome =
  | Handled  (** Key was a global shortcut and was handled *)
  | NotGlobal  (** Key is not a global shortcut, page should handle it *)

(** Handle global shortcuts. Returns Handled if the key was a global shortcut,
    NotGlobal if the page should handle it instead.

    Usage:
    {[
      let handle_key state key ~size =
        if Modal_manager.has_active () then
          (* Modal handles all keys *)
          Modal_manager.handle_key key;
          state
        else
          (* Try global shortcuts first *)
          match Global_shortcuts.handle key with
          | Handled -> state
          | NotGlobal ->
              (* Handle page-specific keys *)
              match key with
              | "c" -> create_instance state
              | ...
    ]}
*)
let handle key =
  match key with
  | "s" ->
      Context.navigate "settings" ;
      Handled
  | "?" ->
      Modal_helpers.show_help_modal () ;
      Handled
  | "m" ->
      Modal_helpers.show_menu_modal () ;
      Handled
  | _ -> NotGlobal

(** Convenience function: warn if a page is trying to use a reserved key *)
let warn_if_reserved key action_name =
  if is_reserved key then
    prerr_endline
      (Printf.sprintf
         "WARNING: Key '%s' is reserved for global shortcuts but is being used \
          for '%s'"
         key
         action_name)
