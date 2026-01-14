(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Ready-to-use modal wrapper around {!Miaou_widgets_layout.File_browser_widget}.

    The modal integrates with {!Modal_manager} and reuses the widget's key hints
    for footer/help display. Key handling is delegated to the widget; selection
    or cancellation triggers the provided callbacks and closes the modal. *)

(** Push the file browser modal.

          @param title Modal title (default: "Select path")
          @param path Initial path (default: "/")
          @param dirs_only Show only directories (default: true)
          @param require_writable Filter to writable entries (default: true)
          @param select_dirs Allow selecting directories (default: true)
          @param show_hidden Show dotfiles (default: false)
          @param on_cancel Called when user cancels (Esc/back)
          @param on_select Called with the selected path on commit *)
val open_modal :
  ?title:string ->
  ?path:string ->
  ?dirs_only:bool ->
  ?require_writable:bool ->
  ?select_dirs:bool ->
  ?show_hidden:bool ->
  ?on_cancel:(unit -> unit) ->
  on_select:(string -> unit) ->
  unit ->
  unit
