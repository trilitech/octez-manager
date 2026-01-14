(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(** Pager widget wired to a file source with Eio fibers.

    This module tails a file using inotify (when available) and falls back to
    polling. All background work runs on the shared {!Miaou_helpers.Fiber_runtime}
    initialized from [Eio_main.run].

    When used from drivers that wrap each page in
    {!Miaou_helpers.Fiber_runtime.with_page_switch}, tail fibers are scoped to
    the page lifecycle and are cancelled automatically on navigation. It is
    still recommended to call {!close} to stop file polling as soon as the page
    no longer needs it. *)

type t

(** [open_file path] loads [path] into a pager and, when [follow] is true,
    starts a background fiber that appends new lines on change.

    @param poll_interval seconds between polls when inotify is unavailable
    @param title custom title to display instead of the file path
    @return [Error msg] if the runtime is not initialized or the file cannot
    be read. *)
val open_file :
  ?follow:bool ->
  ?notify_render:(unit -> unit) ->
  ?poll_interval:float ->
  ?title:string ->
  string ->
  (t, string) result

(** Access the underlying pager. *)
val pager : t -> Pager_widget.t

(** Stop watching and close any external resources. *)
val close : t -> unit
(** [close t] stops tailing and releases any resources. Tail fibers started
    within a page switch are also cancelled automatically when that switch
    closes, but calling [close] ensures prompt shutdown. *)
