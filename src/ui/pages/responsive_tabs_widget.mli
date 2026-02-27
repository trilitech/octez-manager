(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** A tab bar widget that shortens labels from right to left as the terminal
    narrows.

    Each tab carries a list of label variants ordered longest to shortest.
    The widget picks the longest variant that allows the whole bar to fit
    within the available [cols].  Shortening is applied right-to-left so that
    the leftmost tabs retain their full labels as long as possible. *)

(** A tab entry with a list of label variants (longest first). *)
type tab = private {id : string; labels : string list}

(** [tab ~id ~labels] where [labels] must be non-empty, longest variant first.
    @raise Invalid_argument if [labels] is empty. *)
val tab : id:string -> labels:string list -> tab

(** Widget state: ordered list of tabs and the currently selected index. *)
type t = private {tabs : tab list; selected : int}

(** Create a widget from a list of tabs.  The first tab is selected. *)
val make : tab list -> t

(** Return the currently selected tab, if any. *)
val current : t -> tab option

(** Select the tab with the given [id].  No-op if [id] is not found. *)
val select : t -> id:string -> t

(** Move the selection left, right, to the first, or to the last tab. *)
val move : t -> [`Left | `Right | `First | `Last] -> t

(** Handle a key event.  Number keys [1]-[9] select the corresponding tab
    (1-indexed).  Left/Right arrows and h/l move between tabs.
    Returns the updated state and whether the key was handled. *)
val handle_event :
  ?bubble_unhandled:bool ->
  t ->
  key:string ->
  cols:int ->
  t * [`Handled | `Bubble]

(** Render the tab bar.  Labels are shortened from right to left until the
    bar fits within [cols].  The selected tab is highlighted when [focus] is
    true.

    @param focus Whether the tab bar itself has keyboard focus.
    @param cols Available terminal width.
    @return A single-line string ready for display. *)
val render : t -> focus:bool -> cols:int -> string
