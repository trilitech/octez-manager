(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Contextual help system for Miaou pages and modals.

    {1 Overview}

    The driver intercepts the "?" key press and displays contextual help using
    the content set via this module. Pages and modals should call [set] or [push]
    to provide help text, which will be shown in a help overlay when the user
    presses "?".

    {b Important:} Your page's or modal's [handle_key] function will {e never}
    receive the "?" key event—it is reserved by the driver for help display.

    {1 Example Usage}

    In your page or modal's [view] function, set the help hint:

    {[
      let view state ~focus ~size =
        Help_hint.set (Some "Press Space to toggle, Enter to confirm, Esc to cancel") ;
        (* ... render your page ... *)
    ]}

    For modals that can be nested, use [push] and [pop]:

    {[
      let init () =
        Help_hint.push ~short:"?" ~long:"Press Esc to close this modal" () ;
        (* ... *)

      let on_close () =
        Help_hint.pop () ;
        (* ... *)
    ]}

    {1 Short vs Long Hints}

    A hint can provide independent [short] and [long] variants. The driver
    selects which one to render based on available width, preferring [long]
    when the layout is wide enough, falling back to [short] otherwise. *)

type hint = {short : string option; long : string option}

(** Set the active hint (both variants) replacing any existing hints. Passing
    [None] clears all hints. For backward compatibility, [set (Some s)] sets
    the short variant to [s] and clears the long variant. *)
val set : string option -> unit

(** Clear all hints. *)
val clear : unit -> unit

(** Push a new hint on top of the stack. Use this when opening a sub-modal.
    Call [pop] in the modal's on_close callback to restore the previous hint. *)
val push : ?short:string -> ?long:string -> unit -> unit

(** Pop the most recently pushed hint. Safe to call even if the stack is empty. *)
val pop : unit -> unit

(** Get the currently active hint (top of stack), if any. *)
val get_active : unit -> hint option

(** Deprecated: returns only the short variant of the active hint. *)
val get : unit -> string option
