(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

type outcome = [`Commit | `Cancel]

(** Specification for modal width sizing.
    - [Fixed n] uses exactly n columns (clamped to terminal width)
    - [Ratio r] uses r percent of terminal width (e.g., 0.8 for 80%)
    - [Clamped {ratio; min; max}] uses ratio of terminal, clamped to [min, max] *)
type max_width_spec =
  | Fixed of int
  | Ratio of float
  | Clamped of {ratio : float; min : int; max : int}

type ui = {
  title : string;
  left : int option;
  max_width : max_width_spec option;
  dim_background : bool;
}

val has_active : unit -> bool

val clear : unit -> unit

(** Push a modal onto the stack.

    @param commit_on List of keys that close the modal with [`Commit] outcome.
      Pass an empty list [[]] if the modal should handle its own closing logic
      (useful for nested modals that open sub-modals on Enter).

    @param cancel_on List of keys that close the modal with [`Cancel] outcome.
      Pass an empty list [[]] if the modal handles Esc internally.

    @param on_close Callback invoked with the final state and outcome when the
      modal closes.

    {b Note for nested modals}: If your modal opens another modal when the user
    presses Enter, use [push] with [commit_on:[]] instead of [push_default].
    Otherwise, the parent modal will close immediately after the child modal
    opens, because the key check happens {i after} your [handle_key] is called.
*)
val push :
  (module Tui_page.PAGE_SIG with type state = 's) ->
  init:'s Navigation.t ->
  ui:ui ->
  commit_on:string list ->
  cancel_on:string list ->
  on_close:('s Navigation.t -> outcome -> unit) ->
  unit

val handle_key : string -> unit

(** Convenience wrapper: push with sensible defaults.

    Automatically sets [commit_on:["Enter"]] and [cancel_on:["Esc"]].
    This avoids repeating the common boilerplate at call sites.

    {b ⚠ Warning}: If your modal handles [Enter] internally (e.g., to open a
    nested modal), use [push] with [commit_on:[]] instead. Otherwise, the parent
    modal will close immediately after your [handle_key] processes the Enter key,
    because the modal manager checks [commit_on] {i after} calling [handle_key].

    For nested modals, you typically want:
    {[
      Modal_manager.push
        (module My_modal)
        ~init:state
        ~ui:{title = "Parent"; ...}
        ~commit_on:[]   (* Don't auto-close on Enter *)
        ~cancel_on:[]   (* Handle Esc manually *)
        ~on_close:(fun s outcome -> ...)
    ]}
*)
val push_default :
  (module Tui_page.PAGE_SIG with type state = 's) ->
  init:'s Navigation.t ->
  ui:ui ->
  on_close:('s Navigation.t -> outcome -> unit) ->
  unit

val set_current_size : int -> int -> unit

(* Get the last known terminal size (rows, cols) as published by the driver. *)
val get_current_size : unit -> int * int

(** Prevent key propagation when closing a modal.

    When you programmatically close a modal from within [handle_key] (e.g., when
    the user presses Enter or Esc), call this function {i before} calling
    [close_top] to prevent that key from propagating to the parent modal or
    underlying page.

    {b Why you need this}: When a modal closes, the key that triggered the close
    is by default passed to the underlying page/modal. If you close a nested
    modal on Enter, without calling [set_consume_next_key], the parent modal
    will also receive that Enter key, potentially triggering unwanted behavior.

    {b Typical pattern}:
    {[
      let handle_key state ~key ~size =
        match key with
        | "Enter" ->
            Modal_manager.set_consume_next_key () ;
            Modal_manager.close_top `Commit ;
            state
        | "Esc" ->
            Modal_manager.set_consume_next_key () ;
            Modal_manager.close_top `Cancel ;
            state
        | _ -> handle_other_key state key
    ]}

    The flag is automatically cleared after being consumed by the driver.
*)
val set_consume_next_key : unit -> unit

(** Read and clear the consume-next-key flag.

    Returns [true] exactly once after [set_consume_next_key] was called.
    This is used internally by the driver to determine if a key should be
    consumed instead of propagated to the underlying page.

    You typically don't need to call this function yourself unless you're
    implementing custom modal handling logic.
*)
val take_consume_next_key : unit -> bool

(** Request navigation from a modal callback.

    Modal [on_close] callbacks cannot directly modify the parent page's pstate.
    Instead, they can call this function to request navigation. The driver
    checks for pending navigation after modal close and applies it to the pstate.

    {b Example usage in on_close}:
    {[
      ~on_close:(fun _modal_state outcome ->
        match outcome with
        | `Commit -> Modal_manager.set_pending_navigation "next_page"
        | `Cancel -> ())
    ]}

    This eliminates the need for manual refs to communicate navigation from
    modal callbacks, which was error-prone and required pages to check the ref
    in [service_cycle].
*)
val set_pending_navigation : string -> unit

(** Read and clear the pending navigation request.

    Returns [Some page_name] exactly once after [set_pending_navigation] was
    called. This is used by drivers to apply navigation after modal close.
    You typically don't need to call this function yourself.
*)
val take_pending_navigation : unit -> string option

(* Access the UI metadata for the top-most modal, if any. *)
val top_ui_opt : unit -> ui option

(* Return the title of the top-most modal, if any. *)
val top_title_opt : unit -> string option

(** Programmatically close the top-most modal.

    Removes the top modal from the stack and invokes its [on_close] callback
    with the final state and the specified outcome. No-op if no modal is active.

    {b Important}: When calling this from within [handle_key], remember to call
    [set_consume_next_key ()] first to prevent the key from propagating to the
    parent modal or underlying page. See [set_consume_next_key] for details.

    @param outcome Either [`Commit] or [`Cancel], passed to the modal's
      [on_close] callback to indicate how the modal was closed.
*)
val close_top : outcome -> unit

(* Higher-level convenience helpers. These are thin wrappers around existing
   modal pages that simplify common patterns: alerts (message-only),
   confirm (yes/no via a select modal), and prompt (textbox). They accept a
   small callback invoked with the result. These helpers do not block the
   caller; they integrate with the existing modal on_close callback model. *)

val alert :
  (module Tui_page.PAGE_SIG with type state = 's) ->
  init:'s Navigation.t ->
  ?title:string ->
  ?left:int ->
  ?max_width:max_width_spec ->
  ?dim_background:bool ->
  unit ->
  unit

val confirm :
  (module Tui_page.PAGE_SIG with type state = 's) ->
  init:'s Navigation.t ->
  ?title:string ->
  ?left:int ->
  ?max_width:max_width_spec ->
  ?dim_background:bool ->
  on_result:(bool -> unit) ->
  unit ->
  unit

val confirm_with_extract :
  (module Tui_page.PAGE_SIG with type state = 's) ->
  init:'s Navigation.t ->
  ?title:string ->
  ?left:int ->
  ?max_width:max_width_spec ->
  ?dim_background:bool ->
  extract:('s Navigation.t -> 'a option) ->
  on_result:('a option -> unit) ->
  unit ->
  unit

val prompt :
  (module Tui_page.PAGE_SIG with type state = 's) ->
  init:'s Navigation.t ->
  ?title:string ->
  ?left:int ->
  ?max_width:max_width_spec ->
  ?dim_background:bool ->
  extract:('s Navigation.t -> 'a option) ->
  on_result:('a option -> unit) ->
  unit ->
  unit
