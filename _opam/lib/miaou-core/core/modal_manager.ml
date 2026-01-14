(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

module Logger_capability = Miaou_interfaces.Logger_capability

type outcome = [`Commit | `Cancel]

type max_width_spec = Miaou_internals.Modal_snapshot.max_width_spec =
  | Fixed of int
  | Ratio of float
  | Clamped of {ratio : float; min : int; max : int}

type ui = {
  title : string;
  left : int option;
  max_width : max_width_spec option;
  dim_background : bool;
}

let resolve_max_width = Miaou_internals.Modal_snapshot.resolve_max_width

type frame =
  | Frame : {
      p : (module Tui_page.PAGE_SIG with type state = 's);
      mutable st : 's Navigation.t;
      ui : ui;
      commit_on : string list;
      cancel_on : string list;
      on_close : 's Navigation.t -> outcome -> unit;
    }
      -> frame

let stack : frame list ref = ref []

let has_active () = !stack <> []

let clear () = stack := []

let debug_enabled =
  lazy
    (let get_env var =
       match Miaou_interfaces.System.get () with
       | Some sys -> sys.get_env_var var
       | None -> Sys.getenv_opt var
     in
     match get_env "MIAOU_TUI_DEBUG_MODAL" with
     | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
     | _ -> false)

let dprintf fmt =
  if Lazy.force debug_enabled then Printf.eprintf fmt
  else Printf.ifprintf Stdlib.stdout fmt

(* Expose a mutable location for the current terminal geometry. The driver
  should update this on each render (and on resize events) so modal view
  thunks receive the correct size. *)
let current_size = ref {LTerm_geom.rows = 24; cols = 80}

let set_current_size rows cols = current_size := {LTerm_geom.rows; cols}

let current_cols () = !current_size.LTerm_geom.cols

let get_current_size () =
  (!current_size.LTerm_geom.rows, !current_size.LTerm_geom.cols)

let push (type s) (module P : Tui_page.PAGE_SIG with type state = s)
    ~(init : s Navigation.t) ~ui ~commit_on ~cancel_on ~on_close =
  (* Replace any existing frame with the same title to avoid duplicate overlays. *)
  dprintf
    "[DEBUG] Modal_manager.push: title='%s', current_stack_size=%d\n%!"
    ui.title
    (List.length !stack) ;
  List.iteri
    (fun i (Frame r) ->
      dprintf "[DEBUG]   [%d] existing: '%s'\n%!" i r.ui.title)
    !stack ;
  stack := List.filter (fun (Frame r) -> r.ui.title <> ui.title) !stack ;
  let fr =
    Frame {p = (module P); st = init; ui; commit_on; cancel_on; on_close}
  in
  stack := !stack @ [fr] ;
  dprintf
    "[DEBUG] Modal_manager.push: after push, stack_size=%d\n%!"
    (List.length !stack) ;
  (* Also log to Logger capability if available *)
  match Logger_capability.get () with
  | Some logger ->
      logger.logf
        Debug
        (Printf.sprintf
           "PUSH: title='%s' stack_size=%d"
           ui.title
           (List.length !stack))
  | None -> ()

let pop_top () =
  match List.rev !stack with
  | [] -> ()
  | Frame _ :: rest_rev -> stack := List.rev rest_rev

let handle_key key =
  match List.rev !stack with
  | [] -> ()
  | Frame ({commit_on; cancel_on; on_close; _} as r) :: _ ->
      let module P = (val r.p : Tui_page.PAGE_SIG with type state = _) in
      let term_size = !current_size in
      let max_width_opt =
        match r.ui.max_width with
        | None -> None
        | Some spec -> resolve_max_width spec ~cols:term_size.LTerm_geom.cols
      in
      let geom =
        Miaou_internals.Modal_utils.compute_modal_geometry
          ~cols:term_size.LTerm_geom.cols
          ~rows:term_size.LTerm_geom.rows
          ~left_opt:r.ui.left
          ~max_width_opt
      in
      let size =
        {LTerm_geom.rows = geom.max_content_h; cols = geom.content_width}
      in
      (* Let the page handle the key first so it can update its state based on
         the key (e.g. move cursor). After the page updated the state we
         evaluate commit/cancel semantics so commits reflect the new state. *)
      r.st <- P.handle_key r.st key ~size ;
      if List.exists (( = ) key) cancel_on then (
        let st = r.st in
        pop_top () ;
        on_close st `Cancel)
      else if List.exists (( = ) key) commit_on then (
        let st = r.st in
        pop_top () ;
        on_close st `Commit)
      else ()

let render_overlay ~base =
  (* render_overlay is intentionally not implemented here; use modal renderer
     in the executable. The internal library provides a snapshot accessor via
     Miaou.Internal.Modal_snapshot.set_provider which we register below. *)
  let _ = base in
  None

(* Register provider for internal snapshot accessor so the internal lib can
   call back into this module without requiring the internal lib to depend
   on the public lib. *)
let () =
  let provider () =
    List.map
      (fun (Frame r) ->
        let title = r.ui.title in
        let left = r.ui.left in
        let max_width = r.ui.max_width in
        let dim = r.ui.dim_background in
        let view_thunk (size : LTerm_geom.size) =
          let module P = (val r.p : Tui_page.PAGE_SIG with type state = _) in
          P.view r.st ~focus:true ~size
        in
        (title, left, max_width, dim, view_thunk))
      !stack
  in
  try Miaou_internals.Modal_snapshot.set_provider provider with _ -> ()

(* Helper: allow a modal to mark that the next key which closed it should
   be considered consumed and not propagated by the driver. This is useful
   for short-lived informational modals that must not let Enter/Esc hit the
   underlying page. *)
let consume_next_key_flag = ref false

let set_consume_next_key () = consume_next_key_flag := true

let take_consume_next_key () =
  let v = !consume_next_key_flag in
  consume_next_key_flag := false ;
  v

(* Pending navigation from modal on_close callbacks.
   Modal callbacks can call set_pending_navigation to request navigation
   without needing access to the parent page's pstate. The driver checks
   this after modal close and applies it to the pstate. *)
let pending_navigation_ref : string option ref = ref None

let set_pending_navigation page = pending_navigation_ref := Some page

let take_pending_navigation () =
  let v = !pending_navigation_ref in
  pending_navigation_ref := None ;
  v

let top_ui_opt () =
  match List.rev !stack with [] -> None | Frame r :: _ -> Some r.ui

(* Expose the top modal title when present to allow targeted auto-dismissal. *)
let top_title_opt () =
  match top_ui_opt () with Some ui -> Some ui.title | None -> None

(* Duplicate current_size/set_current_size declaration removed; top-level
  definitions near the top of this module are used instead. *)

let close_top (outcome : outcome) =
  match List.rev !stack with
  | [] -> ()
  | Frame r :: _ ->
      (* Pop first so handlers that push new frames are respected. *)
      let st = r.st in
      pop_top () ;
      r.on_close st outcome

let push_default (type s) (module P : Tui_page.PAGE_SIG with type state = s)
    ~init ~ui ~on_close =
  (* Default keys commonly used by modals. *)
  let commit_on = ["Enter"] in
  let cancel_on = ["Esc"] in
  push (module P) ~init ~ui ~commit_on ~cancel_on ~on_close

(* Higher-level convenience helpers. These are non-blocking wrappers that
   push appropriate modal pages and deliver results via callbacks. They use
   `push_default` internally to get the Enter/Esc key behavior. *)

let alert (type s) (module P : Tui_page.PAGE_SIG with type state = s) ~init
    ?(title = "") ?left ?max_width ?(dim_background = true) () =
  let ui = {title; left; max_width; dim_background} in
  push_default
    (module P)
    ~init
    ~ui
    ~on_close:(fun (_ : s Navigation.t) -> function `Commit | `Cancel -> ())

let confirm (type s) (module P : Tui_page.PAGE_SIG with type state = s) ~init
    ?(title = "Confirm") ?left ?max_width ?(dim_background = true) ~on_result ()
    =
  let ui = {title; left; max_width; dim_background} in
  push_default
    (module P)
    ~init
    ~ui
    ~on_close:(fun _st -> function
      | `Commit -> on_result true | `Cancel -> on_result false)

let confirm_with_extract (type s)
    (module P : Tui_page.PAGE_SIG with type state = s) ~init
    ?(title = "Confirm") ?left ?max_width ?(dim_background = true) ~extract
    ~on_result () =
  let ui = {title; left; max_width; dim_background} in
  push_default
    (module P)
    ~init
    ~ui
    ~on_close:(fun st -> function
      | `Commit -> on_result (extract st) | `Cancel -> on_result None)

let prompt (type s) (module P : Tui_page.PAGE_SIG with type state = s) ~init
    ?(title = "Prompt") ?left ?max_width ?(dim_background = true) ~extract
    ~on_result () =
  let ui = {title; left; max_width; dim_background} in
  push_default
    (module P)
    ~init
    ~ui
    ~on_close:(fun st -> function
      | `Commit -> on_result (extract st) | `Cancel -> on_result None)
