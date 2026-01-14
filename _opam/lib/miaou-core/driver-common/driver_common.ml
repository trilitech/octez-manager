(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Miaou_core.Tui_page
module Modal_renderer = Miaou_internals.Modal_renderer
module Modal_manager = Miaou_core.Modal_manager
module Registry = Miaou_core.Registry
module Navigation = Miaou_core.Navigation
module Page_transition_utils = Page_transition_utils
module Pager_notify = Pager_notify

module Modal_utils = struct
  let render_with_modal_overlay ~view ~rows ~cols =
    if Modal_manager.has_active () then
      match
        Modal_renderer.render_overlay ~cols:(Some cols) ~base:view ~rows ()
      with
      | Some v -> v
      | None -> view
    else view
end

module type DRIVER_BACKEND = sig
  type size = {rows : int; cols : int}

  type event = Quit | Refresh | Key of string

  val poll_event : unit -> event

  val render : view:string -> size:size -> unit

  val detect_size : unit -> size

  val init : unit -> unit

  val cleanup : unit -> unit
end

module Make (Backend : DRIVER_BACKEND) = struct
  open Backend

  let run (initial_page : (module PAGE_SIG)) =
    Backend.init () ;
    at_exit Backend.cleanup ;

    let rec loop : type s.
        (module PAGE_SIG with type state = s) ->
        s Navigation.t ->
        [`Quit | `SwitchTo of string] =
     fun (module P : PAGE_SIG with type state = s) (ps : s Navigation.t) ->
      let size = Backend.detect_size () in
      let lterm_size = LTerm_geom.{rows = size.rows; cols = size.cols} in
      let view = P.view ps ~size:lterm_size ~focus:false in
      let view_with_modal =
        Modal_utils.render_with_modal_overlay
          ~view
          ~rows:size.rows
          ~cols:size.cols
      in
      Backend.render ~view:view_with_modal ~size ;

      match Backend.poll_event () with
      | Quit -> `Quit
      | Refresh -> (
          let ps' = P.refresh ps in
          match Navigation.pending ps' with
          | Some "__QUIT__" -> `Quit
          | Some name -> (
              match Registry.find name with
              | Some (module Next : PAGE_SIG) ->
                  let ps_to = Next.init () in
                  loop (module Next) ps_to
              | None -> `Quit)
          | None -> loop (module P) ps')
      | Key k -> (
          let ps' =
            if Modal_manager.has_active () then (
              Modal_manager.handle_key k ;
              ps)
            else
              match k with
              | "Up" -> P.move ps (-1)
              | "Down" -> P.move ps 1
              | "q" | "Q" -> Navigation.quit ps
              | _ -> P.handle_key ps k ~size:lterm_size
          in
          match Navigation.pending ps' with
          | Some "__QUIT__" -> `Quit
          | Some name -> (
              match Registry.find name with
              | Some (module Next : PAGE_SIG) ->
                  let ps_to = Next.init () in
                  loop (module Next) ps_to
              | None -> `Quit)
          | None -> loop (module P) ps')
    in

    let module InitialPage = (val initial_page : PAGE_SIG) in
    let initial_state = InitialPage.init () in
    loop (module InitialPage) initial_state
end
