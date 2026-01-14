(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Miaou_core.Tui_page
module Modal_manager = Miaou_core.Modal_manager
module Navigation = Miaou_core.Navigation

type driver_key = Term_events.driver_key =
  | Quit
  | Refresh
  | Enter
  | NextPage
  | PrevPage
  | Up
  | Down
  | Left
  | Right
  | Other of string

let run_with_key_source ~read_key (module Page : PAGE_SIG) :
    [`Quit | `SwitchTo of string] =
  let default_size = {LTerm_geom.rows = 24; cols = 80} in
  (* Helper: apply any pending navigation from modal callbacks *)
  let apply_pending_modal_nav ps =
    match Modal_manager.take_pending_navigation () with
    | Some page -> Navigation.goto page ps
    | None -> ps
  in
  let check_nav ps =
    let ps = apply_pending_modal_nav ps in
    match Navigation.pending ps with
    | Some "__QUIT__" -> `Quit
    | Some p -> `SwitchTo p
    | None -> `Continue ps
  in
  let rec loop ps =
    match (read_key () : driver_key) with
    | Quit -> `Quit
    | Refresh -> (
        let ps' = Page.service_cycle (Page.refresh ps) 0 in
        match check_nav ps' with
        | `Continue ps'' -> loop ps''
        | `Quit -> `Quit
        | `SwitchTo p -> `SwitchTo p)
    | Enter -> (
        if Modal_manager.has_active () then (
          Modal_manager.handle_key "Enter" ;
          let ps' = Page.refresh ps in
          if Modal_manager.take_consume_next_key () then
            if not (Modal_manager.has_active ()) then
              let ps'' = Page.service_cycle ps' 0 in
              match check_nav ps'' with
              | `Continue ps''' -> loop ps'''
              | (`Quit | `SwitchTo _) as r -> r
            else loop ps'
          else if not (Modal_manager.has_active ()) then
            let ps'' = Page.service_cycle ps' 0 in
            match check_nav ps'' with
            | `Continue ps''' -> loop ps'''
            | (`Quit | `SwitchTo _) as r -> r
          else loop ps')
        else
          match check_nav ps with
          | `Continue _ -> (
              let ps' = Page.handle_key ps "Enter" ~size:default_size in
              match check_nav ps' with
              | `Continue ps'' -> loop ps''
              | `Quit -> `Quit
              | `SwitchTo p -> `SwitchTo p)
          | `Quit -> `Quit
          | `SwitchTo p -> `SwitchTo p)
    | Up | Down | Left | Right | NextPage | PrevPage -> loop ps
    | Other key -> (
        let ps' = Page.handle_key ps key ~size:default_size in
        match check_nav ps' with
        | `Continue ps'' -> loop ps''
        | `Quit -> `Quit
        | `SwitchTo p -> `SwitchTo p)
  in
  Modal_manager.clear () ;
  let ps0 = Page.init () in
  match check_nav ps0 with
  | `Continue ps0' -> loop ps0'
  | `Quit -> `Quit
  | `SwitchTo p -> `SwitchTo p
