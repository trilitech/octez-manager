(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Pager = Miaou_widgets_display.Pager_widget
module File_pager = Miaou_widgets_display.File_pager
module Keys = Miaou.Core.Keys
open Octez_manager_lib

let name = "log_viewer"

type pager_type = Static of Pager.t | FileTail of File_pager.t

type state = {
  instance : string;
  role : string;
  source : Log_viewer.log_source;
  pager : pager_type;
  next_page : string option;
}

type msg = unit

let get_pager = function Static p -> p | FileTail fp -> File_pager.pager fp

let close_pager = function Static _ -> () | FileTail fp -> File_pager.close fp

let init () =
  match Context.take_pending_instance_detail () with
  | Some instance -> (
      match Service_registry.find ~instance with
      | Ok (Some svc) ->
          (* Try to open daily logs first, fall back to journald *)
          let source, pager =
            match
              Log_viewer.get_daily_log_file ~data_dir:svc.Service.data_dir
            with
            | Ok log_file -> (
                match File_pager.open_file ~follow:true log_file with
                | Ok fp -> (Log_viewer.DailyLogs, FileTail fp)
                | Error _ ->
                    (* Fall back to journald *)
                    let content =
                      match
                        Log_viewer.read_logs
                          ~role:svc.Service.role
                          ~instance
                          ~source:Journald
                          ~lines:1000
                      with
                      | Ok lines -> String.concat "\n" lines
                      | Error (`Msg e) -> Printf.sprintf "Error: %s" e
                    in
                    let p =
                      Pager.open_text ~title:("Logs: " ^ instance) content
                    in
                    (Log_viewer.Journald, Static p))
            | Error _ ->
                (* Fall back to journald *)
                let content =
                  match
                    Log_viewer.read_logs
                      ~role:svc.Service.role
                      ~instance
                      ~source:Journald
                      ~lines:1000
                  with
                  | Ok lines -> String.concat "\n" lines
                  | Error (`Msg e) -> Printf.sprintf "Error: %s" e
                in
                let p = Pager.open_text ~title:("Logs: " ^ instance) content in
                (Log_viewer.Journald, Static p)
          in
          {instance; role = svc.Service.role; source; pager; next_page = None}
      | Ok None ->
          let pager =
            Static (Pager.open_text ~title:"Error" "Instance not found")
          in
          {instance; role = ""; source = Journald; pager; next_page = None}
      | Error (`Msg e) ->
          let pager = Static (Pager.open_text ~title:"Error" e) in
          {instance; role = ""; source = Journald; pager; next_page = None})
  | None ->
      let pager =
        Static (Pager.open_text ~title:"Error" "No instance selected")
      in
      {instance = ""; role = ""; source = Journald; pager; next_page = None}

let update s _ = s

let refresh s =
  (* Close the old pager and reload - ignore EBADF from double-close bug *)
  (try close_pager s.pager with
  | Unix.Unix_error (Unix.EBADF, "close", _) -> ()
  | exn -> raise exn) ;
  match s.source with
  | Log_viewer.DailyLogs -> (
      match Service_registry.find ~instance:s.instance with
      | Ok (Some svc) -> (
          match
            Log_viewer.get_daily_log_file ~data_dir:svc.Service.data_dir
          with
          | Ok log_file -> (
              match File_pager.open_file ~follow:true log_file with
              | Ok fp -> {s with pager = FileTail fp}
              | Error msg ->
                  let pager = Static (Pager.open_text ~title:"Error" msg) in
                  {s with pager})
          | Error (`Msg msg) ->
              let pager = Static (Pager.open_text ~title:"Error" msg) in
              {s with pager})
      | _ ->
          let pager =
            Static (Pager.open_text ~title:"Error" "Instance not found")
          in
          {s with pager})
  | Log_viewer.Journald -> (
      match
        Log_viewer.read_logs
          ~role:s.role
          ~instance:s.instance
          ~source:s.source
          ~lines:1000
      with
      | Ok lines ->
          let content = String.concat "\n" lines in
          let pager =
            Static (Pager.open_text ~title:("Logs: " ^ s.instance) content)
          in
          {s with pager}
      | Error (`Msg e) ->
          let pager = Static (Pager.open_text ~title:"Error" ("Error: " ^ e)) in
          {s with pager})

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = s

let back s = {s with next_page = Some "__BACK__"}

let toggle_source s =
  (* Close the old pager - ignore EBADF from double-close bug in FilePager *)
  (try close_pager s.pager with
  | Unix.Unix_error (Unix.EBADF, "close", _) -> ()
  | exn -> raise exn) ;
  let new_source =
    match s.source with
    | Log_viewer.Journald -> Log_viewer.DailyLogs
    | Log_viewer.DailyLogs -> Log_viewer.Journald
  in
  match new_source with
  | Log_viewer.DailyLogs -> (
      (* Use static pager for daily logs (FilePager disabled due to bugs) *)
      match Service_registry.find ~instance:s.instance with
      | Ok (Some svc) -> (
          match
            Log_viewer.get_daily_log_file ~data_dir:svc.Service.data_dir
          with
          | Ok log_file ->
              let content =
                try
                  let ic = open_in log_file in
                  let content = really_input_string ic (in_channel_length ic) in
                  close_in ic ;
                  content
                with _ -> "Error reading log file"
              in
              let pager = Static (Pager.open_text ~title:log_file content) in
              {s with source = new_source; pager}
          | Error (`Msg msg) ->
              let pager = Static (Pager.open_text ~title:"Error" msg) in
              {s with source = new_source; pager})
      | _ ->
          let pager =
            Static (Pager.open_text ~title:"Error" "Instance not found")
          in
          {s with source = new_source; pager})
  | Log_viewer.Journald -> (
      (* Use static pager for journald logs *)
      match
        Log_viewer.read_logs
          ~role:s.role
          ~instance:s.instance
          ~source:new_source
          ~lines:1000
      with
      | Ok lines ->
          let content = String.concat "\n" lines in
          let pager =
            Static (Pager.open_text ~title:("Logs: " ^ s.instance) content)
          in
          {s with source = new_source; pager}
      | Error (`Msg e) ->
          let pager = Static (Pager.open_text ~title:"Error" ("Error: " ^ e)) in
          {s with pager; source = new_source})

let handled_keys () = []

let keymap _s = []

let view s ~focus ~size =
  let source_str =
    match s.source with
    | Log_viewer.Journald -> "journald"
    | Log_viewer.DailyLogs -> "daily logs"
  in
  let title = Printf.sprintf "Logs · %s (source: %s)" s.instance source_str in
  let help =
    "r: refresh  t: toggle source  /: search  n/p: next/prev  f: follow  Esc: \
     back"
  in
  let header_lines = [title; help; ""] in
  let header_height = List.length header_lines in
  let pager_height = size.LTerm_geom.rows - header_height in
  let pager_view = Pager.render ~win:pager_height (get_pager s.pager) ~focus in
  String.concat "\n" header_lines ^ pager_view

let handle_modal_key s key ~size =
  (* Forward keys to pager when in modal/search mode *)
  let win = size.LTerm_geom.rows in
  let current_pager = get_pager s.pager in
  let pager', _ = Pager.handle_key ~win current_pager ~key in
  let new_pager =
    match s.pager with
    | Static _ -> Static pager'
    | FileTail _fp -> if current_pager == pager' then s.pager else Static pager'
  in
  {s with pager = new_pager}

let handle_key s key ~size =
  let current_pager = get_pager s.pager in
  let win = size.LTerm_geom.rows in

  (* Check if pager is in input mode *)
  let pager_in_input_mode =
    match current_pager.Pager.input_mode with
    | `Search_edit | `Lookup -> true
    | `None -> false
  in

  (* Handle Escape key directly - Keys.of_string doesn't parse it correctly *)
  if key = "Esc" || key = "Escape" then
    if pager_in_input_mode then
      (* Let pager handle Esc to close search *)
      let pager', _ = Pager.handle_key ~win current_pager ~key in
      let new_pager =
        match s.pager with
        | Static _ -> Static pager'
        | FileTail _ ->
            if current_pager == pager' then s.pager else Static pager'
      in
      {s with pager = new_pager}
    else (
      (* Otherwise, Esc goes back - IMPORTANT: close the pager first! *)
      close_pager s.pager ;
      {s with next_page = Some "__BACK__"})
  else
    match Keys.of_string key with
    | Some Keys.Escape ->
        (* This might never be reached, but keep it for completeness *)
        if pager_in_input_mode then
          let pager', _ = Pager.handle_key ~win current_pager ~key in
          let new_pager =
            match s.pager with
            | Static _ -> Static pager'
            | FileTail _ ->
                if current_pager == pager' then s.pager else Static pager'
          in
          {s with pager = new_pager}
        else (
          close_pager s.pager ;
          {s with next_page = Some "__BACK__"})
    | Some (Keys.Char "r") when not pager_in_input_mode -> refresh s
    | Some (Keys.Char "t") when not pager_in_input_mode -> toggle_source s
    | _ ->
        (* Delegate all other keys to pager *)
        let pager', consumed = Pager.handle_key ~win current_pager ~key in
        if consumed then
          let new_pager =
            match s.pager with
            | Static _ -> Static pager'
            | FileTail _ ->
                if current_pager == pager' then s.pager else Static pager'
          in
          {s with pager = new_pager}
        else s

let next_page s =
  match s.next_page with
  | Some _ ->
      (* Close pager before navigating away *)
      close_pager s.pager ;
      s.next_page
  | None -> None

let has_modal s =
  match (get_pager s.pager).Pager.input_mode with
  | `Search_edit | `Lookup -> true
  | `None -> false

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let enter = enter

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let keymap = keymap

  let handled_keys = handled_keys

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let next_page = next_page

  let has_modal = has_modal
end

module Page =
  Monitored_page.Make
    (Page_Impl)
    (struct
      let page_name = "log_viewer"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
