(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Pager = Miaou_widgets_display.Pager_widget
module File_pager = Miaou_widgets_display.File_pager
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
module Widgets = Miaou_widgets_display.Widgets
open Octez_manager_lib
module Render_notify = Miaou_helpers.Render_notify

let name = "log_viewer"

type pager_type = Static of Pager.t | FileTail of File_pager.t

type state = {
  instance : string;
  role : string;
  source : Log_viewer.log_source;
  pager : pager_type;
  next_page : string option;
  cleanup_files : string list;
}

type msg = unit

let get_pager = function Static p -> p | FileTail fp -> File_pager.pager fp

let close_pager s =
  (match s.pager with Static _ -> () | FileTail fp -> File_pager.close fp) ;
  List.iter (fun f -> try Sys.remove f with _ -> ()) s.cleanup_files

let open_file_with_tail path =
  match
    File_pager.open_file
      ~follow:true
      ~notify_render:Render_notify.request_render
      path
  with
  | Ok fp ->
      let p = File_pager.pager fp in
      let _ = Pager.handle_key p ~key:"G" in
      Ok fp
  | Error e -> Error e

let open_stream_via_file cmd =
  let temp_file = Filename.temp_file "octez_log_" ".txt" in
  (* Start background process to feed the file *)
  let full_cmd =
    Printf.sprintf "%s > %s 2>&1 &" cmd (Filename.quote temp_file)
  in
  match Sys.command full_cmd with
  | 0 -> (
      (* Give it a moment to create/write *)
      Unix.sleepf 0.1 ;
      match open_file_with_tail temp_file with
      | Ok fp -> Ok (fp, temp_file)
      | Error e -> Error (`Msg e))
  | _ -> Error (`Msg "Failed to start background logger")

let init () =
  match Context.take_pending_instance_detail () with
  | Some instance -> (
      match Service_registry.find ~instance with
      | Ok (Some svc) ->
          (* Try to open daily logs first, fall back to journald *)
          let source, pager, cleanup =
            match
              Log_viewer.get_daily_log_file ~role:svc.Service.role ~instance
            with
            | Ok log_file -> (
                match open_file_with_tail log_file with
                | Ok fp -> (Log_viewer.DailyLogs, FileTail fp, [])
                | Error _ -> (
                    (* Fall back to journald via temp file *)
                    match
                      Log_viewer.get_log_cmd
                        ~role:svc.Service.role
                        ~instance
                        ~source:Journald
                    with
                    | Ok cmd -> (
                        match open_stream_via_file cmd with
                        | Ok (fp, tmp) ->
                            (Log_viewer.Journald, FileTail fp, [tmp])
                        | Error (`Msg e) ->
                            let p = Pager.open_text ~title:"Error" e in
                            (Log_viewer.Journald, Static p, []))
                    | Error (`Msg e) ->
                        let p = Pager.open_text ~title:"Error" e in
                        (Log_viewer.Journald, Static p, [])))
            | Error _ -> (
                (* Fall back to journald via temp file *)
                match
                  Log_viewer.get_log_cmd
                    ~role:svc.Service.role
                    ~instance
                    ~source:Journald
                with
                | Ok cmd -> (
                    match open_stream_via_file cmd with
                    | Ok (fp, tmp) -> (Log_viewer.Journald, FileTail fp, [tmp])
                    | Error (`Msg e) ->
                        let p = Pager.open_text ~title:"Error" e in
                        (Log_viewer.Journald, Static p, []))
                | Error (`Msg e) ->
                    let p = Pager.open_text ~title:"Error" e in
                    (Log_viewer.Journald, Static p, []))
          in
          {
            instance;
            role = svc.Service.role;
            source;
            pager;
            next_page = None;
            cleanup_files = cleanup;
          }
      | Ok None ->
          let pager =
            Static (Pager.open_text ~title:"Error" "Instance not found")
          in
          {
            instance;
            role = "";
            source = Journald;
            pager;
            next_page = None;
            cleanup_files = [];
          }
      | Error (`Msg e) ->
          let pager = Static (Pager.open_text ~title:"Error" e) in
          {
            instance;
            role = "";
            source = Journald;
            pager;
            next_page = None;
            cleanup_files = [];
          })
  | None ->
      let pager =
        Static (Pager.open_text ~title:"Error" "No instance selected")
      in
      {
        instance = "";
        role = "";
        source = Journald;
        pager;
        next_page = None;
        cleanup_files = [];
      }

let update s _ = s

let refresh s =
  (* Close the old pager and cleanup *)
  close_pager s ;
  let s = {s with cleanup_files = []} in

  match s.source with
  | Log_viewer.DailyLogs -> (
      match Service_registry.find ~instance:s.instance with
      | Ok (Some svc) -> (
          match
            Log_viewer.get_daily_log_file
              ~role:svc.Service.role
              ~instance:s.instance
          with
          | Ok log_file -> (
              match open_file_with_tail log_file with
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
      (* Refresh Journald - restart the stream *)
      match
        Log_viewer.get_log_cmd
          ~role:s.role
          ~instance:s.instance
          ~source:s.source
      with
      | Ok cmd -> (
          match open_stream_via_file cmd with
          | Ok (fp, tmp) -> {s with pager = FileTail fp; cleanup_files = [tmp]}
          | Error (`Msg msg) ->
              let pager = Static (Pager.open_text ~title:"Error" msg) in
              {s with pager})
      | Error (`Msg msg) ->
          let pager = Static (Pager.open_text ~title:"Error" msg) in
          {s with pager})

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = s

let close_pager_and_cleanup s =
  (match s.pager with Static _ -> () | FileTail fp -> File_pager.close fp) ;
  List.iter (fun f -> try Sys.remove f with _ -> ()) s.cleanup_files

let back s =
  close_pager_and_cleanup s ;
  {s with next_page = Some "__BACK__"}

let toggle_source s =
  (* Close the old pager - ignore EBADF from double-close bug in FilePager *)
  (try close_pager s with
  | Unix.Unix_error (Unix.EBADF, "close", _) -> ()
  | exn -> raise exn) ;
  let s = {s with cleanup_files = []} in
  let new_source =
    match s.source with
    | Log_viewer.Journald -> Log_viewer.DailyLogs
    | Log_viewer.DailyLogs -> Log_viewer.Journald
  in
  match new_source with
  | Log_viewer.DailyLogs -> (
      match Service_registry.find ~instance:s.instance with
      | Ok (Some svc) -> (
          match
            Log_viewer.get_daily_log_file
              ~role:svc.Service.role
              ~instance:s.instance
          with
          | Ok log_file -> (
              (* Restoring File_pager with follow:true *)
              match open_file_with_tail log_file with
              | Ok fp -> {s with source = new_source; pager = FileTail fp}
              | Error msg ->
                  let pager = Static (Pager.open_text ~title:"Error" msg) in
                  {s with source = new_source; pager})
          | Error (`Msg msg) ->
              let pager = Static (Pager.open_text ~title:"Error" msg) in
              {s with source = new_source; pager})
      | _ ->
          let pager =
            Static (Pager.open_text ~title:"Error" "Instance not found")
          in
          {s with source = new_source; pager})
  | Log_viewer.Journald -> (
      (* Switch to Journald - use open_stream_via_file *)
      match
        Log_viewer.get_log_cmd
          ~role:s.role
          ~instance:s.instance
          ~source:new_source
      with
      | Ok cmd -> (
          match open_stream_via_file cmd with
          | Ok (fp, tmp) ->
              {
                s with
                source = new_source;
                pager = FileTail fp;
                cleanup_files = [tmp];
              }
          | Error (`Msg msg) ->
              let pager = Static (Pager.open_text ~title:"Error" msg) in
              {s with source = new_source; pager})
      | Error (`Msg msg) ->
          let pager = Static (Pager.open_text ~title:"Error" msg) in
          {s with source = new_source; pager})

let handled_keys () = []

let keymap _s = []

let view s ~focus ~size =
  let source_str =
    match s.source with
    | Log_viewer.Journald -> "journald"
    | Log_viewer.DailyLogs -> "daily logs"
  in
  let privilege =
    if Common.is_root () then Widgets.red "● SYSTEM" else Widgets.green "● USER"
  in
  let title =
    Printf.sprintf
      "%s   %s"
      (Widgets.title_highlight
         (Printf.sprintf " Logs: %s " (String.capitalize_ascii s.instance)))
      privilege
  in
  let help =
    Widgets.dim
      (Printf.sprintf
         "Source: %s · r: refresh · t: toggle source · /: search · f: follow · \
          Esc: back"
         source_str)
  in
  let header = [title; help] in
  Vsection.render ~size ~header ~footer:[] ~child:(fun inner_size ->
      Pager.render ~win:inner_size.LTerm_geom.rows (get_pager s.pager) ~focus)

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
      close_pager_and_cleanup s ;
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
          close_pager_and_cleanup s ;
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
      close_pager_and_cleanup s ;
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
