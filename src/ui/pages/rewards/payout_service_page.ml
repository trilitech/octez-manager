(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Pager = Miaou_widgets_display.Pager_widget
module File_pager = Miaou_widgets_display.File_pager
module Keys = Miaou.Core.Keys
module Widgets = Miaou_widgets_display.Widgets
module Navigation = Miaou.Core.Navigation
module Render_notify = Miaou_helpers.Render_notify
open Octez_manager_lib

let name = "payout_service"

type tab = Details | Logs

type pager_type = Static of Pager.t | FileTail of File_pager.t

type cleanup_info = {
  temp_files : string list;
  bg_pid : int option; (* PID of background journalctl process *)
}

type state = {
  instance : string;
  tab : tab;
  details_pager : Pager.t;
  logs_pager : pager_type;
  cleanup : cleanup_info;
}

type msg = unit

type pstate = state Navigation.t

let pending_initial_tab : tab option ref = ref None

let set_initial_tab tab = pending_initial_tab := Some tab

let tab_label = function Details -> "details" | Logs -> "logs"

let get_logs_pager = function
  | Static p -> p
  | FileTail fp -> File_pager.pager fp

let build_details_lines ~instance =
  (* Same content as open_payout_service_detail_only but returns string list *)
  let lines = ref [] in
  let add s = lines := s :: !lines in
  let add_blank () = add "" in
  add "═══ Payout Service Status ═══" ;
  add_blank () ;
  let timer_active = Systemd.is_payout_timer_active ~instance in
  add
    (Printf.sprintf
       "  Timer:      %s"
       (if timer_active then "● Active" else "○ Inactive")) ;
  (match Systemd.get_payout_last_run ~instance with
  | Some info ->
      let status = if info.success then "✓ Success" else "✗ Failed" in
      add (Printf.sprintf "  Last run:   %s" info.timestamp) ;
      add (Printf.sprintf "  Result:     %s" status)
  | None -> add "  Last run:   Never") ;
  add_blank () ;
  add "═══ Timer Details ═══" ;
  add_blank () ;
  (match Systemd.get_payout_timer_next ~instance with
  | Some next -> add (Printf.sprintf "  Next trigger: %s" next)
  | None -> add "  Next trigger: Unknown") ;
  add_blank () ;
  add "═══ Service Configuration ═══" ;
  add_blank () ;
  (match Systemd.cat_payout_service ~instance with
  | Ok content ->
      String.split_on_char '\n' content |> List.iter (fun l -> add ("  " ^ l))
  | Error (`Msg msg) ->
      add (Printf.sprintf "  (Could not read unit file: %s)" msg)) ;
  List.rev !lines

let build_logs_lines ~instance =
  match Systemd.get_payout_service_logs ~instance ~n:500 with
  | Ok output ->
      if String.length (String.trim output) = 0 then ["(No log entries found)"]
      else String.split_on_char '\n' output
  | Error (`Msg msg) -> [Printf.sprintf "Could not fetch logs: %s" msg]

let build_logs_pager ~instance =
  (* Use journalctl streaming for live log tailing *)
  let payout_unit_name = Printf.sprintf "octez-manager-payout@%s" instance in
  let unit_name = payout_unit_name ^ ".service" in
  let cmd =
    if Paths.is_root () then
      Printf.sprintf
        "journalctl -u %s -f -n 500 -o cat"
        (Filename.quote unit_name)
    else
      Printf.sprintf
        "journalctl --user -u %s -f -n 500 -o cat"
        (Filename.quote unit_name)
  in
  let temp_file = Filename.temp_file "om_payout_log_" ".txt" in
  (* Capture the PID of the background process *)
  let full_cmd =
    Printf.sprintf "%s > %s 2>&1 & echo $!" cmd (Filename.quote temp_file)
  in
  let ic = (Unix.open_process_in [@allow_forbidden "PID capture"]) full_cmd in
  let pid_str = try input_line ic with End_of_file -> "" in
  let status = (Unix.close_process_in [@allow_forbidden "PID capture"]) ic in
  let bg_pid =
    try Some (int_of_string (String.trim pid_str)) with _ -> None
  in
  match status with
  | Unix.WEXITED 0 -> (
      (Unix.sleepf [@allow_forbidden "file readiness delay"]) 0.1 ;
      match
        File_pager.open_file
          ~follow:true
          ~notify_render:Render_notify.request_render
          ~title:""
          temp_file
      with
      | Ok fp ->
          let p = File_pager.pager fp in
          let _ = Pager.handle_key p ~key:"G" in
          (FileTail fp, {temp_files = [temp_file]; bg_pid})
      | Error e ->
          let p =
            Pager.open_lines
              ~title:""
              [Printf.sprintf "Error opening log file: %s" e]
          in
          (Static p, {temp_files = [temp_file]; bg_pid}))
  | _ ->
      (* Fallback to static logs *)
      let lines = build_logs_lines ~instance in
      ( Static (Pager.open_lines ~title:"" lines),
        {temp_files = []; bg_pid = None} )

let make_state instance =
  let details_lines = build_details_lines ~instance in
  let logs_pager, cleanup = build_logs_pager ~instance in
  {
    instance;
    tab = Details;
    details_pager = Pager.open_lines ~title:"" details_lines;
    logs_pager;
    cleanup;
  }

let init () =
  let tab = match !pending_initial_tab with Some t -> t | None -> Details in
  pending_initial_tab := None ;
  match Context.take_pending_payout_service () with
  | Some instance ->
      let s = make_state instance in
      Navigation.make {s with tab}
  | None ->
      (* Fallback, shouldn't happen *)
      Navigation.make (make_state "")

let update ps _ = ps

let update_pager_state s pager' =
  match s.tab with
  | Details -> {s with details_pager = pager'}
  | Logs -> (
      match s.logs_pager with
      | Static _ -> {s with logs_pager = Static pager'}
      | FileTail fp ->
          let current = File_pager.pager fp in
          if current == pager' then s else {s with logs_pager = Static pager'})

let cleanup s =
  (match s.logs_pager with
  | FileTail fp -> ( try File_pager.close fp with _ -> ())
  | Static _ -> ()) ;
  (match s.cleanup.bg_pid with
  | Some pid -> (
      try (Unix.kill [@allow_forbidden "cleanup"]) pid Sys.sigterm
      with _ -> ())
  | None -> ()) ;
  List.iter (fun f -> try Sys.remove f with _ -> ()) s.cleanup.temp_files

let refresh ps =
  (* Consume navigation signals from Context *)
  match Context.consume_navigation () with
  | Some (Context.Goto page) -> Navigation.goto page ps
  | Some Context.Back ->
      cleanup ps.Navigation.s ;
      Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None -> ps

let manual_refresh ps =
  (* Manual refresh (r key) - rebuild details pager, keep logs pager streaming *)
  let s = ps.Navigation.s in
  let details_lines = build_details_lines ~instance:s.instance in
  Navigation.update
    (fun s -> {s with details_pager = Pager.open_lines ~title:"" details_lines})
    ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps =
  cleanup ps.Navigation.s ;
  Navigation.back ps

let toggle_tab ps =
  Navigation.update
    (fun s ->
      let new_tab = match s.tab with Details -> Logs | Logs -> Details in
      {s with tab = new_tab})
    ps

let current_pager s =
  match s.tab with
  | Details -> s.details_pager
  | Logs -> get_logs_pager s.logs_pager

let handled_keys () = []

let keymap _ps =
  let noop ps = ps in
  [
    {
      Miaou.Core.Tui_page.key = "?";
      action = noop;
      help = "Help";
      display_only = true;
    };
  ]

let view ps ~focus ~size =
  let s = ps.Navigation.s in
  let tab_str = tab_label s.tab in
  let title =
    Printf.sprintf
      "%s   %s"
      (Widgets.themed_primary
         (Printf.sprintf " Payout Service: %s " s.instance))
      (Widgets.themed_emphasis (String.capitalize_ascii tab_str))
  in
  let help =
    Widgets.themed_muted
      "t: toggle tab . r: refresh . f: follow . /: search . Esc: back"
  in
  let header = [title; help] in
  Themed_page.render_layout ~size ~header ~footer:[] ~child:(fun inner_size ->
      Pager.render
        ~cols:inner_size.LTerm_geom.cols
        ~win:inner_size.LTerm_geom.rows
        (current_pager s)
        ~focus)

let handle_modal_key ps key ~size =
  let s = ps.Navigation.s in
  (* Forward keys to pager when in modal/search mode *)
  let win = size.LTerm_geom.rows in
  let current_pager = current_pager s in
  let pager', _ = Pager.handle_key ~win current_pager ~key in
  let new_state = update_pager_state s pager' in
  Navigation.update (fun _ -> new_state) ps

let handle_key ps key ~size =
  let s = ps.Navigation.s in
  let current_pager_val = current_pager s in
  let win = size.LTerm_geom.rows in

  (* Check if pager is in input mode *)
  let pager_in_input_mode =
    match current_pager_val.Pager.input_mode with
    | `Search_edit | `Lookup | `Help -> true
    | `None -> false
  in

  (* Handle Escape key directly - Keys.of_string doesn't parse it correctly *)
  if key = "Esc" || key = "Escape" then
    if pager_in_input_mode then
      (* Let pager handle Esc to close search *)
      let pager', _ = Pager.handle_key ~win current_pager_val ~key in
      let new_state = update_pager_state s pager' in
      Navigation.update (fun _ -> new_state) ps
    else (
      cleanup s ;
      Navigation.back ps)
  else
    match Keys.of_string key with
    | Some Keys.Escape ->
        (* This might never be reached, but keep it for completeness *)
        if pager_in_input_mode then
          let pager', _ = Pager.handle_key ~win current_pager_val ~key in
          let new_state = update_pager_state s pager' in
          Navigation.update (fun _ -> new_state) ps
        else (
          cleanup s ;
          Navigation.back ps)
    | Some (Keys.Char "r") when not pager_in_input_mode -> manual_refresh ps
    | Some (Keys.Char "t") when not pager_in_input_mode -> toggle_tab ps
    | _ ->
        (* Delegate all other keys to pager *)
        let pager', consumed = Pager.handle_key ~win current_pager_val ~key in
        if consumed then
          let new_state = update_pager_state s pager' in
          Navigation.update (fun _ -> new_state) ps
        else ps

let has_modal ps =
  let s = ps.Navigation.s in
  match (current_pager s).Pager.input_mode with
  | `Search_edit | `Lookup | `Help -> true
  | `None -> false

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let keymap = keymap

  let handled_keys = handled_keys

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let on_key ps key ~size =
    let ps' = handle_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let key_hints _ps = Miaou.Core.Tui_page.[{key = "?"; help = "Help"}]

  let has_modal = has_modal
end

module Page =
  Themed_page.Make
    (Page_Impl)
    (struct
      let page_name = "payout_service"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
