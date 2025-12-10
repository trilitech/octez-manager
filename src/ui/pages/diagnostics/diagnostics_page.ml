(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Sparkline = Miaou_widgets_display.Sparkline_widget
module Keys = Miaou.Core.Keys
open Octez_manager_lib

let name = "diagnostics"

(* Persistent address state *)
let metrics_addr_ref = ref "0.0.0.0:3010"

type state = {
  services : Data.Service_state.t list;
  bg_queue_spark : Sparkline.t;
  scroll_offset : int;
  content_height : int;
  next_page : string option;
}

type msg = unit

let init () =
  (* Check if metrics is already running from env *)
  (match Metrics.get_server_info () with
   | Some (addr, port) -> metrics_addr_ref := Printf.sprintf "%s:%d" addr port
   | None -> (
       match Sys.getenv_opt "OCTEZ_MANAGER_METRICS_ADDR" with
       | Some addr -> metrics_addr_ref := addr
       | None -> ())) ;
  {
    services = Data.load_service_states ();
    bg_queue_spark = Sparkline.create ~width:40 ~max_points:60 ();
    scroll_offset = 0;
    content_height = 0;
    next_page = None;
  }

let update s _ = s

let refresh s =
  (* Update sparklines with current metrics *)
  let bg_depth = Metrics.get_bg_queue_depth () in
  Sparkline.push s.bg_queue_spark (float_of_int bg_depth) ;
  {s with services = Data.load_service_states ()}

let scroll_up s =
  {s with scroll_offset = max 0 (s.scroll_offset - 3)}

let scroll_down s =
  let max_scroll = max 0 (s.content_height - 20) in
  {s with scroll_offset = min max_scroll (s.scroll_offset + 3)}

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = s

let back s = {s with next_page = Some "__BACK__"}

let toggle_recorder s =
  if Metrics_recorder.is_enabled () then (
    Metrics_recorder.stop () ;
    Context.toast_info "Metrics recorder stopped" ;
    s)
  else (
    Metrics_recorder.start () ;
    Context.toast_success "Metrics recorder started" ;
    s)

let toggle_metrics s =
  if Metrics.is_enabled () then (
    Modal_helpers.show_error
      ~title:"Metrics Server"
      "Server cannot be stopped while running. Restart the app to disable." ;
    s)
  else
    match Metrics.parse_addr !metrics_addr_ref with
    | Ok (addr, port) ->
        Metrics.start_server ~addr ~port ;
        Context.toast_success
          (Printf.sprintf "Metrics server started on %s:%d" addr port) ;
        s
    | Error (`Msg msg) ->
        Modal_helpers.show_error ~title:"Invalid Address" msg ;
        s

let edit_metrics_addr s =
  Modal_helpers.prompt_text_modal
    ~title:"Metrics Server Address"
    ~initial:!metrics_addr_ref
    ~on_submit:(fun addr -> metrics_addr_ref := addr)
    () ;
  s

let change_duration s =
  let open Modal_helpers in
  let current = Metrics_recorder.get_duration () in
  let items = Metrics_recorder.all_durations in
  let to_string d =
    let str = Metrics_recorder.duration_to_string d in
    if d = current then str ^ " (current)" else str
  in
  open_choice_modal
    ~title:"Recording Duration"
    ~items
    ~to_string
    ~on_select:(fun dur ->
        Metrics_recorder.set_duration dur ;
        Context.toast_info
          (Printf.sprintf
             "Recording duration set to %s"
             (Metrics_recorder.duration_to_string dur))) ;
  s

let keymap _ =
  [
    ("Esc", back, "Back");
    ("r", refresh, "Refresh");
    ("m", toggle_metrics, "Toggle metrics");
    ("a", edit_metrics_addr, "Edit address");
    ("R", toggle_recorder, "Toggle recorder");
    ("d", change_duration, "Change duration");
    ("Up", scroll_up, "Scroll up");
    ("Down", scroll_down, "Scroll down");
  ]

let header =
  [
    Widgets.title_highlight " Diagnostics & Metrics ";
    Widgets.dim "Live system metrics and service status";
  ]

let footer =
  [
    Widgets.dim
      "↑/↓: scroll  r: refresh  m: toggle metrics  R: toggle recorder  d: duration  Esc: back";
  ]

let view s ~focus:_ ~size =
  let lines = ref [] in
  let add line = lines := line :: !lines in

  (* Service Status Section *)
  add (Widgets.bold "Service Status") ;
  add "" ;
  if s.services = [] then add "  No services registered"
  else
    List.iter
      (fun (st : Data.Service_state.t) ->
        let svc = st.service in
        let status_icon, status_color =
          match st.status with
          | Running -> ("●", 10)
          | Stopped -> ("○", 8)
          | Unknown _ -> ("?", 11)
        in
        let line =
          Printf.sprintf
            "  %s %-20s  %s  %s"
            (Widgets.fg status_color status_icon)
            (Widgets.bold svc.Service.instance)
            (Widgets.dim svc.Service.role)
            (Widgets.dim
               (Printf.sprintf
                  "net:%s mode:%s"
                  svc.Service.network
                  (History_mode.to_string svc.Service.history_mode)))
        in
        add line)
      s.services ;

  add "" ;
  add (Widgets.bold "Real-Time Metrics") ;
  add "" ;

  (* Sparkline *)
  let bg_depth = Metrics.get_bg_queue_depth () in
  let bg_max = Metrics.get_bg_queue_max () in
  add ("  " ^ Charts.render_bg_queue_sparkline s.bg_queue_spark) ;
  add
    (Printf.sprintf
       "  Current: %d/%d  %s"
       bg_depth
       bg_max
       (if bg_depth > 0 then Widgets.fg 11 "⚠ tasks pending"
        else Widgets.fg 10 "✓ idle")) ;

  add "" ;
  add (Widgets.bold "Metrics Recorder") ;
  add "" ;
  let recorder_enabled = Metrics_recorder.is_enabled () in
  let recorder_icon =
    if recorder_enabled then Widgets.fg 10 "●" else Widgets.fg 8 "○"
  in
  let recorder_status =
    if recorder_enabled then "recording" else "stopped"
  in
  let duration_str =
    Metrics_recorder.duration_to_short_string
      (Metrics_recorder.get_duration ())
  in
  add
    (Printf.sprintf
       "  Status: %s %s (duration: %s, press 'd' to change)"
       recorder_icon
       recorder_status
       duration_str) ;
  add (Widgets.dim "  (press 'R' to start/stop recording)") ;

  (* Historical Charts *)
  if recorder_enabled || Metrics_recorder.get_samples () <> [] then (
    add "" ;
    add (Widgets.bold "Historical Metrics") ;
    add "" ;
    let samples = Metrics_recorder.get_samples () in
    let chart_width = min 70 (size.LTerm_geom.cols - 4) in
    
    (* BG Queue Chart *)
    let bg_chart = Charts.render_bg_queue_chart samples ~width:chart_width ~height:10 in
    add bg_chart ;
    add "" ;
    
    (* Service Status Chart *)
    let svc_chart = Charts.render_service_status_chart samples ~width:chart_width ~height:10 in
    add svc_chart ;
    add "" ;
    
    (* Summary Bars *)
    let stats = Metrics_recorder.get_stats () in
    let summary = Charts.render_summary_bars stats ~width:chart_width ~height:8 in
    add summary) ;

  add "" ;
  add (Widgets.bold "Metrics Server Configuration") ;
  add "" ;
  let metrics_enabled = Metrics.is_enabled () in
  let status_icon =
    if metrics_enabled then Widgets.fg 10 "●" else Widgets.fg 8 "○"
  in
  let status_text = if metrics_enabled then "enabled" else "disabled" in
  add (Printf.sprintf "  Status: %s %s" status_icon status_text) ;
  (match Metrics.get_server_info () with
   | Some (addr, port) ->
       add (Printf.sprintf "  Endpoint: http://%s:%d/metrics" addr port) ;
       add (Widgets.dim "  (server is running)")
   | None ->
       add (Printf.sprintf "  Address: %s" !metrics_addr_ref) ;
       add (Widgets.dim "  (press 'm' to start, 'a' to edit address)")) ;

  add "" ;
  add (Widgets.bold "System Information") ;
  add "" ;
  add
    (Printf.sprintf
       "  Privilege: %s"
       (if Common.is_root () then Widgets.red "● SYSTEM"
        else Widgets.green "● USER")) ;

  let all_lines = List.rev !lines in
  let content_height = List.length all_lines in
  let s = {s with content_height} in
  
  (* Apply scrolling *)
  let visible_height = size.LTerm_geom.rows - 4 in (* account for header/footer *)
  let visible_lines =
    all_lines
    |> (fun l -> 
        if List.length l <= visible_height then l
        else
          let start = min s.scroll_offset (max 0 (List.length l - visible_height)) in
          let rec take n acc = function
            | [] -> List.rev acc
            | _ when n = 0 -> List.rev acc
            | x :: xs -> take (n - 1) (x :: acc) xs
          in
          let rec drop n = function
            | [] -> []
            | l when n = 0 -> l
            | _ :: xs -> drop (n - 1) xs
          in
          drop start l |> take visible_height [])
  in
  
  let body = String.concat "\n" visible_lines in
  
  (* Add scroll indicator *)
  let scroll_indicator =
    if content_height > visible_height then
      Printf.sprintf
        " [%d/%d lines, %d%% visible]"
        (min (s.scroll_offset + visible_height) content_height)
        content_height
        (100 * visible_height / content_height)
    else ""
  in
  let header_with_scroll =
    header @ [Widgets.dim scroll_indicator]
  in
  
  Miaou_widgets_layout.Vsection.render
    ~size
    ~header:header_with_scroll
    ~footer
    ~child:(fun _ -> body)

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  s

let handle_key s key ~size:_ =
  Metrics.mark_input_event () ;
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    s)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") | Some (Keys.Char "q") ->
        {s with next_page = Some "__BACK__"}
    | Some (Keys.Char "r") -> refresh s
    | Some (Keys.Char "m") -> toggle_metrics s
    | Some (Keys.Char "a") -> edit_metrics_addr s
    | Some (Keys.Char "R") -> toggle_recorder s
    | Some (Keys.Char "d") -> change_duration s
    | Some Keys.Up -> scroll_up s
    | Some Keys.Down -> scroll_down s
    | Some (Keys.Char "k") -> scroll_up s
    | Some (Keys.Char "j") -> scroll_down s
    | _ -> s

let next_page s = s.next_page

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page : Miaou.Core.Tui_page.PAGE_SIG = struct
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

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let next_page = next_page

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
