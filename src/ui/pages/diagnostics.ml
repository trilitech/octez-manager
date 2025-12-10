(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Sparkline = Miaou_widgets_display.Sparkline_widget
module Keys = Miaou.Core.Keys
open Octez_manager_lib

let name = "diagnostics"

(* Persistent address state *)
let metrics_addr_ref = ref "0.0.0.0:3010"

type state = {
  services : Data.Service_state.t list;
  bg_queue_spark : Sparkline.t;
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
    next_page = None;
  }

let update s _ = s

let refresh s =
  (* Update sparklines with current metrics *)
  let bg_depth = Metrics.get_bg_queue_depth () in
  Sparkline.push s.bg_queue_spark (float_of_int bg_depth) ;
  {s with services = Data.load_service_states ()}

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = s

let back s = {s with next_page = Some "__BACK__"}

let toggle_metrics s =
  if Metrics.is_enabled () then (
    (* Can't easily stop the server - just show message *)
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

let keymap _ =
  [
    ("Esc", back, "Back");
    ("r", refresh, "Refresh");
    ("m", toggle_metrics, "Toggle metrics");
    ("a", edit_metrics_addr, "Edit address");
  ]

(* Format a metric value with units *)
let format_metric value unit_str =
  if value < 1.0 then Printf.sprintf "%.2f%s" value unit_str
  else if value < 10.0 then Printf.sprintf "%.1f%s" value unit_str
  else Printf.sprintf "%.0f%s" value unit_str

let header =
  [
    Widgets.title_highlight " Diagnostics & Metrics ";
    Widgets.dim "Live system metrics and service status";
  ]

let footer = [Widgets.dim "r: refresh  m: toggle metrics  a: edit addr  Esc: back"]

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
                  (Octez_manager_lib.History_mode.to_string svc.Service.history_mode)))
        in
        add line)
      s.services ;

  add "" ;
  add (Widgets.bold "UI Performance Metrics") ;
  add "" ;

  (* Get current metrics snapshot *)
  let bg_depth = Metrics.get_bg_queue_depth () in
  let bg_max = Metrics.get_bg_queue_max () in
  
  (* Render background queue sparkline *)
  let bg_spark_line =
    if not (Sparkline.is_empty s.bg_queue_spark) then
      "  BG Queue (60s): "
      ^ Sparkline.render_with_label
          s.bg_queue_spark
          ~label:""
          ~focus:false
          ~thresholds:[{Sparkline.value = 3.0; color = "11"}]
          ()
    else "  BG Queue (60s): " ^ Widgets.dim "[collecting data...]"
  in
  add bg_spark_line ;
  add
    (Printf.sprintf
       "  Current: %d/%d  %s"
       bg_depth
       bg_max
       (if bg_depth > 0 then Widgets.fg 11 "⚠ tasks pending"
        else Widgets.fg 10 "✓ idle")) ;

  add "" ;
  add (Widgets.bold "Metrics Server Configuration") ;
  add "" ;
  let metrics_enabled = Metrics.is_enabled () in
  let status_icon = if metrics_enabled then Widgets.fg 10 "●" else Widgets.fg 8 "○" in
  let status_text = if metrics_enabled then "enabled" else "disabled" in
  add (Printf.sprintf "  Status: %s %s" status_icon status_text) ;
  
  (* Show current or configured address *)
  (match Metrics.get_server_info () with
   | Some (addr, port) ->
       add (Printf.sprintf "  Endpoint: http://%s:%d/metrics" addr port) ;
       add (Widgets.dim "  (server is running)")
   | None ->
       add (Printf.sprintf "  Address: %s" !metrics_addr_ref) ;
       add (Widgets.dim "  (press 'm' to start, 'a' to edit address)")) ;

  add "" ;
  add (Widgets.dim "  Tip: Metrics update on each refresh (press 'r')") ;

  add "" ;
  add (Widgets.bold "System Information") ;
  add "" ;

  let body = List.rev !lines |> String.concat "\n" in
  Vsection.render ~size ~header ~footer ~child:(fun _ -> body)

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
