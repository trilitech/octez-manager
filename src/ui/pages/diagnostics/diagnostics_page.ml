(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Sparkline = Miaou_widgets_display.Sparkline_widget
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module Box = Miaou_widgets_layout.Box_widget
module Flex = Miaou_widgets_layout.Flex_layout
module C = Miaou_canvas.Canvas
module Style = Miaou_style.Style
module Style_context = Miaou_style.Style_context
open Octez_manager_lib

let name = "diagnostics"

(* Persistent address state *)
let metrics_addr_ref = ref "0.0.0.0:3010"

type state = {
  services : Data.Service_state.t list;
  bg_queue_spark : Sparkline.t;
  scroll_offset : int;
  content_height : int;
  last_visible_height : int; (* Track terminal height *)
}

type msg = unit

type pstate = state Navigation.t

let init () =
  (* Check if metrics is already running from env *)
  (match Metrics.get_server_info () with
  | Some (addr, port) -> metrics_addr_ref := Printf.sprintf "%s:%d" addr port
  | None -> (
      match Sys.getenv_opt "OCTEZ_MANAGER_METRICS_ADDR" with
      | Some addr -> metrics_addr_ref := addr
      | None -> ())) ;
  (* Auto-start recording so historical charts populate immediately on open *)
  Metrics.start_recording () ;
  Navigation.make
    {
      services = Data.load_service_states ();
      bg_queue_spark = Sparkline.create ~width:40 ~max_points:60 ();
      scroll_offset = 0;
      content_height = 0;
      last_visible_height = 20;
    }

let update ps _ = ps

let refresh ps =
  Navigation.update
    (fun s ->
      (* Update sparklines with current metrics *)
      let bg_depth = Metrics.get_bg_queue_depth () in
      Sparkline.push s.bg_queue_spark (float_of_int bg_depth) ;
      {s with services = Data.load_service_states ()})
    ps

(* Called to update content height - we'll calculate it in view and store via this hack *)
let content_height_ref = ref 0

let update_content_height s = {s with content_height = !content_height_ref}

let scroll_up s = {s with scroll_offset = max 0 (s.scroll_offset - 3)}

let scroll_down_impl s ~max_height =
  let max_scroll = max 0 (s.content_height - max_height) in
  {
    s with
    scroll_offset = min max_scroll (s.scroll_offset + 3);
    last_visible_height = max_height;
  }

(* For keymap - uses last known visible height *)
let scroll_down s = scroll_down_impl s ~max_height:s.last_visible_height

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ =
  Navigation.update
    (fun s ->
      (* Auto-refresh sparkline and services *)
      let bg_depth = Metrics.get_bg_queue_depth () in
      Sparkline.push s.bg_queue_spark (float_of_int bg_depth) ;
      {s with services = Data.load_service_states ()})
    ps

let back ps = Navigation.back ps

let toggle_recorder s =
  if Metrics.is_recording () then (
    Metrics.stop_recording () ;
    Context.toast_info "Metrics recorder stopped" ;
    s)
  else (
    Metrics.start_recording () ;
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
  let durations = [(12, "1 minute"); (60, "5 minutes"); (180, "15 minutes")] in
  let current = Metrics.get_recording_duration () in
  let items = durations in
  let to_string (samples, label) =
    if samples = current then label ^ " (current)" else label
  in
  open_choice_modal
    ~title:"Recording Duration"
    ~items
    ~to_string
    ~on_select:(fun (samples, label) ->
      Metrics.set_recording_duration samples ;
      Context.toast_info (Printf.sprintf "Recording duration set to %s" label))
    () ;
  s

let clear_caches s =
  Cache.invalidate_all () ;
  Context.toast_success "All caches cleared" ;
  s

let handled_keys () =
  Miaou.Core.Keys.
    [
      Escape;
      Char "r";
      Char "m";
      Char "a";
      Char "R";
      Char "d";
      Char "c";
      Up;
      Down;
    ]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [
    kb "Esc" "Back";
    kb "r" "Refresh";
    kb "m" "Toggle metrics";
    kb "a" "Edit address";
    kb "R" "Toggle recorder";
    kb "d" "Change duration";
    kb "c" "Clear caches";
    kb "↑/↓" "Navigate";
    kb "?" "Help";
  ]

let resolve_fg ~fallback style =
  let resolved = Style.to_resolved style in
  if resolved.r_fg < 0 then fallback else resolved.r_fg

let resolve_bg ~fallback style =
  let resolved = Style.to_resolved style in
  if resolved.r_bg < 0 then fallback else resolved.r_bg

let render_canvas_header ~width =
  let style_of fg = {C.default_style with fg} in
  let bold_of fg = {C.default_style with fg; bold = true} in
  let bg_base =
    resolve_bg ~fallback:236 (Style_context.background_secondary ())
  in
  let border_fg = resolve_fg ~fallback:69 (Style_context.border ()) in
  let title_fg = resolve_fg ~fallback:147 (Style_context.text_emphasized ()) in
  let label_fg = resolve_fg ~fallback:252 (Style_context.text ()) in
  let ok_fg = resolve_fg ~fallback:10 (Style_context.success ()) in
  let off_fg = resolve_fg ~fallback:8 (Style_context.text_muted ()) in
  let err_fg = resolve_fg ~fallback:196 (Style_context.error ()) in
  let rows = 5 in
  let c = C.create ~rows ~cols:width in
  (* Background fill *)
  C.fill_rect
    c
    ~row:0
    ~col:0
    ~width
    ~height:rows
    ~char:" "
    ~style:{C.default_style with bg = bg_base} ;
  (* Rounded border *)
  C.draw_box
    c
    ~row:0
    ~col:0
    ~width
    ~height:rows
    ~border:Rounded
    ~style:(style_of border_fg) ;
  (* Title centered *)
  let title = " Diagnostics & Metrics " in
  let title_col = max 2 ((width - String.length title) / 2) in
  C.draw_text c ~row:1 ~col:title_col ~style:(bold_of title_fg) title ;
  (* Status indicators on row 3 *)
  let metrics_on = Metrics.is_enabled () in
  let recorder_on = Metrics.is_recording () in
  let is_root = Paths.is_root () in
  let indicators =
    [
      ( (if metrics_on then "●" else "○"),
        (if metrics_on then ok_fg else off_fg),
        "metrics" );
      ( (if recorder_on then "●" else "○"),
        (if recorder_on then ok_fg else off_fg),
        "recorder" );
      ( (if is_root then "●" else "●"),
        (if is_root then err_fg else ok_fg),
        if is_root then "root" else "user" );
    ]
  in
  let total_len =
    List.fold_left
      (fun acc (icon, _, label) ->
        acc + String.length icon + 1 + String.length label + 3)
      0
      indicators
  in
  let start_col = max 2 ((width - total_len) / 2) in
  let _col =
    List.fold_left
      (fun col (icon, color, label) ->
        C.draw_text c ~row:3 ~col ~style:(bold_of color) icon ;
        C.draw_text c ~row:3 ~col:(col + 2) ~style:(style_of label_fg) label ;
        col + String.length icon + 1 + String.length label + 3)
      start_col
      indicators
  in
  C.to_ansi c

let _footer = []

(* Section content renderers - each returns lines for that section *)
let render_services_content services =
  if services = [] then [Widgets.themed_muted "No services registered"]
  else
    List.map
      (fun (st : Data.Service_state.t) ->
        let svc = st.service in
        let status_icon =
          match st.status with
          | Running -> Widgets.themed_success "●"
          | Stopped -> Widgets.themed_muted "○"
          | Unknown _ -> Widgets.themed_warning "?"
        in
        Printf.sprintf
          "%s %-20s  %s  %s"
          status_icon
          (Widgets.themed_emphasis svc.Service.instance)
          (Widgets.themed_muted svc.Service.role)
          (Widgets.themed_muted
             (Printf.sprintf
                "net:%s mode:%s"
                svc.Service.network
                (History_mode.to_string svc.Service.history_mode))))
      services

let render_caches_content () =
  let cache_stats = Cache.get_stats () in
  if cache_stats = [] then [Widgets.themed_muted "No caches registered"]
  else
    let lines =
      List.concat_map
        (fun (name, hits, misses, age, ttl, expired, sub_entries) ->
          let age_str =
            match age with
            | None -> Widgets.themed_muted "empty"
            | Some a ->
                let s = Printf.sprintf "%.1fs/%.1fs" a ttl in
                if expired then Widgets.themed_error s
                else Widgets.themed_success s
          in
          let stats_str =
            if hits + misses > 0 then
              Printf.sprintf " hits:%d misses:%d" hits misses
            else ""
          in
          let count_str =
            if sub_entries <> [] then
              Printf.sprintf " (%d)" (List.length sub_entries)
            else ""
          in
          let main_line =
            Printf.sprintf
              "%-20s  %s%s%s"
              name
              age_str
              count_str
              (Widgets.themed_muted stats_str)
          in
          let sub_lines =
            List.map
              (fun (entry : Cache.sub_entry) ->
                let sub_age_str =
                  let s = Printf.sprintf "%.1fs" entry.age in
                  if entry.expired then Widgets.themed_error s
                  else Widgets.themed_success s
                in
                Printf.sprintf "  └─ %-16s  %s" entry.key sub_age_str)
              sub_entries
          in
          main_line :: sub_lines)
        cache_stats
    in
    lines @ [Widgets.themed_muted "(press 'c' to clear all)"]

let render_realtime_content bg_queue_spark =
  let bg_depth = Metrics.get_bg_queue_depth () in
  let bg_max = Metrics.get_bg_queue_max () in
  [
    Charts.render_bg_queue_sparkline bg_queue_spark;
    Printf.sprintf
      "Current: %d/%d  %s"
      bg_depth
      bg_max
      (if bg_depth > 0 then Widgets.themed_warning "⚠ tasks pending"
       else Widgets.themed_success "✓ idle");
  ]

let render_recorder_content () =
  let recorder_enabled = Metrics.is_recording () in
  let recorder_icon =
    if recorder_enabled then Widgets.themed_success "●"
    else Widgets.themed_muted "○"
  in
  let recorder_status =
    if recorder_enabled then Widgets.themed_success "recording"
    else Widgets.themed_muted "stopped"
  in
  let duration_samples = Metrics.get_recording_duration () in
  let duration_str =
    match duration_samples with
    | 12 -> "1m"
    | 60 -> "5m"
    | 180 -> "15m"
    | n -> Printf.sprintf "%ds" (n * 5)
  in
  [
    Printf.sprintf
      "%s %s %s %s"
      (Widgets.themed_emphasis "Status:")
      recorder_icon
      recorder_status
      (Widgets.themed_muted
         (Printf.sprintf "(duration: %s, 'd' to change)" duration_str));
    Widgets.themed_muted "(press 'R' to start/stop)";
  ]

let render_historical_content ~chart_width =
  let samples = Metrics.get_snapshots () in
  if samples = [] then
    [
      Widgets.themed_muted "Collecting data... (wait ~5 seconds)";
      Widgets.themed_muted "Charts will appear once samples are recorded";
    ]
  else
    let charts =
      [
        Charts.render_bg_queue_chart samples ~width:chart_width ~height:10;
        Charts.render_service_status_chart samples ~width:chart_width ~height:10;
        Charts.render_latency_chart samples ~width:chart_width ~height:10;
        Charts.render_key_to_render_chart samples ~width:chart_width ~height:10;
        Charts.render_summary_bars samples ~width:chart_width ~height:8;
      ]
    in
    List.concat_map (fun c -> String.split_on_char '\n' c @ [""]) charts

let render_scheduler_content () =
  let scheduler_snapshots = Metrics.get_scheduler_snapshots () in
  if scheduler_snapshots = [] then
    [Widgets.themed_muted "No scheduler metrics yet"]
  else
    List.map
      (fun (name, (snap : Metrics.snapshot)) ->
        let avg =
          if snap.count > 0 then snap.sum /. float_of_int snap.count else 0.
        in
        let p50_str =
          match snap.p50 with Some v -> Printf.sprintf "%.1f" v | None -> "-"
        in
        let p90_str =
          match snap.p90 with Some v -> Printf.sprintf "%.1f" v | None -> "-"
        in
        let p99_str =
          match snap.p99 with Some v -> Printf.sprintf "%.1f" v | None -> "-"
        in
        let status_dot =
          match snap.p90 with
          | Some v when v > 100. -> Widgets.themed_error "●"
          | Some v when v > 50. -> Widgets.themed_warning "●"
          | _ -> Widgets.themed_success "●"
        in
        Printf.sprintf
          "%s %-16s  %s avg:%.1fms  p50:%s  p90:%s  p99:%s  (n=%d)"
          status_dot
          name
          (Widgets.themed_muted "|")
          avg
          p50_str
          p90_str
          p99_str
          snap.count)
      scheduler_snapshots

let render_worker_stats_content () =
  let format_stats (stats : Worker_queue.stats) =
    let dedup_pct =
      if stats.requests_total > 0 then
        100.0
        *. float_of_int stats.requests_deduped
        /. float_of_int stats.requests_total
      else 0.0
    in
    let status_dot =
      if stats.p90_ms > 100. then Widgets.themed_error "●"
      else if stats.p90_ms > 50. then Widgets.themed_warning "●"
      else Widgets.themed_success "●"
    in
    let main_line =
      Printf.sprintf
        "%s %-16s  %s reqs:%d dedup:%d(%.0f%%)  p50:%.1fms p90:%.1fms \
         p95:%.1fms p99:%.1fms"
        status_dot
        stats.name
        (Widgets.themed_muted "|")
        stats.requests_total
        stats.requests_deduped
        dedup_pct
        stats.p50_ms
        stats.p90_ms
        stats.p95_ms
        stats.p99_ms
    in
    let top_keys = List.filteri (fun i _ -> i < 5) stats.deduped_by_key in
    let key_lines =
      List.map
        (fun (kd : Worker_queue.key_dedup) ->
          Printf.sprintf
            "    └─ %s %s"
            (Widgets.themed_muted (Printf.sprintf "%5d×" kd.count))
            kd.key)
        top_keys
    in
    main_line :: key_lines
  in
  format_stats (System_metrics_scheduler.get_worker_stats ())
  @ format_stats (Rpc_scheduler.get_worker_stats ())

let render_metrics_server_content () =
  let metrics_enabled = Metrics.is_enabled () in
  let status_icon =
    if metrics_enabled then Widgets.themed_success "●"
    else Widgets.themed_muted "○"
  in
  let status_text =
    if metrics_enabled then Widgets.themed_success "enabled"
    else Widgets.themed_muted "disabled"
  in
  let status_line =
    Printf.sprintf
      "%s %s %s"
      (Widgets.themed_emphasis "Status:")
      status_icon
      status_text
  in
  match Metrics.get_server_info () with
  | Some (addr, port) ->
      [
        status_line;
        Printf.sprintf
          "%s %s"
          (Widgets.themed_emphasis "Endpoint:")
          (Widgets.themed_accent
             (Printf.sprintf "http://%s:%d/metrics" addr port));
        Widgets.themed_muted "(server is running)";
      ]
  | None ->
      [
        status_line;
        Printf.sprintf
          "%s %s"
          (Widgets.themed_emphasis "Address:")
          !metrics_addr_ref;
        Widgets.themed_muted "('m' to start, 'a' to edit address)";
      ]

let render_system_info_content () =
  [
    Printf.sprintf
      "Privilege: %s"
      (if Paths.is_root () then Widgets.themed_error "● SYSTEM"
       else Widgets.themed_success "● USER");
  ]

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  Metrics.record_render ~page:name (fun () ->
      let box_width = min 78 (size.LTerm_geom.cols - 2) in
      let chart_width = min 70 (box_width - 6) in
      let render_box ~title ~widget_name content_lines =
        let content = String.concat "\n" content_lines in
        let box =
          Style_context.with_child_context ~widget_name (fun () ->
              Box.render ~title ~style:Single ~width:box_width content)
        in
        String.split_on_char '\n' box @ [""]
      in

      (* Helper to render a flex row and convert to lines *)
      let render_flex_row items =
        let row = Flex.create ~direction:Row ~gap:{h = 2; v = 0} items in
        let rendered =
          Flex.render row ~size:{LTerm_geom.rows = 8; cols = box_width}
        in
        String.split_on_char '\n' rendered @ [""]
      in

      (* Build all boxes using functional approach *)
      let boxes =
        [
          render_box
            ~title:"Service Status"
            ~widget_name:"diagnostics-service-status"
            (render_services_content s.services);
          render_box
            ~title:"Caches"
            ~widget_name:"diagnostics-caches"
            (render_caches_content ());
        ]
        @ [
            (* Real-Time Metrics + Metrics Recorder side-by-side *)
            render_flex_row
              [
                {
                  render =
                    (fun ~size ->
                      let content =
                        String.concat
                          "\n"
                          (render_realtime_content s.bg_queue_spark)
                      in
                      Style_context.with_child_context
                        ~widget_name:"diagnostics-realtime"
                        (fun () ->
                          Box.render
                            ~title:"Real-Time Metrics"
                            ~style:Single
                            ~width:size.LTerm_geom.cols
                            content));
                  basis = Fill;
                  cross = None;
                };
                {
                  render =
                    (fun ~size ->
                      let content =
                        String.concat "\n" (render_recorder_content ())
                      in
                      Style_context.with_child_context
                        ~widget_name:"diagnostics-recorder"
                        (fun () ->
                          Box.render
                            ~title:"Metrics Recorder"
                            ~style:Single
                            ~width:size.LTerm_geom.cols
                            content));
                  basis = Fill;
                  cross = None;
                };
              ];
          ]
        @ [
            render_box
              ~title:"Historical Metrics"
              ~widget_name:"diagnostics-historical"
              (render_historical_content ~chart_width);
          ]
        @ [
            render_box
              ~title:"Scheduler Performance"
              ~widget_name:"diagnostics-scheduler"
              (render_scheduler_content ());
            render_box
              ~title:"Worker Queue Stats"
              ~widget_name:"diagnostics-worker-queue"
              (render_worker_stats_content ());
          ]
        @ [
            (* Metrics Server + System Information side-by-side *)
            render_flex_row
              [
                {
                  render =
                    (fun ~size ->
                      let content =
                        String.concat "\n" (render_metrics_server_content ())
                      in
                      Style_context.with_child_context
                        ~widget_name:"diagnostics-metrics-server"
                        (fun () ->
                          Box.render
                            ~title:"Metrics Server"
                            ~style:Single
                            ~width:size.LTerm_geom.cols
                            content));
                  basis = Fill;
                  cross = None;
                };
                {
                  render =
                    (fun ~size ->
                      let content =
                        String.concat "\n" (render_system_info_content ())
                      in
                      Style_context.with_child_context
                        ~widget_name:"diagnostics-system-info"
                        (fun () ->
                          Box.render
                            ~title:"System Information"
                            ~style:Single
                            ~width:size.LTerm_geom.cols
                            content));
                  basis = Fill;
                  cross = None;
                };
              ];
          ]
      in

      let all_lines = List.concat boxes in
      let content_height = List.length all_lines in
      content_height_ref := content_height ;

      (* Apply scrolling *)
      let visible_height = size.LTerm_geom.rows - 4 in
      let visible_lines =
        all_lines |> fun l ->
        if List.length l <= visible_height then l
        else
          let start =
            min s.scroll_offset (max 0 (List.length l - visible_height))
          in
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
          drop start l |> take visible_height []
      in

      let canvas_header = render_canvas_header ~width:box_width in

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
      (* Split canvas into individual lines so render_layout allocates the
         correct row count (canvas is 5 rows; List.length must reflect that). *)
      let canvas_lines = String.split_on_char '\n' canvas_header in
      let header = canvas_lines @ [Widgets.themed_muted scroll_indicator] in

      Themed_page.render_layout ~size ~header ~footer:[] ~child:(fun _ -> body))

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size =
  Metrics.mark_input_event () ;
  let s = ps.Navigation.s in
  let s = update_content_height s in
  (* Update with latest height from view *)
  let s = {s with last_visible_height = size.LTerm_geom.rows - 4} in
  let ps = Navigation.update (fun _ -> s) ps in
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some Keys.Escape | Some (Keys.Char "q") -> Navigation.back ps
    | Some (Keys.Char "r") -> refresh ps
    | Some (Keys.Char "m") -> Navigation.update toggle_metrics ps
    | Some (Keys.Char "a") -> Navigation.update edit_metrics_addr ps
    | Some (Keys.Char "R") -> Navigation.update toggle_recorder ps
    | Some (Keys.Char "d") -> Navigation.update change_duration ps
    | Some (Keys.Char "c") -> Navigation.update clear_caches ps
    | Some Keys.Up -> Navigation.update scroll_up ps
    | Some Keys.Down -> Navigation.update scroll_down ps
    | Some (Keys.Char "k") -> Navigation.update scroll_up ps
    | Some (Keys.Char "j") -> Navigation.update scroll_down ps
    | _ -> ps

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page : Miaou.Core.Tui_page.PAGE_SIG = struct
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

  let key_hints _ps =
    Miaou.Core.Tui_page.
      [
        {key = "Esc"; help = "Back"};
        {key = "r"; help = "Refresh"};
        {key = "m"; help = "Toggle metrics"};
        {key = "a"; help = "Edit address"};
        {key = "R"; help = "Toggle recorder"};
        {key = "d"; help = "Change duration"};
        {key = "c"; help = "Clear caches"};
        {key = "↑/↓"; help = "Navigate"};
        {key = "?"; help = "Help"};
      ]

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
