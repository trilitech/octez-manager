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
  last_visible_height : int; (* Track terminal height *)
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
    last_visible_height = 20;
    next_page = None;
  }

let update s _ = s

let refresh s =
  (* Update sparklines with current metrics *)
  let bg_depth = Metrics.get_bg_queue_depth () in
  Sparkline.push s.bg_queue_spark (float_of_int bg_depth) ;
  {s with services = Data.load_service_states ()}

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

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ =
  (* Auto-refresh sparkline and services *)
  let bg_depth = Metrics.get_bg_queue_depth () in
  Sparkline.push s.bg_queue_spark (float_of_int bg_depth) ;
  {s with services = Data.load_service_states ()}

let back s = {s with next_page = Some "__BACK__"}

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
      Context.toast_info (Printf.sprintf "Recording duration set to %s" label)) ;
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
  [
    ("Esc", back, "Back");
    ("r", refresh, "Refresh");
    ("m", toggle_metrics, "Toggle metrics");
    ("a", edit_metrics_addr, "Edit address");
    ("R", toggle_recorder, "Toggle recorder");
    ("d", change_duration, "Change duration");
    ("c", clear_caches, "Clear caches");
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
      "↑/↓: scroll  r: refresh  c: clear caches  m: metrics  R: recorder  Esc: \
       back";
  ]

let view s ~focus:_ ~size =
  Metrics.record_render ~page:name (fun () ->
      let lines = ref [] in
      let add line = lines := line :: !lines in

      (* Service Status Section *)
      add (Widgets.fg 14 (Widgets.bold "━━━ Service Status ━━━")) ;
      add "" ;
      if s.services = [] then add (Widgets.dim "  No services registered")
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
                (Widgets.fg 8 svc.Service.role)
                (Widgets.dim
                   (Printf.sprintf
                      "net:%s mode:%s"
                      svc.Service.network
                      (History_mode.to_string svc.Service.history_mode)))
            in
            add line)
          s.services ;

      add "" ;
      add (Widgets.fg 13 (Widgets.bold "━━━ Caches ━━━")) ;
      add "" ;
      let cache_stats = Cache.get_stats () in
      if cache_stats = [] then add (Widgets.dim "  No caches registered")
      else
        List.iter
          (fun (name, hits, misses, age, ttl, expired, sub_entries) ->
            let age_str =
              match age with
              | None -> Widgets.dim "empty"
              | Some a ->
                  let s = Printf.sprintf "%.1fs/%.1fs" a ttl in
                  if expired then Widgets.red s else Widgets.green s
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
            add
              (Printf.sprintf
                 "  %-20s  %s%s%s"
                 name
                 age_str
                 count_str
                 (Widgets.dim stats_str)) ;
            (* Show sub-entries for keyed caches *)
            List.iter
              (fun (entry : Cache.sub_entry) ->
                let sub_age_str =
                  let s = Printf.sprintf "%.1fs" entry.age in
                  if entry.expired then Widgets.red s else Widgets.green s
                in
                add (Printf.sprintf "    └─ %-16s  %s" entry.key sub_age_str))
              sub_entries)
          cache_stats ;
      add (Widgets.dim "  (press 'c' to clear all caches)") ;

      add "" ;
      add (Widgets.fg 12 (Widgets.bold "━━━ Real-Time Metrics ━━━")) ;
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
      add (Widgets.fg 11 (Widgets.bold "━━━ Metrics Recorder ━━━")) ;
      add "" ;
      let recorder_enabled = Metrics.is_recording () in
      let recorder_icon =
        if recorder_enabled then Widgets.fg 10 "●" else Widgets.fg 8 "○"
      in
      let recorder_status =
        if recorder_enabled then Widgets.fg 10 "recording"
        else Widgets.fg 8 "stopped"
      in
      let duration_samples = Metrics.get_recording_duration () in
      let duration_str =
        match duration_samples with
        | 12 -> "1m"
        | 60 -> "5m"
        | 180 -> "15m"
        | n -> Printf.sprintf "%ds" (n * 5)
      in
      add
        (Printf.sprintf
           "  %s %s %s %s"
           (Widgets.fg 12 "Status:")
           recorder_icon
           recorder_status
           (Widgets.dim
              (Printf.sprintf
                 "(duration: %s, press 'd' to change)"
                 duration_str))) ;
      add (Widgets.dim "  (press 'R' to start/stop recording)") ;

      (* Historical Charts *)
      if recorder_enabled || Metrics.get_snapshots () <> [] then (
        add "" ;
        add (Widgets.fg 13 (Widgets.bold "━━━ Historical Metrics ━━━")) ;
        add "" ;
        let samples = Metrics.get_snapshots () in

        if samples = [] then (
          add (Widgets.dim "  Collecting data... (wait ~5 seconds)") ;
          add (Widgets.dim "  Charts will appear once samples are recorded"))
        else
          let chart_width = min 70 (size.LTerm_geom.cols - 4) in

          (* BG Queue Chart *)
          let bg_chart =
            Charts.render_bg_queue_chart samples ~width:chart_width ~height:10
          in
          String.split_on_char '\n' bg_chart |> List.iter add ;
          add "" ;

          (* Service Status Chart *)
          let svc_chart =
            Charts.render_service_status_chart
              samples
              ~width:chart_width
              ~height:10
          in
          String.split_on_char '\n' svc_chart |> List.iter add ;
          add "" ;

          (* Render Latency Chart *)
          let render_chart =
            Charts.render_latency_chart samples ~width:chart_width ~height:10
          in
          String.split_on_char '\n' render_chart |> List.iter add ;
          add "" ;

          (* Key-to-Render Chart *)
          let key_chart =
            Charts.render_key_to_render_chart
              samples
              ~width:chart_width
              ~height:10
          in
          String.split_on_char '\n' key_chart |> List.iter add ;
          add "" ;

          (* Summary *)
          let summary =
            Charts.render_summary_bars samples ~width:chart_width ~height:8
          in
          String.split_on_char '\n' summary |> List.iter add) ;

      (* Scheduler Metrics Section *)
      add "" ;
      add (Widgets.fg 11 (Widgets.bold "━━━ Scheduler Performance ━━━")) ;
      add "" ;
      let scheduler_snapshots = Metrics.get_scheduler_snapshots () in
      if scheduler_snapshots = [] then
        add (Widgets.dim "  No scheduler metrics recorded yet")
      else
        List.iter
          (fun (name, (snap : Metrics.snapshot)) ->
            let avg =
              if snap.count > 0 then snap.sum /. float_of_int snap.count else 0.
            in
            let p50_str =
              match snap.p50 with
              | Some v -> Printf.sprintf "%.1f" v
              | None -> "-"
            in
            let p90_str =
              match snap.p90 with
              | Some v -> Printf.sprintf "%.1f" v
              | None -> "-"
            in
            let p99_str =
              match snap.p99 with
              | Some v -> Printf.sprintf "%.1f" v
              | None -> "-"
            in
            let color =
              match snap.p90 with
              | Some v when v > 100. -> 9 (* red if p90 > 100ms *)
              | Some v when v > 50. -> 11 (* yellow if p90 > 50ms *)
              | _ -> 10 (* green *)
            in
            add
              (Printf.sprintf
                 "  %s %-16s  %s avg:%.1fms  p50:%s  p90:%s  p99:%s  (n=%d)"
                 (Widgets.fg color "●")
                 name
                 (Widgets.dim "|")
                 avg
                 p50_str
                 p90_str
                 p99_str
                 snap.count))
          scheduler_snapshots ;

      (* Worker Queue Stats Section *)
      add "" ;
      add (Widgets.fg 12 (Widgets.bold "━━━ Worker Queue Stats ━━━")) ;
      add "" ;
      let format_stats (stats : Worker_queue.stats) =
        let dedup_pct =
          if stats.requests_total > 0 then
            100.0
            *. float_of_int stats.requests_deduped
            /. float_of_int stats.requests_total
          else 0.0
        in
        let color =
          if stats.p90_ms > 100. then 9 (* red if p90 > 100ms *)
          else if stats.p90_ms > 50. then 11 (* yellow if p90 > 50ms *)
          else 10 (* green *)
        in
        add
          (Printf.sprintf
             "  %s %-16s  %s reqs:%d dedup:%d(%.0f%%)  p50:%.1fms p90:%.1fms \
              p95:%.1fms p99:%.1fms"
             (Widgets.fg color "●")
             stats.name
             (Widgets.dim "|")
             stats.requests_total
             stats.requests_deduped
             dedup_pct
             stats.p50_ms
             stats.p90_ms
             stats.p95_ms
             stats.p99_ms) ;
        (* Show top deduped keys if any *)
        let top_keys = List.filteri (fun i _ -> i < 5) stats.deduped_by_key in
        List.iter
          (fun (kd : Worker_queue.key_dedup) ->
            add
              (Printf.sprintf
                 "      └─ %s %s"
                 (Widgets.dim (Printf.sprintf "%5d×" kd.count))
                 kd.key))
          top_keys
      in
      format_stats (System_metrics_scheduler.get_worker_stats ()) ;
      format_stats (Rpc_scheduler.get_worker_stats ()) ;

      add "" ;
      add (Widgets.fg 14 (Widgets.bold "━━━ Metrics Server Configuration ━━━")) ;
      add "" ;
      let metrics_enabled = Metrics.is_enabled () in
      let status_icon =
        if metrics_enabled then Widgets.fg 10 "●" else Widgets.fg 8 "○"
      in
      let status_text =
        if metrics_enabled then Widgets.fg 10 "enabled"
        else Widgets.fg 8 "disabled"
      in
      add
        (Printf.sprintf
           "  %s %s %s"
           (Widgets.fg 12 "Status:")
           status_icon
           status_text) ;
      (match Metrics.get_server_info () with
      | Some (addr, port) ->
          add
            (Printf.sprintf
               "  %s %s"
               (Widgets.fg 12 "Endpoint:")
               (Widgets.fg 14 (Printf.sprintf "http://%s:%d/metrics" addr port))) ;
          add (Widgets.dim "  (server is running)")
      | None ->
          add
            (Printf.sprintf
               "  %s %s"
               (Widgets.fg 12 "Address:")
               !metrics_addr_ref) ;
          add (Widgets.dim "  (press 'm' to start, 'a' to edit address)")) ;

      add "" ;
      add (Widgets.fg 12 (Widgets.bold "━━━ System Information ━━━")) ;
      add "" ;
      add
        (Printf.sprintf
           "  Privilege: %s"
           (if Common.is_root () then Widgets.red "● SYSTEM"
            else Widgets.green "● USER")) ;

      let all_lines = List.rev !lines in
      let content_height = List.length all_lines in
      content_height_ref := content_height ;

      (* Store for next state update *)

      (* Apply scrolling *)
      let visible_height = size.LTerm_geom.rows - 4 in
      (* account for header/footer *)
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
      let header_with_scroll = header @ [Widgets.dim scroll_indicator] in

      Miaou_widgets_layout.Vsection.render
        ~size
        ~header:header_with_scroll
        ~footer
        ~child:(fun _ -> body))

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  s

let handle_key s key ~size =
  Metrics.mark_input_event () ;
  let s = update_content_height s in
  (* Update with latest height from view *)
  let s = {s with last_visible_height = size.LTerm_geom.rows - 4} in
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
    | Some (Keys.Char "c") -> clear_caches s
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

  let handled_keys = handled_keys

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
