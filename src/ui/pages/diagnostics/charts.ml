(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Chart widgets for diagnostics page *)

module Line_chart = Miaou_widgets_display.Line_chart_widget
module Bar_chart = Miaou_widgets_display.Bar_chart_widget
module Sparkline = Miaou_widgets_display.Sparkline_widget
module Widgets = Miaou_widgets_display.Widgets
module Style = Miaou_style.Style
module Style_context = Miaou_style.Style_context

let ansi_fg style =
  let resolved = Style.to_resolved style in
  if resolved.r_fg < 0 then None
  else Some (Printf.sprintf "38;5;%d" resolved.r_fg)

(** ANSI color string from theme style for chart thresholds *)
let threshold_color style =
  let resolved = Style.to_resolved style in
  if resolved.r_fg >= 0 then Printf.sprintf "38;5;%d" resolved.r_fg else "32"

(* Helper to trim trailing padding from chart lines *)
let trim_chart_padding chart_str =
  String.split_on_char '\n' chart_str
  |> List.map (fun line ->
      (* Remove trailing spaces and box-drawing characters *)
      let len = String.length line in
      let rec find_end i =
        if i < 0 then 0
        else if i >= len then i
        else
          (* Check for space or UTF-8 box-drawing char (─ = 0xE2 0x94 0x80) *)
          let c = Char.code line.[i] in
          if c = 32 (* space *) then find_end (i - 1)
          else if
            c = 0xE2
            && i + 2 < len
            && Char.code line.[i + 1] = 0x94
            && Char.code line.[i + 2] = 0x80
          then find_end (i - 3) (* skip 3-byte UTF-8 char *)
          else i + 1
      in
      let end_pos = find_end (len - 1) in
      if end_pos <= 0 then "" else String.sub line 0 end_pos)
  |> String.concat "\n"

let render_bg_queue_chart samples ~width ~height =
  if samples = [] then
    Printf.sprintf
      "Background Queue Depth (%dx%d)\n[No data available]"
      width
      height
  else
    let points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          let y = float_of_int s.bg_queue_depth in
          let color =
            if y > 5.0 then ansi_fg (Style_context.warning ())
            else ansi_fg (Style_context.success ())
          in
          (* yellow if busy, green if idle *)
          Line_chart.{x = float_of_int i; y; color})
        samples
    in
    let series =
      [
        Line_chart.
          {
            label = "Queue Depth";
            points;
            color = ansi_fg (Style_context.info ());
          };
      ]
    in
    (* cyan series *)
    let chart =
      Line_chart.create
        ~width
        ~height
        ~series
        ~title:"Background Queue Depth Over Time"
        ()
    in
    let chart_str =
      Line_chart.render
        chart
        ~show_axes:true
        ~show_grid:false
        ~mode:Line_chart.Braille
        ()
    in
    let chart_str = trim_chart_padding chart_str in

    (* Add summary *)
    let depths = List.map (fun s -> s.Metrics.bg_queue_depth) samples in
    let max_depth = List.fold_left max 0 depths in
    let avg_depth =
      (List.fold_left ( + ) 0 depths |> float_of_int)
      /. float_of_int (List.length depths)
    in
    let last_depth = List.hd (List.rev depths) in
    let summary =
      Printf.sprintf
        "%s\n%s %s  %s %s  %s %s"
        chart_str
        (Widgets.themed_emphasis "Current:")
        (Widgets.themed_emphasis (string_of_int last_depth))
        (Widgets.themed_emphasis "Avg:")
        (Printf.sprintf "%.1f" avg_depth)
        (Widgets.themed_emphasis "Max:")
        (Widgets.themed_emphasis (string_of_int max_depth))
    in
    summary

let render_service_status_chart samples ~width ~height =
  if samples = [] then
    Printf.sprintf "Service Status (%dx%d)\n[No data available]" width height
  else
    let active_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          Line_chart.
            {
              x = float_of_int i;
              y = float_of_int s.services_active;
              color = ansi_fg (Style_context.success ());
            }) (* green *)
        samples
    in
    let total_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          Line_chart.
            {
              x = float_of_int i;
              y = float_of_int s.services_total;
              color = ansi_fg (Style_context.text_muted ());
            }) (* gray *)
        samples
    in
    let series =
      [
        Line_chart.
          {
            label = "Active";
            points = active_points;
            color = ansi_fg (Style_context.success ());
          };
        (* green *)
        Line_chart.
          {
            label = "Total";
            points = total_points;
            color = ansi_fg (Style_context.text_muted ());
          };
        (* gray *)
      ]
    in
    let chart =
      Line_chart.create
        ~width
        ~height
        ~series
        ~title:"Service Status Over Time"
        ()
    in
    Line_chart.render
      chart
      ~show_axes:true
      ~show_grid:false
      ~mode:Line_chart.Braille
      ()
    |> trim_chart_padding

let render_latency_chart samples ~width ~height =
  if samples = [] then
    Printf.sprintf "Render Latency (%dx%d)\n[No data available]" width height
  else
    let p50_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          match s.render_p50 with
          | Some v ->
              Some
                Line_chart.
                  {
                    x = float_of_int i;
                    y = v;
                    color = ansi_fg (Style_context.success ());
                  }
              (* green *)
          | None -> None)
        samples
      |> List.filter_map (fun x -> x)
    in
    let p90_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          match s.render_p90 with
          | Some v ->
              Some
                Line_chart.
                  {
                    x = float_of_int i;
                    y = v;
                    color = ansi_fg (Style_context.warning ());
                  }
              (* yellow *)
          | None -> None)
        samples
      |> List.filter_map (fun x -> x)
    in
    let p99_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          match s.render_p99 with
          | Some v ->
              Some
                Line_chart.
                  {
                    x = float_of_int i;
                    y = v;
                    color = ansi_fg (Style_context.error ());
                  }
              (* red *)
          | None -> None)
        samples
      |> List.filter_map (fun x -> x)
    in
    if p50_points = [] && p90_points = [] && p99_points = [] then
      Printf.sprintf
        "Render Latency (%dx%d)\n[No render data collected yet]"
        width
        height
    else
      let series =
        [
          Line_chart.
            {
              label = "p50";
              points = p50_points;
              color = ansi_fg (Style_context.success ());
            };
          (* green *)
          Line_chart.
            {
              label = "p90";
              points = p90_points;
              color = ansi_fg (Style_context.warning ());
            };
          (* yellow *)
          Line_chart.
            {
              label = "p99";
              points = p99_points;
              color = ansi_fg (Style_context.error ());
            };
          (* red *)
        ]
        |> List.filter (fun (s : Line_chart.series) ->
            s.Line_chart.points <> [])
      in
      let chart =
        Line_chart.create
          ~width
          ~height
          ~series
          ~title:"Render Latency (ms) - p50/p90/p99"
          ()
      in
      let thresholds =
        [
          Line_chart.
            {value = 0.0; color = threshold_color (Style_context.success ())};
          (* Green for fast *)
          Line_chart.
            {value = 16.0; color = threshold_color (Style_context.warning ())};
          (* Yellow for 30fps+ *)
          Line_chart.
            {value = 33.0; color = threshold_color (Style_context.error ())};
          (* Red for slow *)
        ]
      in
      let chart_str =
        Line_chart.render
          chart
          ~show_axes:true
          ~show_grid:false
          ~thresholds
          ~mode:Line_chart.Braille
          ()
      in
      let chart_str = trim_chart_padding chart_str in

      (* Add summary with latest percentiles *)
      let last_sample = List.hd (List.rev samples) in
      let p50_str =
        match last_sample.Metrics.render_p50 with
        | Some v -> Printf.sprintf "%.1fms" v
        | None -> "N/A"
      in
      let p90_str =
        match last_sample.Metrics.render_p90 with
        | Some v -> Printf.sprintf "%.1fms" v
        | None -> "N/A"
      in
      let p99_str =
        match last_sample.Metrics.render_p99 with
        | Some v -> Printf.sprintf "%.1fms" v
        | None -> "N/A"
      in
      Printf.sprintf
        "%s\n%s %s  %s %s  %s %s"
        chart_str
        (Widgets.themed_emphasis "p50:")
        (Widgets.themed_emphasis p50_str)
        (Widgets.themed_emphasis "p90:")
        (Widgets.themed_emphasis p90_str)
        (Widgets.themed_emphasis "p99:")
        (Widgets.themed_emphasis p99_str)

let render_key_to_render_chart samples ~width ~height =
  if samples = [] then
    Printf.sprintf
      "Key-to-Render Latency (%dx%d)\n[No data available]"
      width
      height
  else
    let p50_color = ansi_fg (Style_context.info ()) in
    let p90_color = ansi_fg (Style_context.accent ()) in
    let p50_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          match s.key_to_render_p50 with
          | Some v ->
              Some Line_chart.{x = float_of_int i; y = v; color = p50_color}
          | None -> None)
        samples
      |> List.filter_map (fun x -> x)
    in
    let p90_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          match s.key_to_render_p90 with
          | Some v ->
              Some Line_chart.{x = float_of_int i; y = v; color = p90_color}
          | None -> None)
        samples
      |> List.filter_map (fun x -> x)
    in
    if p50_points = [] && p90_points = [] then
      Printf.sprintf
        "Key-to-Render Latency (%dx%d)\n[No interaction data yet]"
        width
        height
    else
      let series =
        [
          Line_chart.{label = "p50"; points = p50_points; color = p50_color};
          Line_chart.{label = "p90"; points = p90_points; color = p90_color};
        ]
        |> List.filter (fun (s : Line_chart.series) ->
            s.Line_chart.points <> [])
      in
      let chart =
        Line_chart.create
          ~width
          ~height
          ~series
          ~title:"Key-to-Render Latency (ms) - Input Responsiveness"
          ()
      in
      let chart_str =
        Line_chart.render
          chart
          ~show_axes:true
          ~show_grid:false
          ~mode:Line_chart.Braille
          ()
      in
      let chart_str = trim_chart_padding chart_str in

      (* Add summary with latest percentiles *)
      let last_sample = List.hd (List.rev samples) in
      let p50_str =
        match last_sample.Metrics.key_to_render_p50 with
        | Some v -> Printf.sprintf "%.1fms" v
        | None -> "N/A"
      in
      let p90_str =
        match last_sample.Metrics.key_to_render_p90 with
        | Some v -> Printf.sprintf "%.1fms" v
        | None -> "N/A"
      in
      Printf.sprintf
        "%s\n%s %s  %s %s"
        chart_str
        (Widgets.themed_emphasis "p50:")
        (Widgets.themed_emphasis p50_str)
        (Widgets.themed_emphasis "p90:")
        (Widgets.themed_emphasis p90_str)

let render_summary_bars samples ~width ~height =
  if samples = [] then
    Printf.sprintf "Summary (%dx%d)\n[No data available]" width height
  else
    let last = List.hd (List.rev samples) in

    (* Create a clearer summary focusing on key metrics *)
    let header = Widgets.themed_emphasis "━━━ Current Snapshot ━━━" in

    (* Services *)
    let services_line =
      Printf.sprintf
        "  %s %s active / %s total"
        (Widgets.themed_emphasis "Services:")
        (Widgets.themed_emphasis (string_of_int last.Metrics.services_active))
        (string_of_int last.services_total)
    in

    (* BG Queue *)
    let bg_status =
      if last.bg_queue_depth = 0 then Widgets.themed_success "idle"
      else
        Widgets.themed_warning (Printf.sprintf "%d tasks" last.bg_queue_depth)
    in
    let bg_line =
      Printf.sprintf "  %s %s" (Widgets.themed_emphasis "BG Queue:") bg_status
    in

    (* Render Performance *)
    let render_line =
      match last.render_p99 with
      | Some p99 ->
          let value = Printf.sprintf "%.1fms" p99 in
          let styled_value =
            if p99 < 16.0 then Widgets.themed_success value
            else if p99 < 33.0 then Widgets.themed_warning value
            else Widgets.themed_error value
          in
          Some
            (Printf.sprintf
               "  %s %s (p99)"
               (Widgets.themed_emphasis "Render:")
               styled_value)
      | None -> None
    in

    String.concat
      "\n"
      ([header; ""; services_line; bg_line] @ Option.to_list render_line)

let render_bg_queue_sparkline spark =
  if Sparkline.is_empty spark then "BG Queue (60s): " ^ "[collecting...]"
  else
    "BG Queue (60s): "
    ^ Sparkline.render_with_label
        spark
        ~label:""
        ~focus:false
        ~thresholds:
          [
            {
              Sparkline.value = 3.0;
              color = threshold_color (Style_context.warning ());
            };
          ]
        ()
