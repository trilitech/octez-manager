(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Chart widgets for diagnostics page *)

module Line_chart = Miaou_widgets_display.Line_chart_widget
module Bar_chart = Miaou_widgets_display.Bar_chart_widget
module Sparkline = Miaou_widgets_display.Sparkline_widget

let render_bg_queue_chart samples ~width ~height =
  if samples = [] then
    Printf.sprintf "Background Queue Depth (%dx%d)\n[No data available]" width height
  else
    let points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          Line_chart.{x = float_of_int i; y = float_of_int s.bg_queue_depth; color = None})
        samples
    in
    let series = [Line_chart.{label = "Queue Depth"; points; color = Some "12"}] in
    let chart =
      Line_chart.create
        ~width
        ~height
        ~series
        ~title:"Background Queue Depth Over Time"
        ()
    in
    Line_chart.render chart ~show_axes:true ~show_grid:false ()

let render_service_status_chart samples ~width ~height =
  if samples = [] then
    Printf.sprintf "Service Status (%dx%d)\n[No data available]" width height
  else
    let active_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          Line_chart.{x = float_of_int i; y = float_of_int s.services_active; color = Some "10"})
        samples
    in
    let total_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          Line_chart.{x = float_of_int i; y = float_of_int s.services_total; color = Some "8"})
        samples
    in
    let series = [
      Line_chart.{label = "Active"; points = active_points; color = Some "10"};
      Line_chart.{label = "Total"; points = total_points; color = Some "8"};
    ] in
    let chart =
      Line_chart.create
        ~width
        ~height
        ~series
        ~title:"Service Status Over Time"
        ()
    in
    Line_chart.render chart ~show_axes:true ~show_grid:false ()

let render_latency_chart samples ~width ~height =
  if samples = [] then
    Printf.sprintf "Render Latency (%dx%d)\n[No data available]" width height
  else
    let p50_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          match s.render_p50 with
          | Some v -> Some Line_chart.{x = float_of_int i; y = v; color = Some "10"}
          | None -> None)
        samples
      |> List.filter_map (fun x -> x)
    in
    let p90_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          match s.render_p90 with
          | Some v -> Some Line_chart.{x = float_of_int i; y = v; color = Some "11"}
          | None -> None)
        samples
      |> List.filter_map (fun x -> x)
    in
    let p99_points =
      List.mapi
        (fun i (s : Metrics.metrics_snapshot) ->
          match s.render_p99 with
          | Some v -> Some Line_chart.{x = float_of_int i; y = v; color = Some "9"}
          | None -> None)
        samples
      |> List.filter_map (fun x -> x)
    in
    if p50_points = [] && p90_points = [] && p99_points = [] then
      Printf.sprintf "Render Latency (%dx%d)\n[No render data collected yet]" width height
    else
      let series =
        [
          Line_chart.{label = "p50"; points = p50_points; color = Some "10"};
          Line_chart.{label = "p90"; points = p90_points; color = Some "11"};
          Line_chart.{label = "p99"; points = p99_points; color = Some "9"};
        ]
        |> List.filter (fun (s : Line_chart.series) -> s.Line_chart.points <> [])
      in
      let chart =
        Line_chart.create
          ~width
          ~height
          ~series
          ~title:"Render Latency (ms) - p50/p90/p99"
          ()
      in
      Line_chart.render chart ~show_axes:true ~show_grid:false ()

let render_summary_bars samples ~width ~height =
  if samples = [] then
    Printf.sprintf "Summary (%dx%d)\n[No data available]" width height
  else
    let last = List.hd (List.rev samples) in
    let data = [
      ("BG Queue", float_of_int last.Metrics.bg_queue_depth, Some "12");
      ("Active Svc", float_of_int last.services_active, Some "10");
      ("Total Svc", float_of_int last.services_total, Some "8");
    ] in
    let data =
      match last.render_p99 with
      | Some p99 -> data @ [("Render p99", p99, Some "9")]
      | None -> data
    in
    let chart =
      Bar_chart.create
        ~width
        ~height
        ~data
        ~title:"Current Metrics"
        ()
    in
    Bar_chart.render chart ~show_values:true ()

let render_bg_queue_sparkline spark =
  if Sparkline.is_empty spark then
    "BG Queue (60s): " ^ "[collecting...]"
  else
    "BG Queue (60s): "
    ^ Sparkline.render_with_label
        spark
        ~label:""
        ~focus:false
        ~thresholds:[{Sparkline.value = 3.0; color = "11"}]
        ()
