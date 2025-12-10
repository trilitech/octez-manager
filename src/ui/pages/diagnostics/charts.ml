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
        (fun i (s : Metrics_recorder.sample) ->
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
        (fun i (s : Metrics_recorder.sample) ->
          Line_chart.{x = float_of_int i; y = float_of_int s.services_active; color = Some "10"})
        samples
    in
    let total_points =
      List.mapi
        (fun i (s : Metrics_recorder.sample) ->
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

let render_summary_bars stats ~width ~height =
  match stats with
  | None -> Printf.sprintf "Summary (%dx%d)\n[No data available]" width height
  | Some (s : Metrics_recorder.stats) ->
      let data = [
        ("Max BG", float_of_int s.max_bg_queue, Some "11");
        ("Avg BG", s.avg_bg_queue, Some "12");
        ("Active Svc", float_of_int s.current_services_active, Some "10");
      ] in
      let chart =
        Bar_chart.create
          ~width
          ~height
          ~data
          ~title:"Metrics Summary"
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
