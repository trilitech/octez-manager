(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Chart widgets for the diagnostics page.

    Renders braille line charts, sparklines, and summary bars from
    metrics snapshots. *)

(** Strip trailing whitespace from chart output lines. *)
val trim_chart_padding : string -> string

(** Render a braille line chart of background queue depth over time. *)
val render_bg_queue_chart :
  Metrics.metrics_snapshot list -> width:int -> height:int -> string

(** Render a braille line chart of service status counts over time. *)
val render_service_status_chart :
  Metrics.metrics_snapshot list -> width:int -> height:int -> string

(** Render a braille line chart of RPC polling latency over time. *)
val render_latency_chart :
  Metrics.metrics_snapshot list -> width:int -> height:int -> string

(** Render a braille line chart of key-to-render time over time. *)
val render_key_to_render_chart :
  Metrics.metrics_snapshot list -> width:int -> height:int -> string

(** Render horizontal summary bars showing queue depth, latency,
    and render performance. *)
val render_summary_bars :
  Metrics.metrics_snapshot list -> width:int -> height:int -> string

(** Render an inline sparkline of background queue depth. *)
val render_bg_queue_sparkline :
  Miaou_widgets_display.Sparkline_widget.t -> string
