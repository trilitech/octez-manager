(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
open Octez_manager_lib

let name = "diagnostics"

type state = {
  services : Data.Service_state.t list;
  next_page : string option;
}

type msg = unit

let init () =
  {services = Data.load_service_states (); next_page = None}

let update s _ = s

let refresh s = {s with services = Data.load_service_states ()}

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = s

let back s = {s with next_page = Some "__BACK__"}

let keymap _ = [("Esc", back, "Back"); ("r", refresh, "Refresh")]

(* Helper to render a sparkline from an array of values *)
let sparkline values width =
  if Array.length values = 0 then String.make width ' '
  else
    let min_v = Array.fold_left Float.min Float.infinity values in
    let max_v = Array.fold_left Float.max Float.neg_infinity values in
    let range = max_v -. min_v in
    let bars = [|" "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█"|] in
    let buf = Buffer.create width in
    for i = 0 to min (width - 1) (Array.length values - 1) do
      let v = values.(i) in
      let normalized =
        if range = 0. then 0.5 else (v -. min_v) /. range
      in
      let idx =
        int_of_float (normalized *. float_of_int (Array.length bars - 1))
        |> max 0
        |> min (Array.length bars - 1)
      in
      Buffer.add_string buf bars.(idx)
    done ;
    Buffer.contents buf

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

let footer = [Widgets.dim "r: refresh  Esc: back"]

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
  
  add
    (Printf.sprintf
       "  Background Queue: %d/%d  %s"
       bg_depth
       bg_max
       (if bg_depth > 0 then Widgets.fg 11 "⚠ tasks pending"
        else Widgets.fg 10 "✓ idle")) ;

  (* Render time metrics - would need to expose these from Metrics module *)
  (* For now, show placeholder *)
  add "" ;
  add (Widgets.dim "  Render latency (p50/p90/p99): [needs metrics exposure]") ;
  add (Widgets.dim "  Key→render latency: [needs metrics exposure]") ;

  add "" ;
  add (Widgets.bold "System Information") ;
  add "" ;
  let metrics_enabled = Metrics.is_enabled () in
  add
    (Printf.sprintf
       "  Metrics server: %s"
       (if metrics_enabled then Widgets.fg 10 "enabled"
        else Widgets.fg 8 "disabled")) ;
  (if metrics_enabled then
     match Sys.getenv_opt "OCTEZ_MANAGER_METRICS_ADDR" with
     | Some addr ->
         add (Printf.sprintf "  Metrics endpoint: http://%s/metrics" addr)
     | None -> ()) ;

  add "" ;
  add
    (Widgets.dim
       "  Tip: Enable metrics server with OCTEZ_MANAGER_METRICS_ADDR=0.0.0.0:3010") ;

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
