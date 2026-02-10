(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module C = Miaou_canvas.Canvas
open Octez_manager_lib

let name = "topology"

type state = {services : Data.Service_state.t list}

type msg = unit

type pstate = state Navigation.t

let init () = Navigation.make {services = Data.load_service_states ()}

let update ps _ = ps

let refresh ps =
  match Context.consume_navigation () with
  | Some (Context.Goto p) -> Navigation.goto p ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      if Context.consume_instances_dirty () then
        Navigation.update
          (fun _s -> {services = Data.load_service_states ()})
          ps
      else ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps = Navigation.back ps

let handled_keys () = Miaou.Core.Keys.[Escape]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "Esc" "Back"; kb "?" "Help"]

(* Build tree structure: roots are services with no depends_on,
   children are those that depend on a root *)
type tree_node = {
  svc : Service.t;
  status : Data.Service_state.status;
  children : tree_node list;
}

let build_tree services =
  let svc_map =
    List.map
      (fun (st : Data.Service_state.t) -> (st.service.Service.instance, st))
      services
  in
  (* Find roots: services with no depends_on or whose parent isn't in our list *)
  let roots =
    List.filter
      (fun (st : Data.Service_state.t) ->
        match st.service.Service.depends_on with
        | None -> true
        | Some parent -> not (List.mem_assoc parent svc_map))
      services
  in
  let rec build_children parent_instance =
    let children =
      List.filter
        (fun (st : Data.Service_state.t) ->
          st.service.Service.depends_on = Some parent_instance)
        services
    in
    List.map
      (fun (st : Data.Service_state.t) ->
        {
          svc = st.service;
          status = st.Data.Service_state.status;
          children = build_children st.service.Service.instance;
        })
      children
  in
  List.map
    (fun (st : Data.Service_state.t) ->
      {
        svc = st.service;
        status = st.Data.Service_state.status;
        children = build_children st.service.Service.instance;
      })
    roots

let role_color = function
  | "node" -> 14
  | "baker" -> 12
  | "accuser" -> 6
  | "dal-node" -> 13
  | _ -> 8

let status_color = function
  | Data.Service_state.Running -> 10
  | Data.Service_state.Stopped -> 8
  | Data.Service_state.Unknown _ -> 9

let style_of fg = {C.default_style with fg}

let bold_of fg = {C.default_style with fg; bold = true}

let dim_style = {C.default_style with dim = true}

(* Draw a service node box on a canvas *)
let draw_node c ~node_w ~row ~col node =
  let canvas_w = C.cols c in
  let w = min node_w (canvas_w - col) in
  if w >= 6 then begin
    let border_color = role_color node.svc.Service.role in
    C.draw_box
      c
      ~row
      ~col
      ~width:w
      ~height:3
      ~border:Rounded
      ~style:(style_of border_color) ;
    let status_char =
      match node.status with
      | Data.Service_state.Running -> "●"
      | Data.Service_state.Stopped -> "○"
      | Data.Service_state.Unknown _ -> "?"
    in
    C.draw_text
      c
      ~row:(row + 1)
      ~col:(col + 1)
      ~style:(bold_of (status_color node.status))
      status_char ;
    let name = node.svc.Service.instance in
    let max_len = w - 4 in
    let display =
      if String.length name > max_len then String.sub name 0 (max_len - 1) ^ "…"
      else name
    in
    C.draw_text
      c
      ~row:(row + 1)
      ~col:(col + 3)
      ~style:(bold_of border_color)
      display
  end

(* Draw a vertical connection line between parent and child *)
let draw_vconnection c ~node_w ~parent_row ~parent_col ~child_row ~child_col =
  let mid_col = parent_col + (node_w / 2) in
  let child_mid = child_col + (node_w / 2) in
  let line_start = parent_row + 3 in
  let line_end = child_row - 1 in
  for r = line_start to line_end do
    C.set_char c ~row:r ~col:mid_col ~char:"│" ~style:dim_style
  done ;
  if mid_col <> child_mid then begin
    let min_c = min mid_col child_mid in
    let max_c = max mid_col child_mid in
    let h_row = line_start + ((line_end - line_start) / 2) in
    for cc = min_c to max_c do
      C.set_char c ~row:h_row ~col:cc ~char:"─" ~style:dim_style
    done ;
    if mid_col < child_mid then begin
      C.set_char c ~row:h_row ~col:mid_col ~char:"└" ~style:dim_style ;
      C.set_char c ~row:h_row ~col:child_mid ~char:"┐" ~style:dim_style
    end
    else begin
      C.set_char c ~row:h_row ~col:mid_col ~char:"┘" ~style:dim_style ;
      C.set_char c ~row:h_row ~col:child_mid ~char:"┌" ~style:dim_style
    end ;
    for r = h_row + 1 to line_end do
      C.set_char c ~row:r ~col:child_mid ~char:"│" ~style:dim_style
    done
  end

(* Wide layout: roots side-by-side, children below *)
let render_wide ~width ~node_w trees =
  let node_h = 3 in
  let h_gap = 2 in
  let v_gap = 3 in
  let slot_w = node_w + h_gap in
  let root_widths =
    List.map (fun root -> max 1 (List.length root.children) * slot_w) trees
  in
  let total_needed = List.fold_left ( + ) 0 root_widths in
  let canvas_w = max width (total_needed + 2) in
  let has_children = List.exists (fun root -> root.children <> []) trees in
  let depth = if has_children then 2 else 1 in
  let canvas_h = (depth * (node_h + v_gap)) + 1 in
  let c = C.create ~rows:canvas_h ~cols:canvas_w in
  let total_used = List.fold_left ( + ) 0 root_widths in
  let start_offset = max 0 ((canvas_w - total_used) / 2) in
  let _cursor =
    List.fold_left2
      (fun region_start root region_w ->
        let num_ch = List.length root.children in
        let child_row = node_h + v_gap in
        let child_cols =
          if num_ch = 0 then []
          else
            List.init num_ch (fun j ->
                region_start + (j * slot_w) + ((slot_w - node_w) / 2))
        in
        let root_col =
          if num_ch = 0 then region_start + ((region_w - node_w) / 2)
          else
            let first_c = List.hd child_cols in
            let last_c = List.nth child_cols (num_ch - 1) in
            (first_c + last_c) / 2
        in
        let root_col = max 0 (min root_col (canvas_w - node_w)) in
        draw_node c ~node_w ~row:0 ~col:root_col root ;
        List.iteri
          (fun j child ->
            let child_col = List.nth child_cols j in
            let child_col = max 0 (min child_col (canvas_w - node_w)) in
            draw_node c ~node_w ~row:child_row ~col:child_col child ;
            draw_vconnection
              c
              ~node_w
              ~parent_row:0
              ~parent_col:root_col
              ~child_row
              ~child_col)
          root.children ;
        region_start + region_w)
      start_offset
      trees
      root_widths
  in
  C.to_ansi c

(* Compact layout: vertical stack, each root with children indented below *)
let render_compact ~width ~node_w trees =
  let node_h = 3 in
  let v_gap = 1 in
  let indent = 4 in
  (* Calculate total height *)
  let total_h =
    List.fold_left
      (fun acc root ->
        let children_h = List.length root.children * (node_h + v_gap) in
        let group_h =
          node_h + if children_h > 0 then v_gap + children_h else 0
        in
        acc + group_h + 1)
      0
      trees
  in
  let canvas_h = max 1 total_h in
  let canvas_w = width in
  let c = C.create ~rows:canvas_h ~cols:canvas_w in
  let child_node_w = min node_w (canvas_w - indent - 1) in
  let _row =
    List.fold_left
      (fun row root ->
        draw_node c ~node_w:(min node_w (canvas_w - 1)) ~row ~col:0 root ;
        let row = row + node_h in
        let row =
          if root.children = [] then row
          else
            let row = row + v_gap in
            List.fold_left
              (fun row child ->
                (* Draw connector: vertical + horizontal to child *)
                let conn_col = node_w / 2 in
                let conn_col = min conn_col (indent - 1) in
                for r = row to row do
                  C.set_char c ~row:r ~col:conn_col ~char:"├" ~style:dim_style ;
                  for cc = conn_col + 1 to indent - 1 do
                    C.set_char c ~row:r ~col:cc ~char:"─" ~style:dim_style
                  done
                done ;
                draw_node c ~node_w:child_node_w ~row ~col:indent child ;
                row + node_h + v_gap)
              row
              root.children
        in
        row + 1)
      0
      trees
  in
  C.to_ansi c

(* Render the topology, choosing layout based on available width *)
let render_topology ~width ~services =
  let trees = build_tree services in
  if trees = [] then Widgets.dim "No services to display"
  else
    (* Adaptive node width: shrink for narrow terminals *)
    let node_w = max 14 (min 22 ((width - 4) / max 1 (List.length trees))) in
    let h_gap = 2 in
    let slot_w = node_w + h_gap in
    (* Check if wide layout fits *)
    let wide_needed =
      List.fold_left
        (fun acc root -> acc + (max 1 (List.length root.children) * slot_w))
        0
        trees
    in
    if wide_needed <= width then render_wide ~width ~node_w trees
    else render_compact ~width ~node_w trees

let header = [Widgets.title_highlight " Network Topology "]

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let box_width = min 120 (size.LTerm_geom.cols - 2) in
  let body = render_topology ~width:box_width ~services:s.services in
  Vsection.render ~size ~header ~content_footer:[] ~child:(fun _ -> body)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some Keys.Escape | Some (Keys.Char "q") -> Navigation.back ps
    | _ -> ps

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

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

  let key_hints _ps =
    Miaou.Core.Tui_page.
      [{key = "Esc"; help = "Back"}; {key = "?"; help = "Help"}]

  let has_modal = has_modal
end

module Page =
  Monitored_page.Make
    (Page_Impl)
    (struct
      let page_name = "topology"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
