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

(* Render the topology as a Canvas *)
let render_topology ~width ~services =
  let trees = build_tree services in
  if trees = [] then Widgets.dim "No services to display"
  else
    (* Calculate layout dimensions *)
    let node_w = 22 in
    let node_h = 3 in
    let h_gap = 4 in
    let v_gap = 3 in
    (* Calculate width needed per root: max of 1 (the root itself) and its
       children count, then multiply by slot width *)
    let slot_w = node_w + h_gap in
    let root_widths =
      List.map (fun root -> max 1 (List.length root.children) * slot_w) trees
    in
    let total_needed = List.fold_left ( + ) 0 root_widths in
    let canvas_w = max width (total_needed + 2) in
    let has_children = List.exists (fun root -> root.children <> []) trees in
    let num_rows = if has_children then 2 else 1 in
    let canvas_h = (num_rows * (node_h + v_gap)) + 1 in
    let c = C.create ~rows:canvas_h ~cols:canvas_w in
    (* Draw a service node box *)
    let draw_node ~row ~col node =
      let w = min node_w (canvas_w - col) in
      if w < 6 then ()
      else begin
        let border_color = role_color node.svc.Service.role in
        C.draw_box
          c
          ~row
          ~col
          ~width:w
          ~height:node_h
          ~border:Rounded
          ~style:(style_of border_color) ;
        (* Status indicator and instance name *)
        let status_char =
          match node.status with
          | Data.Service_state.Running -> "●"
          | Data.Service_state.Stopped -> "○"
          | Data.Service_state.Unknown _ -> "?"
        in
        let sc = status_color node.status in
        C.draw_text
          c
          ~row:(row + 1)
          ~col:(col + 1)
          ~style:(bold_of sc)
          status_char ;
        (* Instance name, truncated to fit *)
        let name = node.svc.Service.instance in
        let max_name_len = w - 4 in
        let display_name =
          if String.length name > max_name_len then
            String.sub name 0 (max_name_len - 1) ^ "…"
          else name
        in
        C.draw_text
          c
          ~row:(row + 1)
          ~col:(col + 3)
          ~style:(bold_of border_color)
          display_name
      end
    in
    (* Draw connecting line from parent to child *)
    let draw_connection ~parent_row ~parent_col ~child_row ~child_col =
      let mid_col = parent_col + (node_w / 2) in
      let child_mid = child_col + (node_w / 2) in
      (* Vertical line down from parent *)
      let line_start = parent_row + node_h in
      let line_end = child_row - 1 in
      for r = line_start to line_end do
        C.set_char c ~row:r ~col:mid_col ~char:"│" ~style:dim_style
      done ;
      (* Horizontal line to child if different column *)
      if mid_col <> child_mid then begin
        let min_c = min mid_col child_mid in
        let max_c = max mid_col child_mid in
        let h_row = line_start + ((line_end - line_start) / 2) in
        for cc = min_c to max_c do
          C.set_char c ~row:h_row ~col:cc ~char:"─" ~style:dim_style
        done ;
        (* Corner pieces *)
        if mid_col < child_mid then begin
          C.set_char c ~row:h_row ~col:mid_col ~char:"└" ~style:dim_style ;
          C.set_char c ~row:h_row ~col:child_mid ~char:"┐" ~style:dim_style
        end
        else begin
          C.set_char c ~row:h_row ~col:mid_col ~char:"┘" ~style:dim_style ;
          C.set_char c ~row:h_row ~col:child_mid ~char:"┌" ~style:dim_style
        end ;
        (* Vertical from horizontal to child *)
        for r = h_row + 1 to line_end do
          C.set_char c ~row:r ~col:child_mid ~char:"│" ~style:dim_style
        done
      end
    in
    (* Layout: each root gets a region wide enough for its children.
       Position roots centered above their children. *)
    let total_used = List.fold_left ( + ) 0 root_widths in
    let start_offset = max 0 ((canvas_w - total_used) / 2) in
    let _cursor =
      List.fold_left2
        (fun region_start root region_w ->
          let num_ch = List.length root.children in
          let child_row = node_h + v_gap in
          (* Place children evenly within the region *)
          let child_cols =
            if num_ch = 0 then []
            else
              List.init num_ch (fun j ->
                  region_start + (j * slot_w) + ((slot_w - node_w) / 2))
          in
          (* Center the root above its children, or in the region center *)
          let root_col =
            if num_ch = 0 then region_start + ((region_w - node_w) / 2)
            else
              let first_c = List.hd child_cols in
              let last_c = List.nth child_cols (num_ch - 1) in
              (first_c + last_c) / 2
          in
          let root_col = max 0 (min root_col (canvas_w - node_w)) in
          draw_node ~row:0 ~col:root_col root ;
          (* Draw children and connections *)
          List.iteri
            (fun j child ->
              let child_col = List.nth child_cols j in
              let child_col = max 0 (min child_col (canvas_w - node_w)) in
              draw_node ~row:child_row ~col:child_col child ;
              draw_connection
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
