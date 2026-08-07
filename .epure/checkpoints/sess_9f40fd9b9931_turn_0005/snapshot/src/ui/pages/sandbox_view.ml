(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the sandbox management page.

    LAYOUT RULE: All layouts use Flex_layout / Grid_layout / Box_widget.
    No manual string alignment, no Pane_layout.
    No Eio calls. *)

module Flex = Miaou_widgets_layout.Flex_layout
module T = Themed_text
module C = Miaou_canvas.Canvas
module Style_context = Miaou_style.Style_context
open Octez_manager_lib

(* ─── Types ─────────────────────────────────────────────────────────────── *)

type node_info = {
  svc : Service.t;
  state : Data.Service_state.t option;
  peers : string list;
}

type baker_info = {
  svc : Service.t;
  state : Data.Service_state.t option;
  delegate_count : int;
  baker_ck_aliases : string list;
}

type accuser_info = {svc : Service.t; state : Data.Service_state.t option}

type sandbox_info = {
  group : Group.t;
  nodes : node_info list;
  bakers : baker_info list;
  accusers : accuser_info list;
}

type state = {sandboxes : sandbox_info list; cursor : int}

(* ─── List Panel ────────────────────────────────────────────────────────── *)

let status_badge (st : Data.Service_state.t option) =
  match st with
  | None -> T.muted "?"
  | Some {status = Running; _} -> T.success "●"
  | Some {status = Stopped; _} -> T.error "○"
  | Some _ -> T.muted "?"

let render_create_item ~selected =
  let arrow = if selected then T.warning "▶" else " " in
  let label =
    T.concat [arrow; " "; T.success "+"; "  "; T.text "New sandbox"]
  in
  let hint = T.muted "   Create a sandbox" in
  T.concat [label; "\n"; hint]

let render_list_item ~selected ~(sb : sandbox_info) =
  let node_dot =
    match sb.nodes with
    | first :: _ -> status_badge first.state
    | [] -> T.muted "?"
  in
  let baker_dot =
    match sb.bakers with
    | first :: _ -> status_badge first.state
    | [] -> T.muted "?"
  in
  let arrow = if selected then T.warning "▶" else " " in
  let label =
    T.concat
      [arrow; " "; node_dot; " "; baker_dot; "  "; T.text "%s" sb.group.name]
  in
  T.concat [label]

let render_list ~sandboxes ~cursor ~size =
  let rows_per_item = 2 in
  let create_row =
    {
      Flex.render = (fun ~size:_ -> render_create_item ~selected:(cursor = 0));
      basis = Flex.Px rows_per_item;
      cross = None;
    }
  in
  let sandbox_rows =
    List.mapi
      (fun i sb ->
        let selected = cursor = i + 1 in
        {
          Flex.render = (fun ~size:_ -> render_list_item ~selected ~sb);
          basis = Flex.Px rows_per_item;
          cross = None;
        })
      sandboxes
  in
  Flex.create
    ~direction:Flex.Column
    ~padding:{Flex.left = 1; right = 1; top = 0; bottom = 0}
    (create_row :: sandbox_rows)
  |> fun f -> Flex.render f ~size

(* ─── Topology Panel ────────────────────────────────────────────────────── *)

let port_of addr =
  match String.rindex_opt addr ':' with
  | Some i -> String.sub addr (i + 1) (String.length addr - i - 1)
  | None -> addr

let find_idx pred lst =
  let rec aux i = function
    | [] -> -1
    | x :: rest -> if pred x then i else aux (i + 1) rest
  in
  aux 0 lst

(* ─── Canvas Topology Helpers ───────────────────────────────────────────── *)

let fg_of_style style fallback =
  let resolved = Miaou_style.Style.to_resolved style in
  if resolved.r_fg >= 0 then resolved.r_fg else fallback

let canvas_dim_style = {C.default_style with dim = true}

let canvas_status_style st =
  let color =
    match st with
    | Data.Service_state.Running -> fg_of_style (Style_context.success ()) 10
    | Data.Service_state.Stopped -> fg_of_style (Style_context.text_muted ()) 8
    | Data.Service_state.Unknown _ -> fg_of_style (Style_context.warning ()) 9
  in
  {C.default_style with fg = color; bold = true}

let canvas_status_char = function
  | Data.Service_state.Running -> "●"
  | Data.Service_state.Stopped -> "○"
  | Data.Service_state.Unknown _ -> "?"

let themed_canvas_defaults () =
  let bg_resolved =
    Miaou_style.Style.to_resolved (Style_context.background ())
  in
  let fg_resolved = Miaou_style.Style.to_resolved (Style_context.text ()) in
  (fg_resolved.r_fg, bg_resolved.r_bg)

let draw_compact_box c ~row ~col ~label ~role ~st =
  let box_w = 7 in
  let border_color =
    match role with
    | "node" -> fg_of_style (Style_context.primary ()) 14
    | "accuser" -> fg_of_style (Style_context.warning ()) 11
    | _ -> fg_of_style (Style_context.accent ()) 12
  in
  C.draw_box
    c
    ~row
    ~col
    ~width:box_w
    ~height:3
    ~border:Rounded
    ~style:{C.default_style with fg = border_color} ;
  C.draw_text
    c
    ~row:(row + 1)
    ~col:(col + 1)
    ~style:(canvas_status_style st)
    (canvas_status_char st) ;
  C.draw_text
    c
    ~row:(row + 1)
    ~col:(col + 3)
    ~style:{C.default_style with fg = border_color; bold = true}
    label ;
  col + (box_w / 2)

let render_sandbox_canvas (sb : sandbox_info) ~width =
  let box_w = 7 in
  let h_gap = 3 in
  let slot_w = box_w + h_gap in
  let node_h = 3 in
  let v_gap = 3 in
  let n_nodes = max 1 (List.length sb.nodes) in
  let n_bakers = List.length sb.bakers in
  let n_accusers = List.length sb.accusers in
  let has_bakers = n_bakers > 0 in
  let has_accusers = n_accusers > 0 in
  let canvas_w =
    max
      width
      (max (n_nodes * slot_w) (max (n_bakers * slot_w) (n_accusers * slot_w)))
  in
  let canvas_h =
    node_h
    + (if has_bakers then v_gap + node_h else 0)
    + if has_accusers then v_gap + node_h else 0
  in
  let c = C.create ~rows:canvas_h ~cols:canvas_w in
  let node_centers =
    List.mapi
      (fun i (ni : node_info) ->
        let col = i * slot_w in
        let st =
          match ni.state with
          | Some s -> s.Data.Service_state.status
          | None -> Data.Service_state.Unknown "?"
        in
        let label = Printf.sprintf "N%d" (i + 1) in
        draw_compact_box c ~row:0 ~col ~label ~role:"node" ~st)
      sb.nodes
  in
  List.iteri
    (fun i (ni : node_info) ->
      List.iter
        (fun peer ->
          let peer_port = port_of peer in
          let j =
            find_idx
              (fun (ni2 : node_info) ->
                let p = port_of ni2.svc.net_addr in
                String.equal p peer_port && not (String.equal p ""))
              sb.nodes
          in
          if j >= 0 && j <> i then begin
            let left = min i j in
            let right = max i j in
            if i = right then begin
              let line_start = (left * slot_w) + box_w in
              let line_end = (right * slot_w) - 1 in
              for cc = line_start to line_end do
                C.set_char c ~row:1 ~col:cc ~char:"─" ~style:canvas_dim_style
              done
            end
          end)
        ni.peers)
    sb.nodes ;
  let baker_row = node_h + v_gap in
  List.iteri
    (fun bi (baker : baker_info) ->
      let parent_center =
        match baker.svc.depends_on with
        | None -> (bi mod n_nodes * slot_w) + (box_w / 2)
        | Some parent ->
            let i =
              find_idx
                (fun (ni : node_info) -> String.equal ni.svc.instance parent)
                sb.nodes
            in
            if i >= 0 then List.nth node_centers i
            else (bi mod n_nodes * slot_w) + (box_w / 2)
      in
      let baker_col = bi * slot_w in
      let baker_center = baker_col + (box_w / 2) in
      let st =
        match baker.state with
        | Some s -> s.Data.Service_state.status
        | None -> Data.Service_state.Unknown "?"
      in
      let label = Printf.sprintf "B%d" (bi + 1) in
      let _mid =
        draw_compact_box
          c
          ~row:baker_row
          ~col:baker_col
          ~label
          ~role:"baker"
          ~st
      in
      for r = baker_row - (v_gap / 2) to baker_row - 1 do
        C.set_char c ~row:r ~col:baker_center ~char:"│" ~style:canvas_dim_style
      done ;
      let mid_row = baker_row - (v_gap / 2) - 1 in
      for r = node_h to mid_row do
        C.set_char c ~row:r ~col:parent_center ~char:"│" ~style:canvas_dim_style
      done ;
      if baker_center <> parent_center then begin
        let lo = min baker_center parent_center in
        let hi = max baker_center parent_center in
        for cc = lo to hi do
          C.set_char c ~row:mid_row ~col:cc ~char:"─" ~style:canvas_dim_style
        done
      end)
    sb.bakers ;
  let accuser_row =
    node_h + (if has_bakers then v_gap + node_h else 0) + v_gap
  in
  List.iteri
    (fun ai (accuser : accuser_info) ->
      let parent_center =
        match accuser.svc.depends_on with
        | None -> box_w / 2
        | Some parent ->
            let i =
              find_idx
                (fun (ni : node_info) -> String.equal ni.svc.instance parent)
                sb.nodes
            in
            if i >= 0 then List.nth node_centers i else box_w / 2
      in
      let accuser_col = ai * slot_w in
      let accuser_center = accuser_col + (box_w / 2) in
      let st =
        match accuser.state with
        | Some s -> s.Data.Service_state.status
        | None -> Data.Service_state.Unknown "?"
      in
      let label = Printf.sprintf "A%d" (ai + 1) in
      let _ =
        draw_compact_box
          c
          ~row:accuser_row
          ~col:accuser_col
          ~label
          ~role:"accuser"
          ~st
      in
      for r = accuser_row - (v_gap / 2) to accuser_row - 1 do
        C.set_char
          c
          ~row:r
          ~col:accuser_center
          ~char:"│"
          ~style:canvas_dim_style
      done ;
      let anchor_row = node_h + if has_bakers then v_gap + node_h else 0 in
      let mid_row = accuser_row - (v_gap / 2) - 1 in
      for r = anchor_row to mid_row do
        C.set_char c ~row:r ~col:parent_center ~char:"│" ~style:canvas_dim_style
      done ;
      if accuser_center <> parent_center then begin
        let lo = min accuser_center parent_center in
        let hi = max accuser_center parent_center in
        for cc = lo to hi do
          C.set_char c ~row:mid_row ~col:cc ~char:"─" ~style:canvas_dim_style
        done
      end)
    sb.accusers ;
  let themed_fg, themed_bg = themed_canvas_defaults () in
  C.to_ansi_with_defaults ~default_fg:themed_fg ~default_bg:themed_bg c

let peer_to_node_name nodes peer_addr =
  let peer_port = port_of peer_addr in
  List.find_map
    (fun (ni : node_info) ->
      let node_port = port_of ni.svc.net_addr in
      if String.equal node_port peer_port && not (String.equal node_port "")
      then Some ni.svc.instance
      else None)
    nodes

let render_dot (state : Data.Service_state.t option) =
  match state with
  | Some {status = Running; _} -> T.success "●"
  | Some {status = Stopped; _} -> T.error "○"
  | Some _ | None -> T.muted "?"

(** Render the topology detail panel.

    [node_metrics] is a pre-fetched association list: instance -> metrics option.
    [stake_pct] is the pre-fetched cached value for this sandbox.
    Neither Rpc_metrics nor any other I/O module is called here. *)
let render_topology (sb : sandbox_info) ~stake_pct
    ~(node_metrics : (string * Rpc_metrics.rpc_metrics option) list) ~size =
  let buf = Buffer.create 512 in
  let push s =
    Buffer.add_string buf s ;
    Buffer.add_char buf '\n'
  in
  push (T.muted "Base Configuration Topology") ;
  push "" ;
  push (T.text "Nodes — %d" (List.length sb.nodes)) ;
  (match sb.nodes with
  | [] -> push (T.muted "  (none)")
  | nodes ->
      List.iter
        (fun (ni : node_info) ->
          let metrics =
            List.assoc_opt ni.svc.instance node_metrics |> Option.join
          in
          let level_str =
            match metrics with
            | Some {Rpc_metrics.head_level = Some l; _} -> T.muted " L%d" l
            | _ -> T.muted " –"
          in
          let sync_str =
            match metrics with
            | Some {Rpc_metrics.bootstrapped = Some true; _} ->
                T.success " synced"
            | Some {Rpc_metrics.bootstrapped = Some false; _} ->
                T.warning " syncing"
            | _ -> ""
          in
          let rpc_port = port_of (Rpc_addr.to_string ni.svc.rpc_addr) in
          let p2p_port = port_of ni.svc.net_addr in
          push
            (T.concat
               [
                 " ";
                 render_dot ni.state;
                 T.text " %s" ni.svc.instance;
                 T.muted "  rpc :%s  p2p :%s" rpc_port p2p_port;
                 level_str;
                 sync_str;
               ]) ;
          List.iter
            (fun peer ->
              let label =
                match peer_to_node_name nodes peer with
                | Some inst ->
                    T.concat [T.muted "%s" peer; T.muted " (→ %s)" inst]
                | None -> T.muted "%s" peer
              in
              push (T.concat [T.muted "   └─ peer: "; label]))
            ni.peers)
        nodes) ;
  push "" ;
  push (T.text "Bakers — %d" (List.length sb.bakers)) ;
  (match sb.bakers with
  | [] -> push (T.muted "  (none)")
  | bakers ->
      List.iter
        (fun (bi : baker_info) ->
          let node_label =
            match bi.svc.depends_on with
            | Some n -> T.muted "  → %s" n
            | None -> ""
          in
          let del_str =
            if bi.delegate_count > 0 then
              T.text "  %d delegates" bi.delegate_count
            else T.muted "  0 delegates"
          in
          let failed_str =
            match bi.state with
            | Some {status = Unknown _; _} -> T.error "  [failed]"
            | _ -> ""
          in
          push
            (T.concat
               [
                 " ";
                 render_dot bi.state;
                 T.text " %s" bi.svc.instance;
                 del_str;
                 node_label;
                 failed_str;
               ]))
        bakers) ;
  (match sb.accusers with
  | [] -> ()
  | accusers ->
      push "" ;
      push (T.text "Accusers — %d" (List.length accusers)) ;
      List.iter
        (fun (ai : accuser_info) ->
          let node_label =
            match ai.svc.depends_on with
            | Some n -> T.muted "  → %s" n
            | None -> ""
          in
          let failed_str =
            match ai.state with
            | Some {status = Unknown _; _} -> T.error "  [failed]"
            | _ -> ""
          in
          push
            (T.concat
               [
                 " ";
                 render_dot ai.state;
                 T.text " %s" ai.svc.instance;
                 node_label;
                 failed_str;
               ]))
        accusers) ;
  push "" ;
  let total_dels =
    List.fold_left (fun a bi -> a + bi.delegate_count) 0 sb.bakers
  in
  let stake_str =
    match stake_pct with
    | Some pct ->
        T.concat [T.success " %.1f%%" pct; T.muted " of network stake"]
    | None -> T.muted " stake: fetching..."
  in
  push (T.concat [T.muted "%d delegates" total_dels; stake_str]) ;
  let make_legend prefix idx inst =
    let label = Printf.sprintf "%s%d=%s" prefix idx inst in
    (String.length label, T.muted "%s" label)
  in
  let node_legend =
    List.mapi
      (fun i (ni : node_info) -> make_legend "N" (i + 1) ni.svc.instance)
      sb.nodes
  in
  let baker_legend =
    List.mapi
      (fun i (bi : baker_info) -> make_legend "B" (i + 1) bi.svc.instance)
      sb.bakers
  in
  let accuser_legend =
    List.mapi
      (fun i (ai : accuser_info) -> make_legend "A" (i + 1) ai.svc.instance)
      sb.accusers
  in
  let legend_items = node_legend @ baker_legend @ accuser_legend in
  if legend_items <> [] then begin
    let sep = T.muted "  " in
    let sep_len = 2 in
    let max_cols = max 40 (size.LTerm_geom.cols - 4) in
    let rec build acc_items acc_len = function
      | [] -> [List.rev acc_items]
      | (w, item) :: rest ->
          let needed = acc_len + (if acc_items = [] then 0 else sep_len) + w in
          if acc_items <> [] && needed > max_cols then
            List.rev acc_items :: build [item] w rest
          else build (item :: acc_items) needed rest
    in
    let lines = build [] 0 legend_items in
    push "" ;
    List.iter (fun line -> push (String.concat sep line)) lines
  end ;
  let content = Buffer.contents buf in
  let avail_cols = size.LTerm_geom.cols - 4 in
  Flex.create
    ~direction:Flex.Column
    ~padding:{Flex.left = 2; right = 1; top = 1; bottom = 0}
    [
      {
        Flex.render = (fun ~size:_ -> content);
        basis = Flex.Px (List.length (String.split_on_char '\n' content));
        cross = None;
      };
      {
        Flex.render =
          (fun ~size:_ ->
            if sb.nodes = [] then ""
            else render_sandbox_canvas sb ~width:avail_cols);
        basis = Flex.Fill;
        cross = None;
      };
    ]
  |> Flex.render ~size

let render_create_detail ~size =
  Flex.create
    ~direction:Flex.Column
    ~padding:{Flex.left = 2; right = 1; top = 2; bottom = 0}
    [
      {
        Flex.render = (fun ~size:_ -> T.text "New Sandbox");
        basis = Flex.Px 1;
        cross = None;
      };
      {
        Flex.render =
          (fun ~size:_ ->
            T.muted "Press Enter to open the sandbox creation wizard.");
        basis = Flex.Px 1;
        cross = None;
      };
    ]
  |> fun f -> Flex.render f ~size

(* ─── Page Layout ───────────────────────────────────────────────────────── *)

let header = ["  Sandboxes"; ""]

let key_hint_pairs =
  [
    ("c", "create");
    ("Enter", "actions");
    ("s", "start");
    ("S", "stop");
    ("d", "destroy");
    ("a", "add account");
    ("r", "RPC");
    ("j/k", "nav");
    ("Esc", "back");
  ]

let list_width total_cols = max 22 (total_cols / 3)

let selected_sandbox s =
  if s.cursor = 0 then None else List.nth_opt s.sandboxes (s.cursor - 1)

let render_content s ~node_metrics ~stake_pct_for ~size =
  let cols = size.LTerm_geom.cols in
  let rows = size.LTerm_geom.rows in
  let lw = list_width cols in
  let layout =
    Flex.create
      ~direction:Flex.Row
      [
        {
          Flex.render =
            (fun ~size ->
              render_list ~sandboxes:s.sandboxes ~cursor:s.cursor ~size);
          basis = Flex.Px lw;
          cross = None;
        };
        {
          Flex.render =
            (fun ~size ->
              let h = size.LTerm_geom.rows in
              String.concat "\n" (List.init h (fun _ -> T.muted "|")));
          basis = Flex.Px 1;
          cross = None;
        };
        {
          Flex.render =
            (fun ~size ->
              match selected_sandbox s with
              | Some sb ->
                  let stake_pct = stake_pct_for sb.group.name in
                  render_topology sb ~stake_pct ~node_metrics ~size
              | None ->
                  if s.cursor = 0 then render_create_detail ~size
                  else render_create_detail ~size);
          basis = Flex.Fill;
          cross = None;
        };
      ]
  in
  Flex.render layout ~size:{LTerm_geom.rows; cols}

(** Main view function.

    [toast] is a pre-rendered toast string (may be empty) — call
    [Context.render_toasts] in the page module before invoking this.
    [node_metrics] is an association list: instance -> metrics option.
    [stake_pct_for] maps a group name to its cached stake percentage. *)
let view s ~toast ~node_metrics ~stake_pct_for ~focus:_ ~size =
  let cols = size.LTerm_geom.cols in
  let footer =
    let hints = Page_layout.render_themed_footer ~cols key_hint_pairs in
    if String.length toast > 0 then [toast] @ hints else hints
  in
  Page_layout.render_layout ~size ~header ~footer ~child:(fun avail ->
      render_content s ~node_metrics ~stake_pct_for ~size:avail)
