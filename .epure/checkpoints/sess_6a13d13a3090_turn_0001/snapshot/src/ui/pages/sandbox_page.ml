(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Sandbox management page — list sandboxes, topology view, actions.

    LAYOUT RULE: All layouts use Flex_layout / Grid_layout / Box_widget.
    No manual string alignment, no Pane_layout. *)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module Flex = Miaou_widgets_layout.Flex_layout
module T = Themed_text
module C = Miaou_canvas.Canvas
module Style_context = Miaou_style.Style_context
open Octez_manager_lib
open Rresult

let ( let* ) = Result.bind

let name = "sandbox"

(* ─── State ────────────────────────────────────────────────────────────── *)

type node_info = {
  svc : Service.t;
  state : Data.Service_state.t option;
  peers : string list;  (** Configured --peer addresses from extra_args *)
}

type baker_info = {
  svc : Service.t;
  state : Data.Service_state.t option;
  delegate_count : int;
  baker_ck_aliases : string list;
      (** Consensus-key aliases from OCTEZ_BAKER_DELEGATES_CSV. *)
}

type accuser_info = {svc : Service.t; state : Data.Service_state.t option}

type sandbox_info = {
  group : Group.t;
  nodes : node_info list;
  bakers : baker_info list;
  accusers : accuser_info list;
}

type state = {sandboxes : sandbox_info list; cursor : int}

type msg = unit

type pstate = state Navigation.t

(* ─── Async stake% cache ────────────────────────────────────────────────── *)

(** Stake fetch uses two tables.
    [stake_fetching] prevents duplicate in-flight fetches.
    [stake_results] is the cached result; it is read directly by [view] via
    [get_stake_pct] on every render frame, so no dirty-flag signal is needed
    when a new result arrives.  The cache is only cleared when the baker
    allocation changes (detected by comparing aliases before/after a reload). *)
let stake_fetching : (string, unit) Hashtbl.t = Hashtbl.create 4

let stake_results : (string, float) Hashtbl.t = Hashtbl.create 4

let stake_lock = Mutex.create ()

let get_stake_pct group_name =
  Mutex.protect stake_lock (fun () -> Hashtbl.find_opt stake_results group_name)

(** Map a consensus-key alias [delegate-(3n+1)] to its base alias [delegate-(3n)].
    Returns [None] for non-CK aliases or unrecognised formats. *)
let ck_alias_to_base_alias alias =
  match String.split_on_char '-' alias with
  | ["delegate"; ns] -> (
      match int_of_string_opt ns with
      | Some n when n mod 3 = 1 -> Some (Printf.sprintf "delegate-%d" (n - 1))
      | _ -> None)
  | _ -> None

let schedule_stake_fetch ~group_name ~endpoint ~wallet_dir ~baker_ck_aliases =
  let should_fetch =
    Mutex.protect stake_lock (fun () ->
        if
          Hashtbl.mem stake_fetching group_name
          || Hashtbl.mem stake_results group_name
        then false
        else (
          Hashtbl.replace stake_fetching group_name () ;
          true))
  in
  if should_fetch then
    Background_runner.enqueue (fun () ->
        (* Compute baker-controlled base-delegate addresses from wallet. *)
        let only_addrs =
          match Yes_wallet_io.read_wallet_pkhs ~wallet_dir with
          | Error _ -> []
          | Ok pkhs ->
              let pkh_map = Hashtbl.create 64 in
              List.iter
                (fun (alias, addr) -> Hashtbl.replace pkh_map alias addr)
                pkhs ;
              baker_ck_aliases
              |> List.filter_map ck_alias_to_base_alias
              |> List.filter_map (Hashtbl.find_opt pkh_map)
              |> List.sort_uniq String.compare
        in
        let result =
          Yes_wallet_io.fetch_stake_pct ~endpoint ~only_addrs ~wallet_dir ()
        in
        (* The view reads stake% directly from get_stake_pct on every frame,
           so no dirty signal is needed — just update the cache. *)
        Mutex.protect stake_lock (fun () ->
            Hashtbl.remove stake_fetching group_name ;
            match result with
            | Ok pct -> Hashtbl.replace stake_results group_name pct
            | Error _ -> ()))

(* ─── Data Loading ──────────────────────────────────────────────────────── *)

let parse_peers (svc : Service.t) =
  let rec loop = function
    | "--peer" :: addr :: rest -> addr :: loop rest
    | _ :: rest -> loop rest
    | [] -> []
  in
  loop svc.extra_args

let load_baker_delegate_info instance =
  match Node_env.read ~inst:instance with
  | Error _ -> (0, [])
  | Ok pairs -> (
      match List.assoc_opt "OCTEZ_BAKER_DELEGATES_CSV" pairs with
      | None | Some "" -> (0, [])
      | Some csv ->
          let aliases =
            csv |> String.split_on_char ',' |> List.map String.trim
            |> List.filter (fun s -> not (String.equal s ""))
          in
          (List.length aliases, aliases))

let load_sandboxes () =
  let service_states = Data.load_service_states () in
  let find_state instance =
    List.find_opt
      (fun (st : Data.Service_state.t) ->
        String.equal st.service.Service.instance instance)
      service_states
  in
  match Group_registry.list_sandboxes () with
  | Error _ -> []
  | Ok groups ->
      List.map
        (fun (g : Group.t) ->
          let services =
            match Lifecycle.group_services ~group_name:g.name () with
            | Ok l -> l
            | Error _ -> []
          in
          let nodes =
            services
            |> List.filter (fun (s : Service.t) -> String.equal s.role "node")
            |> List.map (fun svc ->
                {svc; state = find_state svc.instance; peers = parse_peers svc})
          in
          let bakers =
            services
            |> List.filter (fun (s : Service.t) -> String.equal s.role "baker")
            |> List.map (fun (svc : Service.t) ->
                let delegate_count, baker_ck_aliases =
                  load_baker_delegate_info svc.instance
                in
                {
                  svc;
                  state = find_state svc.instance;
                  delegate_count;
                  baker_ck_aliases;
                })
          in
          let accusers =
            services
            |> List.filter (fun (s : Service.t) ->
                String.equal s.role "accuser")
            |> List.map (fun svc -> {svc; state = find_state svc.instance})
          in
          {group = g; nodes; bakers; accusers})
        groups

let clamp_cursor sandboxes cursor =
  (* +1 for the synthetic "New sandbox" item always at position 0 *)
  let n = 1 + List.length sandboxes in
  max 0 (min cursor (n - 1))

(** Returns the sandbox at the cursor, or [None] when cursor is on the
    "New sandbox" create-item (position 0). *)
let selected_sandbox s =
  if s.cursor = 0 then None else List.nth_opt s.sandboxes (s.cursor - 1)

(* ─── Init / Lifecycle ──────────────────────────────────────────────────── *)

let init () =
  let sandboxes = load_sandboxes () in
  Navigation.make {sandboxes; cursor = 0}

let update ps _ = ps

let maybe_schedule_stake_fetch s =
  match selected_sandbox s with
  | None -> ()
  | Some sb -> (
      match sb.nodes with
      | [] -> ()
      | first :: _ ->
          let endpoint = Rpc_addr.to_endpoint first.svc.rpc_addr in
          let wallet = Sandbox.wallet_dir ~sandbox_name:sb.group.name in
          let baker_ck_aliases =
            List.concat_map (fun bi -> bi.baker_ck_aliases) sb.bakers
          in
          schedule_stake_fetch
            ~group_name:sb.group.name
            ~endpoint
            ~wallet_dir:wallet
            ~baker_ck_aliases)

let refresh ps =
  match Context.consume_navigation () with
  | Some (Context.Goto p) -> Navigation.goto p ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      let ps' =
        if Context.consume_instances_dirty () then begin
          let sandboxes = load_sandboxes () in
          let cursor = clamp_cursor sandboxes ps.Navigation.s.cursor in
          (* Only invalidate stake cache for sandboxes whose baker allocation
             changed (avoids re-fetching on unrelated dirty signals). *)
          let old_sandboxes = ps.Navigation.s.sandboxes in
          let baker_aliases (sb : sandbox_info) =
            List.concat_map (fun bi -> bi.baker_ck_aliases) sb.bakers
            |> List.sort String.compare
          in
          Mutex.protect stake_lock (fun () ->
              List.iter
                (fun (sb : sandbox_info) ->
                  let old_aliases =
                    match
                      List.find_opt
                        (fun (o : sandbox_info) ->
                          String.equal o.group.name sb.group.name)
                        old_sandboxes
                    with
                    | Some o -> baker_aliases o
                    | None -> []
                  in
                  if
                    not (List.equal String.equal old_aliases (baker_aliases sb))
                  then (
                    Hashtbl.remove stake_results sb.group.name ;
                    Hashtbl.remove stake_fetching sb.group.name))
                sandboxes) ;
          Navigation.update (fun _s -> {sandboxes; cursor}) ps
        end
        else ps
      in
      maybe_schedule_stake_fetch ps'.Navigation.s ;
      ps'

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps =
  Context.set_pending_tab Context.Tab_instances ;
  Navigation.back ps

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

(** Find the index of the first element satisfying [pred]; returns -1 if not found. *)
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

(** Get themed default colors for Canvas rendering *)
let themed_canvas_defaults () =
  let bg_resolved =
    Miaou_style.Style.to_resolved (Style_context.background ())
  in
  let fg_resolved = Miaou_style.Style.to_resolved (Style_context.text ()) in
  (fg_resolved.r_fg, bg_resolved.r_bg)

(** Draw a compact service box [● Lbl] width=7, height=3. Returns center col. *)
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

(** Build a compact canvas: nodes row, peer connections, baker row, accuser row.
    Uses short labels (N1/N2… nodes, B1/B2… bakers, A1/A2… accusers). *)
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
  (* Each baker/accuser gets its own sequential slot to avoid overlap. *)
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
  (* Draw nodes and collect their center columns *)
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
  (* Draw peer connections: horizontal line at mid-row between peered nodes.
     Draw only from the rightmost endpoint (i = max(i,j)) to avoid duplicates. *)
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
            (* Only draw once: from the node with higher index *)
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
  (* Draw bakers in sequential columns (one slot each) to avoid overlap when
     multiple bakers share a node.  Connect each baker to its parent via a
     vertical line from the baker's top to node_h, with a horizontal segment
     when baker column differs from parent node column. *)
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
      (* Sequential column: each baker gets its own slot *)
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
      (* Vertical down from baker top *)
      for r = baker_row - (v_gap / 2) to baker_row - 1 do
        C.set_char c ~row:r ~col:baker_center ~char:"│" ~style:canvas_dim_style
      done ;
      (* Vertical down from node bottom to mid-row *)
      let mid_row = baker_row - (v_gap / 2) - 1 in
      for r = node_h to mid_row do
        C.set_char c ~row:r ~col:parent_center ~char:"│" ~style:canvas_dim_style
      done ;
      (* Horizontal segment at mid_row if baker and parent differ *)
      if baker_center <> parent_center then begin
        let lo = min baker_center parent_center in
        let hi = max baker_center parent_center in
        for cc = lo to hi do
          C.set_char c ~row:mid_row ~col:cc ~char:"─" ~style:canvas_dim_style
        done
      end)
    sb.bakers ;
  (* Draw accusers: sequential slots below bakers (or below nodes if no bakers).
     Connect each accuser to its parent node with an L-shaped line. *)
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
      (* Vertical up from accuser top *)
      for r = accuser_row - (v_gap / 2) to accuser_row - 1 do
        C.set_char
          c
          ~row:r
          ~col:accuser_center
          ~char:"│"
          ~style:canvas_dim_style
      done ;
      (* Vertical from node/baker level down to mid-row *)
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

(** Match a configured --peer address to a node name by comparing P2P ports. *)
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

let render_topology (sb : sandbox_info) ~stake_pct ~size =
  let buf = Buffer.create 512 in
  let push s =
    Buffer.add_string buf s ;
    Buffer.add_char buf '\n'
  in
  push (T.muted "Base Configuration Topology") ;
  push "" ;
  (* ── Nodes ── *)
  push (T.text "Nodes — %d" (List.length sb.nodes)) ;
  (match sb.nodes with
  | [] -> push (T.muted "  (none)")
  | nodes ->
      List.iter
        (fun (ni : node_info) ->
          let metrics = Rpc_metrics.get ~instance:ni.svc.instance in
          let level_str =
            match metrics with
            | Some {head_level = Some l; _} -> T.muted " L%d" l
            | _ -> T.muted " –"
          in
          let sync_str =
            match metrics with
            | Some {bootstrapped = Some true; _} -> T.success " synced"
            | Some {bootstrapped = Some false; _} -> T.warning " syncing"
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
  (* ── Bakers ── *)
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
  (* ── Accusers ── *)
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
  (* ── Summary ── *)
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
  (* ── Legend for canvas ── *)
  (* Build legend items as (visible_width, colored_string) pairs for wrapping. *)
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
    (* Word-wrap legend items into lines fitting avail_cols. *)
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

let render_content s ~size =
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
                  let stake_pct = get_stake_pct sb.group.name in
                  render_topology sb ~stake_pct ~size
              | None ->
                  if s.cursor = 0 then render_create_detail ~size
                  else render_create_detail ~size);
          basis = Flex.Fill;
          cross = None;
        };
      ]
  in
  Flex.render layout ~size:{LTerm_geom.rows; cols}

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  Context.tick_spinner () ;
  Context.tick_toasts () ;
  let cols = size.LTerm_geom.cols in
  let toast = Context.render_toasts ~cols in
  let footer =
    let hints = Themed_page.render_themed_footer ~cols key_hint_pairs in
    if String.length toast > 0 then [toast] @ hints else hints
  in
  Themed_page.render_layout ~size ~header ~footer ~child:(fun avail ->
      render_content s ~size:avail)

(* ─── Actions ───────────────────────────────────────────────────────────── *)

let run_background desc f =
  Context.toast_info (T.text "%s..." desc) ;
  Background_runner.submit_blocking
    ~on_complete:(fun () -> Context.mark_instances_dirty ())
    (fun () ->
      match f () with
      | Ok () -> Context.toast_success (T.text "%s: done" desc)
      | Error (`Msg msg) ->
          Context.toast_error (T.text "%s failed: %s" desc msg))

type action =
  | Start
  | Stop
  | Destroy
  | Open_rpc
  | Add_account
  | Add_node
  | Add_baker
  | Reallocate_stake

let action_to_string = function
  | Start -> "Start"
  | Stop -> "Stop"
  | Destroy -> "Destroy"
  | Open_rpc -> "Open RPC Browser"
  | Add_account -> "Add Account"
  | Add_node -> "Add Node"
  | Add_baker -> "Add Baker"
  | Reallocate_stake -> "Reallocate Stake"

let do_start ps group_name =
  run_background (Printf.sprintf "Starting %s" group_name) (fun () ->
      let* _started = Lifecycle.start_group ~quiet:true ~group_name () in
      Ok ()) ;
  ps

let do_stop ps group_name =
  run_background (Printf.sprintf "Stopping %s" group_name) (fun () ->
      let* _stopped = Lifecycle.stop_group ~quiet:true ~group_name () in
      Ok ()) ;
  ps

let do_destroy ps group_name =
  let confirm () =
    run_background (Printf.sprintf "Destroying %s" group_name) (fun () ->
        Sandbox.destroy ~on_log:(fun _ -> ()) ~group_name ())
  in
  Modal_helpers.confirm_modal
    ~title:(Printf.sprintf "Destroy Sandbox '%s'?" group_name)
    ~message:
      (Printf.sprintf
         "Permanently delete sandbox '%s'?\n\
          This removes all services, wallet files, and data and cannot be \
          undone."
         group_name)
    ~on_result:(fun yes -> if yes then confirm ())
    () ;
  ps

let do_open_rpc ps (sb : sandbox_info) =
  (match sb.nodes with
  | svc :: _ ->
      Context.set_pending_instance_detail svc.svc.instance ;
      Context.navigate "rpc-browser"
  | [] -> Context.toast_warn "No node found for this sandbox") ;
  ps

let do_add_account ps group_name =
  let wallet = Sandbox.wallet_dir ~sandbox_name:group_name in
  Modal_helpers.prompt_text_modal
    ~title:"Add Account"
    ~placeholder:(Some "tz1... or tz2... or tz3... or tz4...")
    ~initial:""
    ~on_submit:(fun address ->
      let address = String.trim address in
      match Yes_wallet.curve_of_address address with
      | None ->
          Context.toast_error
            (T.text "Invalid address: must start with tz1/tz2/tz3/tz4")
      | Some _ ->
          run_background
            (Printf.sprintf "Adding account to %s" group_name)
            (fun () ->
              Result.map
                ignore
                (Yes_wallet_io.add_account ~wallet_dir:wallet ~address ())))
    () ;
  ps

let do_add_node ps group_name =
  let desc = Printf.sprintf "Adding node to %s" group_name in
  Context.toast_info (T.text "%s..." desc) ;
  Job_manager.submit
    ~timeout:None
    ~description:desc
    ~on_complete:(fun _ -> Context.mark_instances_dirty ())
    (fun ~append_log () ->
      Sandbox.add_node
        ~on_log:(fun msg -> append_log (msg ^ "\n"))
        ~group_name
        ()
      |> Result.map ignore) ;
  ps

let do_add_baker ps group_name =
  Context.set_pending_sandbox_group group_name ;
  Context.navigate "sandbox-key-alloc" ;
  ps

let open_action_modal ps (sb : sandbox_info) =
  let group_name = sb.group.name in
  let has_node = not (List.is_empty sb.nodes) in
  let has_bakers = not (List.is_empty sb.bakers) in
  let items =
    [Start; Stop]
    @ (if has_node then [Open_rpc] else [])
    @ [Add_account; Add_node; Add_baker]
    @ (if has_bakers then [Reallocate_stake] else [])
    @ [Destroy]
  in
  Modal_helpers.open_choice_modal
    ~title:(Printf.sprintf "Actions · %s" group_name)
    ~items
    ~to_string:action_to_string
    ~on_select:(fun action ->
      match action with
      | Start -> ignore (do_start ps group_name)
      | Stop -> ignore (do_stop ps group_name)
      | Destroy -> ignore (do_destroy ps group_name)
      | Open_rpc -> ignore (do_open_rpc ps sb)
      | Add_account -> ignore (do_add_account ps group_name)
      | Add_node -> ignore (do_add_node ps group_name)
      | Add_baker -> ignore (do_add_baker ps group_name)
      | Reallocate_stake -> ignore (do_add_baker ps group_name))
    () ;
  ps

(* ─── Key Handling ──────────────────────────────────────────────────────── *)

let handle_sandbox_key ps sb key =
  let group_name = sb.group.name in
  match key with
  | "Return" | "Enter" -> open_action_modal ps sb
  | "s" -> do_start ps group_name
  | "S" -> do_stop ps group_name
  | "d" -> do_destroy ps group_name
  | "r" -> do_open_rpc ps sb
  | "a" -> do_add_account ps group_name
  | _ -> ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match key with
    | "Escape" | "q" -> Navigation.back ps
    | "j" | "Down" ->
        Navigation.update
          (fun s -> {s with cursor = clamp_cursor s.sandboxes (s.cursor + 1)})
          ps
    | "k" | "Up" ->
        Navigation.update
          (fun s -> {s with cursor = clamp_cursor s.sandboxes (s.cursor - 1)})
          ps
    | "c" ->
        Context.navigate "sandbox-create" ;
        ps
    | ("Return" | "Enter") when ps.Navigation.s.cursor = 0 ->
        Context.navigate "sandbox-create" ;
        ps
    | _ -> (
        match selected_sandbox ps.Navigation.s with
        | Some sb -> handle_sandbox_key ps sb key
        | None -> ps)

(* ─── PAGE_SIG ──────────────────────────────────────────────────────────── *)

let handled_keys () =
  Keys.
    [
      Escape;
      Char "q";
      Char "j";
      Char "k";
      Down;
      Up;
      Char "c";
      Enter;
      Char "s";
      Char "S";
      Char "d";
      Char "a";
      Char "r";
    ]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [
    kb "c" "Create";
    kb "Enter" "Actions";
    kb "s" "Start";
    kb "S" "Stop";
    kb "d" "Destroy";
    kb "a" "Add account";
    kb "r" "RPC";
    kb "j/k" "Navigate";
    kb "Esc" "Back";
  ]

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

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
    List.map (fun (key, help) -> Miaou.Core.Tui_page.{key; help}) key_hint_pairs

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()
end

module Page =
  Themed_page.Make
    (Page_Impl)
    (struct
      let page_name = name
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
