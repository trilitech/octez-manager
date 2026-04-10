(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Rendering functions for the instances page *)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Grid = Miaou_widgets_layout.Grid_layout
module Box = Miaou_widgets_layout.Box_widget
module Metrics = Rpc_metrics
module Style_context = Miaou_style.Style_context
module Radio_button_widget = Miaou_widgets_input.Radio_button_widget
open Octez_manager_lib
open Instances_state
open Instances_layout

let status_icon (st : Service_state.t) =
  let instance = st.Service_state.service.Service.instance in
  match st.Service_state.status with
  | Service_state.Running -> Widgets.themed_success "●"
  | Service_state.Stopped ->
      (* Stopped but check for recent failure from UI-initiated start *)
      if Option.is_some (get_recent_failure ~instance) then
        Widgets.themed_error "●"
      else Widgets.themed_warning "●"
  | Service_state.Unknown _ ->
      (* Unknown status from systemd means the service failed.
         This catches crashes at startup even when not started via UI. *)
      Widgets.themed_error "●"

let enabled_badge (st : Service_state.t) =
  match st.Service_state.enabled with
  | Some true -> Widgets.themed_secondary "[enabled]"
  | Some false -> Widgets.themed_secondary "[disabled]"
  | None -> Widgets.themed_secondary "[unknown]"

let rpc_status_line ~(service_status : Service_state.status) (svc : Service.t) =
  let stopped =
    match service_status with Service_state.Running -> false | _ -> true
  in
  (* Show service status when not running, prioritizing recent failures *)
  let service_prefix =
    match service_status with
    | Service_state.Running ->
        (* Clear any stale failure on successful run *)
        clear_failure ~instance:svc.Service.instance ;
        None
    | Service_state.Stopped -> (
        (* Check for recent start failure first *)
        match get_recent_failure ~instance:svc.Service.instance with
        | Some error -> Some (Widgets.themed_error ("failed: " ^ error))
        | None -> Some (Widgets.themed_warning "stopped"))
    | Service_state.Unknown msg ->
        Some
          (Widgets.themed_error
             ("failed" ^ if msg = "" then "" else ": " ^ msg))
  in
  match Metrics.get ~instance:svc.Service.instance with
  | None -> (
      (* No metrics yet *)
      match service_prefix with
      | Some prefix -> prefix
      | None ->
          if svc.Service.role = "index" then Widgets.themed_muted "indexing"
          else Widgets.themed_muted "pending")
  | Some
      {
        Metrics.head_level;
        bootstrapped;
        chain_id;
        proto;
        last_error;
        last_block_time;
        _;
      } ->
      let error_prefix =
        match last_error with
        | Some _ -> Some (Widgets.themed_error "no rpc")
        | None -> None
      in
      let lvl =
        match head_level with Some l -> Printf.sprintf "L%d" l | None -> "L?"
      in
      let boot =
        match (error_prefix, service_prefix, bootstrapped) with
        | Some err, _, _ -> err
        | None, Some prefix, _ -> prefix
        | None, None, Some true -> Widgets.themed_success "synced"
        | None, None, Some false -> Widgets.themed_warning "syncing"
        | None, None, None -> Widgets.themed_muted (Context.render_spinner "")
      in
      let staleness =
        match last_block_time with
        | None -> ""
        | Some ts ->
            let age = Unix.gettimeofday () -. ts in
            if age >= 120. then
              Widgets.themed_error (Printf.sprintf "Δ %.0fs" age)
            else if age >= 30. then
              Widgets.themed_warning (Printf.sprintf "Δ %.0fs" age)
            else Widgets.themed_success (Printf.sprintf "Δ %.0fs" age)
      in
      let proto_s =
        match proto with
        | None -> Widgets.themed_muted "?"
        | Some p ->
            let s = String.sub p 0 (min 8 (String.length p)) in
            if stopped then Widgets.themed_muted s else Widgets.themed_text s
      in
      let chain_s =
        match chain_id with
        | None -> Widgets.themed_muted "?"
        | Some c ->
            let s = String.sub c 0 (min 8 (String.length c)) in
            if stopped then Widgets.themed_muted s else Widgets.themed_text s
      in
      let lvl_s =
        if stopped then Widgets.themed_muted lvl else Widgets.themed_text lvl
      in
      let parts =
        [boot; lvl_s; proto_s; chain_s]
        @ if staleness = "" then [] else [staleness]
      in
      String.concat " · " parts

let network_short (n : string) =
  match Snapshots.slug_of_network n with Some slug -> slug | None -> n

let line_for_service idx selected ~folded (st : Service_state.t) =
  let svc = st.Service_state.service in
  let marker =
    if idx + services_start_idx = selected then Widgets.themed_emphasis "➤"
    else " "
  in
  let status = status_icon st in
  let enabled = enabled_badge st in
  (* Add failure badge if service has a recent start/restart failure *)
  let has_failure =
    match st.Service_state.status with
    | Service_state.Running -> false
    | Service_state.Stopped | Service_state.Unknown _ ->
        (* Only show failure badge if there's a recent start/restart failure.
           This avoids showing failure for normal stops. *)
        Option.is_some (get_recent_failure ~instance:svc.Service.instance)
  in
  let failure_badge = if has_failure then Widgets.themed_error " [!]" else "" in
  let instance_str =
    Widgets.themed_text (Printf.sprintf "%-16s" svc.Service.instance)
  in
  (* For nodes: show history mode. For others: no extra info on first line *)
  let role_info =
    match svc.Service.role with
    | "node" ->
        Widgets.themed_text
          (Printf.sprintf
             "%-10s"
             (History_mode.to_string svc.Service.history_mode))
    | _ -> Widgets.themed_text (Printf.sprintf "%-10s" "")
  in
  let network =
    Widgets.themed_text
      (let s = network_short svc.Service.network in
       Printf.sprintf "%-12s" (if s = "" then "-" else s))
  in
  let fold_indicator = if folded then "+" else "−" in
  let first_line =
    Printf.sprintf
      "%s %s %s %s%s %s %s %s"
      marker
      fold_indicator
      status
      instance_str
      failure_badge
      role_info
      network
      enabled
  in
  (* Indent for second line and extra lines - align under instance name.
     marker 1 + space + fold 1 + space + status 1 + space = 6 *)
  let indent_start = 6 in
  (* Render highwatermarks line for bakers (last signed levels) *)
  let baker_highwatermarks_line ~instance =
    let activities = Baker_highwatermarks.read ~instance in
    match Baker_highwatermarks.format_summary activities with
    | None -> Widgets.themed_warning "no signing activity"
    | Some summary -> summary
  in
  (* Check if baker has DAL enabled *)
  let baker_has_dal ~instance =
    match Node_env.read ~inst:instance with
    | Error _ -> false
    | Ok pairs -> (
        match List.assoc_opt "OCTEZ_DAL_CONFIG" pairs with
        | None -> false
        | Some cfg ->
            let cfg = String.trim (String.lowercase_ascii cfg) in
            cfg <> "" && cfg <> "disabled")
  in
  (* Render delegate status for bakers (from RPC) *)
  let delegate_status_line ~instance =
    let delegate_pkhs = Delegate_scheduler.get_baker_delegates ~instance in
    if delegate_pkhs = [] then Widgets.themed_muted "no delegates configured"
    else
      let has_dal = baker_has_dal ~instance in
      let parts =
        List.map
          (fun pkh ->
            let short_pkh =
              if String.length pkh > 8 then String.sub pkh 0 8 ^ "…" else pkh
            in
            (* Try to get cached data *)
            match Delegate_data.get ~pkh with
            | None ->
                (* No data yet - show pending *)
                Printf.sprintf "%s:%s" short_pkh (Widgets.themed_muted "…")
            | Some d ->
                (* Status indicators *)
                let status =
                  if d.is_forbidden then Widgets.themed_error "FORBIDDEN"
                  else if d.deactivated then Widgets.themed_muted "inactive"
                  else
                    (* Missed slots status *)
                    let missed = d.participation.missed_slots in
                    let remaining =
                      d.participation.remaining_allowed_missed_slots
                    in
                    match Delegate_data.missed_slots_status d with
                    | Delegate_data.Critical ->
                        Widgets.themed_error
                          (Printf.sprintf "missed:%d/%d" missed remaining)
                    | Delegate_data.Warning ->
                        Widgets.themed_warning
                          (Printf.sprintf "missed:%d/%d" missed remaining)
                    | Delegate_data.Good ->
                        if missed > 0 then
                          Printf.sprintf "missed:%d/%d" missed remaining
                        else Widgets.themed_success "ok"
                in
                (* DAL participation info if baker has DAL enabled *)
                let dal_info =
                  if not has_dal then ""
                  else
                    let dp = d.dal_participation in
                    let attested = dp.delegate_attested_dal_slots in
                    let attestable = dp.delegate_attestable_dal_slots in
                    let ratio =
                      if attestable > 0 then
                        Printf.sprintf "%d/%d" attested attestable
                      else "-"
                    in
                    let dal_status =
                      if dp.denounced then Widgets.themed_error "denounced"
                      else if
                        (not dp.sufficient_dal_participation) && attestable > 0
                      then
                        Widgets.themed_warning (Printf.sprintf "dal:%s" ratio)
                      else if attestable > 0 then
                        Widgets.themed_success (Printf.sprintf "dal:%s" ratio)
                      else ""
                    in
                    if dal_status = "" then "" else " " ^ dal_status
                in
                Printf.sprintf "%s:%s%s" short_pkh status dal_info)
          delegate_pkhs
      in
      String.concat " · " parts
  in
  let dal_health_line ~instance =
    match Dal_health.get ~instance with
    | None -> Widgets.themed_muted "health: ?"
    | Some health ->
        let status_str =
          match health.Dal_health.status with
          | Dal_health.Up -> Widgets.themed_success "up"
          | Dal_health.Down -> Widgets.themed_error "down"
          | Dal_health.Degraded -> Widgets.themed_warning "degraded"
          | Dal_health.Unknown -> Widgets.themed_muted "?"
        in
        let checks_str =
          if health.Dal_health.checks = [] then ""
          else
            let check_strs =
              List.map
                (fun (c : Dal_health.check) ->
                  let st =
                    match c.status with
                    | Dal_health.Up -> Widgets.themed_success "ok"
                    | Dal_health.Down -> Widgets.themed_error "ko"
                    | Dal_health.Degraded -> Widgets.themed_warning "deg"
                    | Dal_health.Unknown -> Widgets.themed_muted "?"
                  in
                  Printf.sprintf "%s:%s" c.name st)
                health.Dal_health.checks
            in
            " · " ^ String.concat " " check_strs
        in
        Printf.sprintf "health: %s%s" status_str checks_str
  in
  (* Line 2 only shows meaningful content when service is running *)
  let is_running = st.Service_state.status = Service_state.Running in
  let second_line =
    let indent = String.make indent_start ' ' in
    if not is_running then
      (* When stopped/failed, show minimal status *)
      match st.Service_state.status with
      | Service_state.Stopped ->
          (* Stopped but check for recent failure from UI-initiated start *)
          if has_failure then
            match get_recent_failure ~instance:svc.Service.instance with
            | Some error -> indent ^ Widgets.themed_error ("failed: " ^ error)
            | None -> indent ^ Widgets.themed_warning "stopped"
          else indent ^ Widgets.themed_warning "stopped"
      | Service_state.Unknown msg ->
          (* Unknown status from systemd means the service failed *)
          indent ^ Widgets.themed_error ("failed: " ^ msg)
      | Service_state.Running -> indent (* shouldn't happen *)
    else
      match svc.Service.role with
      | "baker" ->
          (* Line 2 for bakers: highwatermarks (last signed levels) *)
          let hwm = baker_highwatermarks_line ~instance:svc.Service.instance in
          Printf.sprintf "%s%s" indent hwm
      | "dal-node" ->
          (* Line 2 for DAL nodes: health status *)
          Printf.sprintf
            "%s%s"
            indent
            (dal_health_line ~instance:svc.Service.instance)
      | "accuser" ->
          (* Line 2 for accusers: simple monitoring status *)
          Printf.sprintf "%s%s" indent (Widgets.themed_success "monitoring")
      | "signatory" ->
          (* Line 2 for signatories: basic status *)
          let status_text =
            match Signatory_metrics.get ~instance:svc.Service.instance with
            | None -> Widgets.themed_muted "pending"
            | Some metrics -> (
                match metrics.Signatory_metrics.health with
                | Signatory_metrics.Up -> Widgets.themed_success "healthy"
                | Signatory_metrics.Down -> Widgets.themed_error "down"
                | Signatory_metrics.Degraded ->
                    Widgets.themed_warning "degraded"
                | Signatory_metrics.Unknown -> Widgets.themed_muted "unknown")
          in
          Printf.sprintf "%s%s" indent status_text
      | "index" ->
          (* Line 2 for indexers: sync status and last indexed level *)
          let status_text =
            match Index_metrics.get ~instance:svc.Service.instance with
            | None -> Widgets.themed_muted "indexing"
            | Some m ->
                let lvl_s =
                  match m.Index_metrics.head_level with
                  | Some l -> Widgets.themed_text (Printf.sprintf "L%d" l)
                  | None -> Widgets.themed_muted "L?"
                in
                let boot =
                  match m.Index_metrics.synced with
                  | Some true -> Widgets.themed_success "synced"
                  | Some false -> Widgets.themed_warning "syncing"
                  | None -> Widgets.themed_muted (Context.render_spinner "")
                in
                String.concat " · " [boot; lvl_s]
          in
          Printf.sprintf "%s%s" indent status_text
      | _ ->
          Printf.sprintf
            "%s%s"
            indent
            (rpc_status_line ~service_status:st.Service_state.status svc)
  in
  (* If folded, return first two lines (header + RPC/health status) *)
  if folded then String.concat "\n" [first_line; second_line]
  else (
    (* Mark as visible for system metrics polling (unfolded = higher refresh rate) *)
    System_metrics_scheduler.mark_visible
      ~role:svc.Service.role
      ~instance:svc.Service.instance ;
    (* Additional lines for nodes, bakers, accusers, dal-nodes, and signatories: metrics + CPU chart *)
    let extra_lines =
      match svc.Service.role with
      | "signatory" -> (
          (* For signatories: show address, keys count, and backend info *)
          let indent = String.make indent_start ' ' in
          match Signatory_metrics.get ~instance:svc.Service.instance with
          | None -> []
          | Some metrics ->
              let parts = [] in
              (* Add address if available *)
              let parts =
                match metrics.Signatory_metrics.address with
                | Some addr -> parts @ [addr]
                | None -> parts
              in
              (* Add keys count *)
              let parts =
                let key_count =
                  List.length metrics.Signatory_metrics.authorized_keys
                in
                parts @ [Printf.sprintf "%d keys" key_count]
              in
              (* Add backend type if available *)
              let parts =
                match metrics.Signatory_metrics.backend with
                | Some backend -> parts @ [backend]
                | None -> parts
              in
              if parts = [] then [] else [indent ^ String.concat " · " parts])
      | "node" | "baker" | "accuser" | "dal-node" | "index" ->
          let focus = idx + services_start_idx = selected in
          let indent = String.make indent_start ' ' in
          (* For bakers: add delegate status line (line 3) *)
          let baker_delegate_line =
            if svc.Service.role = "baker" then
              [indent ^ delegate_status_line ~instance:svc.Service.instance]
            else []
          in
          let version =
            match
              System_metrics_scheduler.get_version
                ~role:svc.Service.role
                ~instance:svc.Service.instance
            with
            | Some v ->
                if String.equal svc.Service.role "index" then
                  System_metrics_scheduler.format_index_version_colored v
                else System_metrics_scheduler.format_version_colored v
            | None -> Widgets.themed_muted "v?"
          in
          let mem =
            System_metrics_scheduler.render_mem_sparkline
              ~role:svc.Service.role
              ~instance:svc.Service.instance
              ~focus
          in
          (* Metrics line: version, memory, disk (for nodes and dal-nodes) *)
          let metrics_parts =
            [version]
            @ (if mem = "" then [] else [Widgets.themed_text "MEM " ^ mem])
            @
            if
              svc.Service.role = "node"
              || svc.Service.role = "dal-node"
              || svc.Service.role = "index"
            then
              let disk =
                match
                  System_metrics_scheduler.get_disk_size
                    ~role:svc.Service.role
                    ~instance:svc.Service.instance
                with
                | Some sz ->
                    Widgets.themed_text (System_metrics.format_bytes sz)
                | None -> Widgets.themed_muted "?"
              in
              [Widgets.themed_text "DISK " ^ disk]
            else []
          in
          let metrics_line = indent ^ String.concat " · " metrics_parts in
          (* Lines 4+: CPU chart (multi-row braille) *)
          let cpu_lines =
            match
              System_metrics_scheduler.render_cpu_chart
                ~role:svc.Service.role
                ~instance:svc.Service.instance
                ~focus
            with
            | None -> []
            | Some (chart, avg) ->
                let chart_rows = String.split_on_char '\n' chart in
                let last_idx = List.length chart_rows - 1 in
                List.mapi
                  (fun i row ->
                    if i = last_idx then
                      Printf.sprintf
                        "%s%s%s %.0f%%"
                        indent
                        (Widgets.themed_text "CPU ")
                        row
                        avg
                    else Printf.sprintf "%s    %s" indent row)
                  chart_rows
          in
          baker_delegate_line @ [metrics_line] @ cpu_lines
      | _ -> []
    in
    String.concat "\n" ([first_line; second_line] @ extra_lines))

(** Render a ghost "Add new" entry *)
let line_for_ghost_add_new idx selected role =
  let marker =
    if idx + services_start_idx = selected then Widgets.themed_emphasis "➤"
    else " "
  in
  let role_name =
    match role with
    | "node" -> "Node"
    | "baker" -> "Baker"
    | "accuser" -> "Accuser"
    | "dal-node" -> "DAL Node"
    | "signatory" -> "Signatory"
    | "index" -> "Indexer"
    | r -> String.capitalize_ascii r
  in
  Printf.sprintf
    "%s %s %s"
    marker
    (Widgets.themed_muted "+")
    (Widgets.themed_muted (Printf.sprintf "Add new %s" role_name))

(** Truncate a string to max visible characters, preserving ANSI codes *)
let truncate_visible ~max_width s =
  let len = String.length s in
  let buf = Buffer.create len in
  (* Skip ANSI escape sequence starting at position i, return new position *)
  let rec skip_ansi i =
    if i >= len then i
    else if s.[i] = 'm' then (
      Buffer.add_char buf 'm' ;
      i + 1)
    else (
      Buffer.add_char buf s.[i] ;
      skip_ansi (i + 1))
  in
  (* Main loop: i = position, visible = visible char count *)
  let rec loop i visible =
    if i >= len || visible >= max_width then (i, visible)
    else if s.[i] = '\027' then (
      Buffer.add_char buf '\027' ;
      let i' = skip_ansi (i + 1) in
      loop i' visible)
    else (
      Buffer.add_char buf s.[i] ;
      loop (i + 1) (visible + 1))
  in
  let final_i, final_visible = loop 0 0 in
  (* Add reset if we truncated mid-formatting *)
  if final_i < len && final_visible >= max_width then
    Buffer.add_string buf "\027[0m" ;
  Buffer.contents buf

(** Pad or truncate a line to exact column width using visible character count *)
let pad_line ~col_width line =
  let visible_len = Miaou_helpers.Helpers.visible_chars_count line in
  if visible_len < col_width then
    line ^ String.make (col_width - visible_len) ' '
  else if visible_len > col_width then
    truncate_visible ~max_width:col_width line
  else line

(** Compute sections based on view_mode *)
let sections_for_view state = Instances_layout.sections_of_state state

let role_key_of_header = function
  | "Nodes" -> "node"
  | "Bakers" -> "baker"
  | "Accusers" -> "accuser"
  | "DAL Nodes" -> "dal-node"
  | "Signatories" -> "signatory"
  | header ->
      let buf = Buffer.create (String.length header) in
      String.iter
        (fun c ->
          let lower = Char.lowercase_ascii c in
          if (lower >= 'a' && lower <= 'z') || (lower >= '0' && lower <= '9')
          then Buffer.add_char buf lower
          else Buffer.add_char buf '-')
        header ;
      Buffer.contents buf

(** Render a single column's content - returns list of lines *)
let render_column ~col_width ~state ~column_groups =
  let items =
    column_items
      ~column_groups
      ~global_display_items:(display_ordered_items state)
  in
  let empty_line = String.make col_width ' ' in
  (* Group items into (header, display_items) pairs *)
  let groups =
    let current_header = ref None in
    let current_items = ref [] in
    let result = ref [] in
    List.iter
      (fun item ->
        match item with
        | Header role_name ->
            (match !current_header with
            | Some hdr -> result := (hdr, List.rev !current_items) :: !result
            | None -> ()) ;
            current_header := Some role_name ;
            current_items := []
        | Item (idx, display_item) ->
            current_items := (idx, display_item) :: !current_items)
      items ;
    (match !current_header with
    | Some hdr -> result := (hdr, List.rev !current_items) :: !result
    | None -> ()) ;
    List.rev !result
  in
  (* Render each group as a Box containing its instances and ghosts *)
  let is_first = ref true in
  let lines =
    List.concat_map
      (fun (role_name, display_items) ->
        let widget_name = "instances-box-" ^ role_key_of_header role_name in
        let instance_lines =
          List.concat_map
            (fun (idx, display_item) ->
              match display_item with
              | Real_service svc ->
                  let is_folded =
                    StringSet.mem svc.service.Service.instance state.folded
                  in
                  let line =
                    line_for_service idx state.selected ~folded:is_folded svc
                  in
                  String.split_on_char '\n' line
              | Ghost_add_new (role, _) ->
                  let line = line_for_ghost_add_new idx state.selected role in
                  [line])
            display_items
        in
        let content = String.concat "\n" instance_lines in
        let box =
          Style_context.with_child_context ~widget_name (fun () ->
              Box.render
                ~title:role_name
                ~style:Rounded
                ~width:col_width
                content)
        in
        let box_lines =
          String.split_on_char '\n' box |> List.map (pad_line ~col_width)
        in
        let result = if !is_first then box_lines else empty_line :: box_lines in
        is_first := false ;
        result)
      groups
  in
  lines

(** Dim inactive column lines to make active column stand out. *)
let dim_inactive_column line =
  (* Wrap entire line in dim formatting *)
  Widgets.themed_muted line

(** Merge multiple column renders into combined lines with per-column scrolling.
    Uses Grid_layout for consistent column layout. *)
let merge_columns ~col_width ~visible_height ~column_scroll ~active_column
    ~columns_content =
  let empty_line = String.make col_width ' ' in
  let num_columns = Array.length columns_content in
  (* Count non-empty columns for dimming decision *)
  let non_empty_cols =
    Array.fold_left
      (fun acc col -> if col <> [] then acc + 1 else acc)
      0
      columns_content
  in
  (* Apply scroll offset to each column and take visible_height lines *)
  let scrolled_columns =
    Array.mapi
      (fun col_idx col ->
        let scroll = column_scroll.(col_idx) in
        let col_len = List.length col in
        (* Clamp scroll to valid range *)
        let scroll = max 0 (min scroll (max 0 (col_len - visible_height))) in
        (* Take visible_height lines starting from scroll offset *)
        let visible =
          col
          |> List.filteri (fun i _ ->
              i >= scroll && i < scroll + visible_height)
        in
        (* Dim inactive columns to make active column stand out (only if multiple non-empty columns) *)
        let visible =
          if non_empty_cols > 1 && col_idx <> active_column then
            List.map dim_inactive_column visible
          else visible
        in
        (* Pad to visible_height if needed *)
        let pad_count = visible_height - List.length visible in
        if pad_count > 0 then
          visible @ List.init pad_count (fun _ -> empty_line)
        else visible)
      columns_content
  in
  (* Use Grid_layout to merge columns *)
  let total_width =
    (col_width * num_columns)
    + (String.length column_separator * (num_columns - 1))
  in
  let cols_spec = List.init num_columns (fun _ -> Grid.Px col_width) in
  let sep_width = String.length column_separator in
  let grid_children =
    Array.to_list
      (Array.mapi
         (fun col_idx col ->
           Grid.cell ~row:0 ~col:col_idx (fun ~size:_ -> String.concat "\n" col))
         scrolled_columns)
  in
  let grid =
    Grid.create
      ~rows:[Grid.Fr 1.]
      ~cols:cols_spec
      ~col_gap:sep_width
      grid_children
  in
  let rendered =
    Grid.render
      grid
      ~size:{LTerm_geom.rows = visible_height; cols = total_width}
  in
  String.split_on_char '\n' rendered

(** Render external services section *)
let render_external_service ~selected_idx ~current_idx ~folded
    (ext : External_service.t) =
  let open External_service in
  let cfg = ext.config in
  let role_str =
    match cfg.role.value with Some r -> role_to_string r | None -> "unknown"
  in
  let network_str = match cfg.network.value with Some n -> n | None -> "?" in
  let status_str = status_label (status_of_unit_state cfg.unit_state) in

  let marker =
    if current_idx = selected_idx then Widgets.themed_emphasis "➤" else " "
  in
  let fold_indicator = if folded then "+" else "−" in
  let status =
    match cfg.unit_state.active_state with
    | "active" -> Widgets.themed_success "●"
    | "failed" -> Widgets.themed_error "●"
    | _ -> Widgets.themed_warning "●"
  in

  (* First line: like managed services *)
  let instance_str =
    Widgets.themed_text (Printf.sprintf "%-16s" ext.suggested_instance_name)
  in
  let role_str = Widgets.themed_text (Printf.sprintf "%-10s" role_str) in
  let network = Widgets.themed_text (Printf.sprintf "%-12s" network_str) in
  let external_badge = Widgets.themed_muted "[external]" in
  let first_line =
    Printf.sprintf
      "%s %s %s %s %s %s %s"
      marker
      fold_indicator
      status
      instance_str
      role_str
      network
      external_badge
  in

  if folded then [first_line]
  else
    (* For external services, use the suggested instance name consistently *)
    let instance_for_metrics = ext.suggested_instance_name in
    let role_for_metrics =
      match cfg.role.value with
      | Some Node -> "node"
      | Some Baker -> "baker"
      | Some Accuser -> "accuser"
      | Some Dal_node -> "dal-node"
      | _ -> "unknown"
    in
    System_metrics_scheduler.mark_visible
      ~role:role_for_metrics
      ~instance:instance_for_metrics ;

    (* Submit poll request with actual unit name for external services *)
    let binary_path =
      match cfg.binary_path.value with
      | Some b -> b
      | None ->
          "octez-node" (* fallback, shouldn't happen for detected services *)
    in
    let data_dir_path =
      match cfg.data_dir.value with Some d -> d | None -> ""
    in
    System_metrics_scheduler.submit_poll
      ~role:role_for_metrics
      ~instance:instance_for_metrics
      ~binary:binary_path
      ~data_dir:data_dir_path
      ~unit_name:cfg.unit_name
      () ;

    (* Second line: RPC/endpoint status *)
    let indent = "      " in
    let line2 =
      match cfg.role.value with
      | Some Node ->
          let rpc = match cfg.rpc_addr.value with Some r -> r | None -> "?" in
          Widgets.themed_text (indent ^ "RPC: " ^ rpc)
      | Some (Baker | Accuser | Dal_node) ->
          let ep =
            match cfg.node_endpoint.value with Some e -> e | None -> "?"
          in
          Widgets.themed_text (indent ^ "Node: " ^ ep)
      | _ -> Widgets.themed_text (indent ^ "Status: " ^ status_str)
    in

    (* System metrics *)
    let focus = current_idx = selected_idx in
    let version =
      match
        System_metrics_scheduler.get_version
          ~role:role_for_metrics
          ~instance:instance_for_metrics
      with
      | Some v -> System_metrics_scheduler.format_version_colored v
      | None -> Widgets.themed_muted "v?"
    in
    let mem =
      System_metrics_scheduler.render_mem_sparkline
        ~role:role_for_metrics
        ~instance:instance_for_metrics
        ~focus
    in
    (* For nodes, add head level, sync status, and staleness (matching managed instances) *)
    let node_info =
      match cfg.role.value with
      | Some Node -> (
          match Rpc_metrics.get ~instance:instance_for_metrics with
          | Some metrics ->
              let head_str =
                match metrics.Rpc_metrics.head_level with
                | Some l -> Widgets.themed_text (Printf.sprintf "L%d" l)
                | None -> Widgets.themed_muted "L?"
              in
              let sync_badge =
                match metrics.Rpc_metrics.bootstrapped with
                | Some true -> Widgets.themed_success "synced"
                | Some false -> Widgets.themed_warning "syncing"
                | None -> Widgets.themed_muted (Context.render_spinner "")
              in
              let staleness =
                match metrics.Rpc_metrics.last_block_time with
                | None -> ""
                | Some ts ->
                    let age = Unix.gettimeofday () -. ts in
                    if age >= 120. then
                      Widgets.themed_error (Printf.sprintf "Δ %.0fs" age)
                    else if age >= 30. then
                      Widgets.themed_warning (Printf.sprintf "Δ %.0fs" age)
                    else Widgets.themed_success (Printf.sprintf "Δ %.0fs" age)
              in
              [head_str; sync_badge]
              @ if staleness = "" then [] else [staleness]
          | None -> [])
      | _ -> []
    in
    let metrics_parts =
      [version]
      @ (if mem = "" then [] else [Widgets.themed_text "MEM " ^ mem])
      @ node_info
    in
    let metrics_line = indent ^ String.concat " · " metrics_parts in

    (* CPU chart *)
    let cpu_lines =
      match
        System_metrics_scheduler.render_cpu_chart
          ~role:role_for_metrics
          ~instance:instance_for_metrics
          ~focus
      with
      | None -> []
      | Some (chart, _avg_cpu) ->
          String.split_on_char '\n' chart
          |> List.map (fun line -> indent ^ line)
    in

    [first_line; line2; metrics_line] @ cpu_lines

let render_external_services_section state =
  if state.external_services = [] then []
  else
    let n = List.length state.external_services in
    if state.external_section_folded then
      (* Collapsed: single header line showing count and fold indicator *)
      let header =
        Widgets.themed_emphasis
          (Printf.sprintf "+ Unmanaged Instances (%d)  [u: expand]" n)
      in
      [header]
    else
      let header =
        Widgets.themed_emphasis (Printf.sprintf "− Unmanaged Instances (%d)" n)
      in
      (* Calculate base index for external services (after menu and managed services) *)
      let external_start_idx =
        services_start_idx + List.length state.services
      in
      let service_lines =
        List.mapi
          (fun idx ext ->
            let current_idx = external_start_idx + idx in
            let is_folded =
              StringSet.mem
                ext.External_service.suggested_instance_name
                state.external_folded
            in
            let lines =
              render_external_service
                ~selected_idx:state.selected
                ~current_idx
                ~folded:is_folded
                ext
            in
            lines)
          state.external_services
        |> List.concat
      in
      header :: service_lines

(** Render the view-mode radio row (visible but not navigable).
    The radio row is always shown without focus since it's toggled via 'g' key.
    Widgets are created fresh from state — not stored. *)
let radio_row view_mode =
  let by_role =
    Radio_button_widget.create
      ~label:"By Role"
      ~selected:(view_mode = By_role)
      ()
  in
  let by_group =
    Radio_button_widget.create
      ~label:"By Group"
      ~selected:(view_mode = By_group)
      ()
  in
  "View: "
  ^ Radio_button_widget.render by_role ~focus:false
  ^ "   "
  ^ Radio_button_widget.render by_group ~focus:false

(** Single-column layout (original) *)
let table_lines_single state =
  let view_row = radio_row state.view_mode in
  let instance_rows =
    (* Group display items by role or group based on view_mode *)
    let sections = sections_for_view state in
    (* Render each section as a Box *)
    let idx = ref 0 in
    let is_first = ref true in
    List.concat_map
      (fun (section_name, display_items) ->
        let hdr =
          match state.view_mode with
          | By_role -> role_header section_name
          | By_group -> section_name
        in
        let widget_name = "instances-box-" ^ role_key_of_header section_name in
        let instance_lines =
          List.concat_map
            (fun display_item ->
              let i = !idx in
              incr idx ;
              match display_item with
              | Real_service svc ->
                  let is_folded =
                    StringSet.mem svc.service.Service.instance state.folded
                  in
                  let row =
                    line_for_service i state.selected ~folded:is_folded svc
                  in
                  String.split_on_char '\n' row
              | Ghost_add_new (role, _) ->
                  let line = line_for_ghost_add_new i state.selected role in
                  [line])
            display_items
        in
        let content = String.concat "\n" instance_lines in
        let box =
          Style_context.with_child_context ~widget_name (fun () ->
              Box.render ~title:hdr ~style:Rounded ~width:78 content)
        in
        let box_lines = String.split_on_char '\n' box in
        let result = if !is_first then box_lines else "" :: box_lines in
        is_first := false ;
        result)
      sections
  in
  let external_rows = render_external_services_section state in
  let external_rows =
    if external_rows = [] then []
    else (* Add separator above external services *)
      let separator = Widgets.themed_muted (String.make 80 '-') in
      "" :: separator :: external_rows
  in
  (view_row :: "" :: instance_rows) @ external_rows

(** Multi-column matrix layout *)
let table_lines_matrix ~cols ~visible_height ~column_scroll state =
  let num_columns =
    calc_num_columns ~cols ~min_column_width ~column_separator
  in
  let sections = sections_for_view state in
  let columns = distribute_to_columns ~num_columns sections in
  let col_width =
    (cols - ((num_columns - 1) * String.length column_separator)) / num_columns
  in
  (* Render each column *)
  let columns_content =
    Array.map
      (fun column_groups -> render_column ~col_width ~state ~column_groups)
      columns
  in
  (* Calculate space needed for external services *)
  let external_lines = render_external_services_section state in
  let external_line_count = List.length external_lines in
  (* Reserve space for external services if present *)
  let reserved_for_external =
    if external_line_count > 0 then external_line_count + 1 else 0
  in
  (* Reduce available height for columns to make room for external services *)
  let columns_visible_height = max 5 (visible_height - reserved_for_external) in
  (* Header row (view-mode radio) spans full width *)
  let view_row = radio_row state.view_mode in
  (* Selection is always in services (services_start_idx = 0) *)
  let effective_active_column = state.active_column in
  let instance_rows =
    merge_columns
      ~col_width
      ~visible_height:columns_visible_height
      ~column_scroll
      ~active_column:effective_active_column
      ~columns_content
  in
  (* Trim trailing empty lines from column grid to place external services directly below *)
  let instance_rows_trimmed =
    let rec trim_end = function
      | [] -> []
      | rows ->
          let last = List.nth rows (List.length rows - 1) in
          if String.trim last = "" then
            trim_end (List.filteri (fun i _ -> i < List.length rows - 1) rows)
          else rows
    in
    trim_end instance_rows
  in
  (* Append external services below the columnar grid *)
  let result = view_row :: "" :: instance_rows_trimmed in
  if external_line_count > 0 then
    let separator = Widgets.themed_muted (String.make (min cols 120) '-') in
    result @ [""; separator] @ external_lines
  else result

let table_lines ?(cols = 80) ?(visible_height = 20) state =
  (* Clear visibility markers at start of render pass *)
  System_metrics_scheduler.clear_visibility () ;
  let num_columns =
    calc_num_columns ~cols ~min_column_width ~column_separator
  in
  (* Always render sections - they will show ghost entries even when empty *)
  if num_columns <= 1 then table_lines_single state
  else
    (* For matrix layout, reserve space for the radio row + separator.
       With services_start_idx = 0, this is just the visual header (2 lines). *)
    let matrix_height = max 5 (visible_height - 2) in
    table_lines_matrix
      ~cols
      ~visible_height:matrix_height
      ~column_scroll:state.column_scroll
      state

let summary_line state =
  let managed = List.length state.services in
  let external_count = List.length state.external_services in
  if external_count = 0 then Printf.sprintf "Total instances: %d" managed
  else Printf.sprintf "Managed: %d | External: %d" managed external_count
