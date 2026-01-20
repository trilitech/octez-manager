(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Rendering functions for the instances page *)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Metrics = Rpc_metrics
open Octez_manager_lib
open Instances_state
open Instances_layout

let status_icon (st : Service_state.t) =
  let instance = st.Service_state.service.Service.instance in
  match st.Service_state.status with
  | Service_state.Running -> Widgets.green "●"
  | Service_state.Stopped ->
      (* Stopped but check for recent failure from UI-initiated start *)
      if Option.is_some (get_recent_failure ~instance) then Widgets.red "●"
      else Widgets.yellow "●"
  | Service_state.Unknown _ ->
      (* Unknown status from systemd means the service failed.
         This catches crashes at startup even when not started via UI. *)
      Widgets.red "●"

let enabled_badge (st : Service_state.t) =
  match st.Service_state.enabled with
  | Some true -> Widgets.dim "[enabled]"
  | Some false -> Widgets.dim "[disabled]"
  | None -> Widgets.dim "[unknown]"

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
        | Some error -> Some (Widgets.red ("failed: " ^ error))
        | None -> Some (Widgets.yellow "stopped"))
    | Service_state.Unknown msg ->
        Some (Widgets.red ("failed" ^ if msg = "" then "" else ": " ^ msg))
  in
  match Metrics.get ~instance:svc.Service.instance with
  | None -> (
      (* No metrics yet *)
      match service_prefix with
      | Some prefix -> prefix
      | None -> Widgets.dim "pending")
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
        | Some _ -> Some (Widgets.red "no rpc")
        | None -> None
      in
      let lvl =
        match head_level with Some l -> Printf.sprintf "L%d" l | None -> "L?"
      in
      let boot =
        match (error_prefix, service_prefix, bootstrapped) with
        | Some err, _, _ -> err
        | None, Some prefix, _ -> prefix
        | None, None, Some true -> Widgets.green "synced"
        | None, None, Some false -> Widgets.yellow "syncing"
        | None, None, None -> Widgets.dim (Context.render_spinner "")
      in
      let staleness =
        match last_block_time with
        | None -> ""
        | Some ts ->
            let age = Unix.gettimeofday () -. ts in
            if age >= 120. then Widgets.red (Printf.sprintf "Δ %.0fs" age)
            else if age >= 30. then
              Widgets.yellow (Printf.sprintf "Δ %.0fs" age)
            else Widgets.green (Printf.sprintf "Δ %.0fs" age)
      in
      let proto_s =
        match proto with
        | None -> Widgets.dim "?"
        | Some p ->
            let s = String.sub p 0 (min 8 (String.length p)) in
            if stopped then Widgets.dim s else s
      in
      let chain_s =
        match chain_id with
        | None -> Widgets.dim "?"
        | Some c ->
            let s = String.sub c 0 (min 8 (String.length c)) in
            if stopped then Widgets.dim s else s
      in
      let lvl_s = if stopped then Widgets.dim lvl else lvl in
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
    if idx + services_start_idx = selected then Widgets.bold "➤" else " "
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
  let failure_badge = if has_failure then Widgets.red " [!]" else "" in
  let instance_str = Printf.sprintf "%-16s" svc.Service.instance in
  (* For nodes: show history mode. For others: no extra info on first line *)
  let role_info =
    match svc.Service.role with
    | "node" ->
        Printf.sprintf "%-10s" (History_mode.to_string svc.Service.history_mode)
    | _ -> Printf.sprintf "%-10s" ""
  in
  let network = Printf.sprintf "%-12s" (network_short svc.Service.network) in
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
    | None -> Widgets.yellow "no signing activity"
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
    if delegate_pkhs = [] then Widgets.dim "no delegates configured"
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
                Printf.sprintf "%s:%s" short_pkh (Widgets.dim "…")
            | Some d ->
                (* Status indicators *)
                let status =
                  if d.is_forbidden then Widgets.red "FORBIDDEN"
                  else if d.deactivated then Widgets.dim "inactive"
                  else
                    (* Missed slots status *)
                    let missed = d.participation.missed_slots in
                    let remaining =
                      d.participation.remaining_allowed_missed_slots
                    in
                    match Delegate_data.missed_slots_status d with
                    | Delegate_data.Critical ->
                        Widgets.red
                          (Printf.sprintf "missed:%d/%d" missed remaining)
                    | Delegate_data.Warning ->
                        Widgets.yellow
                          (Printf.sprintf "missed:%d/%d" missed remaining)
                    | Delegate_data.Good ->
                        if missed > 0 then
                          Printf.sprintf "missed:%d/%d" missed remaining
                        else Widgets.green "ok"
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
                      if dp.denounced then Widgets.red "denounced"
                      else if
                        (not dp.sufficient_dal_participation) && attestable > 0
                      then Widgets.yellow (Printf.sprintf "dal:%s" ratio)
                      else if attestable > 0 then
                        Widgets.green (Printf.sprintf "dal:%s" ratio)
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
    | None -> Widgets.dim "health: ?"
    | Some health ->
        let status_str =
          match health.Dal_health.status with
          | Dal_health.Up -> Widgets.green "up"
          | Dal_health.Down -> Widgets.red "down"
          | Dal_health.Degraded -> Widgets.yellow "degraded"
          | Dal_health.Unknown -> Widgets.dim "?"
        in
        let checks_str =
          if health.Dal_health.checks = [] then ""
          else
            let check_strs =
              List.map
                (fun (c : Dal_health.check) ->
                  let st =
                    match c.status with
                    | Dal_health.Up -> Widgets.green "ok"
                    | Dal_health.Down -> Widgets.red "ko"
                    | Dal_health.Degraded -> Widgets.yellow "deg"
                    | Dal_health.Unknown -> "?"
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
            | Some error -> indent ^ Widgets.red ("failed: " ^ error)
            | None -> indent ^ Widgets.yellow "stopped"
          else indent ^ Widgets.yellow "stopped"
      | Service_state.Unknown msg ->
          (* Unknown status from systemd means the service failed *)
          indent ^ Widgets.red ("failed: " ^ msg)
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
          Printf.sprintf "%s%s" indent (Widgets.green "monitoring")
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
    (* Additional lines for nodes, bakers, accusers, and dal-nodes: metrics + CPU chart *)
    let extra_lines =
      match svc.Service.role with
      | "node" | "baker" | "accuser" | "dal-node" ->
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
            | Some v -> System_metrics_scheduler.format_version_colored v
            | None -> Widgets.dim "v?"
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
            @ (if mem = "" then [] else ["MEM " ^ mem])
            @
            if svc.Service.role = "node" || svc.Service.role = "dal-node" then
              let disk =
                match
                  System_metrics_scheduler.get_disk_size
                    ~role:svc.Service.role
                    ~instance:svc.Service.instance
                with
                | Some sz -> System_metrics.format_bytes sz
                | None -> Widgets.dim "?"
              in
              ["DISK " ^ disk]
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
                      Printf.sprintf "%sCPU %s %.0f%%" indent row avg
                    else Printf.sprintf "%s    %s" indent row)
                  chart_rows
          in
          baker_delegate_line @ [metrics_line] @ cpu_lines
      | _ -> []
    in
    String.concat "\n" ([first_line; second_line] @ extra_lines))

(** Role section headers *)
let role_header = function
  | "node" -> "── Nodes ──"
  | "baker" -> "── Bakers ──"
  | "accuser" -> "── Accusers ──"
  | "dal-node" -> "── DAL Nodes ──"
  | "signer" -> "── Signers ──"
  | r -> Printf.sprintf "── %s ──" (String.capitalize_ascii r)

(** Truncate a string to max visible characters, preserving ANSI codes *)
let truncate_visible ~max_width s =
  let len = String.length s in
  let buf = Buffer.create len in
  let visible = ref 0 in
  let i = ref 0 in
  while !i < len && !visible < max_width do
    let c = s.[!i] in
    if c = '\027' then (
      (* ANSI escape sequence - copy until 'm' *)
      while !i < len && s.[!i] <> 'm' do
        Buffer.add_char buf s.[!i] ;
        incr i
      done ;
      if !i < len then (
        Buffer.add_char buf 'm' ;
        incr i))
    else (
      Buffer.add_char buf c ;
      incr visible ;
      incr i)
  done ;
  (* Add reset if we truncated mid-formatting *)
  if !i < len && !visible >= max_width then Buffer.add_string buf "\027[0m" ;
  Buffer.contents buf

(** Pad or truncate a line to exact column width using visible character count *)
let pad_line ~col_width line =
  let visible_len = Miaou_helpers.Helpers.visible_chars_count line in
  if visible_len < col_width then
    line ^ String.make (col_width - visible_len) ' '
  else if visible_len > col_width then
    truncate_visible ~max_width:col_width line
  else line

(** Render a single column's content - returns list of lines *)
let render_column ~col_width ~state ~column_groups =
  let items = column_items ~column_groups ~global_services:state.services in
  let empty_line = String.make col_width ' ' in
  let _, lines =
    List.fold_left
      (fun (is_first, acc) item ->
        match item with
        | Header role_name ->
            let header = Widgets.bold role_name in
            let header_line = pad_line ~col_width header in
            (* Add empty line before header unless it's the first item *)
            let new_lines =
              if is_first then [header_line] else [empty_line; header_line]
            in
            (false, acc @ new_lines)
        | Instance (idx, svc) ->
            let is_folded =
              StringSet.mem svc.service.Service.instance state.folded
            in
            (* Render instance line(s) *)
            let line =
              line_for_service idx state.selected ~folded:is_folded svc
            in
            (* Split into individual lines and pad each *)
            let instance_lines =
              String.split_on_char '\n' line |> List.map (pad_line ~col_width)
            in
            (false, acc @ instance_lines))
      (true, [])
      items
  in
  lines

(** Dim inactive column lines to make active column stand out. *)
let dim_inactive_column line =
  (* Wrap entire line in dim formatting *)
  Printf.sprintf "\027[2m%s\027[22m" line

(** Merge multiple column renders into combined lines with per-column scrolling *)
let merge_columns ~col_width ~visible_height ~column_scroll ~active_column
    ~columns_content =
  let empty_line = String.make col_width ' ' in
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
  (* Merge columns line by line *)
  List.init visible_height (fun row_idx ->
      let parts =
        Array.to_list
          (Array.map
             (fun col ->
               if row_idx < List.length col then List.nth col row_idx
               else empty_line)
             scrolled_columns)
      in
      String.concat column_separator parts)

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

  let marker = if current_idx = selected_idx then Widgets.bold "➤" else " " in
  let fold_indicator = if folded then "+" else "−" in
  let status =
    match cfg.unit_state.active_state with
    | "active" -> Widgets.green "●"
    | "failed" -> Widgets.red "●"
    | _ -> Widgets.yellow "●"
  in

  (* First line: like managed services *)
  let instance_str = Printf.sprintf "%-16s" ext.suggested_instance_name in
  let role_str = Printf.sprintf "%-10s" role_str in
  let network = Printf.sprintf "%-12s" network_str in
  let first_line =
    Printf.sprintf
      "%s %s %s %s %s %s [external]"
      marker
      fold_indicator
      status
      instance_str
      role_str
      network
  in

  if folded then [first_line]
  else
    (* For external services, use the unit name without .service suffix as the instance *)
    let instance_for_metrics =
      let unit = cfg.unit_name in
      if String.ends_with ~suffix:".service" unit then
        String.sub unit 0 (String.length unit - 8)
      else unit
    in
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
          indent ^ "RPC: " ^ rpc
      | Some (Baker | Accuser | Dal_node) ->
          let ep =
            match cfg.node_endpoint.value with Some e -> e | None -> "?"
          in
          indent ^ "Node: " ^ ep
      | _ -> indent ^ "Status: " ^ status_str
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
      | None -> Widgets.dim "v?"
    in
    let mem =
      System_metrics_scheduler.render_mem_sparkline
        ~role:role_for_metrics
        ~instance:instance_for_metrics
        ~focus
    in
    let metrics_parts = [version] @ if mem = "" then [] else ["MEM " ^ mem] in
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
  if false then
    Format.eprintf
      "[DEBUG RENDER] external_services count: %d, selected: %d@."
      (List.length state.external_services)
      state.selected ;
  if state.external_services = [] then []
  else
    let header = Widgets.bold "── External Services ──" in
    (* Calculate base index for external services (after menu and managed services) *)
    let external_start_idx = services_start_idx + List.length state.services in
    Format.eprintf "[DEBUG RENDER] external_start_idx: %d@." external_start_idx ;
    let service_lines =
      List.mapi
        (fun idx ext ->
          let current_idx = external_start_idx + idx in
          let is_folded =
            StringSet.mem
              ext.External_service.suggested_instance_name
              state.external_folded
          in
          if false then
            Format.eprintf
              "[DEBUG RENDER] Rendering %s at idx %d (folded=%b, selected=%d)@."
              ext.External_service.suggested_instance_name
              current_idx
              is_folded
              state.selected ;
          let lines =
            render_external_service
              ~selected_idx:state.selected
              ~current_idx
              ~folded:is_folded
              ext
          in
          Format.eprintf "[DEBUG RENDER] -> %d lines@." (List.length lines) ;
          lines)
        state.external_services
      |> List.concat
    in
    if false then
      Format.eprintf
        "[DEBUG RENDER] Total service_lines: %d@."
        (List.length service_lines) ;
    header :: service_lines

(** Single-column layout (original) *)
let table_lines_single state =
  let install_row =
    let marker = if state.selected = 0 then Widgets.bold "➤" else " " in
    Printf.sprintf "%s %s" marker (Widgets.bold "[ Install new instance ]")
  in
  let instance_rows =
    if state.services = [] then ["  No managed instances."]
    else
      (* Services are already sorted by role then instance name *)
      (* Group by role and add headers - use prepend + reverse for O(n) *)
      let rec build_rows idx prev_role acc = function
        | [] -> List.rev acc
        | (svc : Service_state.t) :: rest ->
            let role = svc.service.Service.role in
            let acc =
              if Some role <> prev_role then
                Widgets.bold (role_header role) :: "" :: acc
              else acc
            in
            let is_folded =
              StringSet.mem svc.service.Service.instance state.folded
            in
            let row =
              line_for_service idx state.selected ~folded:is_folded svc
            in
            build_rows (idx + 1) (Some role) (row :: acc) rest
      in
      build_rows 0 None [] state.services
  in
  let external_rows = render_external_services_section state in
  let external_rows = if external_rows = [] then [] else "" :: external_rows in
  (install_row :: "" :: instance_rows) @ external_rows

(** Multi-column matrix layout *)
let table_lines_matrix ~cols ~visible_height ~column_scroll state =
  let num_columns =
    calc_num_columns ~cols ~min_column_width ~column_separator
  in
  let role_groups = group_by_role state.services in
  let columns = distribute_to_columns ~num_columns role_groups in
  let col_width =
    (cols - ((num_columns - 1) * String.length column_separator)) / num_columns
  in
  (* Render each column *)
  let columns_content =
    Array.map
      (fun column_groups -> render_column ~col_width ~state ~column_groups)
      columns
  in
  (* Header row (install) spans full width in single line *)
  let install_row =
    let marker = if state.selected = 0 then Widgets.bold "➤" else " " in
    Printf.sprintf "%s %s" marker (Widgets.bold "[ Install new instance ]")
  in
  (* When selection is in menu area, use -1 to dim all columns equally *)
  let effective_active_column =
    if state.selected < services_start_idx then -1 else state.active_column
  in
  let instance_rows =
    merge_columns
      ~col_width
      ~visible_height
      ~column_scroll
      ~active_column:effective_active_column
      ~columns_content
  in
  install_row :: "" :: instance_rows

let table_lines ?(cols = 80) ?(visible_height = 20) state =
  (* Clear visibility markers at start of render pass *)
  System_metrics_scheduler.clear_visibility () ;
  let num_columns =
    calc_num_columns ~cols ~min_column_width ~column_separator
  in
  if state.services = [] then
    let install_row =
      let marker = if state.selected = 0 then Widgets.bold "➤" else " " in
      Printf.sprintf "%s %s" marker (Widgets.bold "[ Install new instance ]")
    in
    let external_rows = render_external_services_section state in
    let external_rows =
      if external_rows = [] then [] else "" :: external_rows
    in
    install_row :: "" :: "  No managed instances." :: external_rows
  else if num_columns <= 1 then table_lines_single state
  else
    (* For matrix layout, subtract for menu rows (install + separator) *)
    let matrix_height = max 5 (visible_height - services_start_idx) in
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
