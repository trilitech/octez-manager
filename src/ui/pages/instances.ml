(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Flex = Miaou_widgets_layout.Flex_layout
module Keys = Miaou.Core.Keys
module Metrics = Rpc_metrics
module Navigation = Miaou.Core.Navigation
module Style = Miaou_style.Style
module Style_context = Miaou_style.Style_context
open Octez_manager_lib
open Rresult

let name = "instances"

(* State management extracted to instances/instances_state.ml *)
include Instances_state

(* Layout logic extracted to instances/instances_layout.ml *)
include Instances_layout

let init_state () =
  let services = load_services () in
  let external_services = External_services_scheduler.get () in
  let groups = load_groups () in
  (* Start with all managed instances folded by default *)
  let all_folded =
    List.fold_left
      (fun acc (st : Service_state.t) ->
        StringSet.add st.service.Service.instance acc)
      StringSet.empty
      services
  in
  (* Start with all external instances folded by default *)
  let all_external_folded =
    List.fold_left
      (fun acc (ext : External_service.t) ->
        StringSet.add ext.suggested_instance_name acc)
      StringSet.empty
      external_services
  in
  (* Default to 1 column, will be updated on first render with actual cols *)
  let num_columns = 1 in
  (* Default to group view if groups exist, else role view *)
  let view_mode = match groups with _ :: _ -> By_group | [] -> By_role in
  Navigation.make
    {
      services;
      external_services;
      selected = 0;
      folded = all_folded;
      external_folded = all_external_folded;
      external_section_folded = true;
      last_updated = Unix.gettimeofday ();
      num_columns;
      active_column = 0;
      column_scroll = Array.make 10 0;
      (* max practical columns based on terminal width; 10 is a safe upper bound *)
      view_mode;
      groups;
      create_menu_open = false;
      create_menu_cursor = 0;
    }

let force_refresh state =
  (* Trigger immediate refresh of external services *)
  External_services_scheduler.refresh () ;
  let services = load_services_fresh () in
  let external_services = External_services_scheduler.get () in
  let groups = load_groups () in
  let ext_for_clamp =
    if state.external_section_folded then [] else external_services
  in
  let selected = clamp_selection services ext_for_clamp state.selected in
  (* Auto-fold newly discovered external services (not seen in previous state).
     Services already known keep their user-set fold state unchanged. *)
  let prev_names =
    List.fold_left
      (fun acc (ext : External_service.t) ->
        StringSet.add ext.suggested_instance_name acc)
      StringSet.empty
      state.external_services
  in
  let external_folded =
    List.fold_left
      (fun acc (ext : External_service.t) ->
        let name = ext.suggested_instance_name in
        if StringSet.mem name prev_names then acc else StringSet.add name acc)
      state.external_folded
      external_services
  in
  let state =
    {
      state with
      services;
      external_services;
      groups;
      selected;
      last_updated = Unix.gettimeofday ();
      external_folded;
    }
  in
  ensure_valid_column state

let show_restart_dependents_modal dependents =
  let restart_all () =
    let cap = Miaou_interfaces.Service_lifecycle.require () in
    dependents
    |> List.iter (fun instance ->
        (* Find the service to get its role *)
        match Service_registry.find ~instance with
        | Ok (Some svc) -> (
            Context.toast_info (Printf.sprintf "Starting %s..." instance) ;
            match
              Miaou_interfaces.Service_lifecycle.start
                cap
                ~role:svc.Service.role
                ~service:instance
            with
            | Ok () ->
                Context.toast_success (Printf.sprintf "%s started" instance)
            | Error msg ->
                Context.toast_error (Printf.sprintf "%s: %s" instance msg))
        | _ ->
            Context.toast_error (Printf.sprintf "Service %s not found" instance)) ;
    Context.mark_instances_dirty ()
  in
  Modal_helpers.open_choice_modal
    ~title:"Restart Stopped Dependents"
    ~items:[`RestartAll; `Dismiss]
    ~to_string:(function
      | `RestartAll ->
          Printf.sprintf "Restart all (%s)" (String.concat ", " dependents)
      | `Dismiss -> "Dismiss (restart later)")
    ~on_select:(function `RestartAll -> restart_all () | `Dismiss -> ())
    ()

let maybe_refresh ps =
  let state = ps.Navigation.s in
  let now = Unix.gettimeofday () in
  let pending_nav = Context.consume_navigation () in
  let ps =
    match pending_nav with
    | Some (Context.Goto p) -> Navigation.goto p ps
    | Some Context.Back -> Navigation.back ps
    | Some Context.Quit -> Navigation.quit ps
    | None -> ps
  in
  (* Check for pending restart dependents after edit *)
  let pending_restart = Context.take_pending_restart_dependents () in
  if pending_restart <> [] then show_restart_dependents_modal pending_restart ;
  let dirty = Context.consume_instances_dirty () in
  if dirty then
    (* Explicit mutation (install, edit, group change, etc.) — bypass the Data
       cache so force_refresh picks up the latest registry state immediately. *)
    Data.force_refresh () ;
  if dirty || now -. state.last_updated > 1. then
    Navigation.update (fun s -> force_refresh s) ps
  else Navigation.update ensure_valid_column ps

(* Render functions extracted to instances/instances_render.ml *)
include Instances_render

(* Action handlers extracted to instances/instances_actions.ml *)
open Instances_actions

(** Single-column navigation: linear up/down with separator skipping.
    Layout: 0-2 = buttons, 3 = radio row (navigable), 4 = separator (skipped),
    5+ = services. *)
let move_selection_single_column s delta =
  let ext = if s.external_section_folded then [] else s.external_services in
  let raw = s.selected + delta in
  let selected = clamp_selection s.services ext raw in
  (* Skip only the separator (index menu_item_count+1 = 4); the radio row (3) is navigable *)
  let sep_idx = menu_item_count + 1 in
  let selected =
    if selected >= sep_idx && selected < services_start_idx then
      if delta > 0 then services_start_idx else menu_item_count
    else selected
  in
  let selected = clamp_selection s.services ext selected in
  {s with selected}

(** Multi-column: navigate within the menu area (indices 0..menu_item_count).
    The radio row at menu_item_count is the last "menu-like" navigable item.
    When pressing Down past the radio row, jump to first service in column 0. *)
let move_selection_menu s delta =
  let selected = max 0 (min (menu_item_count + 1) (s.selected + delta)) in
  if selected >= menu_item_count + 1 && delta > 0 then
    let sections = sections_of_state s in
    let first_svc =
      first_service_in_column
        ~num_columns:s.num_columns
        ~sections
        ~services:(display_ordered_services s)
        0
    in
    {s with selected = first_svc + services_start_idx; active_column = 0}
  else {s with selected}

(** Multi-column: navigate within the external services section
    (below all column-distributed managed services). *)
let move_selection_external s delta =
  let first_external = services_start_idx + List.length s.services in
  if s.selected = first_external && delta < 0 then
    (* Moving up from first external service *)
    if List.length s.services > 0 && s.num_columns > 1 then
      let sections = sections_of_state s in
      let col_indices =
        services_in_column
          ~num_columns:s.num_columns
          ~sections
          ~services:(display_ordered_services s)
          0
      in
      match List.rev col_indices with
      | [] -> {s with selected = menu_item_count; active_column = 0}
      | last_idx :: _ ->
          {s with selected = last_idx + services_start_idx; active_column = 0}
    else if List.length s.services > 0 then
      let last_managed = services_start_idx + List.length s.services - 1 in
      {s with selected = last_managed}
    else {s with selected = menu_item_count}
  else
    let ext = if s.external_section_folded then [] else s.external_services in
    let raw = s.selected + delta in
    let selected = clamp_selection s.services ext raw in
    {s with selected}

(** Multi-column: navigate within managed services, constrained to the
    active column.  Moving up past the first service goes to the last menu
    item; moving down past the last service goes to external services. *)
let move_selection_managed s delta =
  let current_idx = s.selected - services_start_idx in
  let sections = sections_of_state s in
  let col_indices =
    services_in_column
      ~num_columns:s.num_columns
      ~sections
      ~services:(display_ordered_services s)
      s.active_column
  in
  let current_pos =
    List.find_mapi
      (fun i idx -> if idx = current_idx then Some i else None)
      col_indices
    |> Option.value ~default:0
  in
  let new_pos = current_pos + delta in
  if new_pos < 0 then (
    s.column_scroll.(s.active_column) <- 0 ;
    {s with selected = menu_item_count})
  else if new_pos >= List.length col_indices then
    if List.length s.external_services > 0 then
      let first_external = services_start_idx + List.length s.services in
      {s with selected = first_external}
    else s
  else
    let new_idx = List.nth col_indices new_pos in
    let line_start, line_count =
      service_line_position
        ~num_columns:s.num_columns
        ~sections
        ~services:(display_ordered_services s)
        ~folded:s.folded
        new_idx
        s.active_column
    in
    adjust_column_scroll
      ~column_scroll:s.column_scroll
      ~col:s.active_column
      ~line_start
      ~line_count
      ~visible_height:!last_visible_height_ref ;
    {s with selected = new_idx + services_start_idx}

(** Move selection up or down by [delta] steps, handling menu items,
    separator skipping, and multi-column navigation. *)
let move_selection s delta =
  if s.services = [] && s.external_services = [] then {s with selected = 0}
  else if s.num_columns <= 1 then move_selection_single_column s delta
  else if s.selected < services_start_idx then move_selection_menu s delta
  else
    let current_idx = s.selected - services_start_idx in
    let in_external = current_idx >= List.length s.services in
    if in_external then move_selection_external s delta
    else move_selection_managed s delta

module For_tests = struct
  let move_selection = move_selection

  let open_create_menu ps =
    Navigation.update
      (fun s -> {s with create_menu_open = true; create_menu_cursor = 0})
      ps
end

module Page_Impl :
  Miaou.Core.Tui_page.PAGE_SIG with type state = state and type msg = msg =
struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init () = init_state ()

  let update ps _ = ps

  let refresh = maybe_refresh

  let move ps _ = ps

  let service_select ps _ = ps

  let service_cycle ps _ = refresh ps

  let back ps = Navigation.back ps

  let handled_keys () =
    Miaou.Core.Keys.[Enter; Char "g"; Char "G"; Char "d"; Char "t"; Char "x"]

  let keymap _ps =
    let noop ps = ps in
    let kb key help =
      {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
    in
    [
      kb "Enter" "Open";
      kb "g" "Group/Role view";
      kb "G" "Group actions";
      kb "d" "Diagnostics";
      kb "x" "Clear failure";
      kb "?" "Help";
    ]

  let header s =
    let privilege =
      if Paths.is_root () then Widgets.themed_error "● SYSTEM"
      else Widgets.themed_success "● USER"
    in
    [
      Printf.sprintf
        "%s   %s"
        (Widgets.themed_primary " octez-manager ")
        privilege;
      Widgets.themed_secondary (summary_line s);
    ]

  let node_help_hint =
    {|## Node Instance

**Line 1:** Instance status
- `●` running, `○` stopped
- `[enabled]` starts on boot

**Line 2:** RPC status
- `synced`/`syncing` = bootstrap state
- `L12345` = head level
- Protocol & chain ID (8 chars)
- `Δ` = time since last block
- `no rpc` = node not responding

**Line 3:** System metrics
- Version: green=latest, yellow=outdated, red=deprecated, blue=RC
- `MEM` = memory sparkline
- `DISK` = data directory size

**Line 4+:** CPU usage chart

Press **Enter** to open instance menu.|}

  let baker_help_hint =
    {|## Baker Instance

**Line 1:** Instance status
- `●` running, `○` stopped
- `[enabled]` starts on boot

**Line 2:** Signing activity (local baker data)
- Read from `<base_dir>/<chain>_highwatermarks`
- Shows last signed level per delegate
- `no signing activity` = no blocks/attestations signed yet

**Line 3:** Delegate status (from chain RPC)
- Fetched from node every 60s (head~2 for stability)
- `pkh:ok` = no missed slots (green)
- `pkh:missed:N/M` = missed slots vs remaining allowed
  - Yellow: missed >= remaining/2
  - Red: missed > remaining (CRITICAL)
- `pkh:inactive` = delegate is deactivated
- `pkh:FORBIDDEN` = delegate is forbidden (red alert)
- `pkh:…` = data not yet fetched

**Line 4:** System metrics (local process)
- Version: from `--version` output
- `MEM` = RSS memory usage sparkline

**Line 5+:** CPU usage chart (braille)

Press **Enter** to open instance menu.|}

  let dal_help_hint =
    {|## DAL Node Instance

**Line 1:** Instance status
- `●` running, `○` stopped
- `[enabled]` starts on boot

**Line 2:** Health status (from /health RPC)
- `health: up` (green) = all checks passing
- `health: degraded` (yellow) = partial issues
- `health: down` (red) = node unhealthy
- Individual check statuses shown if available

**Line 3:** System metrics
- Version: from `--version` output
- `MEM` = RSS memory usage sparkline
- `DISK` = DAL node data directory size

**Line 4+:** CPU usage chart (braille)

Press **Enter** to open instance menu.|}

  let accuser_help_hint =
    {|## Accuser Instance

**Line 1:** Instance status
- `●` running (green), `○` stopped (yellow), `●` failed (red)
- `[enabled]` starts on boot

**Line 2:** Activity status
- `monitoring` (green) = accuser is watching for double-baking/endorsing

The accuser monitors the chain for misbehavior and
automatically submits denunciation operations when detected.

Press **Enter** to open instance menu.|}

  (* Mutable scroll offset - updated during view to keep selection visible *)
  let scroll_offset_ref = ref 0

  let take n l =
    let rec loop acc n = function
      | [] -> List.rev acc
      | _ when n <= 0 -> List.rev acc
      | x :: xs -> loop (x :: acc) (n - 1) xs
    in
    loop [] n l

  let view ps ~focus:_ ~size =
    let s = ps.Navigation.s in
    (* Set zone-conditional help hints.  Skipped while a modal is active
       because modals manage their own hints via push/pop. *)
    if not (Miaou.Core.Modal_manager.has_active ()) then (
      let has_failure_at_selected () =
        match current_service s with
        | None -> false
        | Some st ->
            Option.is_some
              (get_recent_failure ~instance:st.service.Service.instance)
      in
      let hint_short, hint_long =
        if s.create_menu_open then
          ( "↑↓: select  ·  Enter: open  ·  Esc: cancel",
            "↑↓: select  ·  Enter: open install form  ·  Esc: cancel" )
        else if s.selected = menu_item_count then
          ( "1: ⊕ new  ·  ←/→: Switch view  g: Toggle  K: Wallets  ?: Help",
            "1: new instance  ←/h: By Role  →/l: By Group  g: Toggle view  K: \
             Wallets  d: Diagnostics  ?: Help" )
        else if has_failure_at_selected () then
          ( "Enter: Actions  Tab: Fold  x: Dismiss  K: Wallets  ?: Help",
            "Enter: Actions  Tab: Fold/unfold  x: Clear failure  G: Group \
             actions  K: Wallets  ?: Help" )
        else
          let long_hint =
            match current_service s with
            | Some st when String.equal st.service.Service.role "node" ->
                node_help_hint
            | Some st when String.equal st.service.Service.role "baker" ->
                baker_help_hint
            | Some st when String.equal st.service.Service.role "dal-node" ->
                dal_help_hint
            | Some st when String.equal st.service.Service.role "accuser" ->
                accuser_help_hint
            | _ ->
                "Enter: Actions  Tab: Fold/unfold  G: Group actions  K: \
                 Wallets  d: Diagnostics  ?: Help"
          in
          let unmanaged_hint =
            if s.external_services <> [] then
              if s.external_section_folded then "  u: Show unmanaged"
              else "  u: Hide unmanaged"
            else ""
          in
          ( "Enter: Actions  Tab: Fold  G: Groups  K: Wallets  ?: Help"
            ^ unmanaged_hint,
            long_hint ^ unmanaged_hint )
      in
      Miaou.Core.Help_hint.clear () ;
      Miaou.Core.Help_hint.push ~short:hint_short ~long:hint_long ()) ;
    (* Tick spinner and toasts each render *)
    Context.tick_spinner () ;
    Context.tick_toasts () ;
    Job_manager.tick () ;
    let cols = size.LTerm_geom.cols in
    let progress = Context.render_progress ~cols in
    (* Render active or recent job logs *)
    let job_logs =
      match Job_manager.get_latest_job () with
      | Some job ->
          let is_relevant =
            match job.status with
            | Job_manager.Running | Job_manager.Pending -> true
            | _ -> (
                match job.finished_at with
                | Some t -> Unix.gettimeofday () -. t < 10.0
                | None -> true)
          in
          if not is_relevant then ""
          else
            let log_lines = job.Job_manager.log in
            let tail =
              if log_lines = [] then Widgets.themed_muted "(starting...)"
              else log_lines |> take 5 |> List.rev |> String.concat "\n"
            in
            let status_str =
              match job.status with
              | Job_manager.Running ->
                  let elapsed =
                    Unix.gettimeofday () -. job.Job_manager.started_at
                  in
                  Printf.sprintf "Running (%.0fs)" elapsed
              | Job_manager.Pending -> "Pending"
              | Job_manager.Succeeded -> Widgets.themed_success "Done"
              | Job_manager.Failed msg ->
                  Widgets.themed_error (Printf.sprintf "Failed: %s" msg)
            in
            let phase_str =
              if job.Job_manager.phase = "" then ""
              else " " ^ Widgets.themed_accent ("[" ^ job.phase ^ "]")
            in
            "\n"
            ^ Widgets.themed_muted
                (Printf.sprintf
                   "--- Job: %s (%s)%s ---"
                   job.description
                   status_str
                   phase_str)
            ^ "\n" ^ tail
      | None -> ""
    in
    let toast_lines_str = Context.render_toasts ~cols in
    let header_lines = header s in
    let header_block = String.concat "\n" header_lines in
    let separator = Widgets.themed_border (Widgets.hr ~width:cols ()) in
    let render_body ~size:inner_size =
      (* Available rows for content (reserve space for progress/toasts/logs) *)
      let progress_lines =
        if String.trim progress = "" then 0
        else List.length (String.split_on_char '\n' progress)
      in
      let log_lines_count =
        if job_logs = "" then 0
        else List.length (String.split_on_char '\n' job_logs)
      in
      let toast_lines =
        if String.length toast_lines_str = 0 then 0
        else List.length (String.split_on_char '\n' toast_lines_str)
      in
      let avail_rows =
        inner_size.LTerm_geom.rows - progress_lines - log_lines_count
        - toast_lines - 1
      in
      let avail_rows = max 5 avail_rows in
      (* Update visible height for scroll calculations - subtract menu rows *)
      last_visible_height_ref := avail_rows - services_start_idx ;
      let num_columns =
        calc_num_columns ~cols ~min_column_width ~column_separator
      in
      (* Matrix layout handles its own scrolling per-column *)
      if num_columns > 1 then
        let table = table_lines ~cols ~visible_height:avail_rows s in
        let body = String.concat "\n" table in
        let body =
          if s.create_menu_open then
            render_create_dropdown s.create_menu_cursor ^ "\n" ^ body
          else body
        in
        let body =
          if String.trim progress = "" then body else progress ^ "\n" ^ body
        in
        let body = if job_logs = "" then body else body ^ job_logs in
        if String.length toast_lines_str > 0 then body ^ "\n" ^ toast_lines_str
        else body
      else
        (* Single column: use global scrolling *)
        let table = table_lines ~cols ~visible_height:avail_rows s in
        let dropdown_lines =
          if s.create_menu_open then
            String.split_on_char
              '\n'
              (render_create_dropdown s.create_menu_cursor)
          else []
        in
        let all_lines =
          dropdown_lines
          @ List.concat_map (fun s -> String.split_on_char '\n' s) table
        in
        let total_lines = List.length all_lines in
        (* Calculate line index where current selection starts.
           s.selected meanings:
             0 -> radio row (view mode)
             1 -> separator (skipped in navigation)
             2+ -> service at index (s.selected - services_start_idx)

           Table structure from table_lines_single:
             [view_row; ""; ...instance_rows...]
           where instance_rows = headers interleaved with services.

           We need to find where the selected item starts in all_lines.
        *)
        let selection_line_start, selection_line_count =
          if s.selected < services_start_idx then
            (* Menu items: count lines for entries 0..s.selected-1 *)
            let line_start =
              let rec count idx acc =
                if idx >= s.selected then acc
                else if idx >= List.length table then acc
                else
                  let entry = List.nth table idx in
                  let lines = String.split_on_char '\n' entry in
                  count (idx + 1) (acc + List.length lines)
              in
              count 0 0
            in
            let line_count =
              if s.selected >= List.length table then 1
              else
                List.length
                  (String.split_on_char '\n' (List.nth table s.selected))
            in
            (line_start, line_count)
          else
            (* Service selection: s.selected = services_start_idx + service_index.
               Count menu lines, then iterate through services
               adding header lines when role changes. *)
            let target_svc_idx = s.selected - services_start_idx in
            (* Menu lines: install + "" *)
            let menu_lines =
              let rec count idx acc =
                if idx >= services_start_idx then acc
                else if idx >= List.length table then acc
                else
                  let entry = List.nth table idx in
                  count
                    (idx + 1)
                    (acc + List.length (String.split_on_char '\n' entry))
              in
              count 0 0
            in
            (* Count lines through services until target *)
            let rec count_service_lines svc_idx prev_role acc services =
              match services with
              | [] -> (acc, 1) (* fallback *)
              | (st : Service_state.t) :: rest ->
                  let role = st.service.Service.role in
                  (* Add header lines if role changed *)
                  let header_lines =
                    if Some role <> prev_role then
                      (* Role header + empty line before it (except first) *)
                      if prev_role = None then 1 else 2
                    else 0
                  in
                  let acc = acc + header_lines in
                  if svc_idx = target_svc_idx then
                    (* Found target service *)
                    let is_folded =
                      StringSet.mem st.service.Service.instance s.folded
                    in
                    let line_count = if is_folded then 2 else 6 in
                    (acc, line_count)
                  else
                    (* Count this service's lines and continue *)
                    let is_folded =
                      StringSet.mem st.service.Service.instance s.folded
                    in
                    let svc_lines = if is_folded then 2 else 6 in
                    count_service_lines
                      (svc_idx + 1)
                      (Some role)
                      (acc + svc_lines)
                      rest
            in
            let svc_line_start, line_count =
              count_service_lines 0 None 0 s.services
            in
            (menu_lines + svc_line_start, line_count)
        in
        (* Adjust scroll offset to keep selection visible *)
        let scroll = !scroll_offset_ref in
        let scroll =
          if selection_line_start < scroll then selection_line_start
          else if
            selection_line_start + selection_line_count > scroll + avail_rows
          then selection_line_start + selection_line_count - avail_rows
          else scroll
        in
        (* Clamp scroll to valid range *)
        let scroll = max 0 (min scroll (max 0 (total_lines - avail_rows))) in
        scroll_offset_ref := scroll ;
        let visible_lines =
          all_lines
          |> List.mapi (fun i l -> (i, l))
          |> List.filter (fun (i, _) -> i >= scroll && i < scroll + avail_rows)
          |> List.map snd
        in
        let up_indicator =
          if scroll > 0 then [Widgets.themed_muted "↑ more"] else []
        in
        let down_indicator =
          if scroll + avail_rows < total_lines then
            [Widgets.themed_muted "↓ more"]
          else []
        in
        let content_lines = up_indicator @ visible_lines @ down_indicator in
        let base = String.concat "\n" content_lines in
        let body =
          if String.trim progress = "" then base else progress ^ "\n" ^ base
        in
        let body = if job_logs = "" then body else body ^ job_logs in
        if String.length toast_lines_str > 0 then body ^ "\n" ^ toast_lines_str
        else body
    in
    let layout =
      Flex.create
        ~direction:Flex.Column
        [
          {
            render = (fun ~size:_ -> header_block);
            basis = Flex.Px (List.length header_lines);
            cross = None;
          };
          {render = (fun ~size:_ -> separator); basis = Flex.Px 1; cross = None};
          {render = render_body; basis = Flex.Fill; cross = None};
        ]
    in
    let rendered = Flex.render layout ~size in
    let bg_style = Style_context.background () in
    let resolved = Style.to_resolved bg_style in
    let rows = size.LTerm_geom.rows in
    let lines = String.split_on_char '\n' rendered in
    let line_count = List.length lines in
    let lines =
      if line_count < rows then
        lines @ List.init (rows - line_count) (fun _ -> "")
      else if line_count > rows then take rows lines
      else lines
    in
    let lines =
      if resolved.Style.r_bg < 0 then lines
      else
        List.map
          (fun line ->
            let padded = Widgets.pad_to_cols_line ~cols line in
            Widgets.apply_bg_fill ~bg:resolved.Style.r_bg padded)
          lines
    in
    String.concat "\n" lines

  let check_navigation ps =
    match Context.consume_navigation () with
    | Some (Context.Goto p) -> Navigation.goto p ps
    | Some Context.Back -> Navigation.back ps
    | Some Context.Quit -> Navigation.quit ps
    | None -> ps

  let handle_modal_key ps key ~size:_ =
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation ps

  let is_quit_key key =
    let lower = String.lowercase_ascii key in
    lower = "esc" || lower = "escape" || lower = "c-c" || lower = "ctrl+c"
    || lower = "^c" || String.equal key "\003"

  let move_selection s delta = move_selection s delta

  let force_refresh_cmd s = force_refresh s

  let toggle_fold s =
    (* Check if we're on an external service *)
    let external_start_idx = services_start_idx + List.length s.services in
    if s.selected >= external_start_idx then
      (* Toggle external service *)
      let ext_idx = s.selected - external_start_idx in
      match List.nth_opt s.external_services ext_idx with
      | None -> s
      | Some ext ->
          let inst = ext.External_service.suggested_instance_name in
          let external_folded =
            if StringSet.mem inst s.external_folded then
              StringSet.remove inst s.external_folded
            else StringSet.add inst s.external_folded
          in
          {s with external_folded}
    else
      (* Toggle managed service *)
      match current_service s with
      | None -> s (* In menu area, Tab does nothing now *)
      | Some st ->
          let inst = st.service.Service.instance in
          let folded =
            if StringSet.mem inst s.folded then StringSet.remove inst s.folded
            else StringSet.add inst s.folded
          in
          {s with folded}

  (** Move to a different column (for matrix layout) *)
  let move_column s delta =
    let num_cols = s.num_columns in
    if num_cols <= 1 then s
    else if s.selected < services_start_idx then
      (* In menu area, left/right should do nothing - menu spans all columns *)
      s
    else
      (* In services area: move to same position in target column *)
      let current_idx = s.selected - services_start_idx in
      let sections = sections_of_state s in
      let ordered = display_ordered_services s in
      let current_col_indices =
        services_in_column
          ~num_columns:num_cols
          ~sections
          ~services:ordered
          s.active_column
      in
      let current_pos =
        List.find_mapi
          (fun i idx -> if idx = current_idx then Some i else None)
          current_col_indices
        |> Option.value ~default:0
      in
      let new_col = (s.active_column + delta + num_cols) mod num_cols in
      let target_col_indices =
        services_in_column
          ~num_columns:num_cols
          ~sections
          ~services:ordered
          new_col
      in
      if target_col_indices = [] then
        (* Target column is empty, stay in current column *)
        s
      else
        (* Move to same position (clamped) in target column *)
        let target_pos = min current_pos (List.length target_col_indices - 1) in
        let target_idx = List.nth target_col_indices target_pos in
        {
          s with
          active_column = new_col;
          selected = target_idx + services_start_idx;
        }

  let create_menu_items =
    [|"Node"; "Baker"; "DAL Node"; "Accuser"; "Signatory"|]

  let navigate_create_item cursor =
    match cursor with
    | 0 -> Context.navigate Install_node_form_v3.name
    | 1 -> Context.navigate Install_baker_form_v3.name
    | 2 -> Context.navigate Install_dal_node_form_v3.name
    | 3 -> Context.navigate Install_accuser_form_v3.name
    | 4 -> Context.navigate Install_signatory_form.name
    | _ -> ()

  let handle_key ps key ~size =
    let s = ps.Navigation.s in
    (* Update num_columns based on current terminal size *)
    let cols = size.LTerm_geom.cols in
    let num_columns =
      calc_num_columns ~cols ~min_column_width ~column_separator
    in
    let s = {s with num_columns} in
    let ps = Navigation.update (fun _ -> s) ps in
    if Miaou.Core.Modal_manager.has_active () then (
      Miaou.Core.Modal_manager.handle_key key ;
      check_navigation ps)
    else if s.create_menu_open then
      (* Create dropdown is open: handle its navigation *)
      let max_cursor = Array.length create_menu_items - 1 in
      let ps =
        match Keys.of_string key with
        | Some Keys.Up | Some (Keys.Char "k") ->
            Navigation.update
              (fun s ->
                {s with create_menu_cursor = max 0 (s.create_menu_cursor - 1)})
              ps
        | Some Keys.Down | Some (Keys.Char "j") ->
            Navigation.update
              (fun s ->
                {
                  s with
                  create_menu_cursor = min max_cursor (s.create_menu_cursor + 1);
                })
              ps
        | Some Keys.Enter ->
            navigate_create_item s.create_menu_cursor ;
            Navigation.update (fun s -> {s with create_menu_open = false}) ps
        | Some Keys.Escape ->
            Navigation.update (fun s -> {s with create_menu_open = false}) ps
        | _ -> ps
      in
      check_navigation ps
    else if is_quit_key key then back ps
    else
      let ps =
        match Keys.of_string key with
        | Some Keys.Up -> Navigation.update (fun s -> move_selection s (-1)) ps
        | Some Keys.Down -> Navigation.update (fun s -> move_selection s 1) ps
        | Some (Keys.Char "k") ->
            Navigation.update (fun s -> move_selection s (-1)) ps
        | Some (Keys.Char "j") ->
            Navigation.update (fun s -> move_selection s 1) ps
        | Some Keys.Left ->
            Navigation.update
              (fun s ->
                if s.selected = menu_item_count then
                  {s with view_mode = By_role}
                else move_column s (-1))
              ps
        | Some Keys.Right ->
            Navigation.update
              (fun s ->
                if s.selected = menu_item_count then
                  {s with view_mode = By_group}
                else move_column s 1)
              ps
        | Some (Keys.Char "h") ->
            Navigation.update
              (fun s ->
                if s.selected = menu_item_count then
                  {s with view_mode = By_role}
                else move_column s (-1))
              ps
        | Some (Keys.Char "l") ->
            Navigation.update
              (fun s ->
                if s.selected = menu_item_count then
                  {s with view_mode = By_group}
                else move_column s 1)
              ps
        | Some Keys.Tab -> Navigation.update toggle_fold ps
        | Some Keys.Enter -> Navigation.update activate_selection ps
        | Some (Keys.Char "g") ->
            Navigation.update
              (fun s ->
                let view_mode =
                  match s.view_mode with
                  | By_role -> By_group
                  | By_group -> By_role
                in
                {s with view_mode})
              ps
        | Some (Keys.Char "G") -> Navigation.update group_actions_modal ps
        | Some (Keys.Char "b") ->
            (* b still works as a shortcut to Binaries tab *)
            Context.set_pending_tab Context.Tab_binaries ;
            ps
        | Some (Keys.Char "d") -> Navigation.update go_to_diagnostics ps
        | Some (Keys.Char "t") -> Navigation.update go_to_topology ps
        | Some (Keys.Char "r") ->
            (* r still works as a shortcut to RPCs tab *)
            Context.set_pending_tab Context.Tab_rpcs ;
            ps
        | Some (Keys.Char "u") ->
            (* Toggle the Unmanaged Instances section fold *)
            Navigation.update
              (fun s ->
                if s.external_services = [] then s
                else
                  let ext_section_folded = not s.external_section_folded in
                  (* When folding, clamp cursor away from now-hidden external items *)
                  let ext =
                    if ext_section_folded then [] else s.external_services
                  in
                  let selected = clamp_selection s.services ext s.selected in
                  {
                    s with
                    external_section_folded = ext_section_folded;
                    selected;
                  })
              ps
        | Some (Keys.Char "x") -> Navigation.update dismiss_failure ps
        | Some (Keys.Char " ") -> Navigation.update force_refresh_cmd ps
        | Some (Keys.Char "q") | Some (Keys.Char "C-c") -> back ps
        | _ -> ps
      in
      (* Keep active_column in sync with selection *)
      let ps =
        let s = ps.Navigation.s in
        if s.selected >= services_start_idx && s.num_columns > 1 then
          let svc_idx = s.selected - services_start_idx in
          let sections = sections_of_state s in
          let col =
            column_for_service
              ~num_columns:s.num_columns
              ~sections
              ~services:(display_ordered_services s)
              svc_idx
          in
          Navigation.update (fun s -> {s with active_column = col}) ps
        else ps
      in
      check_navigation ps

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()

  let on_key ps key ~size =
    let ps' = handle_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let key_hints _ps = []
end

module Page =
  Monitored_page.Make
    (Page_Impl)
    (struct
      let page_name = "instances"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
