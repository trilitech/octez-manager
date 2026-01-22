(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
module Metrics = Rpc_metrics
module Navigation = Miaou.Core.Navigation
open Octez_manager_lib
open Rresult

let name = "instances"

(* State management extracted to instances/instances_state.ml *)
include Instances_state

(* Layout logic extracted to instances/instances_layout.ml *)
include Instances_layout

let init_state () =
  let services = load_services () in
  let external_services = load_external_services () in
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
  Navigation.make
    {
      services;
      external_services;
      selected = 0;
      folded = all_folded;
      external_folded = all_external_folded;
      last_updated = Unix.gettimeofday ();
      num_columns;
      active_column = 0;
      column_scroll = Array.make 10 0;
      (* max practical columns based on terminal width; 10 is a safe upper bound *)
    }

let force_refresh state =
  (* Trigger immediate refresh of external services *)
  External_services_scheduler.refresh () ;
  let services = load_services_fresh () in
  let external_services = load_external_services () in
  let selected = clamp_selection services external_services state.selected in
  let state =
    {
      state with
      services;
      external_services;
      selected;
      last_updated = Unix.gettimeofday ();
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
    match pending_nav with Some p -> Navigation.goto p ps | None -> ps
  in
  (* Check for pending restart dependents after edit *)
  let pending_restart = Context.take_pending_restart_dependents () in
  if pending_restart <> [] then show_restart_dependents_modal pending_restart ;
  if Context.consume_instances_dirty () || now -. state.last_updated > 1. then
    Navigation.update (fun s -> force_refresh s) ps
  else Navigation.update ensure_valid_column ps

(* Render functions extracted to instances/instances_render.ml *)
include Instances_render

(* Action handlers extracted to instances/instances_actions.ml *)
open Instances_actions

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

  let back ps =
    (* Instances is the home page - back/Esc should quit the TUI.
       Navigate to special __EXIT__ page to signal quit to the framework. *)
    Context.navigate "__EXIT__" ;
    ps

  let handled_keys () =
    Miaou.Core.Keys.
      [Enter; Char "b"; Char "c"; Char "r"; Char "R"; Char "d"; Char "x"]

  let keymap _ps =
    let activate ps = Navigation.update activate_selection ps in
    let create ps = Navigation.update create_menu_modal ps in
    let diag ps = Navigation.update go_to_diagnostics ps in
    let binaries ps = Navigation.update go_to_binaries ps in
    let dismiss ps = Navigation.update dismiss_failure ps in
    let noop ps = ps in
    let kb key action help =
      {Miaou.Core.Tui_page.key; action; help; display_only = false}
    in
    [
      kb "Enter" activate "Open";
      kb "c" create "Create";
      kb "d" diag "Diagnostics";
      kb "b" binaries "Binaries";
      kb "x" dismiss "Clear failure";
      {
        Miaou.Core.Tui_page.key = "?";
        action = noop;
        help = "Help";
        display_only = true;
      };
    ]

  let header s =
    let privilege =
      if Common.is_root () then Widgets.red "● SYSTEM"
      else Widgets.green "● USER"
    in
    let hint = "Hint: c create · b binaries · d diagnostics · ? help" in
    [
      Printf.sprintf
        "%s   %s    %s"
        (Widgets.title_highlight " octez-manager ")
        privilege
        (Widgets.dim hint);
      Widgets.dim (summary_line s);
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
    (* Set contextual help hint based on selection *)
    (match current_service s with
    | Some st when st.service.Service.role = "node" ->
        Miaou.Core.Help_hint.set (Some node_help_hint)
    | Some st when st.service.Service.role = "baker" ->
        Miaou.Core.Help_hint.set (Some baker_help_hint)
    | Some st when st.service.Service.role = "dal-node" ->
        Miaou.Core.Help_hint.set (Some dal_help_hint)
    | Some st when st.service.Service.role = "accuser" ->
        Miaou.Core.Help_hint.set (Some accuser_help_hint)
    | _ -> Miaou.Core.Help_hint.set (Some "Press Enter to select, ? for help")) ;
    (* Tick spinner and toasts each render *)
    Context.tick_spinner () ;
    Context.tick_toasts () ;
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
              if log_lines = [] then Widgets.dim "(starting...)"
              else log_lines |> take 5 |> List.rev |> String.concat "\n"
            in
            let status_str =
              match job.status with
              | Job_manager.Running -> "Running"
              | Job_manager.Pending -> "Pending"
              | Job_manager.Succeeded -> Widgets.green "Done"
              | Job_manager.Failed _ -> Widgets.red "Failed"
            in
            let phase_str =
              if job.Job_manager.phase = "" then ""
              else " " ^ Widgets.cyan ("[" ^ job.phase ^ "]")
            in
            "\n"
            ^ Widgets.dim
                (Printf.sprintf
                   "--- Job: %s (%s)%s ---"
                   job.description
                   status_str
                   phase_str)
            ^ "\n" ^ tail
      | None -> ""
    in
    let toast_lines_str = Context.render_toasts ~cols in
    Vsection.render
      ~size
      ~header:(header s)
      ~content_footer:[]
      ~child:(fun inner_size ->
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
        last_visible_height_ref := avail_rows - services_start_idx () ;
        let num_columns =
          calc_num_columns ~cols ~min_column_width ~column_separator
        in
        (* Matrix layout handles its own scrolling per-column *)
        if num_columns > 1 then
          let table = table_lines ~cols ~visible_height:avail_rows s in
          let body = String.concat "\n" table in
          let body =
            if String.trim progress = "" then body else progress ^ "\n" ^ body
          in
          let body = if job_logs = "" then body else body ^ job_logs in
          if String.length toast_lines_str > 0 then
            body ^ "\n" ^ toast_lines_str
          else body
        else
          (* Single column: use global scrolling *)
          let table = table_lines ~cols ~visible_height:avail_rows s in
          let all_lines =
            List.concat_map (fun s -> String.split_on_char '\n' s) table
          in
          let total_lines = List.length all_lines in
          (* Calculate line index where current selection starts.
             s.selected meanings:
               0 -> install menu
               1 -> separator (skipped in navigation)
               2+ -> service at index (s.selected - services_start_idx ())

             Table structure from table_lines_single:
               [install; ""; ...instance_rows...]
             where instance_rows = headers interleaved with services.

             We need to find where the selected item starts in all_lines.
          *)
          let selection_line_start, selection_line_count =
            if s.selected < services_start_idx () then
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
              (* Service selection: s.selected = services_start_idx () + service_index.
                 Count menu lines, then iterate through services
                 adding header lines when role changes. *)
              let target_svc_idx = s.selected - services_start_idx () in
              (* Menu lines: install + "" *)
              let menu_lines =
                let rec count idx acc =
                  if idx >= services_start_idx () then acc
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
            |> List.filter (fun (i, _) ->
                i >= scroll && i < scroll + avail_rows)
            |> List.map snd
          in
          let up_indicator =
            if scroll > 0 then [Widgets.dim "↑ more"] else []
          in
          let down_indicator =
            if scroll + avail_rows < total_lines then [Widgets.dim "↓ more"]
            else []
          in
          let content_lines = up_indicator @ visible_lines @ down_indicator in
          let base = String.concat "\n" content_lines in
          let body =
            if String.trim progress = "" then base else progress ^ "\n" ^ base
          in
          let body = if job_logs = "" then body else body ^ job_logs in
          if String.length toast_lines_str > 0 then
            body ^ "\n" ^ toast_lines_str
          else body)

  let check_navigation ps =
    match Context.consume_navigation () with
    | Some p -> Navigation.goto p ps
    | None -> ps

  let handle_modal_key ps key ~size:_ =
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation ps

  let is_quit_key key =
    let lower = String.lowercase_ascii key in
    lower = "esc" || lower = "escape" || lower = "c-c" || lower = "ctrl+c"
    || lower = "^c" || String.equal key "\003"

  let move_selection s delta =
    if s.services = [] && s.external_services = [] then
      (* Only Install button (0) when no services at all - ignore navigation *)
      {s with selected = 0}
    else if s.num_columns <= 1 then
      (* Single column mode: simple linear navigation *)
      let raw = s.selected + delta in
      let selected = clamp_selection s.services s.external_services raw in
      (* Skip separator during navigation *)
      let selected =
        if selected = menu_item_count () then selected + delta else selected
      in
      let selected = clamp_selection s.services s.external_services selected in
      {s with selected}
    else if
      (* Multi-column mode: navigate within current column *)
      s.selected < services_start_idx ()
    then
      (* In menu area, simple navigation *)
      let selected = max 0 (min (menu_item_count ()) (s.selected + delta)) in
      (* Jump from menu to first service in first column (column 0) *)
      if selected >= menu_item_count () && delta > 0 then
        let first_svc =
          first_service_in_column
            ~num_columns:s.num_columns
            ~services:s.services
            0
        in
        {s with selected = first_svc + services_start_idx (); active_column = 0}
      else {s with selected}
    else
      (* In services area: stay within column *)
      let current_idx = s.selected - services_start_idx () in
      (* Check if we're in external services area *)
      let external_start_idx = List.length s.services in
      let in_external = current_idx >= external_start_idx in
      if in_external then
        (* External services: linear navigation below all columns *)
        (* Check if moving up from first external service *)
        let first_external = services_start_idx () + List.length s.services in
        if s.selected = first_external && delta < 0 then
          (* Move to last service in first column (column 0), or menu if none *)
          if List.length s.services > 0 && s.num_columns > 1 then
            (* Multi-column: jump to last service in first column *)
            let col_indices =
              services_in_column
                ~num_columns:s.num_columns
                ~services:s.services
                0
            in
            match List.rev col_indices with
            | [] ->
                (* First column is empty, go to menu *)
                {s with selected = 0; active_column = 0}
            | last_idx :: _ ->
                {
                  s with
                  selected = last_idx + services_start_idx ();
                  active_column = 0;
                }
          else if List.length s.services > 0 then
            (* Single-column: jump to last managed service *)
            let last_managed =
              services_start_idx () + List.length s.services - 1
            in
            {s with selected = last_managed}
          else
            (* No managed services, jump to menu *)
            {s with selected = 0}
        else
          (* Normal external service navigation *)
          let raw = s.selected + delta in
          let selected = clamp_selection s.services s.external_services raw in
          {s with selected}
      else
        (* Managed services: column-based navigation *)
        let col_indices =
          services_in_column
            ~num_columns:s.num_columns
            ~services:s.services
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
          (* Moving up from first service goes to Install button *)
          (* Scroll to top of column *)
          s.column_scroll.(s.active_column) <- 0 ;
          {s with selected = 0})
        else if new_pos >= List.length col_indices then
          if
            (* At bottom of managed services column *)
            List.length s.external_services > 0
          then
            (* Jump to first external service *)
            let first_external =
              services_start_idx () + List.length s.services
            in
            {s with selected = first_external}
          else
            (* No external services, stay put *)
            s
        else
          let new_idx = List.nth col_indices new_pos in
          (* Adjust scroll to keep selection visible *)
          let line_start, line_count =
            service_line_position
              ~num_columns:s.num_columns
              ~services:s.services
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
          {s with selected = new_idx + services_start_idx ()}

  let force_refresh_cmd s = force_refresh s

  let toggle_fold s =
    (* Check if we're on an external service *)
    let external_start_idx = services_start_idx () + List.length s.services in
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
    else if s.selected < services_start_idx () then
      (* In menu area, left/right should do nothing - menu spans all columns *)
      s
    else
      (* In services area: move to same position in target column *)
      let current_idx = s.selected - services_start_idx () in
      let current_col_indices =
        services_in_column
          ~num_columns:num_cols
          ~services:s.services
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
        services_in_column ~num_columns:num_cols ~services:s.services new_col
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
          selected = target_idx + services_start_idx ();
        }

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
        | Some Keys.Left -> Navigation.update (fun s -> move_column s (-1)) ps
        | Some Keys.Right -> Navigation.update (fun s -> move_column s 1) ps
        | Some (Keys.Char "h") ->
            Navigation.update (fun s -> move_column s (-1)) ps
        | Some (Keys.Char "l") ->
            Navigation.update (fun s -> move_column s 1) ps
        | Some Keys.Tab -> Navigation.update toggle_fold ps
        | Some Keys.Enter -> Navigation.update activate_selection ps
        | Some (Keys.Char "c") -> Navigation.update create_menu_modal ps
        | Some (Keys.Char "x") -> Navigation.update dismiss_failure ps
        | Some (Keys.Char " ") -> Navigation.update force_refresh_cmd ps
        | Some (Keys.Char "Esc")
        | Some (Keys.Char "Escape")
        | Some (Keys.Char "q")
        | Some (Keys.Char "C-c") ->
            back ps
        | _ -> ps
      in
      (* Keep active_column in sync with selection *)
      let ps =
        let s = ps.Navigation.s in
        if s.selected >= services_start_idx () && s.num_columns > 1 then
          let svc_idx = s.selected - services_start_idx () in
          let col =
            column_for_service
              ~num_columns:s.num_columns
              ~services:s.services
              svc_idx
          in
          Navigation.update (fun s -> {s with active_column = col}) ps
        else ps
      in
      check_navigation ps

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()
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
