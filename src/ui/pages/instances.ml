(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
module Service_state = Data.Service_state
module Metrics = Rpc_metrics
open Octez_manager_lib
open Rresult

let ( let* ) = Result.bind

let name = "instances"

module StringSet = Set.Make (String)

type state = {
  services : Service_state.t list;
  selected : int;
  folded : StringSet.t; (* instance names that are folded *)
  last_updated : float;
  next_page : string option;
}

type msg = unit

let clamp_selection services idx =
  let len = List.length services + 3 in
  max 0 (min idx (len - 1))

let load_services () = Data.load_service_states ()

let load_services_fresh () = Data.load_service_states ~detail:false ()

let init_state () =
  let services = load_services () in
  {
    services;
    selected = 0;
    folded = StringSet.empty;
    last_updated = Unix.gettimeofday ();
    next_page = None;
  }

let force_refresh state =
  let services = load_services_fresh () in
  let selected = clamp_selection services state.selected in
  {state with services; selected; last_updated = Unix.gettimeofday ()}

let maybe_refresh state =
  let now = Unix.gettimeofday () in
  let pending_nav = Context.consume_navigation () in
  let state =
    match pending_nav with
    | Some p -> {state with next_page = Some p}
    | None -> {state with next_page = None}
    (* Clear next_page after navigation consumed *)
  in
  if Context.consume_instances_dirty () || now -. state.last_updated > 5. then
    force_refresh state
  else state

let current_service state =
  if state.selected < 3 then None
  else List.nth_opt state.services (state.selected - 3)

let with_service state handler =
  match current_service state with
  | None ->
      Modal_helpers.show_error ~title:"Instances" "Select an instance first" ;
      state
  | Some svc -> handler svc

let status_icon (st : Service_state.t) =
  match st.Service_state.status with
  | Service_state.Running -> Widgets.green "●"
  | Service_state.Stopped -> Widgets.yellow "○"
  | Service_state.Unknown msg ->
      Widgets.red ("?" ^ if msg = "" then "" else " " ^ msg)

let enabled_badge (st : Service_state.t) =
  match st.Service_state.enabled with
  | Some true -> Widgets.dim "[enabled]"
  | Some false -> Widgets.dim "[disabled]"
  | None -> Widgets.dim "[unknown]"

let rpc_status_line ~(service_status : Service_state.status) (svc : Service.t) =
  let stopped =
    match service_status with Service_state.Running -> false | _ -> true
  in
  (* Show service status when not running *)
  let service_prefix =
    match service_status with
    | Service_state.Running -> None
    | Service_state.Stopped -> Some (Widgets.yellow "stopped")
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
  let marker = if idx + 3 = selected then Widgets.bold "➤" else " " in
  let status = status_icon st in
  let enabled = enabled_badge st in
  let role_str =
    let r = svc.Service.role in
    let padded = Printf.sprintf "%-10s" r in
    match r with
    | "node" -> Widgets.blue padded
    | "baker" -> Widgets.yellow padded
    | "accuser" -> Widgets.magenta padded
    | _ -> padded
  in
  let instance_str = Printf.sprintf "%-16s" svc.Service.instance in
  let history =
    Printf.sprintf "%-10s" (History_mode.to_string svc.Service.history_mode)
  in
  let network = Printf.sprintf "%-12s" (network_short svc.Service.network) in
  let fold_indicator = if folded then "▸" else "▾" in
  let first_line =
    Printf.sprintf
      "%s %s %s %s %s %s %s %s"
      marker
      fold_indicator
      status
      instance_str
      role_str
      history
      network
      enabled
  in
  (* If folded, return only the first line *)
  if folded then first_line
  else
    (* Align RPC under the role column visually. We rely on fixed column widths
     (marker 1 + fold 1 + status 1 + instance 16) with spaces between. *)
    let role_column_start = 1 + 1 + 1 + 1 + 1 + 1 + 16 + 1 in
    (* Render highwatermarks line for bakers (last signed levels) *)
    let baker_highwatermarks_line ~instance =
      let activities = Baker_highwatermarks.get ~instance in
      match Baker_highwatermarks.format_summary activities with
      | None -> Widgets.dim "no signing activity"
      | Some summary -> summary
    in
    (* Render delegate status for bakers (from RPC) *)
    let delegate_status_line ~instance =
      let delegate_pkhs = Delegate_scheduler.get_baker_delegates ~instance in
      if delegate_pkhs = [] then Widgets.dim "no delegates configured"
      else
        let has_dal = Delegate_scheduler.baker_has_dal ~instance in
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
                          (not dp.sufficient_dal_participation)
                          && attestable > 0
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
    let second_line =
      match svc.Service.role with
      | "baker" ->
          (* Line 2 for bakers: highwatermarks (last signed levels) *)
          let hwm = baker_highwatermarks_line ~instance:svc.Service.instance in
          Printf.sprintf "%s%s" (String.make role_column_start ' ') hwm
      | "dal-node" ->
          (* Line 2 for DAL nodes: health status *)
          Printf.sprintf
            "%s%s"
            (String.make role_column_start ' ')
            (dal_health_line ~instance:svc.Service.instance)
      | _ ->
          Printf.sprintf
            "%s%s"
            (String.make role_column_start ' ')
            (rpc_status_line ~service_status:st.Service_state.status svc)
    in
    (* Additional lines for nodes, bakers, and dal-nodes: metrics + CPU chart *)
    let extra_lines =
      match svc.Service.role with
      | "node" | "baker" | "dal-node" ->
          let focus = idx + 3 = selected in
          let indent = String.make role_column_start ' ' in
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
    String.concat "\n" ([first_line; second_line] @ extra_lines)

let table_lines state =
  let install_row =
    let marker = if state.selected = 0 then Widgets.bold "➤" else " " in
    Printf.sprintf "%s %s" marker (Widgets.bold "[ Install new instance ]")
  in
  let manage_wallet_row =
    let marker = if state.selected = 1 then Widgets.bold "➤" else " " in
    Printf.sprintf "%s %s" marker (Widgets.bold "[ Manage wallet ]")
  in
  let instance_rows =
    if state.services = [] then ["  No managed instances."]
    else
      state.services
      |> List.mapi (fun idx (svc : Service_state.t) ->
          let is_folded =
            StringSet.mem svc.service.Service.instance state.folded
          in
          line_for_service idx state.selected ~folded:is_folded svc)
  in
  install_row :: manage_wallet_row :: "" :: instance_rows

let summary_line state =
  let total = List.length state.services in
  Printf.sprintf "Total instances: %d" total

let run_unit_action ~verb ~instance action =
  let title = Printf.sprintf "%s %s" (String.capitalize_ascii verb) instance in
  match action () with
  | Ok () ->
      Context.toast_success (Printf.sprintf "%s: %s" instance verb) ;
      Context.mark_instances_dirty ()
  | Error (`Msg msg) ->
      Context.toast_error (Printf.sprintf "%s: %s failed" instance verb) ;
      Modal_helpers.show_error ~title msg

(* Long-running actions (snapshot refresh) run asynchronously via Job_manager *)
let run_async_action ?on_complete ~verb ~instance action =
  let description =
    Printf.sprintf "%s %s" (String.capitalize_ascii verb) instance
  in
  Context.toast_info (Printf.sprintf "%s: starting %s..." instance verb) ;
  Job_manager.submit
    ~description
    (fun () ->
      match action () with
      | Ok () ->
          Context.toast_success (Printf.sprintf "%s: %s done" instance verb) ;
          Context.mark_instances_dirty () ;
          Ok ()
      | Error (`Msg msg) ->
          Context.toast_error (Printf.sprintf "%s: %s failed" instance verb) ;
          Modal_helpers.show_error
            ~title:
              (Printf.sprintf "%s %s" (String.capitalize_ascii verb) instance)
            msg ;
          Context.mark_instances_dirty () ;
          Error (`Msg msg))
    ~on_complete:(function
      | Job_manager.Succeeded -> (
          match on_complete with Some f -> f () | None -> ())
      | Job_manager.Failed _ -> (
          match on_complete with Some f -> f () | None -> ())
      | _ -> ())

let require_installer () =
  match
    Miaou_interfaces.Capability.get Manager_interfaces.Installer_capability.key
  with
  | Some cap ->
      let module I = (val (cap : Manager_interfaces.Installer_capability.t)) in
      Ok (module I : Manager_interfaces.Installer)
  | None -> Error (`Msg "Installer capability not available")

let require_tezos_node_manager () =
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Tezos_node_manager_capability.key
  with
  | Some cap ->
      let module I =
        (val (cap : Manager_interfaces.Tezos_node_manager_capability.t))
      in
      Ok (module I : Manager_interfaces.Tezos_node_manager)
  | None -> Error (`Msg "Tezos node manager capability not available")

let remove_modal state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      Modal_helpers.open_choice_modal
        ~title:(Printf.sprintf "Remove · %s" svc.Service.instance)
        ~items:[`Remove; `RemoveData; `Purge]
        ~to_string:(function
          | `Remove -> "Remove (keep data)"
          | `RemoveData -> "Remove + delete data"
          | `Purge -> "Purge (also drop user/logs)")
        ~on_select:(fun choice ->
          let instance = svc.Service.instance in
          let verb, action =
            match choice with
            | `Remove ->
                ( "remove",
                  fun () ->
                    Rpc_scheduler.stop_head_monitor instance ;
                    let* (module I) = require_installer () in
                    I.remove_service ~delete_data_dir:false ~instance )
            | `RemoveData ->
                ( "remove",
                  fun () ->
                    Rpc_scheduler.stop_head_monitor instance ;
                    let* (module I) = require_installer () in
                    I.remove_service ~delete_data_dir:true ~instance )
            | `Purge ->
                ( "purge",
                  fun () ->
                    Rpc_scheduler.stop_head_monitor instance ;
                    let* (module I) = require_installer () in
                    I.purge_service ~instance )
          in
          run_unit_action ~verb ~instance action) ;
      state)

let journalctl_args unit_name =
  if Common.is_root () then
    ["journalctl"; "-u"; unit_name; "--no-pager"; "-n"; "200"]
  else ["journalctl"; "--user"; "-u"; unit_name; "--no-pager"; "-n"; "200"]

(* Replaced by log_viewer page navigation *)
let _view_logs_old state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      let title = Printf.sprintf "Logs · %s" svc.Service.instance in
      let env =
        match Node_env.read ~inst:svc.Service.instance with
        | Ok pairs -> pairs
        | Error _ -> []
      in
      (* Find the directory where daily logs are written *)
      let logs_dir () =
        let lookup key =
          match List.assoc_opt key env with
          | Some v when String.trim v <> "" -> Some (String.trim v)
          | _ -> None
        in
        match svc.Service.role with
        | "node" ->
            (* Node: <data_dir>/daily_logs/ *)
            Filename.concat svc.Service.data_dir "daily_logs"
        | "baker" ->
            (* Baker: <base_dir>/logs/octez-baker/ *)
            let base =
              Option.value
                (lookup "OCTEZ_BAKER_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Filename.concat (Filename.concat base "logs") "octez-baker"
        | "accuser" ->
            (* Accuser: <base_dir>/logs/octez-accuser/ *)
            let base =
              Option.value
                (lookup "OCTEZ_CLIENT_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Filename.concat (Filename.concat base "logs") "octez-accuser"
        | "dal-node" ->
            (* DAL node: <data_dir>/daily_logs/ *)
            let base =
              Option.value
                (lookup "OCTEZ_DAL_DATA_DIR")
                ~default:svc.Service.data_dir
            in
            Filename.concat base "daily_logs"
        | "signer" ->
            (* Signer: <base_dir>/logs/octez-signer/ *)
            let base =
              Option.value
                (lookup "OCTEZ_SIGNER_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Filename.concat (Filename.concat base "logs") "octez-signer"
        | _ -> Filename.concat svc.Service.data_dir "daily_logs"
      in
      let daily_logs () =
        let dir = logs_dir () in
        if Sys.file_exists dir && Sys.is_directory dir then
          Sys.readdir dir |> Array.to_list
          |> List.map (Filename.concat dir)
          |> List.filter Sys.file_exists
        else []
      in
      let latest path_candidates =
        path_candidates
        |> List.filter_map (fun p ->
            try Some ((Unix.stat p).Unix.st_mtime, p) with _ -> None)
        |> List.sort (fun (a, _) (b, _) -> Float.compare b a)
        |> function
        | (_, p) :: _ -> Some p
        | [] -> None
      in
      let tail_file path =
        match Common.run_out ["tail"; "-n"; "200"; path] with
        | Ok text ->
            Modal_helpers.open_text_modal
              ~title
              ~lines:(String.split_on_char '\n' text) ;
            state
        | Error (`Msg msg) ->
            Modal_helpers.show_error ~title msg ;
            state
      in
      let show_journald () =
        let unit = Systemd.unit_name svc.Service.role svc.Service.instance in
        match Common.run_out (journalctl_args unit) with
        | Ok text ->
            Modal_helpers.open_text_modal
              ~title
              ~lines:(String.split_on_char '\n' text)
        | Error (`Msg msg) -> Modal_helpers.show_error ~title msg
      in
      (* All octez binaries write daily logs - offer choice if they exist *)
      let logs = daily_logs () in
      match latest logs with
      | Some path ->
          Modal_helpers.open_choice_modal
            ~title:"View Logs"
            ~items:[`Journald; `DailyLogs]
            ~to_string:(function
              | `Journald -> "Journald (systemd)"
              | `DailyLogs -> "Daily Logs (octez)")
            ~on_select:(function
              | `Journald -> show_journald ()
              | `DailyLogs -> ignore (tail_file path)) ;
          state
      | None ->
          (* No daily logs found, just show journald *)
          show_journald () ;
          state)

let refresh_modal state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      Modal_helpers.open_choice_modal
        ~title:(Printf.sprintf "Refresh snapshot · %s" svc.Service.instance)
        ~items:[`Auto; `AutoNoCheck; `CustomUri]
        ~to_string:(function
          | `Auto -> "Auto (latest tzinit snapshot)"
          | `AutoNoCheck -> "Auto (--no-check)"
          | `CustomUri -> "Custom URI")
        ~on_select:(fun choice ->
          let instance = svc.Service.instance in
          let run_refresh ?snapshot_uri ?(no_check = false) () =
            let now = Unix.gettimeofday () in
            Rpc_metrics.set
              ~instance
              {
                Rpc_metrics.chain_id = None;
                head_level = None;
                bootstrapped = None;
                last_rpc_refresh = Some now;
                node_version = None;
                data_size = None;
                proto = None;
                last_error = None;
                last_block_time = None;
              } ;
            Context.mark_instances_dirty () ;
            (* Stop RPC monitor while refreshing to avoid stale connections *)
            Rpc_scheduler.stop_head_monitor instance ;
            Context.progress_start
              ~label:"Downloading snapshot"
              ~estimate_secs:120.
              ~width:48 ;
            run_async_action
              ~verb:"refresh"
              ~instance
              ~on_complete:(fun () -> Context.progress_finish ())
              (fun () ->
                let* (module NM) = require_tezos_node_manager () in
                NM.refresh_instance_from_snapshot
                  ~instance
                  ?snapshot_uri
                  ~no_check
                  ~on_download_progress:(fun pct _ ->
                    Context.progress_set
                      ~label:"Downloading snapshot"
                      ~progress:(float_of_int pct /. 100.)
                      ())
                  ())
          in
          match choice with
          | `Auto -> run_refresh ()
          | `AutoNoCheck -> run_refresh ~no_check:true ()
          | `CustomUri ->
              Modal_helpers.prompt_text_modal
                ~title:"Snapshot URI"
                ~placeholder:(Some "file:///path/to/snapshot or https://...")
                ~on_submit:(fun text ->
                  let trimmed = String.trim text in
                  if trimmed = "" then
                    Modal_helpers.show_error
                      ~title:"Snapshot URI"
                      "Snapshot URI cannot be empty"
                  else run_refresh ~snapshot_uri:trimmed ())
                ()) ;
      state)

let instance_actions_modal state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      Modal_helpers.open_choice_modal
        ~title:("Actions · " ^ svc.Service.instance)
        ~items:
          [`Details; `Start; `Stop; `Restart; `RefreshSnapshot; `Logs; `Remove]
        ~to_string:(function
          | `Details -> "Details"
          | `Start -> "Start"
          | `Stop -> "Stop"
          | `Restart -> "Restart"
          | `RefreshSnapshot -> "Refresh from snapshot"
          | `Logs -> "View Logs"
          | `Remove -> "Remove")
        ~on_select:(fun choice ->
          let instance = svc.Service.instance in
          let role = svc.Service.role in
          match choice with
          | `Details ->
              Context.set_pending_instance_detail instance ;
              Context.navigate Instance_details.name
          | `Start ->
              run_unit_action ~verb:"start" ~instance (fun () ->
                  let cap = Miaou_interfaces.Service_lifecycle.require () in
                  Miaou_interfaces.Service_lifecycle.start
                    cap
                    ~role
                    ~service:instance
                  |> Result.map_error (fun e -> `Msg e))
          | `Stop ->
              run_unit_action ~verb:"stop" ~instance (fun () ->
                  let cap = Miaou_interfaces.Service_lifecycle.require () in
                  Miaou_interfaces.Service_lifecycle.stop
                    cap
                    ~role
                    ~service:instance
                  |> Result.map_error (fun e -> `Msg e))
          | `Restart ->
              run_unit_action ~verb:"restart" ~instance (fun () ->
                  let cap = Miaou_interfaces.Service_lifecycle.require () in
                  Miaou_interfaces.Service_lifecycle.restart
                    cap
                    ~role
                    ~service:instance
                  |> Result.map_error (fun e -> `Msg e))
          | `RefreshSnapshot -> refresh_modal state |> ignore
          | `Logs ->
              Context.set_pending_instance_detail instance ;
              Context.navigate Log_viewer_page.name
          | `Remove -> remove_modal state |> ignore) ;
      state)

let create_menu_modal state =
  let open Modal_helpers in
  open_choice_modal
    ~title:"Create Service"
    ~items:[`Node; `Baker; `Accuser; `DalNode; `Signer]
    ~to_string:(function
      | `Node -> "Node"
      | `Baker -> "Baker"
      | `Accuser -> "Accuser"
      | `DalNode -> "DAL Node"
      | `Signer -> "Signer")
    ~on_select:(function
      | `Node -> Context.navigate Install_node_form_v3.name
      | `Baker -> Context.navigate Install_baker_form_v3.name
      | `Accuser -> Context.navigate Install_accuser_form_v3.name
      | `DalNode -> Context.navigate Install_dal_node_form_v3.name
      | `Signer -> Context.navigate Install_signer_form_v3.name) ;
  state

let go_to_diagnostics state =
  Context.navigate Diagnostics.name ;
  state

let manage_wallet_modal state =
  Modal_helpers.select_client_base_dir_modal
    ~on_select:(fun path ->
      let content =
        Printf.sprintf
          "# Wallet Management\n\n\
           Using wallet directory: `%s`\n\n\
           ## No Wallet UI Yet\n\n\
           Octez Manager does not provide a wallet interface yet.\n\
           Please use `octez-client` to manage keys and addresses.\n\n\
           For documentation, see: https://octez.tezos.com"
          path
      in
      let rendered = Miaou.Internal.Modal_utils.markdown_to_ansi content in
      Modal_helpers.open_text_modal
        ~title:"Wallet Management"
        ~lines:(String.split_on_char '\n' rendered))
    () ;
  state

let activate_selection s =
  if s.selected = 0 then create_menu_modal s
  else if s.selected = 1 then manage_wallet_modal s
  else
    match current_service s with
    | Some _ -> instance_actions_modal s
    | None -> s

module Page_Impl :
  Miaou.Core.Tui_page.PAGE_SIG with type state = state and type msg = msg =
struct
  type nonrec state = state

  type nonrec msg = msg

  let init () = init_state ()

  let update s _ = s

  let refresh = maybe_refresh

  let move s _ = s

  let enter s = activate_selection s

  let service_select s _ = s

  let service_cycle s _ = refresh s

  let back s = s

  let handled_keys () =
    Miaou.Core.Keys.[Enter; Char "c"; Char "r"; Char "R"; Char "d"]

  let keymap _ =
    [
      ("Enter", activate_selection, "Open");
      ("c", create_menu_modal, "Create service");
      ("r", refresh_modal, "Refresh snapshot");
      ("R", refresh_modal, "Refresh snapshot");
      ("d", go_to_diagnostics, "Diagnostics");
    ]

  let header s =
    let privilege =
      if Common.is_root () then Widgets.red "● SYSTEM"
      else Widgets.green "● USER"
    in
    let hint = "Hint: ↑/↓ move · Enter open · Esc back" in
    [
      Printf.sprintf
        "%s   %s    %s"
        (Widgets.title_highlight " octez-manager ")
        privilege
        (Widgets.dim hint);
      Widgets.dim (summary_line s);
    ]

  let footer ~cols:_ =
    [
      Widgets.dim
        "Arrows: move  Tab: fold  Enter: actions  c: create  r: refresh  Esc: \
         back";
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

  (* Mutable scroll offset - updated during view to keep selection visible *)
  let scroll_offset_ref = ref 0

  let view s ~focus:_ ~size =
    (* Set contextual help hint based on selection *)
    (match current_service s with
    | Some st when st.service.Service.role = "node" ->
        Miaou.Core.Help_hint.set (Some node_help_hint)
    | Some st when st.service.Service.role = "baker" ->
        Miaou.Core.Help_hint.set (Some baker_help_hint)
    | Some st when st.service.Service.role = "dal-node" ->
        Miaou.Core.Help_hint.set (Some dal_help_hint)
    | _ -> Miaou.Core.Help_hint.set (Some "Press Enter to select, ? for help")) ;
    (* Tick spinner and toasts each render *)
    Context.tick_spinner () ;
    Context.tick_toasts () ;
    let cols = size.LTerm_geom.cols in
    let progress = Context.render_progress ~cols in
    let toast_lines_str = Context.render_toasts ~cols in
    let footer_lines = footer ~cols in
    Vsection.render
      ~size
      ~header:(header s)
      ~footer:footer_lines
      ~child:(fun inner_size ->
        (* Get all table lines and flatten to individual lines *)
        let table = table_lines s in
        let all_lines =
          List.concat_map (fun s -> String.split_on_char '\n' s) table
        in
        let total_lines = List.length all_lines in
        (* Calculate line index where current selection starts *)
        let selection_line_start =
          (* Count lines before current selection *)
          let rec count_lines idx acc =
            if idx >= s.selected then acc
            else if idx >= List.length table then acc
            else
              let entry = List.nth table idx in
              let lines = String.split_on_char '\n' entry in
              count_lines (idx + 1) (acc + List.length lines)
          in
          count_lines 0 0
        in
        let selection_line_count =
          if s.selected >= List.length table then 1
          else
            let entry = List.nth table s.selected in
            List.length (String.split_on_char '\n' entry)
        in
        (* Available rows for content (reserve space for progress/toasts) *)
        let progress_lines =
          if String.trim progress = "" then 0
          else List.length (String.split_on_char '\n' progress)
        in
        let toast_lines =
          if String.length toast_lines_str = 0 then 0
          else List.length (String.split_on_char '\n' toast_lines_str)
        in
        let avail_rows =
          inner_size.LTerm_geom.rows - progress_lines - toast_lines - 1
        in
        let avail_rows = max 5 avail_rows in
        (* Adjust scroll offset to keep selection visible *)
        let scroll = !scroll_offset_ref in
        let scroll =
          (* If selection is above visible area, scroll up *)
          if selection_line_start < scroll then selection_line_start
            (* If selection bottom is below visible area, scroll down *)
          else if
            selection_line_start + selection_line_count > scroll + avail_rows
          then selection_line_start + selection_line_count - avail_rows
          else scroll
        in
        let scroll = max 0 (min scroll (max 0 (total_lines - avail_rows))) in
        scroll_offset_ref := scroll ;
        (* Slice visible lines *)
        let visible_lines =
          all_lines
          |> List.mapi (fun i l -> (i, l))
          |> List.filter (fun (i, _) -> i >= scroll && i < scroll + avail_rows)
          |> List.map snd
        in
        (* Add scroll indicators *)
        let up_indicator = if scroll > 0 then [Widgets.dim "↑ more"] else [] in
        let down_indicator =
          if scroll + avail_rows < total_lines then [Widgets.dim "↓ more"]
          else []
        in
        (* Build final body *)
        let content_lines = up_indicator @ visible_lines @ down_indicator in
        let base = String.concat "\n" content_lines in
        let body =
          if String.trim progress = "" then base else progress ^ "\n" ^ base
        in
        if String.length toast_lines_str > 0 then body ^ "\n" ^ toast_lines_str
        else body)

  let check_navigation s =
    match Context.consume_navigation () with
    | Some p -> {s with next_page = Some p}
    | None -> s

  let handle_modal_key s key ~size:_ =
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation s

  let is_quit_key key =
    let lower = String.lowercase_ascii key in
    lower = "esc" || lower = "escape" || lower = "c-c" || lower = "ctrl+c"
    || lower = "^c" || String.equal key "\003"

  let move_selection s delta =
    if s.services = [] then
      (* Only menu items (0 and 1) when no services *)
      let selected = max 0 (min 1 (s.selected + delta)) in
      {s with selected}
    else
      let raw = s.selected + delta in
      let selected = clamp_selection s.services raw in
      (* Skip position 2 (separator between menu and services) *)
      let selected = if selected = 2 then selected + delta else selected in
      let selected = clamp_selection s.services selected in
      {s with selected}

  let force_refresh_cmd s = force_refresh s

  let toggle_fold s =
    match current_service s with
    | None -> s
    | Some st ->
        let inst = st.service.Service.instance in
        let folded =
          if StringSet.mem inst s.folded then StringSet.remove inst s.folded
          else StringSet.add inst s.folded
        in
        {s with folded}

  let handle_key s key ~size:_ =
    let s =
      if Miaou.Core.Modal_manager.has_active () then (
        Miaou.Core.Modal_manager.handle_key key ;
        s)
      else if is_quit_key key then {s with next_page = Some "__BACK__"}
      else
        match Keys.of_string key with
        | Some Keys.Up -> move_selection s (-1)
        | Some Keys.Down -> move_selection s 1
        | Some (Keys.Char "k") -> move_selection s (-1)
        | Some (Keys.Char "j") -> move_selection s 1
        | Some Keys.Tab -> toggle_fold s
        | Some Keys.Enter -> activate_selection s
        | Some (Keys.Char "c") -> create_menu_modal s
        | Some (Keys.Char " ") -> force_refresh_cmd s
        | Some (Keys.Char "r") -> refresh_modal s
        | Some (Keys.Char "R") -> refresh_modal s
        | Some (Keys.Char "Esc")
        | Some (Keys.Char "Escape")
        | Some (Keys.Char "q")
        | Some (Keys.Char "C-c") ->
            {s with next_page = Some "__BACK__"}
        | _ -> s
    in
    check_navigation s

  let next_page s = s.next_page

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
