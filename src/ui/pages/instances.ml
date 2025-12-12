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

type filter = All | Role of string

type state = {
  services : Service_state.t list;
  filtered : Service_state.t list;
  filter : filter;
  selected : int;
  last_updated : float;
  next_page : string option;
}

type msg = unit

let filter_label = function All -> "all" | Role role -> role

let filter_equal a b =
  match (a, b) with
  | All, All -> true
  | Role x, Role y -> String.equal x y
  | _ -> false

let unique_roles services =
  services
  |> List.map (fun (st : Service_state.t) ->
      st.Service_state.service.Service.role)
  |> List.sort_uniq String.compare

let filter_candidates services =
  All :: List.map (fun role -> Role role) (unique_roles services)

let apply_filter filter services =
  match filter with
  | All -> services
  | Role role ->
      List.filter
        (fun (st : Service_state.t) ->
          String.equal st.Service_state.service.Service.role role)
        services

let clamp_selection filtered idx =
  let len = List.length filtered + 2 in
  max 0 (min idx (len - 1))

let load_services () = Data.load_service_states ()

let load_services_fresh () = Data.load_service_states ~detail:false ()

let init_state filter =
  let services = load_services () in
  let filtered = apply_filter filter services in
  {
    services;
    filtered;
    filter;
    selected = 0;
    last_updated = Unix.gettimeofday ();
    next_page = None;
  }

let force_refresh state =
  let services = load_services_fresh () in
  let filtered = apply_filter state.filter services in
  let selected = clamp_selection filtered state.selected in
  {state with services; filtered; selected; last_updated = Unix.gettimeofday ()}

let maybe_refresh state =
  let now = Unix.gettimeofday () in
  let next_page = Context.consume_navigation () in
  let state =
    match next_page with
    | Some p -> {state with next_page = Some p}
    | None -> {state with next_page = None}
  in
  if Context.consume_instances_dirty () || now -. state.last_updated > 5. then
    force_refresh state
  else state

let set_filter state filter =
  let filtered = apply_filter filter state.services in
  let selected = clamp_selection filtered state.selected in
  {state with filter; filtered; selected}

let cycle_filter state =
  let candidates = filter_candidates state.services in
  let total = List.length candidates in
  if total = 0 then state
  else
    let rec find_index idx = function
      | [] -> None
      | f :: rest ->
          if filter_equal f state.filter then Some idx
          else find_index (idx + 1) rest
    in
    let next_idx =
      match find_index 0 candidates with
      | None -> 0
      | Some idx -> (idx + 1) mod total
    in
    let next_filter = List.nth candidates next_idx in
    set_filter state next_filter

let current_service state =
  if state.selected < 2 then None
  else List.nth_opt state.filtered (state.selected - 2)

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
        last_rpc_refresh;
        chain_id;
        proto;
        last_error;
        last_block_time;
        _;
      } ->
      let error_prefix =
        match last_error with
        | Some msg ->
            let msg =
              if String.length msg > 64 then String.sub msg 0 64 else msg
            in
            Some (Widgets.red (Printf.sprintf "rpc error: %s" msg))
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
        | None -> Widgets.dim ""
        | Some ts ->
            let age = Unix.gettimeofday () -. ts in
            let label =
              if age >= 120. then Widgets.red (Printf.sprintf "Δ %.0fs" age)
              else if age >= 30. then
                Widgets.yellow (Printf.sprintf "Δ %.0fs" age)
              else Widgets.green (Printf.sprintf "Δ %.0fs" age)
            in
            label
      in
      let age =
        if stopped then Widgets.dim ""
        else
          match last_rpc_refresh with
          | None -> Widgets.dim ""
          | Some ts ->
              let secs = Unix.gettimeofday () -. ts in
              let label =
                if secs >= 60. then Printf.sprintf "(%.0fm)" (secs /. 60.)
                else Printf.sprintf "(%.0fs)" secs
              in
              Widgets.dim label
      in
      let proto_s =
        match proto with
        | None -> Widgets.dim "proto:?"
        | Some p ->
            let s =
              Printf.sprintf
                "proto:%s"
                (String.sub p 0 (min 6 (String.length p)))
            in
            if stopped then Widgets.dim s else s
      in
      let chain_s =
        match chain_id with
        | None -> Widgets.dim "chain:?"
        | Some c ->
            let s =
              Printf.sprintf
                "chain:%s"
                (String.sub c 0 (min 6 (String.length c)))
            in
            if stopped then Widgets.dim s else s
      in
      let lvl_s = if stopped then Widgets.dim lvl else lvl in
      Printf.sprintf
        "%s · %s · %s · %s · %s %s"
        boot
        lvl_s
        proto_s
        chain_s
        staleness
        age

let network_short (n : string) =
  match Snapshots.slug_of_network n with Some slug -> slug | None -> n

let line_for_service idx selected (st : Service_state.t) =
  let svc = st.Service_state.service in
  let marker = if idx + 2 = selected then Widgets.bold "➤" else " " in
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
  let history, network =
    match svc.Service.role with
    | "baker" ->
        (* Bakers inherit network/mode from their node; hide these columns to
           keep the focus on the instance itself. *)
        (Printf.sprintf "%-10s" "", Printf.sprintf "%-12s" "")
    | _ ->
        ( Printf.sprintf
            "%-10s"
            (History_mode.to_string svc.Service.history_mode),
          Printf.sprintf "%-12s" (network_short svc.Service.network) )
  in
  let first_line =
    Printf.sprintf
      "%s %s %s %s %s %s %s"
      marker
      status
      instance_str
      role_str
      history
      network
      enabled
  in
  (* Align RPC under the role column visually. We rely on fixed column widths
     (marker 1 + status 1 + instance 16) with spaces between. *)
  let role_column_start = 1 + 1 + 1 + 1 + 16 + 1 in
  let second_line =
    match svc.Service.role with
    | "baker" ->
        let msg = Widgets.dim "RPC not available for bakers; use logs." in
        Printf.sprintf "%s%s" (String.make role_column_start ' ') msg
    | _ ->
        Printf.sprintf
          "%s%s"
          (String.make role_column_start ' ')
          (rpc_status_line ~service_status:st.Service_state.status svc)
  in
  String.concat "\n" [first_line; second_line]

let table_lines state =
  let install_row =
    let marker = if state.selected = 0 then Widgets.bold "➤" else " " in
    Printf.sprintf "%s %s" marker (Widgets.bold "[ Install new instance ]")
  in
  let instance_rows =
    if state.filtered = [] then
      ["  No managed instances match the current filter."]
    else
      state.filtered
      |> List.mapi (fun idx svc -> line_for_service idx state.selected svc)
  in
  install_row :: "" :: instance_rows

let summary_line state =
  let total = List.length state.services in
  let filtered = List.length state.filtered in
  Printf.sprintf
    "Filter: %s · showing %d/%d"
    (filter_label state.filter)
    filtered
    total

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
          let role = svc.Service.role in
          let verb, action =
            match choice with
            | `Remove ->
                ( "remove",
                  fun () ->
                    let* (module I) = require_installer () in
                    I.remove_service ~delete_data_dir:false ~instance ~role )
            | `RemoveData ->
                ( "remove",
                  fun () ->
                    let* (module I) = require_installer () in
                    I.remove_service ~delete_data_dir:true ~instance ~role )
            | `Purge ->
                ( "purge",
                  fun () ->
                    let* (module I) = require_installer () in
                    I.purge_service ~instance ~role )
          in
          run_unit_action ~verb ~instance action) ;
      state)

let journalctl_args unit_name =
  if Common.is_root () then
    ["journalctl"; "-u"; unit_name; "--no-pager"; "-n"; "200"]
  else ["journalctl"; "--user"; "-u"; unit_name; "--no-pager"; "-n"; "200"]

let view_logs state =
  with_service state (fun svc_state ->
      let svc = svc_state.Service_state.service in
      let title = Printf.sprintf "Logs · %s" svc.Service.instance in
      let open Option in
      let baker_base_dir () =
        let env =
          match Node_env.read ~inst:svc.Service.instance with
          | Ok pairs -> pairs
          | Error _ -> []
        in
        match List.assoc_opt "OCTEZ_BAKER_BASE_DIR" env with
        | Some v when String.trim v <> "" -> String.trim v
        | _ -> Common.default_role_dir "baker" svc.Service.instance
      in
      let baker_daily_logs () =
        let dir = Filename.concat (baker_base_dir ()) "daily_logs" in
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
      match svc.Service.role with
      | "baker" -> (
          match svc.Service.logging_mode with
          | Logging_mode.Journald ->
              let unit =
                Systemd.unit_name svc.Service.role svc.Service.instance
              in
              (match Common.run_out (journalctl_args unit) with
              | Ok text ->
                  Modal_helpers.open_text_modal
                    ~title
                    ~lines:(String.split_on_char '\n' text)
              | Error (`Msg msg) -> Modal_helpers.show_error ~title msg) ;
              state
          | Logging_mode.File _ -> (
              (* When file logging is configured for bakers, only the
                 rotation under daily_logs is reliable. *)
              let logs = baker_daily_logs () in
              match latest logs with
              | Some path -> tail_file path
              | None ->
                  Modal_helpers.show_error
                    ~title
                    "No baker daily logs found under base_dir/daily_logs." ;
                  state))
      | _ -> (
          match svc.Service.logging_mode with
          | Logging_mode.Journald ->
              let unit =
                Systemd.unit_name svc.Service.role svc.Service.instance
              in
              (match Common.run_out (journalctl_args unit) with
              | Ok text ->
                  Modal_helpers.open_text_modal
                    ~title
                    ~lines:(String.split_on_char '\n' text)
              | Error (`Msg msg) -> Modal_helpers.show_error ~title msg) ;
              state
          | Logging_mode.File {path; _} ->
              let trimmed = String.trim path in
              if trimmed = "" then (
                Modal_helpers.show_error ~title "Log file path is empty" ;
                state)
              else if Sys.file_exists trimmed then tail_file trimmed
              else (
                Modal_helpers.show_error ~title "Log file does not exist" ;
                state)))

let refresh_modal state =
  if state.filtered = [] then (
    Modal_helpers.show_error
      ~title:"Refresh snapshot"
      "Select a service first (none matches the current filter)." ;
    state)
  else
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
              Context.set_pending_instance_detail instance role ;
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
          | `Logs -> view_logs state |> ignore
          | `Remove -> remove_modal state |> ignore) ;
      state)

let bulk_action_modal state =
  if state.filtered = [] then (
    Modal_helpers.show_error ~title:"Bulk actions" "No instances match filter" ;
    state)
  else
    let apply action_label action_fn =
      let results =
        List.map
          (fun svc_state ->
            let svc = svc_state.Service_state.service in
            let outcome = action_fn svc in
            (svc.Service.instance, outcome))
          state.filtered
      in
      let successes, failures =
        List.fold_left
          (fun (oks, errs) (inst, res) ->
            match res with
            | Ok () -> (inst :: oks, errs)
            | Error (`Msg msg) -> (oks, (inst, msg) :: errs))
          ([], [])
          results
      in
      if successes <> [] then Context.mark_instances_dirty () ;
      let success_lines =
        if successes = [] then ["  (none)"]
        else successes |> List.rev |> List.map (fun inst -> "  - " ^ inst)
      in
      let failure_lines =
        if failures = [] then ["  (none)"]
        else
          failures |> List.rev
          |> List.concat_map (fun (inst, msg) -> ["  - " ^ inst; "    " ^ msg])
      in
      let lines =
        [
          Printf.sprintf "Action: %s" action_label;
          Printf.sprintf "Targets: %d" (List.length state.filtered);
          "";
          Printf.sprintf "Succeeded (%d):" (List.length successes);
        ]
        @ success_lines
        @ [""; Printf.sprintf "Failed (%d):" (List.length failures)]
        @ failure_lines
      in
      Modal_helpers.open_text_modal ~title:("Bulk " ^ action_label) ~lines
    in
    Modal_helpers.open_choice_modal
      ~title:"Bulk actions"
      ~items:[`Start; `Stop; `Restart]
      ~to_string:(function
        | `Start -> "Start all filtered"
        | `Stop -> "Stop all filtered"
        | `Restart -> "Restart all filtered")
      ~on_select:(function
        | `Start ->
            apply "start" (fun svc ->
                let cap = Miaou_interfaces.Service_lifecycle.require () in
                Miaou_interfaces.Service_lifecycle.start
                  cap
                  ~service:svc.Service.instance
                  ~role:svc.Service.role
                |> Result.map_error (fun e -> `Msg e))
        | `Stop ->
            apply "stop" (fun svc ->
                let cap = Miaou_interfaces.Service_lifecycle.require () in
                Miaou_interfaces.Service_lifecycle.stop
                  cap
                  ~service:svc.Service.instance
                  ~role:svc.Service.role
                |> Result.map_error (fun e -> `Msg e))
        | `Restart ->
            apply "restart" (fun svc ->
                let cap = Miaou_interfaces.Service_lifecycle.require () in
                Miaou_interfaces.Service_lifecycle.restart
                  cap
                  ~service:svc.Service.instance
                  ~role:svc.Service.role
                |> Result.map_error (fun e -> `Msg e))) ;
    state

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
      | `Node -> Context.navigate Install_node_form.name
      | `Baker -> Context.navigate Install_baker_form.name
      | _ ->
          show_error
            ~title:"Not Implemented"
            "This service creation is not yet implemented via UI.") ;
  state

let go_to_diagnostics state =
  Context.navigate Diagnostics.name ;
  state

let activate_selection s =
  if s.selected = 0 then create_menu_modal s
  else
    match current_service s with
    | Some _ -> instance_actions_modal s
    | None -> s

module Page_Impl :
  Miaou.Core.Tui_page.PAGE_SIG with type state = state and type msg = msg =
struct
  type nonrec state = state

  type nonrec msg = msg

  let init () = init_state All

  let update s _ = s

  let refresh = maybe_refresh

  let move s _ = s

  let enter s = activate_selection s

  let service_select s _ = s

  let service_cycle s _ = refresh s

  let back s = s

  let handled_keys () =
    Miaou.Core.Keys.[Enter; Char "c"; Char "f"; Char "b"; Char "r"; Char "R"; Char "d"]

  let keymap _ =
    [
      ("Enter", activate_selection, "Open");
      ("c", create_menu_modal, "Create service");
      ("f", cycle_filter, "Cycle filter");
      ("b", bulk_action_modal, "Bulk actions");
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
        "Arrows: move  Enter: actions  c: create  f: filter  b: bulk  m: menu  \
         Esc: back";
    ]

  let view s ~focus:_ ~size =
    (* Tick spinner and toasts each render *)
    Context.tick_spinner () ;
    Context.tick_toasts () ;
    let cols = size.LTerm_geom.cols in
    let progress = Context.render_progress ~cols in
    let body =
      let base = String.concat "\n" (table_lines s) in
      if String.trim progress = "" then base else progress ^ "\n" ^ base
    in
    let toast_lines = Context.render_toasts ~cols in
    let body_with_toasts =
      if String.length toast_lines > 0 then body ^ "\n" ^ toast_lines else body
    in
    let footer_lines = footer ~cols in
    Vsection.render
      ~size
      ~header:(header s)
      ~footer:footer_lines
      ~child:(fun _ -> body_with_toasts)

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
    if s.filtered = [] then {s with selected = 0}
    else
      let raw = s.selected + delta in
      let selected = clamp_selection s.filtered raw in
      let selected = if selected = 1 then selected + delta else selected in
      let selected = clamp_selection s.filtered selected in
      {s with selected}

  let force_refresh_cmd s = force_refresh s

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
        | Some Keys.Enter -> activate_selection s
        | Some (Keys.Char "c") -> create_menu_modal s
        | Some (Keys.Char "f") -> cycle_filter s
        | Some (Keys.Char "b") -> bulk_action_modal s
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

module Page = Monitored_page.Make(Page_Impl)(struct
  let page_name = "instances"
end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
