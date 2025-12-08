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
  let services = load_services () in
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
  let instance_str = Printf.sprintf "%-20s" svc.Service.instance in
  Printf.sprintf
    "%s %s %s %s %-18s %s"
    marker
    status
    instance_str
    role_str
    (History_mode.to_string svc.Service.history_mode)
    enabled

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
      Modal_helpers.show_success
        ~title
        (Printf.sprintf "%s completed." (String.capitalize_ascii verb)) ;
      Context.mark_instances_dirty ()
  | Error (`Msg msg) -> Modal_helpers.show_error ~title msg

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
      match svc.Service.logging_mode with
      | Logging_mode.Journald ->
          let unit = Systemd.unit_name svc.Service.role svc.Service.instance in
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
          else if Sys.file_exists trimmed then (
            match Common.run_out ["tail"; "-n"; "200"; trimmed] with
            | Ok text ->
                Modal_helpers.open_text_modal
                  ~title
                  ~lines:(String.split_on_char '\n' text) ;
                state
            | Error (`Msg msg) ->
                Modal_helpers.show_error ~title msg ;
                state)
          else (
            Modal_helpers.show_error ~title "Log file does not exist" ;
            state))

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
            run_unit_action ~verb:"refresh" ~instance (fun () ->
                let* (module NM) = require_tezos_node_manager () in
                NM.refresh_instance_from_snapshot
                  ~instance
                  ?snapshot_uri
                  ~no_check
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

let activate_selection s =
  if s.selected = 0 then create_menu_modal s
  else
    match current_service s with
    | Some _ -> instance_actions_modal s
    | None -> s

module Page :
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

  let keymap _ =
    [
      ("Enter", activate_selection, "Open");
      ("c", create_menu_modal, "Create service");
      ("f", cycle_filter, "Cycle filter");
      ("b", bulk_action_modal, "Bulk actions");
      ("R", refresh_modal, "Refresh snapshot");
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

  let footer =
    [
      Widgets.dim
        "Arrows: move  Enter: actions  c: create  f: filter  b: bulk  m: menu  \
         Esc: back";
    ]

  let view s ~focus:_ ~size =
    let body = String.concat "\n" (table_lines s) in
    Vsection.render ~size ~header:(header s) ~footer ~child:(fun _ -> body)

  let check_navigation s =
    match Context.consume_navigation () with
    | Some p -> {s with next_page = Some p}
    | None -> s

  let handle_modal_key s key ~size:_ =
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation s

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
        | Some (Keys.Char "r") -> force_refresh_cmd s
        | Some (Keys.Char "R") -> refresh_modal s
        | Some (Keys.Char "Esc") | Some (Keys.Char "q") ->
            {s with next_page = Some "__BACK__"}
        | _ -> s
    in
    check_navigation s

  let next_page s = s.next_page

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
