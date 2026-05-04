(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Import wizard - convert external services to managed instances *)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
open Octez_manager_lib

let name = "import_wizard"

type step = Import_wizard_view.step =
  | SelectService
  | ConfigureImport
  | ReviewImport
  | Importing

type state = Import_wizard_view.state = {
  step : step;
  external_services : External_service.t list;
  selected_idx : int;
  selected_service : External_service.t option;
  strategy : Import.import_strategy;
  custom_name : string option;
  network_override : string option;
  error : string option;
  cascade : bool;
  cascade_chain : External_service.t list;
  cascade_analysis : Import_cascade.dependency_analysis option;
}

type msg = unit

type pstate = state Navigation.t

let init () =
  let external_services = External_services_scheduler.get () in
  Navigation.make
    {
      step = SelectService;
      external_services;
      selected_idx = 0;
      selected_service = None;
      strategy = Import.Takeover;
      custom_name = None;
      network_override = None;
      error = None;
      cascade = false;
      cascade_chain = [];
      cascade_analysis = None;
    }

let update ps _ =
  (* Check for pending navigation (e.g., from job completion callback) *)
  let ps =
    match Context.consume_navigation () with
    | Some (Context.Goto page) -> Navigation.goto page ps
    | Some Context.Back -> Navigation.back ps
    | Some Context.Quit -> Navigation.quit ps
    | None -> ps
  in
  ps

let refresh ps =
  (* Check for pending navigation (e.g., from job completion callback) *)
  let ps =
    match Context.consume_navigation () with
    | Some (Context.Goto page) -> Navigation.goto page ps
    | Some Context.Back -> Navigation.back ps
    | Some Context.Quit -> Navigation.quit ps
    | None -> ps
  in
  (* Check for pending network override from modal *)
  let ps =
    match Context.take_pending_import_network () with
    | Some network -> (
        (* Set the network override in state *)
        let ps' =
          Navigation.update
            (fun s -> {s with network_override = Some network; error = None})
            ps
        in
        (* Automatically validate and proceed to next step *)
        let s = ps'.Navigation.s in
        match s.selected_service with
        | Some svc -> (
            match
              Import.validate_importable
                ?network_override:s.network_override
                svc
            with
            | Ok () ->
                (* Validation succeeded - proceed to ConfigureImport step *)
                Navigation.update
                  (fun s ->
                    {
                      s with
                      step = ConfigureImport;
                      selected_service = Some svc;
                      error = None;
                    })
                  ps'
            | Error (`Msg msg) ->
                (* Should not happen since we validated in modal, but handle it *)
                Navigation.update (fun s -> {s with error = Some msg}) ps')
        | None -> ps')
    | None -> ps
  in
  Navigation.update
    (fun s ->
      let external_services = External_services_scheduler.get () in
      let selected_idx =
        min s.selected_idx (max 0 (List.length external_services - 1))
      in
      {s with external_services; selected_idx})
    ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps =
  let s = ps.Navigation.s in
  match s.step with
  | SelectService -> Navigation.back ps
  | ConfigureImport ->
      Navigation.update
        (fun s -> {s with step = SelectService; error = None})
        ps
  | ReviewImport ->
      Navigation.update
        (fun s -> {s with step = ConfigureImport; error = None})
        ps
  | Importing ->
      (* Allow exiting from Importing state *)
      Navigation.back ps

let with_selected_service s on_error on_success =
  match List.nth_opt s.external_services s.selected_idx with
  | None -> on_error "No service selected"
  | Some svc -> on_success svc

let rec next_step ps =
  let s = ps.Navigation.s in
  match s.step with
  | SelectService ->
      with_selected_service
        s
        (fun msg -> Navigation.update (fun s -> {s with error = Some msg}) ps)
        (fun svc ->
          match
            Import.validate_importable ?network_override:s.network_override svc
          with
          | Error (`Msg msg)
            when String.starts_with ~prefix:"Network could not be detected" msg
            ->
              (* Network detection failed - show network selection modal *)
              (* Set selected_service so refresh can use it after modal closes *)
              let ps' =
                Navigation.update
                  (fun s -> {s with selected_service = Some svc})
                  ps
              in
              show_network_override_modal svc ;
              ps'
          | Error (`Msg msg) ->
              Navigation.update (fun s -> {s with error = Some msg}) ps
          | Ok () ->
              Navigation.update
                (fun s ->
                  {
                    s with
                    step = ConfigureImport;
                    selected_service = Some svc;
                    error = None;
                  })
                ps)
  | ConfigureImport ->
      Navigation.update (fun s -> {s with step = ReviewImport; error = None}) ps
  | ReviewImport -> start_import ps
  | Importing -> ps

and show_network_override_modal svc =
  (* Use the exact same network list as node installation form *)
  let network_infos =
    match Form_builder_bundles.get_network_infos () with
    | Ok infos -> infos
    | Error _ -> []
  in
  let normalize s = String.lowercase_ascii (String.trim s) in
  let sorted =
    network_infos
    |> List.sort (fun (a : Teztnets.network_info) (b : Teztnets.network_info) ->
        String.compare (normalize a.human_name) (normalize b.human_name))
  in
  let items = (sorted |> List.map (fun n -> `Net n)) @ [`Custom] in
  let to_string = function
    | `Net n -> Form_builder_bundles.format_network_choice n
    | `Custom -> "Custom URL or slug..."
  in
  let on_select = function
    | `Net n -> (
        let network = n.Teztnets.network_url in
        (* Validate with the selected network *)
        match Import.validate_importable ~network_override:network svc with
        | Ok () ->
            (* Store network override and proceed automatically *)
            Context.set_pending_import_network network
        | Error (`Msg msg) ->
            Context.toast_error (Printf.sprintf "Validation failed: %s" msg))
    | `Custom ->
        Modal_helpers.prompt_text_modal
          ~title:"Network"
          ~initial:""
          ~placeholder:(Some "Network URL or slug")
          ~on_submit:(fun network ->
            match Import.validate_importable ~network_override:network svc with
            | Ok () -> Context.set_pending_import_network network
            | Error (`Msg msg) ->
                Context.toast_error (Printf.sprintf "Validation failed: %s" msg))
          ()
  in
  Modal_helpers.open_choice_modal
    ~title:"Network Override Required"
    ~items
    ~to_string
    ~on_select
    ()

and start_import ps =
  let s = ps.Navigation.s in
  match s.selected_service with
  | None ->
      Navigation.update
        (fun s -> {s with error = Some "No service selected"})
        ps
  | Some external_svc ->
      (* Shared options construction *)
      let overrides : Import.field_overrides =
        {
          network = s.network_override;
          data_dir = None;
          base_dir = None;
          rpc_addr = None;
          net_addr = None;
          delegates = None;
        }
      in
      let options : Import.import_options =
        {
          strategy = s.strategy;
          new_instance_name = s.custom_name;
          overrides;
          dry_run = false;
          interactive = false;
          preserve_data = true;
          quiet = false;
        }
      in
      (* Shared completion callback *)
      let on_complete status =
        match status with
        | Job_manager.Succeeded ->
            Cache.invalidate_all () ;
            Context.mark_instances_dirty () ;
            let msg =
              if s.cascade then "Cascade import successful!"
              else "Service imported successfully!"
            in
            Context.toast_success msg ;
            Context.set_pending_tab Context.Tab_instances ;
            Context.navigate_back ()
        | Job_manager.Failed msg ->
            Context.toast_error (Printf.sprintf "Import failed: %s" msg)
        | Job_manager.Pending | Job_manager.Running -> ()
      in
      (* Import task differs based on cascade mode *)
      let import_task ~append_log () =
        if s.cascade then
          match
            Import.import_cascade
              ~on_log:append_log
              ~options
              ~external_svc
              ~all_services:s.external_services
              ()
          with
          | Ok _results -> Ok ()
          | Error e -> Error e
        else
          match
            Import.import_service
              ~on_log:append_log
              ~all_external_services:s.external_services
              ~options
              ~external_svc
              ()
          with
          | Ok _result -> Ok ()
          | Error e -> Error e
      in
      let description =
        if s.cascade then "Import cascade" else "Import external service"
      in
      Job_manager.submit ~on_complete ~timeout:None ~description import_task ;
      Navigation.update (fun s -> {s with step = Importing; error = None}) ps

let handled_keys () = Miaou.Core.Keys.[Escape; Enter; Up; Down; Tab]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "Esc" "Back"; kb "Enter" "Select"]

let move_selection ps delta =
  Navigation.update
    (fun s ->
      let len = List.length s.external_services in
      if len = 0 then s
      else
        let selected_idx = (s.selected_idx + delta + len) mod len in
        {s with selected_idx})
    ps

let compute_cascade_chain s =
  (* Compute cascade chain based on strategy and selected service *)
  match s.selected_service with
  | None -> ([], None)
  | Some svc ->
      let chain =
        match s.strategy with
        | Import.Takeover ->
            Import_cascade.get_full_cascade
              ~service:svc
              ~all_services:s.external_services
        | Import.Clone ->
            Import_cascade.get_dependency_chain
              ~service:svc
              ~all_services:s.external_services
      in
      let analysis =
        Import_cascade.analyze_dependencies
          ~services:s.external_services
          ~target_services:chain
      in
      (chain, Some analysis)

let toggle_strategy ps =
  Navigation.update
    (fun s ->
      let strategy =
        match s.strategy with
        | Import.Takeover -> Import.Clone
        | Import.Clone -> Import.Takeover
      in
      (* If cascade is enabled, recompute with new strategy *)
      if s.cascade then
        let cascade_chain, cascade_analysis = compute_cascade_chain s in
        {s with strategy; cascade_chain; cascade_analysis}
      else {s with strategy; cascade_chain = []; cascade_analysis = None})
    ps

let toggle_cascade ps =
  let s = ps.Navigation.s in
  let new_cascade = not s.cascade in
  if new_cascade then
    (* Cascade enabled: compute the chain *)
    let cascade_chain, cascade_analysis = compute_cascade_chain s in
    Navigation.update
      (fun s -> {s with cascade = new_cascade; cascade_chain; cascade_analysis})
      ps
  else
    (* Cascade disabled: clear the chain *)
    Navigation.update
      (fun s ->
        {
          s with
          cascade = new_cascade;
          cascade_chain = [];
          cascade_analysis = None;
        })
      ps

let header s = Import_wizard_view.header s

let view ps ~focus ~size =
  let s = ps.Navigation.s in
  Import_wizard_view.view s ~focus ~size

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    let s = ps.Navigation.s in
    match (s.step, Keys.of_string key) with
    | _, Some (Keys.Char "r") -> refresh ps
    | SelectService, Some Keys.Up -> move_selection ps (-1)
    | SelectService, Some Keys.Down -> move_selection ps 1
    | SelectService, Some Keys.Tab -> move_selection ps 1
    | SelectService, Some Keys.Enter -> next_step ps
    | SelectService, Some Keys.Escape -> Navigation.back ps
    | ConfigureImport, Some (Keys.Char " ") -> toggle_strategy ps
    | ConfigureImport, Some (Keys.Char "c") -> toggle_cascade ps
    | ConfigureImport, Some Keys.Enter -> next_step ps
    | ConfigureImport, Some Keys.Escape -> back ps
    | ReviewImport, Some Keys.Enter -> next_step ps
    | ReviewImport, Some Keys.Escape -> back ps
    | Importing, Some Keys.Escape ->
        (* Allow exiting from Importing state *)
        Navigation.back ps
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

  let handled_keys = handled_keys

  let keymap = keymap

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
      [
        {key = "Esc"; help = "Back / Previous"};
        {key = "Enter"; help = "Select / Next"};
      ]

  let has_modal = has_modal
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
