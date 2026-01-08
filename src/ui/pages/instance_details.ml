(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "instance_details"

type state = {
  instance : string;
  service : Service.t option;
  error : string option;
}

type msg = unit

type pstate = state Navigation.t

let load_service instance =
  match Service_registry.find ~instance with
  | Ok (Some svc) -> Ok svc
  | Ok None -> Error ("Instance not found: " ^ instance)
  | Error (`Msg e) -> Error e

let init () =
  let state =
    match Context.take_pending_instance_detail () with
    | Some instance -> (
        match load_service instance with
        | Ok service -> {instance; service = Some service; error = None}
        | Error e -> {instance; service = None; error = Some e})
    | None ->
        {instance = ""; service = None; error = Some "No instance selected"}
  in
  Navigation.make state

let update ps _ = ps

let refresh ps =
  match Context.consume_navigation () with
  | Some p -> Navigation.goto p ps
  | None -> ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps = Navigation.back ps

let handled_keys () = Miaou.Core.Keys.[Escape; Enter]

let keymap _ = [("Enter", (fun ps -> ps), "Actions"); ("Esc", back, "Back")]

let header s =
  [
    Widgets.title_highlight (" Instance Details . " ^ s.instance);
    (match s.service with
    | Some svc -> Widgets.dim (svc.Service.role ^ " @ " ^ svc.Service.network)
    | None -> "");
  ]

let footer = [Widgets.dim "Enter: actions  Esc: back"]

let view_details svc =
  let render_fields fields =
    let width =
      fields
      |> List.fold_left (fun acc (label, _) -> max acc (String.length label)) 0
    in
    fields
    |> List.map (fun (label, value) ->
        Printf.sprintf
          "%s %s"
          (Widgets.dim (Printf.sprintf "%-*s" width label))
          value)
  in
  let env =
    match Node_env.read ~inst:svc.Service.instance with
    | Ok pairs -> pairs
    | Error _ -> []
  in
  let lookup key =
    match List.assoc_opt key env with Some v -> String.trim v | None -> ""
  in
  let service_paths =
    Systemd.get_service_paths
      ~role:svc.Service.role
      ~instance:svc.Service.instance
  in
  let log_file =
    match
      Log_viewer.get_daily_log_file
        ~role:svc.Service.role
        ~instance:svc.Service.instance
    with
    | Ok path -> [("Log File (Latest)", path)]
    | Error _ -> []
  in
  let service_metadata =
    let path =
      Filename.concat
        (Service_registry.services_dir ())
        (svc.Service.instance ^ ".json")
    in
    [("Service Metadata", path)]
  in
  let specific_paths =
    match svc.Service.role with
    | "node" ->
        let config_file = Filename.concat svc.Service.data_dir "config.json" in
        let identity_file =
          Filename.concat svc.Service.data_dir "identity.json"
        in
        [
          ("Data Directory", svc.Service.data_dir);
          ("Config File", config_file);
          ("Identity File", identity_file);
        ]
    | "baker" ->
        let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
        let client_config = Filename.concat base_dir "config" in
        [("Base Directory", base_dir); ("Client Config", client_config)]
    | "accuser" ->
        let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
        let client_config = Filename.concat base_dir "config" in
        [("Base Directory", base_dir); ("Client Config", client_config)]
    | "dal-node" | "dal" ->
        let data_dir = lookup "OCTEZ_DAL_DATA_DIR" in
        let config_file = Filename.concat data_dir "config.json" in
        let identity_file = Filename.concat data_dir "identity.json" in
        [
          ("DAL Data Dir", data_dir);
          ("Config File", config_file);
          ("Identity File", identity_file);
        ]
    | _ -> [("Data Directory", svc.Service.data_dir)]
  in
  let paths = specific_paths @ service_paths @ log_file @ service_metadata in
  let details =
    match svc.Service.role with
    | "baker" ->
        let delegates =
          match lookup "OCTEZ_BAKER_DELEGATES_CSV" with
          | "" -> "(none)"
          | csv ->
              csv |> String.split_on_char ',' |> List.map String.trim
              |> List.filter (( <> ) "")
              |> ( function [] -> ["(none)"] | xs -> xs )
              |> String.concat ", "
        in
        let node_mode = lookup "OCTEZ_BAKER_NODE_MODE" in
        let node_endpoint = lookup "OCTEZ_NODE_ENDPOINT" in
        let dal_config = lookup "OCTEZ_DAL_CONFIG" in
        let dal_display =
          if dal_config = "disabled" then "(opt-out: --without-dal)"
          else if dal_config = "" then "(auto)"
          else dal_config
        in
        let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
        let extra_args = lookup "OCTEZ_BAKER_EXTRA_ARGS" in
        let logging = Logging_mode.to_string svc.Service.logging_mode in
        [
          ("Instance", svc.Service.instance);
          ("Role", svc.Service.role);
          ("Network", svc.Service.network);
          ("History Mode", History_mode.to_string svc.Service.history_mode);
          ("Baker Base Dir", if base_dir = "" then "(unset)" else base_dir);
          ("Delegates", delegates);
          ("Node Mode", if node_mode = "" then "remote" else node_mode);
          ( "Node Endpoint",
            if node_endpoint = "" then "(unset)" else node_endpoint );
          ("DAL Config", dal_display);
          ("Service User", svc.Service.service_user);
          ("Bin Dir", svc.Service.app_bin_dir);
          ("Created At", svc.Service.created_at);
          ("Logging", logging);
          ("Extra Args", if extra_args = "" then "(none)" else extra_args);
        ]
    | _ ->
        [
          ("Instance", svc.Service.instance);
          ("Role", svc.Service.role);
          ("Network", svc.Service.network);
          ("History Mode", History_mode.to_string svc.Service.history_mode);
          ("RPC Addr", svc.Service.rpc_addr);
          ("P2P Addr", svc.Service.net_addr);
          ("Service User", svc.Service.service_user);
          ("Bin Dir", svc.Service.app_bin_dir);
          ("Created At", svc.Service.created_at);
          ("Logging", Logging_mode.to_string svc.Service.logging_mode);
          ("Extra Args", String.concat " " svc.Service.extra_args);
        ]
  in
  render_fields details
  @ [""; Widgets.bold "Files & Paths"]
  @ render_fields paths

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let body =
    match (s.error, s.service) with
    | Some err, _ -> [Widgets.red ("Error: " ^ err)]
    | None, Some svc -> view_details svc
    | None, None -> ["Loading..."]
  in
  Vsection.render ~size ~header:(header s) ~footer ~child:(fun _ ->
      String.concat "\n" body)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let require_package_manager () =
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Package_manager_capability.key
  with
  | Some cap ->
      let module I =
        (val (cap : Manager_interfaces.Package_manager_capability.t))
      in
      Ok (module I : Manager_interfaces.Package_manager)
  | None -> Error (`Msg "Package manager capability not available")

let apply_node_update ps update_fn =
  let s = ps.Navigation.s in
  match s.service with
  | None -> ps
  | Some svc -> (
      let req =
        {
          instance = svc.Service.instance;
          network = svc.Service.network;
          history_mode = svc.Service.history_mode;
          data_dir = Some svc.Service.data_dir;
          rpc_addr = svc.Service.rpc_addr;
          net_addr = svc.Service.net_addr;
          service_user = svc.Service.service_user;
          app_bin_dir = svc.Service.app_bin_dir;
          logging_mode = svc.Service.logging_mode;
          extra_args = svc.Service.extra_args;
          auto_enable = true;
          bootstrap = Genesis;
          preserve_data = false;
          snapshot_no_check = false;
        }
      in
      let req = update_fn req in
      let res =
        let* (module PM) = require_package_manager () in
        PM.install_node req
      in
      match res with
      | Ok new_svc ->
          Modal_helpers.show_success ~title:"Success" "Configuration updated." ;
          Navigation.update (fun s -> {s with service = Some new_svc}) ps
      | Error (`Msg e) ->
          Modal_helpers.show_error ~title:"Error" e ;
          ps)

let edit_config_modal ps =
  let s = ps.Navigation.s in
  match s.service with
  | None -> ps
  | Some svc ->
      if svc.Service.role <> "node" then (
        Modal_helpers.show_error
          ~title:"Not Supported"
          "Editing is currently only supported for nodes." ;
        ps)
      else
        (* Only extra args can be edited - logging is always journald *)
        let initial = String.concat " " svc.Service.extra_args in
        Modal_helpers.prompt_text_modal
          ~title:("Extra Arguments . " ^ svc.Service.instance)
          ~initial
          ~on_submit:(fun text ->
            let extra_args =
              String.split_on_char ' ' text
              |> List.map String.trim
              |> List.filter (( <> ) "")
            in
            ignore (apply_node_update ps (fun req -> {req with extra_args})))
          () ;
        ps

(* Perform edit instance side effects and navigate via Context *)
let do_edit_instance svc =
  (* Stop the service (cascade stops dependents) *)
  let stopped_dependents = svc.Service.dependents in
  (match
     Installer.stop_service ~quiet:true ~instance:svc.Service.instance ()
   with
  | Ok () -> ()
  | Error (`Msg msg) -> Context.toast_warn ("Failed to stop service: " ^ msg)) ;
  (* Set the edit context *)
  Context.set_pending_edit_service ~service:svc ~stopped_dependents ;
  (* Navigate to the appropriate install form based on role *)
  let form_page =
    match svc.Service.role with
    | "node" -> "install_node_form_v3"
    | "baker" -> "install_baker_form_v3"
    | "accuser" -> "install_accuser_form_v3"
    | "dal-node" | "dal" -> "install_dal_node_form_v3"
    | _ -> "instances"
  in
  Context.navigate form_page

let confirm_edit_modal ps svc =
  if svc.Service.dependents = [] then (
    do_edit_instance svc ;
    ps)
  else (
    Modal_helpers.open_choice_modal
      ~title:"Confirm Edit"
      ~items:[`Confirm; `Cancel]
      ~to_string:(function
        | `Confirm ->
            Printf.sprintf
              "Proceed (will stop: %s)"
              (String.concat ", " svc.Service.dependents)
        | `Cancel -> "Cancel")
      ~on_select:(fun choice ->
        match choice with
        | `Confirm -> do_edit_instance svc
        | `Cancel -> ()) ;
    ps)

let open_actions_modal ps =
  let s = ps.Navigation.s in
  match s.service with
  | None -> ps
  | Some svc ->
      Modal_helpers.open_choice_modal
        ~title:("Actions . " ^ svc.Service.instance)
        ~items:[`Edit; `EditConfig; `Overrides]
        ~to_string:(function
          | `Edit -> "Edit Instance"
          | `EditConfig -> "Edit Configuration"
          | `Overrides -> "Service Overrides")
        ~on_select:(fun choice ->
          match choice with
          | `Edit -> ignore (confirm_edit_modal ps svc)
          | `EditConfig -> ignore (edit_config_modal ps)
          | `Overrides ->
              Modal_helpers.show_error
                ~title:"Not Implemented"
                "Overrides not implemented yet") ;
      ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") | Some (Keys.Char "q") -> Navigation.back ps
    | Some Keys.Enter -> open_actions_modal ps
    | _ -> ps

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

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

  let has_modal = has_modal
end

module Page =
  Monitored_page.Make
    (Page_Impl)
    (struct
      let page_name = "instance_details"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
