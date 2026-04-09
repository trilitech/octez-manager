(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Box = Miaou_widgets_layout.Box_widget
module Desc_list = Miaou_widgets_display.Description_list
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module Style_context = Miaou_style.Style_context
open Octez_manager_lib

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
  | Some (Context.Goto p) -> Navigation.goto p ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      (* Reload service data in case it was updated *)
      if Context.consume_instances_dirty () then
        let s = ps.Navigation.s in
        match load_service s.instance with
        | Ok service ->
            Navigation.update
              (fun s -> {s with service = Some service; error = None})
              ps
        | Error e -> Navigation.update (fun s -> {s with error = Some e}) ps
      else ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps = Navigation.back ps

let handled_keys () = Miaou.Core.Keys.[Escape]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "Esc" "Back"; kb "?" "Help"]

let header s =
  [
    Widgets.themed_primary (" Instance Details . " ^ s.instance);
    (match s.service with
    | Some svc ->
        let display_role =
          if svc.Service.role = "index" then "indexer" else svc.Service.role
        in
        let network =
          if svc.Service.network <> "" then svc.Service.network
          else
            match svc.Service.depends_on with
            | Some parent_instance -> (
                match Service_registry.find ~instance:parent_instance with
                | Ok (Some parent_svc) -> parent_svc.Service.network
                | _ -> "")
            | None -> ""
        in
        Widgets.themed_muted (display_role ^ " @ " ^ network)
    | None -> "");
  ]

let _footer = []

let render_signatory_keys ~box_width instance =
  (* Parse signatory.yaml to get detailed key information *)
  match Signatory_config.get_authorized_keys ~instance with
  | Ok [] ->
      Box.render
        ~title:"Authorized Keys"
        ~style:Rounded
        ~color:11
        ~width:box_width
        "(none)"
  | Ok keys ->
      let render_key key =
        let allows_str =
          match key.Signatory_config.allows with
          | [] -> "    (no specific operations allowed)"
          | ops ->
              ops |> List.map (fun op -> "    • " ^ op) |> String.concat "\n"
        in
        "  " ^ Widgets.cyan key.Signatory_config.pkh ^ "\n" ^ allows_str
      in
      let content = keys |> List.map render_key |> String.concat "\n\n" in
      Box.render
        ~title:"Authorized Keys"
        ~style:Rounded
        ~color:11
        ~width:box_width
        content
  | Error (`Msg err) ->
      Box.render
        ~title:"Authorized Keys"
        ~style:Rounded
        ~color:11
        ~width:box_width
        (Widgets.yellow ("Unable to parse config: " ^ err))

let view_details ~box_width svc =
  let render_fields items =
    Desc_list.create ~key_width:18 ~items ()
    |> Desc_list.render ~cols:(box_width - 4) ~wrap:true ~focus:false
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
        [("Base Directory", base_dir)]
    | "accuser" ->
        let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
        [("Base Directory", base_dir)]
    | "dal-node" | "dal" ->
        let data_dir = lookup "OCTEZ_DAL_DATA_DIR" in
        let config_file = Filename.concat data_dir "config.json" in
        let identity_file = Filename.concat data_dir "identity.json" in
        [
          ("DAL Data Dir", data_dir);
          ("Config File", config_file);
          ("Identity File", identity_file);
        ]
    | "signatory" ->
        let data_dir = Signatory.signatory_data_dir svc.Service.instance in
        let config_file =
          Signatory.signatory_config_path svc.Service.instance
        in
        let keys_dir = Filename.concat data_dir "keys" in
        let secrets_file = Filename.concat keys_dir "secret.json" in
        [
          ("Data Directory", data_dir);
          ("Config File", config_file);
          ("Keys Directory", keys_dir);
          ("Secrets File", secrets_file);
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
        let env_args = lookup "OCTEZ_BAKER_COMMAND_ARGS" in
        let svc_args = String.concat " " svc.Service.extra_args in
        let extra_args =
          match (env_args, svc_args) with
          | "", "" -> ""
          | a, "" | "", a -> a
          | a, b -> a ^ " " ^ b
        in
        let logging = Logging_mode.to_string svc.Service.logging_mode in
        let node_depends =
          match svc.Service.depends_on with Some inst -> inst | None -> ""
        in
        let dal_depends = lookup "OCTEZ_DAL_INSTANCE" in
        let depends_on =
          match (node_depends, dal_depends) with
          | "", "" -> "(none)"
          | n, "" -> n
          | "", d -> d
          | n, d -> n ^ ", " ^ d
        in
        let extra_nodes =
          match lookup "OCTEZ_EXTRA_NODE_ENDPOINTS" with
          | "" -> "(none)"
          | csv -> (
              csv |> String.split_on_char ',' |> List.map String.trim
              |> List.filter (( <> ) "")
              |> function
              | [] -> "(none)"
              | xs -> String.concat ", " xs)
        in
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
          ("Extra Nodes", extra_nodes);
          ("Depends On", depends_on);
          ("DAL Config", dal_display);
          ("Service User", svc.Service.service_user);
          ("Bin Dir", svc.Service.app_bin_dir);
          ("Created At", svc.Service.created_at);
          ("Logging", logging);
          ("Extra Args", if extra_args = "" then "(none)" else extra_args);
        ]
    | "accuser" ->
        let node_endpoint = lookup "OCTEZ_NODE_ENDPOINT" in
        let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
        let env_args = lookup "OCTEZ_BAKER_COMMAND_ARGS" in
        let svc_args = String.concat " " svc.Service.extra_args in
        let extra_args =
          match (env_args, svc_args) with
          | "", "" -> ""
          | a, "" | "", a -> a
          | a, b -> a ^ " " ^ b
        in
        let depends_on =
          match svc.Service.depends_on with
          | Some inst -> inst
          | None -> "(none)"
        in
        [
          ("Instance", svc.Service.instance);
          ("Role", svc.Service.role);
          ("Network", svc.Service.network);
          ("Base Dir", if base_dir = "" then "(unset)" else base_dir);
          ( "Node Endpoint",
            if node_endpoint = "" then "(unset)" else node_endpoint );
          ("Depends On", depends_on);
          ("Service User", svc.Service.service_user);
          ("Bin Dir", svc.Service.app_bin_dir);
          ("Created At", svc.Service.created_at);
          ("Logging", Logging_mode.to_string svc.Service.logging_mode);
          ("Extra Args", if extra_args = "" then "(none)" else extra_args);
        ]
    | "dal-node" | "dal" ->
        let node_endpoint = lookup "OCTEZ_NODE_ENDPOINT" in
        let dal_rpc = lookup "OCTEZ_DAL_RPC_ADDR" in
        let dal_net = lookup "OCTEZ_DAL_NET_ADDR" in
        let env_args = lookup "OCTEZ_SERVICE_ARGS" in
        let svc_args = String.concat " " svc.Service.extra_args in
        let extra_args =
          match (env_args, svc_args) with
          | "", "" -> ""
          | a, "" | "", a -> a
          | a, b -> a ^ " " ^ b
        in
        let depends_on =
          match svc.Service.depends_on with
          | Some inst -> inst
          | None -> "(none)"
        in
        let dependents =
          match svc.Service.dependents with
          | [] -> "(none)"
          | deps -> String.concat ", " deps
        in
        [
          ("Instance", svc.Service.instance);
          ("Role", svc.Service.role);
          ("Network", svc.Service.network);
          ( "Node Endpoint",
            if node_endpoint = "" then "(unset)" else node_endpoint );
          ("Depends On", depends_on);
          ("Dependents", dependents);
          ("DAL RPC Addr", if dal_rpc = "" then "(unset)" else dal_rpc);
          ("DAL P2P Addr", if dal_net = "" then "(unset)" else dal_net);
          ("Service User", svc.Service.service_user);
          ("Bin Dir", svc.Service.app_bin_dir);
          ("Created At", svc.Service.created_at);
          ("Logging", Logging_mode.to_string svc.Service.logging_mode);
          ("Extra Args", if extra_args = "" then "(unset)" else extra_args);
        ]
    | "signatory" ->
        let address = lookup "SIGNATORY_ADDRESS" in
        let metrics_address = lookup "SIGNATORY_METRICS_ADDRESS" in
        let backend = lookup "SIGNATORY_BACKEND" in
        let watermark = lookup "SIGNATORY_WATERMARK" in
        let dependents =
          match svc.Service.dependents with
          | [] -> "(none)"
          | deps -> String.concat ", " deps
        in
        (* Get health status from metrics cache *)
        let health_status =
          match Signatory_metrics.get ~instance:svc.Service.instance with
          | Some metrics -> (
              match metrics.Signatory_metrics.health with
              | Signatory_metrics.Up -> Widgets.green "Healthy"
              | Signatory_metrics.Down -> Widgets.red "Down"
              | Signatory_metrics.Degraded -> Widgets.yellow "Degraded"
              | Signatory_metrics.Unknown -> Widgets.dim "Unknown")
          | None -> Widgets.dim "Not monitored"
        in
        [
          ("Instance", svc.Service.instance);
          ("Role", svc.Service.role);
          ("Health", health_status);
          ("Server Address", if address = "" then "(unset)" else address);
          ( "Metrics Address",
            if metrics_address = "" then "(none)" else metrics_address );
          ("Backend", if backend = "" then "(unset)" else backend);
          ("Watermark", if watermark = "" then "(unset)" else watermark);
          ("Dependents", dependents);
          ("Service User", svc.Service.service_user);
          ("Bin Dir", svc.Service.app_bin_dir);
          ("Created At", svc.Service.created_at);
          ("Logging", Logging_mode.to_string svc.Service.logging_mode);
        ]
    | "index" ->
        let node_endpoint = lookup "OCTEZ_NODE_ENDPOINT" in
        let indexer_dir = lookup "OCTEZ_INDEXER_DIR" in
        let rpc_addr = lookup "OCTEZ_INDEX_RPC_ADDR" in
        let env_args = lookup "OCTEZ_SERVICE_ARGS" in
        let svc_args = String.concat " " svc.Service.extra_args in
        let extra_args =
          match (env_args, svc_args) with
          | "", "" -> ""
          | a, "" | "", a -> a
          | a, b -> a ^ " " ^ b
        in
        let depends_on =
          match svc.Service.depends_on with
          | Some inst -> inst
          | None -> "(none)"
        in
        let network =
          if svc.Service.network <> "" then svc.Service.network
          else
            match svc.Service.depends_on with
            | Some parent_instance -> (
                match Service_registry.find ~instance:parent_instance with
                | Ok (Some parent_svc) -> parent_svc.Service.network
                | _ -> "")
            | None -> ""
        in
        let dependents =
          match svc.Service.dependents with
          | [] -> "(none)"
          | deps -> String.concat ", " deps
        in
        [
          ("Instance", svc.Service.instance);
          ("Role", "indexer");
          ("Network", if network = "" then "(unknown)" else network);
          ("Indexer Dir", if indexer_dir = "" then "(unset)" else indexer_dir);
          ("RPC Addr", if rpc_addr = "" then "(unset)" else rpc_addr);
          ( "Node Endpoint",
            if node_endpoint = "" then "(unset)" else node_endpoint );
          ("Depends On", depends_on);
          ("Dependents", dependents);
          ("Service User", svc.Service.service_user);
          ("Bin Dir", svc.Service.app_bin_dir);
          ("Created At", svc.Service.created_at);
          ("Logging", Logging_mode.to_string svc.Service.logging_mode);
          ("Extra Args", if extra_args = "" then "(none)" else extra_args);
        ]
    | _ ->
        (* Default case - typically node *)
        let dependents =
          match svc.Service.dependents with
          | [] -> "(none)"
          | deps -> String.concat ", " deps
        in
        [
          ("Instance", svc.Service.instance);
          ("Role", svc.Service.role);
          ("Network", svc.Service.network);
          ("History Mode", History_mode.to_string svc.Service.history_mode);
          ("RPC Addr", Rpc_addr.to_string svc.Service.rpc_addr);
          ("P2P Addr", svc.Service.net_addr);
          ("Dependents", dependents);
          ("Service User", svc.Service.service_user);
          ("Bin Dir", svc.Service.app_bin_dir);
          ("Created At", svc.Service.created_at);
          ("Logging", Logging_mode.to_string svc.Service.logging_mode);
          ("Extra Args", String.concat " " svc.Service.extra_args);
        ]
  in
  let display_role =
    if svc.Service.role = "index" then "Indexer"
    else String.capitalize_ascii svc.Service.role
  in
  let role_title = display_role ^ " Details" in
  (* Return optional keys box for signatory *)
  let keys_box =
    if svc.Service.role = "signatory" then
      Some (render_signatory_keys ~box_width svc.Service.instance)
    else None
  in
  (role_title, render_fields details, render_fields paths, keys_box)

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let box_width = min 78 (size.LTerm_geom.cols - 2) in
  let body =
    match (s.error, s.service) with
    | Some err, _ -> Widgets.themed_error ("Error: " ^ err)
    | None, None -> "Loading..."
    | None, Some svc ->
        let title, details, paths, keys_box_opt = view_details ~box_width svc in
        let details_box =
          Style_context.with_child_context
            ~widget_name:"instance-details-main"
            (fun () ->
              Box.render ~title ~style:Rounded ~width:box_width details)
        in
        let paths_box =
          Style_context.with_child_context
            ~widget_name:"instance-details-paths"
            (fun () ->
              Box.render
                ~title:"Files & Paths"
                ~style:Rounded
                ~width:box_width
                paths)
        in
        (* Add authorized keys box for signatory *)
        let boxes =
          match keys_box_opt with
          | Some keys_box -> [details_box; keys_box; paths_box]
          | None -> [details_box; paths_box]
        in
        String.concat "\n" boxes
  in
  Themed_page.render_layout ~size ~header:(header s) ~footer:[] ~child:(fun _ ->
      body)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some Keys.Escape | Some (Keys.Char "q") -> Navigation.back ps
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
    Miaou.Core.Tui_page.
      [{key = "Esc"; help = "Back"}; {key = "?"; help = "Help"}]

  let has_modal = has_modal
end

module Page =
  Themed_page.Make
    (Page_Impl)
    (struct
      let page_name = "instance_details"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
