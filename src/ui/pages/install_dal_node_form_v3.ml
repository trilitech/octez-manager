(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** DAL node installation form using field bundles. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_dal_node_form_v3"

(** Model uses the bundle config types directly *)
type model = {
  core : Form_builder_common.core_service_config;
  client : Form_builder_common.client_config;
  dal_data_dir : string;  (** DAL node's data directory *)
  rpc_addr : string;  (** DAL node's own RPC address *)
  net_addr : string;  (** DAL node's P2P address *)
  (* Edit mode fields *)
  edit_mode : bool;
  original_instance : string option; [@warning "-69"]
  original_dal_data_dir : string option;  (** Preserved in edit mode *)
  stopped_dependents : string list;
}

let base_initial_model () =
  {
    core =
      {
        instance_name = "dal";
        service_user = Form_builder_common.default_service_user ();
        app_bin_dir =
          Form_builder_common.default_app_bin_dir ~binary_name:"octez-dal-node";
        enable_on_boot = true;
        start_now = true;
        extra_args = "";
      };
    client =
      {
        base_dir =
          Form_builder_common.default_base_dir ~role:"dal" ~instance:"dal";
        node = `None;
        node_endpoint = "127.0.0.1:8732";
      };
    dal_data_dir = Common.default_role_dir "dal-node" "dal";
    rpc_addr = "127.0.0.1:10732";
    net_addr = "0.0.0.0:11732";
    edit_mode = false;
    original_instance = None;
    original_dal_data_dir = None;
    stopped_dependents = [];
  }

let make_initial_model () =
  match Context.take_pending_edit_service () with
  | Some edit_ctx
    when edit_ctx.service.Service.role = "dal-node"
         || edit_ctx.service.Service.role = "dal" ->
      let svc = edit_ctx.service in
      (* Read DAL env to get config *)
      let env =
        match Node_env.read ~inst:svc.Service.instance with
        | Ok pairs -> pairs
        | Error _ -> []
      in
      let lookup key =
        match List.assoc_opt key env with Some v -> String.trim v | None -> ""
      in
      let client_base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
      let node_endpoint = lookup "OCTEZ_NODE_ENDPOINT" in
      let dal_rpc = lookup "OCTEZ_DAL_RPC_ADDR" in
      let dal_net = lookup "OCTEZ_DAL_NET_ADDR" in
      let dal_data_dir = lookup "OCTEZ_DAL_DATA_DIR" in
      let extra_args = lookup "OCTEZ_DAL_EXTRA_ARGS" in
      {
        core =
          {
            instance_name = svc.Service.instance;
            service_user = svc.Service.service_user;
            app_bin_dir = svc.Service.app_bin_dir;
            enable_on_boot = true;
            start_now = false;
            (* Don't auto-start after edit *)
            extra_args;
          };
        client =
          {
            base_dir =
              (if client_base_dir = "" then
                 Common.default_role_dir "dal-node" svc.Service.instance
               else client_base_dir);
            node =
              (match svc.Service.depends_on with
              | Some inst -> `Service inst
              | None -> `Endpoint node_endpoint);
            node_endpoint =
              (if node_endpoint = "" then "127.0.0.1:8732" else node_endpoint);
          };
        dal_data_dir =
          (if dal_data_dir = "" then
             Common.default_role_dir "dal-node" svc.Service.instance
           else dal_data_dir);
        rpc_addr = (if dal_rpc = "" then "127.0.0.1:10732" else dal_rpc);
        net_addr = (if dal_net = "" then "0.0.0.0:11732" else dal_net);
        edit_mode = true;
        original_instance = Some svc.Service.instance;
        original_dal_data_dir =
          (if dal_data_dir = "" then None else Some dal_data_dir);
        stopped_dependents = edit_ctx.stopped_dependents;
      }
  | _ -> base_initial_model ()

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

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  {
    title = " Install DAL Node ";
    initial_model = make_initial_model;
    (* Compose fields from bundles following standard ordering *)
    fields =
      (fun model ->
        (* 1. Dependencies: node (via client_fields_with_autoname) *)
        client_fields_with_autoname
          ~role:"dal"
          ~binary:"octez-dal-node"
          ~binary_validator:Form_builder_common.has_octez_dal_node_binary
          ~get_core:(fun m -> m.core)
          ~set_core:(fun core m -> {m with core})
          ~get_client:(fun m -> m.client)
          ~set_client:(fun client m -> {m with client})
          ~edit_mode:model.edit_mode
          ~skip_base_dir:true (* DAL node doesn't use client base dir *)
          ()
        (* 2. Network params - N/A *)
        (* 3. App bin dir *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-dal-node"
            ~subcommand:["run"]
            ~binary_validator:Form_builder_common.has_octez_dal_node_binary
            ~skip_instance_name:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 4. Base dir - already included in client_fields_with_autoname *)
        (* 5. Role-specific - N/A *)
        (* 6. Addresses and ports *)
        @ [
            (* DAL node's own RPC address - with port validation *)
            Form_builder.validated_text
              ~label:"DAL RPC Addr"
              ~get:(fun m -> m.rpc_addr)
              ~set:(fun rpc_addr m -> {m with rpc_addr})
              ~validate:(fun m ->
                let addr = m.rpc_addr in
                let exclude_instance =
                  if m.edit_mode then m.original_instance else None
                in
                match
                  Port_validation.validate_addr
                    ~addr
                    ?exclude_instance
                    ~example:"127.0.0.1:10732"
                    ()
                with
                | Ok () -> Ok ()
                | Error err ->
                    Error
                      (Printf.sprintf
                         "DAL RPC Addr: %s"
                         (Port_validation.pp_error err)));
            (* DAL node's P2P address - with port validation *)
            Form_builder.validated_text
              ~label:"DAL P2P Addr"
              ~get:(fun m -> m.net_addr)
              ~set:(fun net_addr m -> {m with net_addr})
              ~validate:(fun m ->
                let addr = m.net_addr in
                let exclude_instance =
                  if m.edit_mode then m.original_instance else None
                in
                match
                  Port_validation.validate_addr
                    ~addr
                    ?exclude_instance
                    ~example:"0.0.0.0:11732"
                    ()
                with
                | Ok () -> Ok ()
                | Error err ->
                    Error
                      (Printf.sprintf
                         "DAL P2P Addr: %s"
                         (Port_validation.pp_error err)));
            (* DAL node's data directory *)
            Form_builder.custom
              ~label:"DAL Data Dir"
              ~get:(fun m -> m.dal_data_dir)
              ~validate:(fun m ->
                Form_builder_common.is_nonempty m.dal_data_dir)
              ~validate_msg:(fun _ -> Some "Data directory is required")
              ~edit:(fun model_ref ->
                if model.edit_mode then
                  Modal_helpers.show_error
                    ~title:"DAL Data Dir"
                    "Data directory cannot be changed after creation."
                else
                  Modal_helpers.open_file_browser_modal
                    ~initial_path:!model_ref.dal_data_dir
                    ~dirs_only:true
                    ~require_writable:true
                    ~on_select:(fun path ->
                      model_ref := {!model_ref with dal_data_dir = path})
                    ())
              ();
          ]
        (* 7. Extra args *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-dal-node"
            ~subcommand:["run"]
            ~binary_validator:Form_builder_common.has_octez_dal_node_binary
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 8. Service fields *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-dal-node"
            ~subcommand:["run"]
            ~binary_validator:Form_builder_common.has_octez_dal_node_binary
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_extra_args:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 9. Instance name *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-dal-node"
            ~subcommand:["run"]
            ~binary_validator:Form_builder_common.has_octez_dal_node_binary
            ~skip_app_bin_dir:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ());
    pre_submit =
      Some
        (fun model ->
          (* Validate node selection *)
          match model.client.node with
          | `None -> Error (`Msg "Node selection is required for DAL node")
          | `Service inst ->
              let states = Form_builder_common.cached_service_states () in
              let node_exists =
                List.exists
                  (fun (s : Data.Service_state.t) ->
                    s.service.Service.role = "node"
                    && s.service.Service.instance = inst)
                  states
              in
              if not node_exists then
                Error
                  (`Msg (Printf.sprintf "Node instance '%s' not found" inst))
              else Ok ()
          | `Endpoint ep ->
              if Form_builder_common.is_nonempty ep then Ok ()
              else Error (`Msg "Node endpoint cannot be empty"));
    on_init = None;
    on_refresh = None;
    pre_submit_modal = None;
    on_submit =
      (fun model ->
        let states = Form_builder_common.cached_service_states () in

        (* Resolve node endpoint *)
        let node_endpoint =
          match model.client.node with
          | `Service inst -> (
              let node =
                List.find_opt
                  (fun (s : Data.Service_state.t) ->
                    s.service.Service.role = "node"
                    && s.service.Service.instance = inst)
                  states
              in
              match node with
              | Some n ->
                  let addr =
                    String.trim n.Data.Service_state.service.Service.rpc_addr
                  in
                  Config.endpoint_of_rpc addr
              | None -> "http://127.0.0.1:8732")
          | `Endpoint ep -> Config.endpoint_of_rpc ep
          | `None -> "http://127.0.0.1:8732"
        in

        (* Always use journald - octez binaries handle their own file logging *)
        let logging_mode = Logging_mode.default in

        (* Prepare extra args *)
        let extra_args =
          Form_builder_common.prepare_extra_args model.core.extra_args
        in

        (* Resolve network from node if available *)
        let network =
          match model.client.node with
          | `Service inst -> (
              let node =
                List.find_opt
                  (fun (s : Data.Service_state.t) ->
                    s.service.Service.role = "node"
                    && s.service.Service.instance = inst)
                  states
              in
              match node with
              | Some n -> Some n.Data.Service_state.service.Service.network
              | None -> None)
          | _ -> None
        in

        (* DAL data dir (for --data-dir command option) *)
        (* In edit mode, preserve existing data dir to avoid data loss *)
        (* Otherwise, use the user-provided value from the form *)
        let dal_data_dir =
          if model.edit_mode then
            match model.original_dal_data_dir with
            | Some dir -> dir
            | None -> model.dal_data_dir
          else
            let trimmed = String.trim model.dal_data_dir in
            if trimmed = "" then
              Common.default_role_dir "dal-node" model.core.instance_name
            else trimmed
        in

        (* Service args are command options only (after "run --data-dir") *)
        let service_args = extra_args in

        (* Build daemon request *)
        let depends_on =
          match model.client.node with `Service inst -> Some inst | _ -> None
        in
        let req : Installer_types.daemon_request =
          {
            role = "dal-node";
            instance = model.core.instance_name;
            network = Option.value ~default:"mainnet" network;
            history_mode = History_mode.default;
            data_dir = dal_data_dir;
            rpc_addr = model.rpc_addr;
            (* DAL's own RPC address *)
            net_addr = model.net_addr;
            (* DAL's P2P address *)
            service_user = model.core.service_user;
            app_bin_dir = model.core.app_bin_dir;
            logging_mode;
            service_args;
            extra_env =
              [
                ("OCTEZ_NODE_ENDPOINT", node_endpoint);
                ("OCTEZ_DAL_DATA_DIR", dal_data_dir);
                ("OCTEZ_DAL_RPC_ADDR", model.rpc_addr);
                ("OCTEZ_DAL_NET_ADDR", model.net_addr);
              ];
            extra_paths = [dal_data_dir];
            auto_enable = model.core.enable_on_boot;
            depends_on;
            preserve_data = model.edit_mode;
          }
        in

        (* In edit mode, stop the service before applying changes *)
        let* () =
          if model.edit_mode then
            (* Use original instance name when stopping (may be different if renaming) *)
            let stop_instance =
              Option.value
                ~default:model.core.instance_name
                model.original_instance
            in
            match
              Lifecycle.stop_service ~quiet:true ~instance:stop_instance ()
            with
            | Ok () -> Ok ()
            | Error (`Msg _) ->
                Ok () (* Continue anyway - service might be stopped *)
          else Ok ()
        in
        (* Execute installation *)
        let* () =
          if Common.is_root () then
            System_user.ensure_service_account
              ~quiet:true
              ~name:model.core.service_user
              ()
          else Ok ()
        in
        let* (module PM) = require_package_manager () in
        let* _service = PM.install_daemon ~quiet:true req in
        (* Handle rename: clean up old instance if name changed *)
        let* () =
          match (model.edit_mode, model.original_instance) with
          | true, Some old_name when old_name <> model.core.instance_name ->
              Installer.cleanup_renamed_instance
                ~quiet:true
                ~old_instance:old_name
                ~new_instance:model.core.instance_name
                ()
          | _ -> Ok ()
        in
        (* Invalidate caches so UI shows updated data *)
        Dal_health.clear_instance ~instance:model.core.instance_name ;
        Context.mark_instances_dirty () ;
        (* Queue restart dependents for modal on instances page *)
        if model.edit_mode && model.stopped_dependents <> [] then
          Context.set_pending_restart_dependents model.stopped_dependents ;
        if model.core.start_now then
          match Miaou_interfaces.Service_lifecycle.get () with
          | Some sl ->
              Miaou_interfaces.Service_lifecycle.start
                sl
                ~role:"dal-node"
                ~service:model.core.instance_name
              |> Result.map_error (fun e -> `Msg e)
          | None -> Error (`Msg "Service lifecycle capability not available")
        else Ok ());
  }

module Page = Form_builder.Make (struct
  type nonrec model = model

  let spec = spec
end)

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
