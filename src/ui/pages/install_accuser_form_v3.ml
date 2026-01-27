(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Accuser installation form using field bundles.

    Demonstrates the power of field bundles - this entire form is ~100 lines
    compared to 297 lines in v2, while maintaining full functionality. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_accuser_form_v3"

(** Model uses the bundle config types directly *)
type model = {
  core : Form_builder_common.core_service_config;
  client : Form_builder_common.client_config;
  (* Edit mode fields *)
  edit_mode : bool;
  original_instance : string option; [@warning "-69"]
  stopped_dependents : string list;
}

let base_initial_model () =
  {
    core =
      {
        instance_name = "accuser";
        service_user = Form_builder_common.default_service_user ();
        app_bin_dir =
          Form_builder_common.default_app_bin_dir ~binary_name:"octez-baker";
        bin_source = None;
        enable_on_boot = true;
        start_now = true;
        extra_args = "";
      };
    client =
      {
        base_dir =
          Form_builder_common.default_base_dir
            ~role:"accuser"
            ~instance:"accuser";
        node = `None;
        node_endpoint = "127.0.0.1:8732";
      };
    edit_mode = false;
    original_instance = None;
    stopped_dependents = [];
  }

let make_initial_model () =
  match Context.take_pending_edit_service () with
  | Some edit_ctx when edit_ctx.service.Service.role = "accuser" ->
      let svc = edit_ctx.service in
      (* Read accuser env to get config *)
      let env =
        match Node_env.read ~inst:svc.Service.instance with
        | Ok pairs -> pairs
        | Error _ -> []
      in
      let lookup key =
        match List.assoc_opt key env with Some v -> String.trim v | None -> ""
      in
      let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
      let node_endpoint = lookup "OCTEZ_NODE_ENDPOINT" in
      let extra_args = lookup "OCTEZ_BAKER_COMMAND_ARGS" in
      {
        core =
          {
            instance_name = svc.Service.instance;
            service_user = svc.Service.service_user;
            app_bin_dir = svc.Service.app_bin_dir;
            bin_source = svc.Service.bin_source;
            enable_on_boot = true;
            start_now = false;
            (* Don't auto-start after edit *)
            extra_args;
          };
        client =
          {
            base_dir =
              (if base_dir = "" then
                 Common.default_role_dir "accuser" svc.Service.instance
               else base_dir);
            node =
              (match svc.Service.depends_on with
              | Some inst -> `Service inst
              | None -> `Endpoint node_endpoint);
            node_endpoint =
              (if node_endpoint = "" then "127.0.0.1:8732" else node_endpoint);
          };
        edit_mode = true;
        original_instance = Some svc.Service.instance;
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

(** Custom node selection field with auto-naming *)
let node_selection_field =
  Form_builder.custom
    ~label:"Node"
    ~get:(fun m ->
      match m.client.node with
      | `None -> "None"
      | `Service inst -> inst
      | `Endpoint ep -> if ep = "" then "Custom" else ep)
    ~validate:(fun m ->
      match m.client.node with
      | `None -> false
      | `Service inst ->
          (* Use non-blocking cache to avoid syscalls during typing *)
          let states =
            Form_builder_common.cached_service_states_nonblocking ()
          in
          List.exists
            (fun (s : Data.Service_state.t) ->
              s.service.Service.role = "node"
              && s.service.Service.instance = inst)
            states
      | `Endpoint ep ->
          Form_builder_common.is_nonempty ep
          && Option.is_some (Form_builder_common.parse_host_port ep))
    ~validate_msg:(fun m ->
      match m.client.node with
      | `None -> Some "Node selection is required"
      | `Service inst ->
          (* Use non-blocking cache to avoid syscalls during typing *)
          let states =
            Form_builder_common.cached_service_states_nonblocking ()
          in
          let exists =
            List.exists
              (fun (s : Data.Service_state.t) ->
                s.service.Service.role = "node"
                && s.service.Service.instance = inst)
              states
          in
          if not exists then
            Some (Printf.sprintf "Node instance '%s' not found" inst)
          else None
      | `Endpoint ep ->
          if Option.is_none (Form_builder_common.parse_host_port ep) then
            Some
              "Invalid endpoint format (must be host:port, e.g., \
               127.0.0.1:8732)"
          else None)
    ~edit:(fun model_ref ->
      (* edit uses blocking version - only called when opening modal *)
      let states = Form_builder_common.cached_service_states () in
      let nodes =
        List.filter
          (fun (s : Data.Service_state.t) -> s.service.Service.role = "node")
          states
      in
      let items = (nodes |> List.map (fun n -> `Node n)) @ [`Endpoint] in
      let to_string = function
        | `Node n ->
            let svc = n.Data.Service_state.service in
            Printf.sprintf
              "Node · %s (%s)"
              svc.Service.instance
              svc.Service.network
        | `Endpoint -> "Custom endpoint (host:port)..."
      in
      let on_select = function
        | `Node n ->
            let svc = n.Data.Service_state.service in
            let current_name =
              Form_builder_common.normalize !model_ref.core.instance_name
            in
            let should_autoname =
              (current_name = "" || String.equal current_name "accuser")
              && not !model_ref.edit_mode
            in
            let endpoint = Form_builder_common.endpoint_of_service svc in
            let new_client =
              {
                !model_ref.client with
                node = `Service svc.Service.instance;
                node_endpoint = endpoint;
              }
            in
            model_ref := {!model_ref with client = new_client} ;
            if should_autoname then (
              (* Strip "node-" prefix to avoid "accuser-node-shadownet" *)
              let dep_name =
                let inst = svc.Service.instance in
                if String.starts_with ~prefix:"node-" inst then
                  String.sub inst 5 (String.length inst - 5)
                else inst
              in
              let new_name = Printf.sprintf "accuser-%s" dep_name in
              let default_dir = Common.default_role_dir "accuser" new_name in
              let new_core = {!model_ref.core with instance_name = new_name} in
              let new_client =
                {!model_ref.client with base_dir = default_dir}
              in
              model_ref :=
                {!model_ref with core = new_core; client = new_client} ;
              (* Maybe use app_bin_dir from node *)
              if
                Form_builder_common.has_octez_baker_binary
                  svc.Service.app_bin_dir
                && not
                     (Form_builder_common.has_octez_baker_binary
                        !model_ref.core.app_bin_dir)
              then
                let new_core =
                  {!model_ref.core with app_bin_dir = svc.Service.app_bin_dir}
                in
                model_ref := {!model_ref with core = new_core})
        | `Endpoint ->
            Modal_helpers.prompt_text_modal
              ~title:"Node Endpoint"
              ~placeholder:(Some "host:port (e.g., 127.0.0.1:8732)")
              ~initial:
                (match !model_ref.client.node with
                | `Endpoint ep -> ep
                | _ -> "")
              ~on_submit:(fun ep ->
                let new_client =
                  {
                    !model_ref.client with
                    node = `Endpoint ep;
                    node_endpoint = ep;
                  }
                in
                model_ref := {!model_ref with client = new_client})
              ()
      in
      Modal_helpers.open_choice_modal
        ~title:"Select Node"
        ~items
        ~to_string
        ~on_select
        ())
    ()

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  {
    title = " Install Accuser ";
    initial_model = make_initial_model;
    fields =
      (fun model ->
        (* 1. Dependencies: node *)
        [node_selection_field]
        (* 2. Network params - N/A *)
        (* 3. App bin dir *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-baker"
            ~subcommand:["run"; "accuser"]
            ~binary_validator:Form_builder_common.has_octez_baker_binary
            ~skip_instance_name:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 4. Base dir *)
        @ [
            client_base_dir
              ~label:"Base Dir"
              ~get:(fun m -> m.client.base_dir)
              ~set:(fun base_dir m ->
                {m with client = {m.client with base_dir}})
              ~validate:(fun m ->
                Form_builder_common.is_nonempty m.client.base_dir)
              ();
          ]
        (* 5-6. Role-specific and addresses - N/A *)
        (* 7. Extra args *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-baker"
            ~subcommand:["run"; "accuser"]
            ~binary_validator:Form_builder_common.has_octez_baker_binary
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
            ~binary:"octez-baker"
            ~subcommand:["run"; "accuser"]
            ~binary_validator:Form_builder_common.has_octez_baker_binary
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
            ~binary:"octez-baker"
            ~subcommand:["run"; "accuser"]
            ~binary_validator:Form_builder_common.has_octez_baker_binary
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
          | `None -> Error (`Msg "Node selection is required for accuser")
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
                  if
                    String.starts_with
                      ~prefix:"http://"
                      (String.lowercase_ascii addr)
                    || String.starts_with
                         ~prefix:"https://"
                         (String.lowercase_ascii addr)
                  then addr
                  else "http://" ^ addr
              | None -> "http://127.0.0.1:8732")
          | `Endpoint ep -> ep
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

        (* Build base_dir *)
        let base_dir =
          let trimmed = String.trim model.client.base_dir in
          if trimmed = "" then
            Common.default_role_dir "accuser" model.core.instance_name
          else trimmed
        in

        (* Service args are command options only (after "run accuser") *)
        let service_args = extra_args in

        (* Build daemon request *)
        let depends_on =
          match model.client.node with `Service inst -> Some inst | _ -> None
        in
        let req : Installer_types.daemon_request =
          {
            role = "accuser";
            instance = model.core.instance_name;
            network = Option.value ~default:"shadownet" network;
            history_mode = History_mode.default;
            data_dir =
              Common.default_role_dir "accuser" model.core.instance_name;
            rpc_addr = node_endpoint;
            net_addr = "";
            service_user = model.core.service_user;
            app_bin_dir = model.core.app_bin_dir;
            bin_source = model.core.bin_source;
            logging_mode;
            service_args;
            extra_env =
              [
                ("OCTEZ_CLIENT_BASE_DIR", base_dir);
                ("OCTEZ_NODE_ENDPOINT", node_endpoint);
              ];
            extra_paths = [base_dir];
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
              Removal.cleanup_renamed_instance
                ~quiet:true
                ~old_instance:old_name
                ~new_instance:model.core.instance_name
                ()
          | _ -> Ok ()
        in
        (* Invalidate caches and mark instances dirty to refresh UI *)
        System_metrics_scheduler.invalidate_version
          ~role:"accuser"
          ~instance:model.core.instance_name ;
        Context.mark_instances_dirty () ;
        (* Queue restart dependents for modal on instances page *)
        if model.edit_mode && model.stopped_dependents <> [] then
          Context.set_pending_restart_dependents model.stopped_dependents ;
        if model.core.start_now then
          match Miaou_interfaces.Service_lifecycle.get () with
          | Some sl ->
              Miaou_interfaces.Service_lifecycle.start
                sl
                ~role:"accuser"
                ~service:model.core.instance_name
              |> Result.map_error (fun e -> `Msg e)
          | None -> Error (`Msg "Service lifecycle capability not available")
        else Ok ());
  }

module Page = Form_builder.Make (struct
  type nonrec model = model

  let spec = spec
end)

module For_tests = struct
  let initial_base_dir = (make_initial_model ()).client.base_dir
end

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
