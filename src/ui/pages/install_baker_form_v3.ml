(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Baker installation form using field bundles.

    Demonstrates bundle composition for complex forms with custom fields. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_baker_form_v3"

(** {1 Custom Types} *)

type dal_selection =
  | Dal_none
  | Dal_instance of string
  | Dal_endpoint of string

(** {1 Model} *)

type model = {
  core : Form_builder_common.core_service_config;
  client : Form_builder_common.client_config;
  (* Baker-specific fields *)
  parent_node : string; (* empty = external *)
  node_data_dir : string;
  dal : dal_selection;
  delegates : string list;
  liquidity_baking_vote : string;
  (* Edit mode fields *)
  edit_mode : bool;
  original_instance : string option;
  stopped_dependents : string list;
}

let base_initial_model () =
  {
    core =
      {
        instance_name = "baker";
        service_user = Form_builder_common.default_service_user ();
        app_bin_dir =
          Form_builder_common.default_app_bin_dir ~binary_name:"octez-baker";
        enable_on_boot = true;
        start_now = true;
        extra_args = "";
      };
    client =
      {
        base_dir = Common.default_role_dir "baker" "baker";
        node = `None;
        node_endpoint = "127.0.0.1:8732";
      };
    parent_node = "";
    node_data_dir = "";
    dal = Dal_none;
    delegates = [];
    liquidity_baking_vote = "pass";
    edit_mode = false;
    original_instance = None;
    stopped_dependents = [];
  }

let make_initial_model () =
  match Context.take_pending_edit_service () with
  | Some edit_ctx when edit_ctx.service.Service.role = "baker" ->
      let svc = edit_ctx.service in
      (* Read baker env to get delegates and other config *)
      let env =
        match Node_env.read ~inst:svc.Service.instance with
        | Ok pairs -> pairs
        | Error _ -> []
      in
      let lookup key =
        match List.assoc_opt key env with Some v -> String.trim v | None -> ""
      in
      let delegates =
        match lookup "OCTEZ_BAKER_DELEGATES_CSV" with
        | "" -> []
        | csv ->
            csv |> String.split_on_char ',' |> List.map String.trim
            |> List.filter (( <> ) "")
      in
      let node_endpoint = lookup "OCTEZ_NODE_ENDPOINT" in
      let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
      let extra_args = lookup "OCTEZ_BAKER_EXTRA_ARGS" in
      let dal_config = lookup "OCTEZ_DAL_CONFIG" in
      let dal_instance = lookup "OCTEZ_DAL_INSTANCE" in
      let dal =
        if dal_config = "disabled" then Dal_none
        else if dal_instance <> "" then Dal_instance dal_instance
        else if dal_config = "" then Dal_none
        else Dal_endpoint dal_config
      in
      let lb_vote = lookup "OCTEZ_BAKER_LIQUIDITY_BAKING_VOTE" in
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
              (if base_dir = "" then
                 Common.default_role_dir "baker" svc.Service.instance
               else base_dir);
            node =
              (match svc.Service.depends_on with
              | Some inst -> `Service inst
              | None -> `None);
            node_endpoint =
              (if node_endpoint = "" then "127.0.0.1:8732" else node_endpoint);
          };
        parent_node =
          (match svc.Service.depends_on with Some inst -> inst | None -> "");
        node_data_dir = "";
        dal;
        delegates;
        liquidity_baking_vote = (if lb_vote = "" then "pass" else lb_vote);
        edit_mode = true;
        original_instance = Some svc.Service.instance;
        stopped_dependents = edit_ctx.stopped_dependents;
      }
  | _ -> base_initial_model ()

(** {1 Helper Functions} *)

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

let has_octez_baker_binary dir =
  let trimmed = String.trim dir in
  if trimmed = "" then false
  else
    let candidate = Filename.concat trimmed "octez-baker" in
    Sys.file_exists candidate
    &&
      try
        Unix.access candidate [Unix.X_OK] ;
        true
      with Unix.Unix_error _ -> false

let node_services states =
  states
  |> List.filter (fun (s : Data.Service_state.t) ->
      String.equal s.service.Service.role "node")

let find_node states inst =
  node_services states
  |> List.find_opt (fun (s : Data.Service_state.t) ->
      String.equal
        (Form_builder_common.normalize s.service.Service.instance)
        (Form_builder_common.normalize inst))

let dal_services states =
  states
  |> List.filter (fun (s : Data.Service_state.t) ->
      let role = Form_builder_common.normalize s.service.Service.role in
      String.equal role "dal-node" || String.equal role "dal")

let find_dal states inst =
  dal_services states
  |> List.find_opt (fun (s : Data.Service_state.t) ->
      String.equal
        (Form_builder_common.normalize s.service.Service.instance)
        (Form_builder_common.normalize inst))

let endpoint_with_scheme rpc_addr =
  let trimmed = String.trim rpc_addr in
  if trimmed = "" then "http://127.0.0.1:8732"
  else if
    String.starts_with ~prefix:"http://" (String.lowercase_ascii trimmed)
    || String.starts_with ~prefix:"https://" (String.lowercase_ascii trimmed)
  then trimmed
  else "http://" ^ trimmed

let node_endpoint_of_service (svc : Service.t) =
  endpoint_with_scheme svc.Service.rpc_addr

let dal_endpoint_of_service (svc : Service.t) =
  endpoint_with_scheme svc.Service.rpc_addr

let endpoint_host_port ep =
  let ep = endpoint_with_scheme ep in
  try
    let uri = Uri.of_string ep in
    match (Uri.host uri, Uri.port uri) with
    | Some host, Some port -> Printf.sprintf "%s:%d" host port
    | Some host, None -> Printf.sprintf "%s:8732" host
    | _ -> ep
  with _ -> ep

let baker_node_mode model states =
  match find_node states model.parent_node with
  | Some _ -> `Local
  | None ->
      if Form_builder_common.is_nonempty model.node_data_dir then `Local
      else `Remote

let resolve_node_data_dir model states =
  match find_node states model.parent_node with
  | Some svc -> svc.Data.Service_state.service.Service.data_dir
  | None -> String.trim model.node_data_dir

let resolve_node_endpoint model states =
  match find_node states model.parent_node with
  | Some svc -> node_endpoint_of_service svc.Data.Service_state.service
  | None -> endpoint_with_scheme model.client.node_endpoint

let resolve_dal_config model states =
  match model.dal with
  | Dal_none -> Dal_disabled
  | Dal_endpoint ep -> Dal_endpoint (endpoint_with_scheme ep)
  | Dal_instance inst -> (
      match find_dal states inst with
      | Some svc ->
          Dal_endpoint (dal_endpoint_of_service svc.Data.Service_state.service)
      | None -> Dal_auto)

(** {1 Custom Fields} *)

let parent_node_field =
  Form_builder.custom
    ~label:"Parent Node"
    ~get:(fun m -> if m.parent_node = "" then "External" else m.parent_node)
    ~edit:(fun model_ref ->
      let states = Form_builder_common.cached_service_states () in
      let nodes = node_services states in
      let items = `External :: List.map (fun n -> `Node n) nodes in
      let to_string = function
        | `External -> "External/None (use custom endpoint)"
        | `Node n ->
            let svc = n.Data.Service_state.service in
            Printf.sprintf
              "Node · %s (%s)"
              svc.Service.instance
              svc.Service.network
      in
      let on_select = function
        | `External ->
            model_ref :=
              {
                !model_ref with
                parent_node = "";
                client = {!model_ref.client with node = `None};
              }
        | `Node n ->
            let svc = n.Data.Service_state.service in
            let current_name =
              Form_builder_common.normalize !model_ref.core.instance_name
            in
            let should_autoname =
              current_name = "" || String.equal current_name "baker"
            in
            model_ref :=
              {
                !model_ref with
                parent_node = svc.Service.instance;
                (* Derive endpoint + data dir from parent node *)
                client =
                  {
                    !model_ref.client with
                    node_endpoint = node_endpoint_of_service svc;
                  };
                node_data_dir = svc.Service.data_dir;
              } ;
            if should_autoname then (
              let new_name = Printf.sprintf "baker-%s" svc.Service.instance in
              let default_dir = Common.default_role_dir "baker" new_name in
              let new_core = {!model_ref.core with instance_name = new_name} in
              let new_client =
                {!model_ref.client with base_dir = default_dir}
              in
              model_ref :=
                {!model_ref with core = new_core; client = new_client} ;
              (* Maybe use app_bin_dir from node *)
              if
                has_octez_baker_binary svc.Service.app_bin_dir
                && not (has_octez_baker_binary !model_ref.core.app_bin_dir)
              then
                let new_core =
                  {!model_ref.core with app_bin_dir = svc.Service.app_bin_dir}
                in
                model_ref := {!model_ref with core = new_core})
      in
      Modal_helpers.open_choice_modal
        ~title:"Parent Node"
        ~items
        ~to_string
        ~on_select)
    ()

let dal_node_field =
  Form_builder.custom
    ~label:"DAL Node"
    ~get:(fun m ->
      match m.dal with
      | Dal_none -> "None"
      | Dal_instance inst -> inst
      | Dal_endpoint ep -> if ep = "" then "Custom" else ep)
    ~validate:(fun m ->
      match m.dal with
      | Dal_none -> true
      | Dal_instance inst ->
          (* Use non-blocking cache to avoid syscalls during typing *)
          let states =
            Form_builder_common.cached_service_states_nonblocking ()
          in
          Option.is_some (find_dal states inst)
      | Dal_endpoint ep ->
          Option.is_some
            (Form_builder_common.parse_host_port (endpoint_host_port ep)))
    ~edit:(fun model_ref ->
      (* edit uses blocking version - only called when opening modal *)
      let states = Form_builder_common.cached_service_states () in
      let dal_nodes = dal_services states in
      let items =
        [`None] @ (dal_nodes |> List.map (fun n -> `Dal n)) @ [`Custom]
      in
      let to_string = function
        | `None -> "None"
        | `Dal n ->
            let svc = n.Data.Service_state.service in
            let endpoint = dal_endpoint_of_service svc in
            Printf.sprintf "DAL · %s (%s)" svc.Service.instance endpoint
        | `Custom -> "Custom endpoint"
      in
      let on_select choice =
        match choice with
        | `None -> model_ref := {!model_ref with dal = Dal_none}
        | `Dal n ->
            let svc = n.Data.Service_state.service in
            model_ref :=
              {!model_ref with dal = Dal_instance svc.Service.instance} ;
            (* Maybe use app_bin_dir from DAL service *)
            if
              has_octez_baker_binary svc.Service.app_bin_dir
              && not (has_octez_baker_binary !model_ref.core.app_bin_dir)
            then
              let new_core =
                {!model_ref.core with app_bin_dir = svc.Service.app_bin_dir}
              in
              model_ref := {!model_ref with core = new_core}
        | `Custom ->
            Modal_helpers.prompt_text_modal
              ~title:"DAL Endpoint"
              ~placeholder:(Some "host:port (e.g., 127.0.0.1:10732)")
              ~initial:
                (match !model_ref.dal with Dal_endpoint ep -> ep | _ -> "")
              ~on_submit:(fun ep ->
                model_ref := {!model_ref with dal = Dal_endpoint ep})
              ()
      in
      Modal_helpers.open_choice_modal
        ~title:"DAL Node"
        ~items
        ~to_string
        ~on_select)
    ()

let node_data_dir_field =
  Form_builder.custom
    ~label:"Node Data Dir"
    ~get:(fun m -> m.node_data_dir)
    ~validate:(fun m ->
      (* Use non-blocking cache to avoid syscalls during typing *)
      let states = Form_builder_common.cached_service_states_nonblocking () in
      let selected_node = find_node states m.parent_node in
      let node_mode = baker_node_mode m states in
      match node_mode with
      | `Local ->
          Form_builder_common.is_nonempty (resolve_node_data_dir m states)
          || Option.is_some selected_node
      | `Remote -> true)
    ~validate_msg:(fun m ->
      (* Use non-blocking cache to avoid syscalls during typing *)
      let states = Form_builder_common.cached_service_states_nonblocking () in
      let node_mode = baker_node_mode m states in
      match node_mode with
      | `Local -> Some "Node data directory is required for local mode"
      | `Remote -> None)
    ~edit:(fun model_ref ->
      (* edit uses blocking version - only called when opening modal *)
      let states = Form_builder_common.cached_service_states () in
      match find_node states !model_ref.parent_node with
      | Some _ ->
          Modal_helpers.show_error
            ~title:"Node Data Dir"
            "Derived from the selected parent node. Clear Parent Node to edit."
      | None ->
          Modal_helpers.select_node_data_dir_modal
            ~on_select:(fun path ->
              model_ref := {!model_ref with node_data_dir = path})
            ())
    ()

let node_endpoint_field =
  Form_builder.custom
    ~label:"Node Endpoint"
    ~get:(fun m -> m.client.node_endpoint)
    ~validate:(fun m ->
      match
        Form_builder_common.parse_host_port
          (endpoint_host_port m.client.node_endpoint)
      with
      | Some _ -> true
      | None -> false)
    ~validate_msg:(fun _ ->
      Some "Format must be host:port (e.g., 127.0.0.1:8732)")
    ~edit:(fun model_ref ->
      let states = Form_builder_common.cached_service_states () in
      match find_node states !model_ref.parent_node with
      | Some _ ->
          Modal_helpers.show_error
            ~title:"Node Endpoint"
            "Derived from the selected parent node. Clear Parent Node to edit."
      | None ->
          Modal_helpers.prompt_validated_text_modal
            ~title:"Node Endpoint (host:port)"
            ~initial:!model_ref.client.node_endpoint
            ~validator:(fun text ->
              match
                Form_builder_common.parse_host_port (endpoint_host_port text)
              with
              | Some _ -> Ok ()
              | None -> Error "Format must be host:port (e.g., 127.0.0.1:8732)")
            ~on_submit:(fun v ->
              let client = !model_ref.client in
              model_ref :=
                {!model_ref with client = {client with node_endpoint = v}})
            ())
    ()

(** {1 Form Specification} *)

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  let baker_mode_for_help model =
    try baker_node_mode model (Form_builder_common.cached_service_states ())
    with _ -> `Remote
  in
  {
    title = " Install Baker ";
    initial_model = make_initial_model;
    fields =
      (fun model ->
        (* Base dir field - readonly in edit mode *)
        let base_dir_field =
          if model.edit_mode then
            readonly ~label:"Baker Base Dir" ~get:(fun m -> m.client.base_dir)
            |> with_hint "Base directory cannot be changed after creation."
          else
            client_base_dir
              ~label:"Baker Base Dir"
              ~get:(fun m -> m.client.base_dir)
              ~set:(fun base_dir m ->
                {m with client = {m.client with base_dir}})
              ~validate:(fun m ->
                Form_builder_common.is_nonempty m.client.base_dir)
              ()
        in
        [
          (* Instance name with auto-update of base_dir (only in create mode) *)
          validated_text
            ~label:"Instance Name"
            ~get:(fun m -> m.core.instance_name)
            ~set:(fun instance_name m ->
              let old = m.core.instance_name in
              let default_dir = Common.default_role_dir "baker" instance_name in
              (* In edit mode, never auto-change base_dir - data is already there *)
              let keep_base_dir =
                m.edit_mode
                || String.trim m.client.base_dir <> ""
                   && not
                        (String.equal
                           m.client.base_dir
                           (Common.default_role_dir "baker" old))
              in
              let new_core = {m.core with instance_name} in
              let new_client =
                {
                  m.client with
                  base_dir =
                    (if keep_base_dir then m.client.base_dir else default_dir);
                }
              in
              {m with core = new_core; client = new_client})
            ~validate:(fun m ->
              let states = Form_builder_common.cached_service_states () in
              if not (Form_builder_common.is_nonempty m.core.instance_name) then
                Error "Instance name is required"
              else if
                Form_builder_common.instance_in_use ~states m.core.instance_name
                && not
                     (m.edit_mode
                     && m.original_instance = Some m.core.instance_name)
              then Error "Instance name already exists"
              else Ok ());
          parent_node_field;
          dal_node_field;
          node_endpoint_field;
          (* Node data dir - readonly in edit mode, interactive in create mode *)
          (if model.edit_mode then
             readonly ~label:"Node Data Dir" ~get:(fun m ->
                 let states =
                   Form_builder_common.cached_service_states_nonblocking ()
                 in
                 resolve_node_data_dir m states)
             |> with_hint
                  "Node data directory cannot be changed after creation."
           else node_data_dir_field);
          base_dir_field;
          string_list
            ~label:"Delegates"
            ~get:(fun m -> m.delegates)
            ~set:(fun delegates m -> {m with delegates})
            ~get_suggestions:(fun m ->
              if String.trim m.client.base_dir = "" then []
              else
                match
                  Keys_reader.read_public_key_hashes ~base_dir:m.client.base_dir
                with
                | Ok keys -> List.map (fun k -> k.Keys_reader.value) keys
                | Error _ -> [])
            ();
          choice
            ~label:"Liquidity Baking Vote"
            ~get:(fun m -> m.liquidity_baking_vote)
            ~set:(fun liquidity_baking_vote m -> {m with liquidity_baking_vote})
            ~items:["pass"; "on"; "off"]
            ~to_string:(fun x -> x);
        ]
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-baker"
            ~subcommand:["run"]
            ~baker_mode:baker_mode_for_help
            ~binary_validator:has_octez_baker_binary
            ~skip_instance_name:true
            ~edit_mode:model.edit_mode
            ());
    pre_submit = None;
    on_init = None;
    on_refresh = None;
    pre_submit_modal = None;
    on_submit =
      (fun model ->
        let states = Form_builder_common.cached_service_states () in
        let selected_node = find_node states model.parent_node in
        let node_endpoint = resolve_node_endpoint model states in
        let dal_config = resolve_dal_config model states in

        (* Always use journald - octez binaries handle their own file logging *)
        let logging_mode = Logging_mode.default in

        let extra_args =
          Form_builder_common.prepare_extra_args model.core.extra_args
        in

        let base_dir =
          let trimmed = String.trim model.client.base_dir in
          if trimmed = "" then
            Common.default_role_dir "baker" model.core.instance_name
          else trimmed
        in

        (* Extract DAL node instance name if using local DAL *)
        let dal_node =
          match model.dal with Dal_instance inst -> Some inst | _ -> None
        in
        let req =
          {
            Installer_types.instance = model.core.instance_name;
            node_mode =
              (match selected_node with
              | Some svc ->
                  Installer_types.Local_instance
                    svc.Data.Service_state.service.Service.instance
              | None -> Installer_types.Remote_endpoint node_endpoint);
            base_dir = Some base_dir;
            delegates = model.delegates;
            dal_config;
            dal_node;
            liquidity_baking_vote =
              (if String.trim model.liquidity_baking_vote = "" then None
               else Some (String.trim model.liquidity_baking_vote));
            extra_args;
            service_user = model.core.service_user;
            app_bin_dir = model.core.app_bin_dir;
            logging_mode;
            auto_enable = model.core.enable_on_boot;
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
              Installer.stop_service ~quiet:true ~instance:stop_instance ()
            with
            | Ok () -> Ok ()
            | Error (`Msg _) ->
                Ok () (* Continue anyway - service might be stopped *)
          else Ok ()
        in
        let* () =
          if Common.is_root () then
            System_user.ensure_service_account
              ~quiet:true
              ~name:model.core.service_user
              ()
          else Ok ()
        in
        let* (module PM) = require_package_manager () in
        let* _ = PM.install_baker ~quiet:true req in
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
        (* Queue restart dependents for modal on instances page *)
        if model.edit_mode && model.stopped_dependents <> [] then
          Context.set_pending_restart_dependents model.stopped_dependents ;
        if model.core.start_now then
          match Miaou_interfaces.Service_lifecycle.get () with
          | Some sl ->
              Miaou_interfaces.Service_lifecycle.start
                sl
                ~role:"baker"
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
  let initial_model = make_initial_model

  let baker_node_mode = baker_node_mode
end

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
