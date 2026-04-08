(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** octez-index installation form. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_index_form_v3"

type model = {
  core : Form_builder_common.core_service_config;
  client : Form_builder_common.client_config;  (** node dependency *)
  base_dir : string;  (** OCTEZ_INDEXER_DIR — locked in edit mode *)
  rpc_addr : string;  (** OCTEZ_INDEX_RPC_ADDR, default 0.0.0.0:8733 *)
  baker : string option;  (** selected baker instance (hint only) *)
  watched_addresses : string list;
  db_name : string;  (** empty = use default; locked in edit mode *)
  edit_mode : bool;
  original_instance : string option; [@warning "-69"]
  original_base_dir : string option;
  stopped_dependents : string list;
}

let base_initial_model () =
  {
    core =
      {
        instance_name = "index";
        service_user = Form_builder_common.default_service_user ();
        app_bin_dir =
          Form_builder_common.default_app_bin_dir ~binary_name:"octez-index";
        bin_source = None;
        enable_on_boot = true;
        start_now = true;
        extra_args = "";
        group = None;
      };
    client =
      {
        base_dir = "";
        (* index has no client base_dir *)
        node = `None;
        node_endpoint = "127.0.0.1:8732";
      };
    base_dir = Paths.default_role_dir "index" "index";
    rpc_addr = "0.0.0.0:8733";
    baker = None;
    watched_addresses = [];
    db_name = "";
    edit_mode = false;
    original_instance = None;
    original_base_dir = None;
    stopped_dependents = [];
  }

let ensure_ports_initialized model_ref =
  let current = !model_ref in
  Form_builder_common.ensure_ports
    ~roles:["index"]
    ~slots:
      [
        {
          current = current.rpc_addr;
          default_host = "0.0.0.0";
          start_port = 8733;
          setter =
            (fun value -> model_ref := {!model_ref with rpc_addr = value});
        };
      ]
    ()

let make_initial_model () =
  match Context.take_pending_edit_service () with
  | Some edit_ctx when edit_ctx.service.Service.role = "index" ->
      let svc = edit_ctx.service in
      let env =
        match Node_env.read ~inst:svc.Service.instance with
        | Ok pairs -> pairs
        | Error _ -> []
      in
      let lookup key =
        match List.assoc_opt key env with Some v -> String.trim v | None -> ""
      in
      let node_endpoint = lookup "OCTEZ_NODE_ENDPOINT" in
      let base_dir = lookup "OCTEZ_INDEXER_DIR" in
      let rpc_addr = lookup "OCTEZ_INDEX_RPC_ADDR" in
      let service_args = lookup "OCTEZ_SERVICE_ARGS" in
      let baker_inst = lookup "OCTEZ_INDEX_BAKER_INST" in
      (* Parse watched_addresses and db_name from OCTEZ_SERVICE_ARGS *)
      let args = Form_builder_common.parse_shellwords service_args in
      let watched_addresses =
        let rec extract = function
          | "--watched-address" :: v :: rest -> v :: extract rest
          | _ :: rest -> extract rest
          | [] -> []
        in
        extract args
      in
      let db_name =
        let rec find = function
          | "--db-name" :: v :: _ -> v
          | _ :: rest -> find rest
          | [] -> ""
        in
        find args
      in
      (* Restore baker hint — verify baker instance still exists *)
      let baker =
        if baker_inst = "" then None
        else
          match Service_registry.find ~instance:baker_inst with
          | Ok (Some _) -> Some baker_inst
          | _ -> None
      in
      {
        core =
          {
            instance_name = svc.Service.instance;
            service_user = svc.Service.service_user;
            app_bin_dir = svc.Service.app_bin_dir;
            bin_source = svc.Service.bin_source;
            enable_on_boot = true;
            start_now = false;
            extra_args = "";
            group = svc.Service.group;
          };
        client =
          {
            base_dir = "";
            node =
              (match svc.Service.depends_on with
              | Some inst -> `Service inst
              | None -> `Endpoint node_endpoint);
            node_endpoint =
              (if node_endpoint = "" then "127.0.0.1:8732" else node_endpoint);
          };
        base_dir =
          (if base_dir = "" then
             Paths.default_role_dir "index" svc.Service.instance
           else base_dir);
        rpc_addr = (if rpc_addr = "" then "0.0.0.0:8733" else rpc_addr);
        baker;
        watched_addresses;
        db_name;
        edit_mode = true;
        original_instance = Some svc.Service.instance;
        original_base_dir = (if base_dir = "" then None else Some base_dir);
        stopped_dependents = edit_ctx.stopped_dependents;
      }
  | _ ->
      let model_ref = ref (base_initial_model ()) in
      ensure_ports_initialized model_ref ;
      !model_ref

(** Baker picker: opens a choice modal of managed baker instances *)
let baker_field =
  Form_builder.custom
    ~label:"Baker (optional)"
    ~get:(fun m -> match m.baker with None -> "none" | Some inst -> inst)
    ~validate:(fun _ -> true)
    ~edit:(fun model_ref ->
      let states = Form_builder_common.cached_service_states () in
      let baker_instances =
        states
        |> List.filter_map (fun (s : Data.Service_state.t) ->
            if s.service.Service.role = "baker" then
              Some s.service.Service.instance
            else None)
      in
      let items = `None :: List.map (fun i -> `Baker i) baker_instances in
      Modal_helpers.open_choice_modal
        ~title:"Baker (optional)"
        ~items
        ~to_string:(function `None -> "None" | `Baker inst -> inst)
        ~on_select:(function
          | `None -> model_ref := {!model_ref with baker = None}
          | `Baker inst -> model_ref := {!model_ref with baker = Some inst})
        ())
    ()

(** Watched-addresses multi-select field *)
let watched_addresses_field =
  Form_builder.custom
    ~label:"Watched Addresses"
    ~get:(fun m ->
      match m.watched_addresses with
      | [] -> "all (empty = watch all)"
      | addrs -> String.concat ", " addrs)
    ~validate:(fun _ -> true)
    ~edit:(fun model_ref ->
      let get_suggestions () =
        match !model_ref.baker with
        | None -> []
        | Some baker_inst -> (
            match Node_env.read ~inst:baker_inst with
            | Error _ -> []
            | Ok pairs -> (
                let baker_base_dir =
                  match List.assoc_opt "OCTEZ_BAKER_BASE_DIR" pairs with
                  | Some d when String.trim d <> "" -> Some (String.trim d)
                  | _ -> None
                in
                match baker_base_dir with
                | None -> []
                | Some base_dir -> (
                    match Keys_reader.read_public_key_hashes ~base_dir with
                    | Ok keys -> List.map (fun k -> k.Keys_reader.value) keys
                    | Error _ -> [])))
      in
      let alias_map =
        let all_keys = Wallets_page.get_all_keys () in
        List.fold_left
          (fun acc (hash, alias, _base_dir) -> (hash, alias) :: acc)
          []
          all_keys
      in
      let build_items () =
        let current = !model_ref.watched_addresses in
        let suggestions = get_suggestions () in
        (current |> List.map (fun d -> `Toggle d))
        @ (suggestions
          |> List.filter (fun s -> not (List.mem s current))
          |> List.map (fun s -> `Toggle s))
        @ [`Add; `Clear]
      in
      let to_string = function
        | `Toggle item ->
            let current = !model_ref.watched_addresses in
            let checked = List.mem item current in
            let checkbox = if checked then "[x]" else "[ ]" in
            let display =
              match List.assoc_opt item alias_map with
              | Some alias -> Printf.sprintf "%s (%s)" alias item
              | None -> item
            in
            Printf.sprintf "%s %s" checkbox display
        | `Add -> "Add address (manual)"
        | `Clear -> "Clear all"
      in
      let on_select = function
        | `Toggle item ->
            let current = !model_ref.watched_addresses in
            let updated =
              if List.mem item current then
                List.filter (fun x -> x <> item) current
              else current @ [item]
            in
            model_ref := {!model_ref with watched_addresses = updated} ;
            `KeepOpen
        | `Add ->
            Modal_helpers.prompt_validated_text_modal
              ~title:"Add Watched Address"
              ~validator:(fun v ->
                if String.trim v = "" then Error "Cannot be empty"
                else if
                  String.starts_with ~prefix:"tz1" v
                  || String.starts_with ~prefix:"tz2" v
                  || String.starts_with ~prefix:"tz3" v
                  || String.starts_with ~prefix:"tz4" v
                then Ok ()
                else Error "Address must start with tz1, tz2, tz3, or tz4")
              ~on_submit:(fun v ->
                let v = String.trim v in
                let current = !model_ref.watched_addresses in
                if not (List.mem v current) then
                  model_ref :=
                    {!model_ref with watched_addresses = current @ [v]})
              () ;
            `KeepOpen
        | `Clear ->
            model_ref := {!model_ref with watched_addresses = []} ;
            `KeepOpen
      in
      Modal_helpers.open_multiselect_modal
        ~title:"Watched Addresses"
        ~items:build_items
        ~to_string
        ~on_select
        ())
    ()

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  {
    title = " Install Index ";
    initial_model = make_initial_model;
    fields =
      (fun model ->
        (* 1. Node dependency *)
        client_fields_with_autoname
          ~role:"index"
          ~binary:"octez-index"
          ~binary_validator:Form_builder_common.has_octez_index_binary
          ~get_core:(fun m -> m.core)
          ~set_core:(fun core m -> {m with core})
          ~get_client:(fun m -> m.client)
          ~set_client:(fun client m -> {m with client})
          ~edit_mode:model.edit_mode
          ~skip_base_dir:true
          ()
        (* 2. Baker picker *)
        @ [baker_field]
        (* 3. App bin dir *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-index"
            ~subcommand:["run"]
            ~binary_validator:Form_builder_common.has_octez_index_binary
            ~skip_instance_name:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 4. RPC address *)
        @ [
            Form_builder.validated_text
              ~label:"RPC Addr"
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
                    ~example:"0.0.0.0:8733"
                    ()
                with
                | Ok () -> Ok ()
                | Error err ->
                    Error
                      (Printf.sprintf
                         "RPC Addr: %s"
                         (Port_validation.pp_error err)));
            (* 5. Base dir — locked in edit mode *)
            Form_builder.custom
              ~label:"Base Dir"
              ~get:(fun m -> m.base_dir)
              ~validate:(fun m -> Form_builder_common.is_nonempty m.base_dir)
              ~validate_msg:(fun _ -> Some "Base directory is required")
              ~edit:(fun model_ref ->
                if model.edit_mode then
                  Modal_helpers.show_error
                    ~title:"Base Dir"
                    "Base directory cannot be changed after creation."
                else
                  Modal_helpers.open_file_browser_modal
                    ~initial_path:!model_ref.base_dir
                    ~dirs_only:true
                    ~require_writable:true
                    ~on_select:(fun path ->
                      model_ref := {!model_ref with base_dir = path})
                    ())
              ();
            (* 6. Watched addresses *)
            watched_addresses_field;
            (* 7. DB name — locked in edit mode *)
            Form_builder.custom
              ~label:"DB Name"
              ~get:(fun m ->
                if m.db_name = "" then "(default: db.sqlite)" else m.db_name)
              ~validate:(fun m ->
                (* No path separators allowed *)
                not (String.contains m.db_name '/'))
              ~validate_msg:(fun _ -> Some "DB name must not contain '/'")
              ~edit:(fun model_ref ->
                if model.edit_mode then
                  Modal_helpers.show_error
                    ~title:"DB Name"
                    "Database name cannot be changed after creation."
                else
                  Modal_helpers.prompt_validated_text_modal
                    ~title:"DB Name (leave empty for default)"
                    ~validator:(fun v ->
                      if String.contains v '/' then
                        Error "DB name must not contain '/'"
                      else Ok ())
                    ~on_submit:(fun v ->
                      model_ref := {!model_ref with db_name = String.trim v})
                    ())
              ();
          ]
        (* 8. Extra args *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-index"
            ~subcommand:["run"]
            ~binary_validator:Form_builder_common.has_octez_index_binary
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 9. Service fields *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-index"
            ~subcommand:["run"]
            ~binary_validator:Form_builder_common.has_octez_index_binary
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_extra_args:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 10. Instance name *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-index"
            ~subcommand:["run"]
            ~binary_validator:Form_builder_common.has_octez_index_binary
            ~skip_app_bin_dir:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 11. Group *)
        @ [
            group_field
              ~get_core:(fun m -> m.core)
              ~set_core:(fun core m -> {m with core})
              ~edit_mode:model.edit_mode
              ();
          ]);
    pre_submit =
      Some
        (fun model ->
          match model.client.node with
          | `None -> Error (`Msg "Node selection is required for octez-index")
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
                  Rpc_addr.to_string
                    n.Data.Service_state.service.Service.rpc_addr
              | None -> "127.0.0.1:8732")
          | `Endpoint ep -> ep
          | `None -> "127.0.0.1:8732"
        in
        let logging_mode = Logging_mode.default in
        let extra_args =
          Form_builder_common.prepare_extra_args model.core.extra_args
        in
        let base_dir =
          if model.edit_mode then
            match model.original_base_dir with
            | Some dir -> dir
            | None -> model.base_dir
          else
            let trimmed = String.trim model.base_dir in
            if trimmed = "" then
              Paths.default_role_dir "index" model.core.instance_name
            else trimmed
        in
        let depends_on =
          match model.client.node with `Service inst -> Some inst | _ -> None
        in
        (* Baker hint goes via extra_env *)
        let baker_env =
          match model.baker with
          | Some inst -> [("OCTEZ_INDEX_BAKER_INST", inst)]
          | None -> []
        in
        let req : Installer_types.index_request =
          {
            instance = model.core.instance_name;
            base_dir;
            rpc_addr = Rpc_addr.of_string model.rpc_addr;
            watched_addresses = model.watched_addresses;
            db_name =
              (let s = String.trim model.db_name in
               if s = "" then None else Some s);
            node_endpoint;
            depends_on;
            service_user = model.core.service_user;
            app_bin_dir = model.core.app_bin_dir;
            bin_source = model.core.bin_source;
            logging_mode;
            extra_args;
            extra_env = baker_env;
            auto_enable = model.core.enable_on_boot;
            preserve_data = model.edit_mode;
          }
        in
        (* In edit mode, stop the service before applying changes *)
        let* () =
          if model.edit_mode then
            let stop_instance =
              Option.value
                ~default:model.core.instance_name
                model.original_instance
            in
            match
              Lifecycle.stop_service ~quiet:true ~instance:stop_instance ()
            with
            | Ok () -> Ok ()
            | Error (`Msg _) -> Ok ()
          else Ok ()
        in
        let* () =
          if Paths.is_root () then
            System_user.ensure_service_account
              ~quiet:true
              ~name:model.core.service_user
              ()
          else Ok ()
        in
        let* (module PM) = Form_builder_common.require_package_manager () in
        let* _service = PM.install_index ~quiet:true req in
        let* () =
          Form_builder_common.set_service_group
            ~instance_name:model.core.instance_name
            ~group:model.core.group
        in
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
        System_metrics_scheduler.invalidate_version
          ~role:"index"
          ~instance:model.core.instance_name ;
        Context.mark_instances_dirty () ;
        if model.edit_mode && model.stopped_dependents <> [] then
          Context.set_pending_restart_dependents model.stopped_dependents ;
        if model.core.start_now then
          match Miaou_interfaces.Service_lifecycle.get () with
          | Some sl ->
              Miaou_interfaces.Service_lifecycle.start
                sl
                ~role:"index"
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
