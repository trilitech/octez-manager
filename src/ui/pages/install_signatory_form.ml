(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Signatory remote signer installation form using field bundles.
    
    Implements installation UI for Signatory remote signers with File backend
    support. Allows creating new instances and editing existing ones. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

(** Compute default keys directory for signatory instance.
    Matches the installer's signatory_data_dir and keys_dir logic. *)
let default_keys_dir instance =
  let base =
    if Paths.is_root () then "/var/lib/octez"
    else Filename.concat (Paths.xdg_data_home ()) "octez"
  in
  let data_dir = Filename.concat (Filename.concat base "signatory") instance in
  Filename.concat data_dir "keys"

(** Compute default data directory for signatory instance.
    Matches the installer's signatory_data_dir logic. *)
let default_signatory_data_dir instance =
  let base =
    if Paths.is_root () then "/var/lib/octez"
    else Filename.concat (Paths.xdg_data_home ()) "octez"
  in
  Filename.concat (Filename.concat base "signatory") instance

let name = "install_signatory_form"

(** Model contains both common service config and signatory-specific fields *)
type model = {
  core : Form_builder_common.core_service_config;
  (* Signatory-specific fields *)
  backend : signatory_backend;
  authorized_keys : authorized_key list; (* Changed to authorized_key list *)
  address : string;
  metrics_address : string;
  watermark : watermark_backend;
  (* Edit mode fields *)
  edit_mode : bool;
  original_instance : string option;
  stopped_dependents : string list;
}

let base_initial_model () =
  {
    core =
      {
        instance_name = "signatory";
        service_user = Form_builder_common.default_service_user ();
        app_bin_dir = Form_builder_common.default_signatory_app_bin_dir ();
        bin_source = None;
        enable_on_boot = true;
        start_now = true;
        extra_args = "";
        group = Context.take_pending_group ();
      };
    backend = File (default_keys_dir "signatory");
    authorized_keys = [];
    address = "127.0.0.1:6732";
    metrics_address = "";
    watermark = Memory;
    edit_mode = false;
    original_instance = None;
    stopped_dependents = [];
  }

let make_initial_model () =
  match Context.take_pending_edit_service () with
  | Some edit_ctx when edit_ctx.service.Service.role = "signatory" ->
      let svc = edit_ctx.service in
      (* Read signatory env to get config *)
      let env =
        match Node_env.read ~inst:svc.Service.instance with
        | Ok pairs -> pairs
        | Error _ -> []
      in
      let lookup key =
        match List.assoc_opt key env with Some v -> String.trim v | None -> ""
      in
      (* Parse backend from env - only File backend is supported *)
      let keys_dir = lookup "SIGNATORY_KEYS_DIR" in
      let backend =
        let dir =
          if keys_dir <> "" then keys_dir
          else default_keys_dir svc.Service.instance
        in
        File dir
      in
      (* Parse authorized keys from signatory.yaml *)
      let authorized_keys =
        match
          Signatory_config.get_authorized_keys ~instance:svc.Service.instance
        with
        | Ok key_infos ->
            (* Convert key_info list to authorized_key list *)
            List.map
              (fun (key_info : Signatory_config.key_info) ->
                let permissions =
                  List.filter_map
                    Signatory_config.operation_of_string
                    key_info.allows
                in
                {pkh = key_info.pkh; permissions})
              key_infos
        | Error _ -> []
      in
      let address = lookup "SIGNATORY_ADDRESS" in
      let metrics_address = lookup "SIGNATORY_METRICS_ADDRESS" in
      (* Parse watermark backend *)
      let watermark_backend = lookup "SIGNATORY_WATERMARK_BACKEND" in
      let watermark =
        if watermark_backend = "file" then
          let path = lookup "SIGNATORY_WATERMARK_FILE" in
          File_watermark
            (if path = "" then
               default_signatory_data_dir svc.Service.instance
               ^ "/watermark.json"
             else path)
        else Memory
      in
      let extra_args = lookup "SIGNATORY_COMMAND_ARGS" in
      {
        core =
          {
            instance_name = svc.Service.instance;
            service_user = svc.Service.service_user;
            app_bin_dir = svc.Service.app_bin_dir;
            bin_source = svc.Service.bin_source;
            enable_on_boot =
              Option.value ~default:true svc.Service.enabled_on_boot;
            start_now = false;
            extra_args;
            group = svc.Service.group;
          };
        backend;
        authorized_keys;
        address = (if address = "" then "127.0.0.1:6732" else address);
        metrics_address;
        (* Keep as-is, even if empty *)
        watermark;
        edit_mode = true;
        original_instance = Some svc.Service.instance;
        stopped_dependents = edit_ctx.stopped_dependents;
      }
  | _ -> base_initial_model ()

(** Validate Tezos public key hash format *)
let validate_tezos_key key =
  let key = String.trim key in
  if String.length key < 36 then false
  else
    let prefixes = ["tz1"; "tz2"; "tz3"; "tz4"] in
    List.exists (fun prefix -> String.starts_with ~prefix key) prefixes

(** Backend selection field with modal *)
let backend_field =
  Form_builder.custom
    ~label:"Backend"
    ~get:(fun m ->
      match m.backend with
      | File path -> Printf.sprintf "File (%s)" path
      | _ -> "File")
    ~validate:(fun m ->
      match m.backend with
      | File path -> Form_builder_common.is_nonempty path
      | _ -> false)
    ~validate_msg:(fun m ->
      match m.backend with
      | File path when not (Form_builder_common.is_nonempty path) ->
          Some "Keys directory path is required"
      | _ -> None)
    ~edit:(fun model_ref ->
      let items = [`File] in
      let to_string = function
        | `File -> "File · Keys stored in local directory"
      in
      let on_select choice =
        match choice with
        | `File ->
            Modal_helpers.prompt_text_modal
              ~title:"Keys Directory"
              ~placeholder:(Some "/path/to/keys")
              ~initial:
                (match !model_ref.backend with
                | File path -> path
                | _ -> default_keys_dir !model_ref.core.instance_name)
              ~on_submit:(fun path ->
                model_ref := {!model_ref with backend = File path})
              ()
      in
      Modal_helpers.open_choice_modal
        ~title:"Backend Type"
        ~items
        ~to_string
        ~on_select
        ())
    ()
  |> Form_builder.with_hint
       "Key storage backend (only File is currently supported)"

(** Helper to open permissions configuration modal for a key. *)
let open_permissions_modal ~pkh ~initial_permissions ~on_submit () =
  (* Track selected permissions using a ref *)
  let selected = ref initial_permissions in
  let all_ops = Signatory_config.all_operations in

  (* Build items function that returns updated state with Done button *)
  let items () =
    let perm_items =
      List.map
        (fun op ->
          let is_selected = List.mem op !selected in
          `Permission (op, is_selected))
        all_ops
    in
    perm_items @ [`Done]
  in

  (* Extract stable key from item for cursor tracking *)
  let item_key = function
    | `Permission (op, _) -> `Op op
    | `Done -> `DoneButton
  in

  let to_string = function
    | `Permission (op, is_selected) ->
        let checkbox = if is_selected then "[✓] " else "[ ] " in
        let name = Signatory_config.operation_to_string op in
        checkbox ^ name
    | `Done ->
        let count = List.length !selected in
        Printf.sprintf "✓ Done (%d selected)" count
  in

  let on_select = function
    | `Permission (op, is_selected) ->
        if is_selected then
          (* Deselect: remove from list *)
          selected := List.filter (fun o -> o <> op) !selected
        else
          (* Select: add to list *)
          selected := op :: !selected ;
        `KeepOpen
    | `Done ->
        (* Defer on_submit until after modal closes to avoid timing issues *)
        Background_runner.enqueue (fun () -> on_submit !selected) ;
        `Close
  in

  (* Open multiselect modal with cursor preservation *)
  Modal_helpers.open_multiselect_modal
    ~title:
      (Printf.sprintf
         "Configure Permissions for %s"
         (if String.length pkh > 40 then String.sub pkh 0 37 ^ "..." else pkh))
    ~items
    ~to_string
    ~item_key
    ~on_select
    ()

(** Authorized keys list editor *)
let authorized_keys_field =
  Form_builder.custom
    ~label:"Authorized Keys"
    ~get:(fun m ->
      match m.authorized_keys with
      | [] -> "(none)"
      | keys -> Printf.sprintf "%d key(s)" (List.length keys))
    ~validate:(fun m -> m.authorized_keys <> [])
    ~validate_msg:(fun m ->
      if m.authorized_keys = [] then
        Some "At least one authorized key is required"
      else None)
    ~edit:(fun model_ref ->
      let to_string = function
        | `Key (_, (key : authorized_key)) ->
            let pkh = key.pkh in
            let perm_count = List.length key.permissions in
            let suffix =
              Printf.sprintf
                " (%d perm%s)"
                perm_count
                (if perm_count = 1 then "" else "s")
            in
            let display_pkh =
              if String.length pkh > 35 then String.sub pkh 0 32 ^ "..."
              else pkh
            in
            display_pkh ^ suffix
        | `Add -> "+ Add new key"
      in
      let rec open_menu () =
        let on_select = function
          | `Key (idx, key) ->
              (* Show menu: edit permissions or remove *)
              let items = [`EditPermissions; `Remove; `Cancel] in
              let to_string = function
                | `EditPermissions -> "Edit permissions"
                | `Remove -> "Remove key"
                | `Cancel -> "Cancel"
              in
              let on_select_action = function
                | `EditPermissions ->
                    (* Open permissions modal for existing key *)
                    open_permissions_modal
                      ~pkh:key.pkh
                      ~initial_permissions:key.permissions
                      ~on_submit:(fun perms ->
                        let updated_keys =
                          List.mapi
                            (fun i k ->
                              if i = idx then {k with permissions = perms}
                              else k)
                            !model_ref.authorized_keys
                        in
                        model_ref :=
                          {!model_ref with authorized_keys = updated_keys} ;
                        open_menu ())
                      ()
                | `Remove ->
                    let new_keys =
                      List.filteri
                        (fun i _ -> i <> idx)
                        !model_ref.authorized_keys
                    in
                    model_ref := {!model_ref with authorized_keys = new_keys} ;
                    if new_keys <> [] then open_menu ()
                | `Cancel -> open_menu ()
              in
              Modal_helpers.open_choice_modal
                ~title:"Key Action"
                ~items
                ~to_string
                ~on_select:on_select_action
                ()
          | `Add ->
              (* Show submenu: manual entry or select from existing *)
              let items = [`SelectExisting; `Manual] in
              let to_string = function
                | `SelectExisting -> "Select from existing keys"
                | `Manual -> "Enter manually"
              in
              let on_select_add_method = function
                | `Manual ->
                    Modal_helpers.prompt_text_modal
                      ~title:"Add Authorized Key"
                      ~placeholder:
                        (Some "tz1abc... or tz2abc... or tz3abc... or tz4abc...")
                      ~initial:""
                      ~on_submit:(fun key ->
                        let key = String.trim key in
                        if not (validate_tezos_key key) then (
                          Context.toast_error "Invalid Tezos key format" ;
                          open_menu ())
                        else if
                          List.exists
                            (fun (k : authorized_key) -> k.pkh = key)
                            !model_ref.authorized_keys
                        then (
                          Context.toast_error "Key already exists" ;
                          open_menu ())
                        else
                          (* Open permissions modal after key validation *)
                          open_permissions_modal
                            ~pkh:key
                            ~initial_permissions:
                              (Signatory_config.default_permissions ())
                            ~on_submit:(fun perms ->
                              let new_key = {pkh = key; permissions = perms} in
                              model_ref :=
                                {
                                  !model_ref with
                                  authorized_keys =
                                    !model_ref.authorized_keys @ [new_key];
                                } ;
                              open_menu ())
                            ())
                      ()
                | `SelectExisting ->
                    (* Get all keys from all base directories *)
                    let all_keys = Wallets_page.get_all_keys () in
                    if all_keys = [] then (
                      Context.toast_warn "No keys found in any base directory" ;
                      open_menu ())
                    else
                      let key_items =
                        List.map
                          (fun (hash, alias, base_dir) ->
                            (hash, alias, base_dir))
                          all_keys
                      in
                      let to_string (hash, alias, _base_dir) =
                        Printf.sprintf "%s (%s)" alias hash
                      in
                      let on_select_key (key_hash, _alias, _base_dir) =
                        if
                          List.exists
                            (fun (k : authorized_key) -> k.pkh = key_hash)
                            !model_ref.authorized_keys
                        then (
                          Context.toast_error "Key already added" ;
                          open_menu ())
                        else
                          (* Open permissions modal after key selection *)
                          open_permissions_modal
                            ~pkh:key_hash
                            ~initial_permissions:
                              (Signatory_config.default_permissions ())
                            ~on_submit:(fun perms ->
                              let new_key =
                                {pkh = key_hash; permissions = perms}
                              in
                              model_ref :=
                                {
                                  !model_ref with
                                  authorized_keys =
                                    !model_ref.authorized_keys @ [new_key];
                                } ;
                              open_menu ())
                            ()
                      in
                      Modal_helpers.open_choice_modal
                        ~title:"Select Key"
                        ~items:key_items
                        ~to_string
                        ~on_select:on_select_key
                        ()
              in
              Modal_helpers.open_choice_modal
                ~title:"Add Authorized Key"
                ~items
                ~to_string
                ~on_select:on_select_add_method
                ()
        in
        let items =
          List.mapi (fun i key -> `Key (i, key)) !model_ref.authorized_keys
          @ [`Add]
        in
        Modal_helpers.open_choice_modal
          ~title:"Authorized Keys"
          ~items
          ~to_string
          ~on_select
          ()
      in
      open_menu ())
    ()

(** HTTP address field *)
let address_field =
  Form_builder.validated_text
    ~label:"HTTP Address"
    ~get:(fun m -> m.address)
    ~set:(fun address m -> {m with address})
    ~validate:(fun m ->
      if not (Form_builder_common.is_nonempty m.address) then
        Error "HTTP address is required"
      else
        let exclude_instance =
          match m.original_instance with Some inst -> Some inst | None -> None
        in
        match
          Port_validation.validate_addr
            ~addr:m.address
            ?exclude_instance
            ~example:"127.0.0.1:6732"
            ()
        with
        | Ok () -> Ok ()
        | Error err -> Error (Port_validation.pp_error err))

(** Metrics address field *)
let metrics_address_field =
  Form_builder.custom
    ~label:"Metrics Endpoint"
    ~get:(fun m -> if m.metrics_address = "" then "None" else m.metrics_address)
    ~validate:(fun m ->
      if m.metrics_address = "" then true
      else
        let exclude_instance =
          match m.original_instance with Some inst -> Some inst | None -> None
        in
        match
          Port_validation.validate_addr
            ~addr:m.metrics_address
            ?exclude_instance
            ~example:"127.0.0.1:9583"
            ()
        with
        | Ok () -> true
        | Error _ -> false)
    ~edit:(fun model_ref ->
      let items = [`None; `Custom] in
      let to_string = function
        | `None -> "None · Metrics disabled"
        | `Custom -> "Custom · Prometheus endpoint"
      in
      let on_select = function
        | `None -> model_ref := {!model_ref with metrics_address = ""}
        | `Custom ->
            Modal_helpers.prompt_validated_text_modal
              ~title:"Metrics Endpoint"
              ~placeholder:(Some "127.0.0.1:9583")
              ~initial:
                (if !model_ref.metrics_address = "" then "127.0.0.1:9583"
                 else !model_ref.metrics_address)
              ~validator:(fun addr ->
                let exclude_instance =
                  match !model_ref.original_instance with
                  | Some inst -> Some inst
                  | None -> None
                in
                match
                  Port_validation.validate_addr
                    ~addr
                    ?exclude_instance
                    ~example:"127.0.0.1:9583"
                    ()
                with
                | Ok () -> Ok ()
                | Error err -> Error (Port_validation.pp_error err))
              ~on_submit:(fun addr ->
                model_ref := {!model_ref with metrics_address = addr})
              ()
      in
      Modal_helpers.open_choice_modal
        ~title:"Metrics Endpoint"
        ~items
        ~to_string
        ~on_select
        ())
    ()
  |> Form_builder.with_hint
       "Prometheus metrics endpoint for monitoring (optional)"

(** Watermark backend selection *)
let watermark_field =
  Form_builder.custom
    ~label:"Watermark Storage"
    ~get:(fun m ->
      match m.watermark with
      | Memory -> "Memory"
      | File_watermark path -> Printf.sprintf "File (%s)" path
      | _ -> "Memory")
    ~validate:(fun _m -> true)
    ~validate_msg:(fun _m -> None)
    ~edit:(fun model_ref ->
      let items = [`Memory; `File] in
      let to_string = function
        | `Memory -> "Memory · In-memory (lost on restart)"
        | `File -> "File · Persistent watermark file"
      in
      let on_select = function
        | `Memory -> model_ref := {!model_ref with watermark = Memory}
        | `File ->
            Modal_helpers.prompt_text_modal
              ~title:"Watermark File Path"
              ~placeholder:(Some "/path/to/watermark.json")
              ~initial:
                (match !model_ref.watermark with
                | File_watermark path -> path
                | _ ->
                    default_signatory_data_dir !model_ref.core.instance_name
                    ^ "/watermark.json")
              ~on_submit:(fun path ->
                model_ref := {!model_ref with watermark = File_watermark path})
              ()
      in
      Modal_helpers.open_choice_modal
        ~title:"Watermark Storage (prevents double-signing)"
        ~items
        ~to_string
        ~on_select
        ())
    ()

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  {
    title = " Install Signatory ";
    initial_model = make_initial_model;
    fields =
      (fun model ->
        (* 1. Backend selection *)
        [backend_field]
        (* 2. App bin dir *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"signatory"
            ~subcommand:["launch"; "http"; "signer"]
            ~binary_validator:Form_builder_common.has_signatory_binary
            ~app_bin_dir_modal:Modal_helpers.select_signatory_app_bin_dir_modal
            ~app_bin_dir_hint:
              "Directory containing signatory binary. Must be accessible to \
               the service user."
            ~skip_instance_name:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 3. Signatory-specific fields *)
        @ [
            authorized_keys_field;
            address_field;
            metrics_address_field;
            watermark_field;
          ]
        (* 4. Service fields *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"signatory"
            ~subcommand:["launch"; "http"; "signer"]
            ~binary_validator:Form_builder_common.has_signatory_binary
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_extra_args:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 5. Instance name *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"signatory"
            ~subcommand:["launch"; "http"; "signer"]
            ~binary_validator:Form_builder_common.has_signatory_binary
            ~skip_app_bin_dir:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 6. Group *)
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
          (* Validate at least one authorized key *)
          if model.authorized_keys = [] then
            Error (`Msg "At least one authorized key is required")
          else
            (* Validate all keys *)
            let invalid_keys =
              List.filter
                (fun (key : authorized_key) -> not (validate_tezos_key key.pkh))
                model.authorized_keys
            in
            if invalid_keys <> [] then
              Error
                (`Msg
                   (Printf.sprintf
                      "Invalid key format: %s"
                      (String.concat
                         ", "
                         (List.map (fun k -> k.pkh) invalid_keys))))
            else Ok ());
    on_init = None;
    on_refresh = None;
    pre_submit_modal = None;
    on_submit =
      (fun model ->
        (* Always use journald logging *)
        let logging_mode = Logging_mode.default in

        (* Backend is already in the correct type from the model *)
        let backend = model.backend in

        (* Build signatory request *)
        let req : Installer_types.signatory_request =
          {
            instance = model.core.instance_name;
            backend;
            authorized_keys =
              (* Deduplicate keys by pkh to handle any potential double-adds *)
              model.authorized_keys
              |> List.sort_uniq
                   (fun (a : authorized_key) (b : authorized_key) ->
                     String.compare a.pkh b.pkh);
            address = model.address;
            metrics_address = model.metrics_address;
            watermark = model.watermark;
            service_user = model.core.service_user;
            app_bin_dir = model.core.app_bin_dir;
            bin_source = model.core.bin_source;
            logging_mode;
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

        (* Execute installation *)
        let* () =
          if Paths.is_root () then
            System_user.ensure_service_account
              ~quiet:true
              ~name:model.core.service_user
              ()
          else Ok ()
        in
        let* _service = Signatory.install_signatory ~quiet:true req in

        let* () =
          Form_builder_common.set_service_group
            ~instance_name:model.core.instance_name
            ~group:model.core.group
        in

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
          ~role:"signatory"
          ~instance:model.core.instance_name ;
        Context.mark_instances_dirty () ;

        (* Queue restart dependents for modal on instances page *)
        if model.edit_mode && model.stopped_dependents <> [] then
          Context.set_pending_restart_dependents model.stopped_dependents ;

        (* Start the service if requested, but only if not already started by enable *)
        if model.core.start_now && not model.core.enable_on_boot then
          match Miaou_interfaces.Service_lifecycle.get () with
          | Some sl ->
              Miaou_interfaces.Service_lifecycle.start
                sl
                ~role:"signatory"
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
