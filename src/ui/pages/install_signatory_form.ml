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

let name = "install_signatory_form"

(** Model contains both common service config and signatory-specific fields *)
type model = {
  core : Form_builder_common.core_service_config;
  (* Signatory-specific fields *)
  backend_kind : string;
  keys_dir : string; (* Configuration file/directory for backend *)
  authorized_keys : string list;
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
      };
    backend_kind = "file";
    keys_dir = Paths.default_role_dir "signatory" "signatory";
    authorized_keys = [];
    address = "127.0.0.1:6732";
    metrics_address = "127.0.0.1:9090";
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
      (* Parse backend kind and keys dir from env *)
      let backend_kind_str = lookup "SIGNATORY_BACKEND_KIND" in
      let backend_kind =
        if backend_kind_str = "" then "file"
        else String.lowercase_ascii backend_kind_str
      in
      let keys_dir =
        let dir = lookup "SIGNATORY_KEYS_DIR" in
        if dir <> "" then dir
        else Paths.default_role_dir "signatory" svc.Service.instance
      in
      (* Parse authorized keys (comma-separated) *)
      let authorized_keys_str = lookup "SIGNATORY_AUTHORIZED_KEYS" in
      let authorized_keys =
        if authorized_keys_str = "" then []
        else
          String.split_on_char ',' authorized_keys_str
          |> List.map String.trim
          |> List.filter (fun s -> s <> "")
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
               Paths.default_role_dir "signatory" svc.Service.instance
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
            enable_on_boot = true;
            start_now = false;
            extra_args;
          };
        backend_kind;
        keys_dir;
        authorized_keys;
        address = (if address = "" then "127.0.0.1:6732" else address);
        metrics_address =
          (if metrics_address = "" then "127.0.0.1:9090" else metrics_address);
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

(** Backend kind field - simple text input *)
let backend_kind_field =
  Form_builder.validated_text
    ~label:"Backend Kind"
    ~get:(fun m -> m.backend_kind)
    ~set:(fun backend_kind m ->
      {m with backend_kind = String.lowercase_ascii (String.trim backend_kind)})
    ~validate:(fun m ->
      if not (Form_builder_common.is_nonempty m.backend_kind) then
        Error "Backend kind is required"
      else Ok ())

(** Configuration file/directory field - shown conditionally based on backend *)
let config_file_field =
  Form_builder.validated_text
    ~label:"Configuration File"
    ~get:(fun m -> m.keys_dir)
    ~set:(fun keys_dir m -> {m with keys_dir})
    ~validate:(fun m ->
      (* Only validate if backend is 'file' *)
      if m.backend_kind = "file" then
        if Form_builder_common.is_nonempty m.keys_dir then Ok ()
        else Error "Configuration file is required for 'file' backend"
      else Ok ())

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
        | `Key (_, key) ->
            if String.length key > 40 then String.sub key 0 37 ^ "..." else key
        | `Add -> "+ Add new key"
      in
      let rec open_menu () =
        let on_select = function
          | `Key (idx, _key) ->
              (* Toggle key - offer remove *)
              let items = [`Remove; `Cancel] in
              let to_string = function
                | `Remove -> "Remove key"
                | `Cancel -> "Cancel"
              in
              let on_select_action = function
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
              Modal_helpers.prompt_text_modal
                ~title:"Add Authorized Key"
                ~placeholder:
                  (Some "tz1abc... or tz2abc... or tz3abc... or tz4abc...")
                ~initial:""
                ~on_submit:(fun key ->
                  let key = String.trim key in
                  if validate_tezos_key key then (
                    model_ref :=
                      {
                        !model_ref with
                        authorized_keys = !model_ref.authorized_keys @ [key];
                      } ;
                    open_menu ())
                  else Context.toast_error "Invalid Tezos key format")
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
        match Form_builder_common.parse_host_port m.address with
        | Some _ -> Ok ()
        | None ->
            Error "Invalid format (must be host:port, e.g., 127.0.0.1:6732)")

(** Metrics address field *)
let metrics_address_field =
  Form_builder.validated_text
    ~label:"Metrics Address"
    ~get:(fun m -> m.metrics_address)
    ~set:(fun metrics_address m -> {m with metrics_address})
    ~validate:(fun m ->
      if not (Form_builder_common.is_nonempty m.metrics_address) then
        Error "Metrics address is required"
      else
        match Form_builder_common.parse_host_port m.metrics_address with
        | Some _ -> Ok ()
        | None ->
            Error "Invalid format (must be host:port, e.g., 127.0.0.1:9090)")

(** Watermark backend selection *)
let watermark_field =
  Form_builder.custom
    ~label:"Watermark"
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
        | `Memory -> "Memory · In-memory watermark storage"
        | `File -> "File · Persistent file storage"
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
                    Paths.default_role_dir
                      "signatory"
                      !model_ref.core.instance_name
                    ^ "/watermark.json")
              ~on_submit:(fun path ->
                model_ref := {!model_ref with watermark = File_watermark path})
              ()
      in
      Modal_helpers.open_choice_modal
        ~title:"Select Watermark Backend"
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
        (* 1. Backend kind *)
        [backend_kind_field]
        (* 2. Configuration file (conditional - only for 'file' backend) *)
        @ (if model.backend_kind = "file" then [config_file_field] else [])
        (* 3. App bin dir *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"signatory"
            ~subcommand:["launch"; "http"; "signer"]
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
        (* 4. Signatory-specific fields *)
        @ [
            authorized_keys_field;
            address_field;
            metrics_address_field;
            watermark_field;
          ]
        (* 5. Extra args *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"signatory"
            ~subcommand:["launch"; "http"; "signer"]
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 6. Service fields *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"signatory"
            ~subcommand:["launch"; "http"; "signer"]
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_extra_args:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ()
        (* 7. Instance name *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"signatory"
            ~subcommand:["launch"; "http"; "signer"]
            ~skip_app_bin_dir:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ~original_instance:model.original_instance
            ());
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
                (fun key -> not (validate_tezos_key key))
                model.authorized_keys
            in
            if invalid_keys <> [] then
              Error
                (`Msg
                   (Printf.sprintf
                      "Invalid key format: %s"
                      (String.concat ", " invalid_keys)))
            else Ok ());
    on_init = None;
    on_refresh = None;
    pre_submit_modal = None;
    on_submit =
      (fun model ->
        (* Always use journald logging *)
        let logging_mode = Logging_mode.default in

        (* Construct backend from backend_kind and keys_dir *)
        let backend =
          match String.lowercase_ascii (String.trim model.backend_kind) with
          | "file" -> File model.keys_dir
          | _ ->
              (* For unsupported backends, fail with error *)
              failwith
                (Printf.sprintf
                   "Unsupported backend kind: %s"
                   model.backend_kind)
        in

        (* Build signatory request *)
        let req : Installer_types.signatory_request =
          {
            instance = model.core.instance_name;
            backend;
            authorized_keys = model.authorized_keys;
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

        if model.core.start_now then
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
