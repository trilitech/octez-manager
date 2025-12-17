(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Signer installation form using field bundles. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_signer_form_v3"

(** Model uses the bundle config types directly *)
type model = {
  core : Form_builder_common.core_service_config;
  client : Form_builder_common.client_config;
}

let initial_model = {
  core = {
    instance_name = "signer";
    service_user = Form_builder_common.default_service_user ();
    app_bin_dir = "/usr/bin";
    logging = `File;
    enable_on_boot = true;
    start_now = true;
    extra_args = "";
  };
  client = {
    base_dir = "";
    node = `None;
    node_endpoint = "";
  };
}

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

(* Binary validator for signer *)
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

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  {
    title = " Install Signer ";
    initial_model;

    (* Compose fields from bundles with auto-naming support *)
    fields =
      core_service_fields
        ~get_core:(fun m -> m.core)
        ~set_core:(fun core m -> {m with core})
        ~binary:"octez-baker"
        ~subcommand:["run"; "signer"]
        ~binary_validator:has_octez_baker_binary
        ()
      @ client_fields_with_autoname
        ~role:"signer"
        ~binary:"octez-baker"
        ~binary_validator:has_octez_baker_binary
        ~get_core:(fun m -> m.core)
        ~set_core:(fun core m -> {m with core})
        ~get_client:(fun m -> m.client)
        ~set_client:(fun client m -> {m with client})
        ();

    pre_submit = Some (fun model ->
      (* Validate node selection *)
      match model.client.node with
      | `None -> Error (`Msg "Node selection is required for signer")
      | `Service inst ->
          let states = Data.load_service_states () in
          let node_exists =
            List.exists
              (fun (s : Data.Service_state.t) ->
                s.service.Service.role = "node"
                && s.service.Service.instance = inst)
              states
          in
          if not node_exists then
            Error (`Msg (Printf.sprintf "Node instance '%s' not found" inst))
          else Ok ()
      | `Endpoint ep ->
          if Form_builder_common.is_nonempty ep then Ok ()
          else Error (`Msg "Node endpoint cannot be empty"));

    on_init = None;
  on_refresh = None;
  pre_submit_modal = None;

    on_submit = (fun model ->
      let states = Data.load_service_states () in

      (* Resolve node endpoint *)
      let node_endpoint =
        match model.client.node with
        | `Service inst ->
            let node =
              List.find_opt
                (fun (s : Data.Service_state.t) ->
                  s.service.Service.role = "node"
                  && s.service.Service.instance = inst)
                states
            in
            (match node with
            | Some n ->
                let addr = String.trim n.Data.Service_state.service.Service.rpc_addr in
                if String.starts_with ~prefix:"http://" (String.lowercase_ascii addr)
                   || String.starts_with ~prefix:"https://" (String.lowercase_ascii addr)
                then addr
                else "http://" ^ addr
            | None -> "http://127.0.0.1:8732")
        | `Endpoint ep -> ep
        | `None -> "http://127.0.0.1:8732"
      in

      (* Build logging mode *)
      let logging_mode =
        match model.core.logging with
        | `Journald -> Logging_mode.Journald
        | `File ->
            let dir =
              Common.default_log_dir ~role:"signer" ~instance:model.core.instance_name
            in
            let path = Filename.concat dir "signer.log" in
            Logging_mode.File {path; rotate = true}
      in

      (* Prepare extra args *)
      let extra_args = Form_builder_common.prepare_extra_args model.core.extra_args in

      (* Resolve network from node if available *)
      let network =
        match model.client.node with
        | `Service inst ->
            let node =
              List.find_opt
                (fun (s : Data.Service_state.t) ->
                  s.service.Service.role = "node"
                  && s.service.Service.instance = inst)
                states
            in
            (match node with
            | Some n -> Some n.Data.Service_state.service.Service.network
            | None -> None)
        | _ -> None
      in

      (* Build base_dir *)
      let base_dir =
        let trimmed = String.trim model.client.base_dir in
        if trimmed = "" then
          Common.default_role_dir "signer" model.core.instance_name
        else trimmed
      in

      (* Build service args: global options before "run signer" *)
      let service_args =
        ["--endpoint"; node_endpoint; "--base-dir"; base_dir; "run"; "signer"]
        @ extra_args
      in

      (* Build daemon request *)
      let req : Installer_types.daemon_request = {
        role = "signer";
        instance = model.core.instance_name;
        network = Option.value ~default:"mainnet" network;
        history_mode = History_mode.default;
        data_dir = Common.default_role_dir "signer" model.core.instance_name;
        rpc_addr = node_endpoint;
        net_addr = "";
        service_user = model.core.service_user;
        app_bin_dir = model.core.app_bin_dir;
        logging_mode;
        service_args;
        extra_env = [];
        extra_paths = [];
        auto_enable = model.core.enable_on_boot;
      } in

      (* Execute installation *)
      let* () =
        if Common.is_root () then
          System_user.ensure_service_account ~name:model.core.service_user
        else Ok ()
      in
      let* (module PM) = require_package_manager () in
      let* _service = PM.install_daemon req in
      if model.core.start_now then
        match Miaou_interfaces.Service_lifecycle.get () with
        | Some sl ->
            Miaou_interfaces.Service_lifecycle.start
              sl
              ~role:"signer"
              ~service:model.core.instance_name
            |> Result.map_error (fun e -> `Msg e)
        | None -> Error (`Msg "Service lifecycle capability not available")
      else Ok ());
  }

module Page = Form_builder.Make(struct
  type nonrec model = model
  let spec = spec
end)

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
