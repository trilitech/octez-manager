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

(** Signer-specific config *)
type signer_config = {
  address : string;
  port : string;
  require_auth : bool;
  password_file : string;
}

(** Model for signer form *)
type model = {
  core : Form_builder_common.core_service_config;
  base_dir : string;
  signer : signer_config;
}

let make_initial_model () =
  {
    core =
      {
        instance_name = "signer";
        service_user = Form_builder_common.default_service_user ();
        app_bin_dir = "/usr/bin";
        enable_on_boot = true;
        start_now = true;
        extra_args = "";
      };
    base_dir =
      Form_builder_common.default_base_dir ~role:"signer" ~instance:"signer";
    signer =
      {
        address = "127.0.0.1";
        port = "6732";
        require_auth = false;
        password_file = "";
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

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  {
    title = " Install Signer ";
    initial_model = make_initial_model;
    fields =
      core_service_fields
        ~get_core:(fun m -> m.core)
        ~set_core:(fun core m -> {m with core})
        ~binary:"octez-signer"
        ~subcommand:["launch"; "socket"; "signer"]
        ~binary_validator:Form_builder_common.has_octez_signer_binary
        ()
      @ [
          client_base_dir
            ~label:"Base Dir"
            ~get:(fun m -> m.base_dir)
            ~set:(fun base_dir m -> {m with base_dir})
            ~validate:(fun m -> Form_builder_common.is_nonempty m.base_dir)
            ();
          text
            ~label:"Listen Address"
            ~get:(fun m -> m.signer.address)
            ~set:(fun address m ->
              {m with signer = {m.signer with address}});
          text
            ~label:"Listen Port"
            ~get:(fun m -> m.signer.port)
            ~set:(fun port m -> {m with signer = {m.signer with port}});
          toggle
            ~label:"Require Authentication"
            ~get:(fun m -> m.signer.require_auth)
            ~set:(fun require_auth m ->
              {m with signer = {m.signer with require_auth}});
          text
            ~label:"Password File (optional)"
            ~get:(fun m -> m.signer.password_file)
            ~set:(fun password_file m ->
              {m with signer = {m.signer with password_file}});
        ];
    pre_submit = None;
    on_init = None;
    on_refresh = None;
    pre_submit_modal = None;
    on_submit =
      (fun model ->
        (* Always use journald - octez binaries handle their own file logging *)
        let logging_mode = Logging_mode.default in

        (* Prepare extra args (command options after "launch socket signer") *)
        let extra_args =
          Form_builder_common.prepare_extra_args model.core.extra_args
        in

        (* Build base_dir *)
        let base_dir =
          let trimmed = String.trim model.base_dir in
          if trimmed = "" then
            Common.default_role_dir "signer" model.core.instance_name
          else trimmed
        in

        (* Build global args (-A for require-auth, -f for password file) *)
        let global_args =
          (if model.signer.require_auth then ["-A"] else [])
          @
          let pf = String.trim model.signer.password_file in
          if pf <> "" then ["-f"; pf] else []
        in
        let global_args_str = String.concat " " global_args in

        (* Service args are command options only (after "launch socket signer --address --port") *)
        let service_args = extra_args in

        (* Build daemon request *)
        let req : Installer_types.daemon_request =
          {
            role = "signer";
            instance = model.core.instance_name;
            network = "mainnet";
            history_mode = History_mode.default;
            data_dir = base_dir;
            rpc_addr =
              Printf.sprintf "%s:%s" model.signer.address model.signer.port;
            net_addr = "";
            service_user = model.core.service_user;
            app_bin_dir = model.core.app_bin_dir;
            logging_mode;
            service_args;
            extra_env =
              [
                ("OCTEZ_SIGNER_BASE_DIR", base_dir);
                ("OCTEZ_SIGNER_ADDRESS", model.signer.address);
                ("OCTEZ_SIGNER_PORT", model.signer.port);
                ("OCTEZ_SIGNER_GLOBAL_ARGS", global_args_str);
              ];
            extra_paths = [base_dir];
            auto_enable = model.core.enable_on_boot;
          }
        in

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

module Page = Form_builder.Make (struct
  type nonrec model = model

  let spec = spec
end)

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
