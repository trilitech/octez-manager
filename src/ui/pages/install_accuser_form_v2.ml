(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Accuser installer using declarative form builder. *)

open Octez_manager_lib

let name = "install_accuser_form_v2"

(*****************************************************************************)
(*                             MODEL DEFINITION                              *)
(*****************************************************************************)

type node_choice = [
  | `Service of string     (* Node instance name *)
  | `Endpoint of string    (* External endpoint *)
  | `None
]

type model = {
  instance_name : string;
  node : node_choice;
  base_dir : string;
  service_user : string;
  app_bin_dir : string;
  logging : [`Journald | `File];
  enable_on_boot : bool;
  start_now : bool;
  extra_args : string;
}

(*****************************************************************************)
(*                            HELPER FUNCTIONS                               *)
(*****************************************************************************)

let default_service_user () =
  if Common.is_root () then "octez"
  else
    match Unix.getpwuid (Unix.geteuid ()) with
    | pw when String.trim pw.Unix.pw_name <> "" -> pw.Unix.pw_name
    | _ -> "octez"

let initial_model = {
  instance_name = "accuser";
  node = `None;
  base_dir = "";
  service_user = default_service_user ();
  app_bin_dir = "/usr/bin";
  logging = `File;
  enable_on_boot = true;
  start_now = true;
  extra_args = "";
}

(** Check if octez-baker binary exists and is executable *)
let has_octez_baker model =
  let trimmed = String.trim model.app_bin_dir in
  if trimmed = "" then false
  else
    let candidate = Filename.concat trimmed "octez-baker" in
    Sys.file_exists candidate
    &&
      try
        Unix.access candidate [Unix.X_OK] ;
        true
      with Unix.Unix_error _ -> false

(** Get endpoint from node choice *)
let get_endpoint ~states node_choice =
  match node_choice with
  | `Service inst -> (
      match List.find_opt
              (fun s -> s.Data.Service_state.service.Service.instance = inst
                        && s.Data.Service_state.service.Service.role = "node")
              states
      with
      | Some node_state ->
          let svc = node_state.Data.Service_state.service in
          let host, port =
            match String.split_on_char ':' svc.Service.rpc_addr with
            | [h; p] -> (String.trim h, String.trim p)
            | _ -> ("127.0.0.1", "8732")
          in
          Ok (Printf.sprintf "http://%s:%s" host port)
      | None -> Error "Selected node not found")
  | `Endpoint ep ->
      (* Parse host:port format *)
      (match String.split_on_char ':' ep with
      | [host; port] -> Ok (Printf.sprintf "http://%s:%s" (String.trim host) (String.trim port))
      | _ -> Error "Invalid endpoint format (expected host:port)")
  | `None -> Error "Node selection required"

(** Get network from node instance *)
let get_network ~states node_choice =
  match node_choice with
  | `Service inst -> (
      match List.find_opt
              (fun s -> s.Data.Service_state.service.Service.instance = inst)
              states
      with
      | Some s -> s.Data.Service_state.service.Service.network
      | None -> "mainnet")
  | _ -> "mainnet"

(*****************************************************************************)
(*                              FORM SPECIFICATION                           *)
(*****************************************************************************)

let spec = Form_builder.{
  title = " Install Accuser ";
  initial_model;

  fields = [
    (* Instance Name *)
    validated_text
      ~label:"Instance Name"
      ~get:(fun m -> m.instance_name)
      ~set:(fun v m -> {m with instance_name = v})
      ~validate:(fun m ->
        let name = String.trim m.instance_name in
        if name = "" then Error "Instance name required"
        else Ok ());

    (* Node Selection *)
    service_or_endpoint
      ~label:"Node"
      ~role:"node"
      ~get:(fun m -> m.node)
      ~set:(fun v m -> {m with node = v})
      ~external_label:"External endpoint (host:port)..."
      ();

    (* Client Base Directory *)
    client_base_dir
      ~label:"Base Dir"
      ~get:(fun m -> m.base_dir)
      ~set:(fun v m -> {m with base_dir = v})
      ~validate:(fun m -> String.trim m.base_dir <> "")
      ();

    (* Service User *)
    text
      ~label:"Service User"
      ~get:(fun m -> m.service_user)
      ~set:(fun v m -> {m with service_user = v});

    (* App Binary Directory *)
    app_bin_dir
      ~label:"App Bin Dir"
      ~get:(fun m -> m.app_bin_dir)
      ~set:(fun v m -> {m with app_bin_dir = v})
      ~validate:has_octez_baker
      ();

    (* Logging Mode *)
    choice
      ~label:"Logging"
      ~get:(fun m -> m.logging)
      ~set:(fun v m -> {m with logging = v})
      ~to_string:(function `Journald -> "Journald" | `File -> "File")
      ~items:[`Journald; `File];

    (* Enable on Boot *)
    toggle
      ~label:"Enable on Boot"
      ~get:(fun m -> m.enable_on_boot)
      ~set:(fun v m -> {m with enable_on_boot = v});

    (* Start Now *)
    toggle
      ~label:"Start Now"
      ~get:(fun m -> m.start_now)
      ~set:(fun v m -> {m with start_now = v});

    (* Extra Args with Binary Help Explorer *)
    extra_args
      ~label:"Extra Args"
      ~get_args:(fun m -> m.extra_args)
      ~set_args:(fun v m -> {m with extra_args = v})
      ~get_bin_dir:(fun m -> m.app_bin_dir)
      ~binary:"octez-baker"
      ~subcommand:["run"; "accuser"]
      ();
  ];

  (* Pre-submission validation *)
  pre_submit = Some (fun model ->
    let states = Data.load_service_states () in

    (* Check instance name uniqueness *)
    let instance_exists =
      List.exists
        (fun s -> s.Data.Service_state.service.Service.instance = model.instance_name)
        states
    in
    if instance_exists then
      Error (`Msg (Printf.sprintf "Instance '%s' already exists" model.instance_name))

    (* Validate node selection *)
    else match get_endpoint ~states model.node with
    | Error msg -> Error (`Msg msg)
    | Ok _ -> Ok ());

  (* Installation handler *)
  on_submit = (fun model ->
    let states = Data.load_service_states () in

    match get_endpoint ~states model.node with
    | Error msg -> Error (`Msg msg)
    | Ok endpoint ->
        let network = get_network ~states model.node in

        (* Parse extra args *)
        let extra_args_list =
          if String.trim model.extra_args = "" then []
          else String.split_on_char ' ' model.extra_args
        in

        (* Build service args: global options before "run accuser" *)
        let service_args =
          ["--endpoint"; endpoint; "--base-dir"; model.base_dir; "run"; "accuser"]
          @ extra_args_list
        in

        (* Configure logging *)
        let logging_mode =
          match model.logging with
          | `Journald -> Logging_mode.Journald
          | `File ->
              let dir = Common.default_log_dir ~role:"accuser" ~instance:model.instance_name in
              let path = Filename.concat dir "accuser.log" in
              Logging_mode.File {path; rotate = true}
        in

        (* Build installation request *)
        let req : Installer_types.daemon_request = {
          role = "accuser";
          instance = model.instance_name;
          network;
          history_mode = History_mode.default;
          data_dir = Common.default_role_dir "accuser" model.instance_name;
          rpc_addr = endpoint;
          net_addr = "";
          service_user = model.service_user;
          app_bin_dir = model.app_bin_dir;
          logging_mode;
          service_args;
          extra_env = [];
          extra_paths = [];
          auto_enable = model.enable_on_boot;
        } in

        (* Install the accuser *)
        match Installer.install_daemon req with
        | Ok service ->
            (* Register base_dir in directory registry *)
            let _ =
              Directory_registry.add
                ~path:model.base_dir
                ~dir_type:Client_base_dir
                ~linked_services:[service.Service.instance]
            in

            (* Start if requested *)
            if model.start_now then (
              match Systemd.start ~role:"accuser" ~instance:model.instance_name with
              | Ok () -> ()
              | Error _ -> ()
            );

            Modal_helpers.show_success
              ~title:"Success"
              (Printf.sprintf "Accuser %s installed successfully" model.instance_name);
            Ok ()
        | Error (`Msg msg) -> Error (`Msg msg)
  );
}

(*****************************************************************************)
(*                           PAGE MODULE                                     *)
(*****************************************************************************)

module Page = Form_builder.Make(struct
  type nonrec model = model
  let spec = spec
end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
