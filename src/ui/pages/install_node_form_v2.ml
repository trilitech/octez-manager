(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Node installation form using declarative form builder.

    KNOWN LIMITATION: The preserve_data flow (asking "Refresh vs Keep" when
    data directory exists) is not fully implemented. The original form shows
    a conditional choice modal during submission, which doesn't fit well with
    the form builder's result-based on_submit approach.

    The form works correctly for:
    - Fresh installations
    - Installations where the data directory doesn't exist

    For existing directories, the install will currently fail with an error
    message prompting the user to handle it manually. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_node_form_v2"

type tzinit_snapshot = {
  network_slug : string;
  kind_slug : string;
  label : string;
}

type snapshot_selection = [`None | `Url of string | `Tzinit of tzinit_snapshot]

type preserve_data = [`Auto | `Keep | `Refresh]

type model = {
  instance_name : string;
  network : string;
  history_mode : string;
  data_dir : string;
  app_bin_dir : string;
  rpc_addr : string;
  p2p_addr : string;
  service_user : string;
  logging : [`Journald | `File];
  enable_on_boot : bool;
  start_now : bool;
  snapshot : snapshot_selection;
  extra_args : string;
  preserve_data : preserve_data;
}

let default_service_user () =
  if Common.is_root () then "octez"
  else
    match Unix.getpwuid (Unix.geteuid ()) with
    | pw when String.trim pw.Unix.pw_name <> "" -> pw.Unix.pw_name
    | _ -> "octez"

let initial_model = {
  instance_name = "node";
  network = "mainnet";
  history_mode = "rolling";
  data_dir = "";
  app_bin_dir = "/usr/bin";
  rpc_addr = "127.0.0.1:8732";
  p2p_addr = "0.0.0.0:9732";
  service_user = default_service_user ();
  logging = `File;
  enable_on_boot = true;
  start_now = true;
  snapshot = `None;
  extra_args = "";
  preserve_data = `Auto;
}

(* Network cache *)
let network_cache : Teztnets.network_info list ref = ref []

let of_rresult = function Ok v -> Ok v | Error (`Msg msg) -> Error msg

let fetch_network_infos () =
  let fallback () = of_rresult (Teztnets.list_networks ()) in
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Network_explorer_capability.key
  with
  | Some cap -> (
      let module N = (val cap : Manager_interfaces.Network_explorer) in
      match of_rresult (N.list_networks ()) with
      | Ok infos -> Ok infos
      | Error _ -> fallback ())
  | None -> fallback ()

let get_network_infos () =
  if !network_cache <> [] then Ok !network_cache
  else
    let* infos = fetch_network_infos () in
    let normalize s = String.lowercase_ascii (String.trim s) in
    let seen = Hashtbl.create 31 in
    let deduped =
      infos
      |> List.filter (fun (i : Teztnets.network_info) ->
          let key = normalize i.network_url in
          if Hashtbl.mem seen key then false
          else (
            Hashtbl.add seen key () ;
            true))
    in
    network_cache := deduped ;
    Ok deduped

let normalize_string s = String.lowercase_ascii (String.trim s)

let network_display_name value =
  let normalized_value = normalize_string value in
  match
    List.find_opt
      (fun (info : Teztnets.network_info) ->
        normalize_string info.network_url = normalized_value
        || normalize_string info.alias = normalized_value)
      !network_cache
  with
  | Some info -> Option.value ~default:info.alias info.human_name
  | None -> value

let format_network_choice (info : Teztnets.network_info) =
  let label = Option.value ~default:info.alias info.human_name in
  if normalize_string info.network_url = normalize_string info.alias then label
  else Printf.sprintf "%s · %s" label info.network_url

(* Snapshot handling *)
let snapshot_cache : (string, Snapshots.entry list * float) Hashtbl.t =
  Hashtbl.create 7

let snapshot_cache_lock = Mutex.create ()

let fetch_snapshot_list slug =
  let fallback () = of_rresult (Snapshots.list ~network_slug:slug) in
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Snapshot_provider_capability.key
  with
  | Some cap -> (
      let module P = (val cap : Manager_interfaces.Snapshot_provider) in
      match of_rresult (P.list ~network_slug:slug) with
      | Ok entries -> Ok entries
      | Error _ -> fallback ())
  | None -> fallback ()

let cache_snapshot slug entries =
  Mutex.protect snapshot_cache_lock (fun () ->
      Hashtbl.replace snapshot_cache slug (entries, Unix.gettimeofday ()))

let snapshot_entries_from_cache slug =
  Mutex.protect snapshot_cache_lock (fun () ->
      match Hashtbl.find_opt snapshot_cache slug with
      | Some (entries, _ts) -> Some entries
      | None -> None)

let get_snapshot_entries network =
  match Snapshots.slug_of_network network with
  | None -> Error "Unable to derive network slug"
  | Some slug -> (
      match snapshot_entries_from_cache slug with
      | Some entries -> Ok (slug, entries)
      | None -> (
          match fetch_snapshot_list slug with
          | Ok entries ->
              cache_snapshot slug entries ;
              Ok (slug, entries)
          | Error msg -> Error msg))

(* Validation helpers *)
let is_nonempty s = String.trim s <> ""
let normalize s = String.lowercase_ascii (String.trim s)

let parse_host_port (s : string) : (string * int) option =
  match String.split_on_char ':' s with
  | [host; port] -> (
      try
        let p = int_of_string (String.trim port) in
        if p > 0 && p < 65536 && String.trim host <> "" then Some (host, p)
        else None
      with _ -> None)
  | _ -> None

let is_port_in_use (port : int) : bool =
  match
    Miaou_interfaces.Capability.get Manager_interfaces.System_capability.key
  with
  | Some cap ->
      let module Sys = (val cap : Manager_interfaces.System) in
      Sys.is_port_in_use port
  | None -> false

let parse_port addr =
  match String.split_on_char ':' addr with
  | [_; port_str] -> (
      try Some (int_of_string (String.trim port_str)) with _ -> None)
  | _ -> None

let ports_from_states states =
  let rpc_ports =
    states
    |> List.filter_map (fun (s : Data.Service_state.t) ->
        match s.service.Service.role with
        | "node" -> parse_port s.service.Service.rpc_addr
        | _ -> None)
  in
  let p2p_ports =
    states
    |> List.filter_map (fun (s : Data.Service_state.t) ->
        match s.service.Service.role with
        | "node" -> parse_port s.service.Service.net_addr
        | _ -> None)
  in
  (rpc_ports, p2p_ports)

let instance_in_use ~states name =
  let target = normalize name in
  target <> ""
  && List.exists
       (fun (s : Data.Service_state.t) ->
         String.equal target (normalize s.service.Service.instance))
       states

let data_dir_in_use ~states path =
  let trimmed = String.trim path in
  if trimmed = "" then false
  else
    List.exists
      (fun (st : Data.Service_state.t) ->
        String.equal (String.trim st.service.Service.data_dir) trimmed)
      states

let service_user_valid ~user =
  if Common.is_root () then true
  else Result.is_ok (System_user.validate_user_for_service ~user)

let has_octez_node_binary dir =
  let trimmed = String.trim dir in
  if trimmed = "" then false
  else
    let candidate = Filename.concat trimmed "octez-node" in
    Sys.file_exists candidate
    &&
      try
        Unix.access candidate [Unix.X_OK] ;
        true
      with Unix.Unix_error _ -> false

let dir_nonempty path =
  let trimmed = String.trim path in
  if trimmed = "" then false
  else
    try
      let st = Unix.stat trimmed in
      st.Unix.st_kind = Unix.S_DIR
      &&
      let entries = Sys.readdir trimmed in
      Array.exists (fun e -> e <> "." && e <> "..") entries
    with Unix.Unix_error _ | Sys_error _ -> false

(* Check if history mode conflicts with selected snapshot *)
let history_snapshot_conflict ~history_mode ~snapshot ~network =
  match snapshot with
  | `None | `Url _ -> false
  | `Tzinit tz -> (
      match Snapshots.slug_of_network network with
      | None -> false
      | Some slug -> (
          match snapshot_entries_from_cache slug with
          | None -> false
          | Some entries -> (
              match
                List.find_opt
                  (fun (e : Snapshots.entry) ->
                    e.network = tz.network_slug && e.slug = tz.kind_slug)
                  entries
              with
              | None -> false
              | Some entry -> (
                  match entry.history_mode with
                  | Some snap_mode when String.trim snap_mode <> "" -> (
                      match History_mode.of_string history_mode with
                      | Ok requested ->
                          not
                            (Installer.For_tests.history_mode_matches
                               ~requested
                               ~snapshot_mode:snap_mode)
                      | Error _ -> true)
                  | _ -> false))))

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

(* Custom field for Network selection *)
let network_field =
  Form_builder.custom
    ~label:"Network"
    ~get:(fun m ->
      if !network_cache = [] then m.network
      else network_display_name m.network)
    ~edit:(fun model_ref ->
      match get_network_infos () with
      | Error msg -> Modal_helpers.show_error ~title:"Network" msg
      | Ok nets ->
          let sorted =
            nets
            |> List.sort
                 (fun (a : Teztnets.network_info) (b : Teztnets.network_info) ->
                   String.compare
                     (normalize_string
                        (Option.value ~default:a.alias a.human_name))
                     (normalize_string
                        (Option.value ~default:b.alias b.human_name)))
          in
          Modal_helpers.open_choice_modal
            ~title:"Network"
            ~items:sorted
            ~to_string:format_network_choice
            ~on_select:(fun info ->
              model_ref := {!model_ref with network = info.network_url; snapshot = `None}))
    ()

(* Snapshot choice type for modal *)
type snapshot_choice =
  | Snapshot_none
  | Snapshot_custom
  | Snapshot_entry of Snapshots.entry

let snapshot_choice_label = function
  | Snapshot_none -> "None (start from genesis)"
  | Snapshot_custom -> "Custom snapshot URL"
  | Snapshot_entry entry ->
      let hm =
        match entry.history_mode with
        | Some mode when String.trim mode <> "" -> " · " ^ mode
        | _ -> ""
      in
      Printf.sprintf "%s (%s%s)" entry.label entry.slug hm

(* Custom field for Snapshot selection *)
let snapshot_field =
  Form_builder.custom
    ~label:"Snapshot"
    ~get:(fun m ->
      match m.snapshot with
      | `None -> "None"
      | `Url u -> u
      | `Tzinit sel -> Printf.sprintf "tzinit · %s (%s)" sel.label sel.kind_slug)
    ~validate:(fun m ->
      let has_conflict =
        history_snapshot_conflict
          ~history_mode:m.history_mode
          ~snapshot:m.snapshot
          ~network:m.network
      in
      not has_conflict
      && match m.snapshot with
         | `None -> true
         | `Url u -> is_nonempty u
         | `Tzinit _ -> true)
    ~edit:(fun model_ref ->
      let current_network = String.trim (!model_ref).network in
      let base_choices = [Snapshot_none; Snapshot_custom] in
      let choices =
        if current_network = "" then base_choices
        else
          match get_snapshot_entries current_network with
          | Ok (_slug, entries) ->
              base_choices @ List.map (fun e -> Snapshot_entry e) entries
          | Error _ -> base_choices
      in
      Modal_helpers.open_choice_modal
        ~title:"Snapshot"
        ~items:choices
        ~to_string:snapshot_choice_label
        ~on_select:(function
        | Snapshot_none -> model_ref := {!model_ref with snapshot = `None}
        | Snapshot_custom ->
            Modal_helpers.prompt_text_modal
              ~title:"Snapshot URL"
              ~initial:(match (!model_ref).snapshot with `Url u -> u | _ -> "")
              ~on_submit:(fun url ->
                model_ref := {!model_ref with snapshot = `Url url})
              ()
        | Snapshot_entry entry ->
            model_ref := {!model_ref with
              snapshot = `Tzinit {
                network_slug = entry.Snapshots.network;
                kind_slug = entry.Snapshots.slug;
                label = entry.Snapshots.label;
              }}))
    ()

let spec =
  let open Form_builder in
  {
    title = " Install Node ";
    initial_model;
    fields = [
      validated_text
        ~label:"Instance Name"
        ~get:(fun m -> m.instance_name)
        ~set:(fun instance_name m ->
          {m with
            instance_name;
            data_dir =
              (if m.data_dir = "" then Common.default_data_dir instance_name
               else m.data_dir);
            preserve_data = `Auto})
        ~validate:(fun m ->
          let states = Data.load_service_states () in
          if not (is_nonempty m.instance_name) then
            Error "Instance name is required"
          else if instance_in_use ~states m.instance_name then
            Error "Instance name already exists"
          else Ok ());

      network_field;

      choice
        ~label:"History Mode"
        ~get:(fun m -> m.history_mode)
        ~set:(fun history_mode m -> {m with history_mode})
        ~items:["rolling"; "full"; "archive"]
        ~to_string:(fun x -> x);

      node_data_dir
        ~label:"Data Dir"
        ~get:(fun m -> m.data_dir)
        ~set:(fun data_dir m -> {m with data_dir; preserve_data = `Auto})
        ~validate:(fun m ->
          let states = Data.load_service_states () in
          is_nonempty m.data_dir && not (data_dir_in_use ~states m.data_dir))
        ();

      app_bin_dir
        ~label:"App Bin Dir"
        ~get:(fun m -> m.app_bin_dir)
        ~set:(fun app_bin_dir m -> {m with app_bin_dir})
        ~validate:(fun m -> has_octez_node_binary m.app_bin_dir)
        ();

      validated_text
        ~label:"RPC Address"
        ~get:(fun m -> m.rpc_addr)
        ~set:(fun rpc_addr m -> {m with rpc_addr})
        ~validate:(fun m ->
          let states = Data.load_service_states () in
          match parse_host_port m.rpc_addr with
          | None -> Error "Format must be host:port (e.g., 127.0.0.1:8732)"
          | Some (_host, port) ->
              if port < 1024 || port > 65535 then
                Error "Port must be 1024-65535"
              else
                let rpc_ports, p2p_ports = ports_from_states states in
                if List.mem port rpc_ports || List.mem port p2p_ports then
                  Error (Printf.sprintf "Port %d is used by another Octez instance" port)
                else if is_port_in_use port then
                  Error (Printf.sprintf "Port %d is in use" port)
                else Ok ());

      validated_text
        ~label:"P2P Address"
        ~get:(fun m -> m.p2p_addr)
        ~set:(fun p2p_addr m -> {m with p2p_addr})
        ~validate:(fun m ->
          let states = Data.load_service_states () in
          match parse_host_port m.p2p_addr with
          | None -> Error "Format must be host:port (e.g., 0.0.0.0:9732)"
          | Some (_host, port) ->
              if port < 1024 || port > 65535 then
                Error "Port must be 1024-65535"
              else
                let rpc_ports, p2p_ports = ports_from_states states in
                if List.mem port rpc_ports || List.mem port p2p_ports then
                  Error (Printf.sprintf "Port %d is used by another Octez instance" port)
                else if is_port_in_use port then
                  Error (Printf.sprintf "Port %d is in use" port)
                else Ok ());

      validated_text
        ~label:"Service User"
        ~get:(fun m -> m.service_user)
        ~set:(fun service_user m -> {m with service_user})
        ~validate:(fun m ->
          if not (is_nonempty m.service_user) then
            Error "Service user is required"
          else if not (service_user_valid ~user:m.service_user) then
            Error "Service user does not exist (run as root to create)"
          else Ok ());

      choice
        ~label:"Logging"
        ~get:(fun m -> m.logging)
        ~set:(fun logging m -> {m with logging})
        ~items:[`File; `Journald]
        ~to_string:(function `File -> "File" | `Journald -> "Journald");

      toggle
        ~label:"Enable on Boot"
        ~get:(fun m -> m.enable_on_boot)
        ~set:(fun enable_on_boot m -> {m with enable_on_boot});

      toggle
        ~label:"Start Now"
        ~get:(fun m -> m.start_now)
        ~set:(fun start_now m -> {m with start_now});

      snapshot_field;

      extra_args
        ~label:"Extra Args"
        ~get_args:(fun m -> m.extra_args)
        ~set_args:(fun extra_args m -> {m with extra_args})
        ~get_bin_dir:(fun m -> m.app_bin_dir)
        ~binary:"octez-node"
        ~subcommand:["run"]
        ();
    ];

    pre_submit = None;

    on_submit = (fun model ->
      (* Check if we need to show preserve_data choice modal *)
      let data_dir =
        if String.trim model.data_dir = "" then
          Common.default_data_dir model.instance_name
        else model.data_dir
      in
      let needs_choice = dir_nonempty data_dir && model.preserve_data = `Auto in

      if needs_choice then (
        (* Show choice modal and abort this submission *)
        (* Note: The choice modal doesn't update the model - user must use a different approach *)
        Modal_helpers.open_choice_modal
          ~title:"Data directory exists"
          ~items:[`Refresh; `Keep]
          ~to_string:(function
            | `Refresh -> "Refresh (wipe and import)"
            | `Keep -> "Keep existing data")
          ~on_select:(fun _choice ->
            (* TODO: This needs a different approach - the modal can't easily update the model *)
            ()) ;
        Error (`Msg "Choose how to handle existing data directory - feature not fully implemented"))
      else
        (* Proceed with installation *)
        let history_mode =
          match History_mode.of_string model.history_mode with
          | Ok m -> m
          | Error _ -> History_mode.Rolling
        in
        let logging_mode =
          match model.logging with
          | `Journald -> Logging_mode.Journald
          | `File ->
              let dir =
                Common.default_log_dir ~role:"node" ~instance:model.instance_name
              in
              let path = Filename.concat dir "node.log" in
              Logging_mode.File {path; rotate = true}
        in
        let bootstrap =
          match model.snapshot with
          | `None -> Genesis
          | `Url u -> Snapshot {src = Some u; kind = None}
          | `Tzinit choice ->
              Snapshot {src = None; kind = Some choice.kind_slug}
        in
        let extra_args =
          if model.extra_args = "" then []
          else
            String.split_on_char ' ' model.extra_args
            |> List.filter (fun s -> s <> "")
        in
        let req : Installer_types.node_request = {
          instance = model.instance_name;
          network = model.network;
          history_mode;
          data_dir = Some data_dir;
          rpc_addr = model.rpc_addr;
          net_addr = model.p2p_addr;
          service_user = model.service_user;
          app_bin_dir = model.app_bin_dir;
          logging_mode;
          extra_args;
          auto_enable = model.enable_on_boot;
          bootstrap;
          preserve_data = model.preserve_data = `Keep;
          snapshot_no_check = false;
        } in
        let* () =
          if Common.is_root () then
            System_user.ensure_service_account ~name:model.service_user
          else Ok ()
        in
        let* (module I) = require_package_manager () in
        let* _service = I.install_node req in
        Ok ());
  }

module Page = Form_builder.Make(struct
  type nonrec model = model
  let spec = spec
end)

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
