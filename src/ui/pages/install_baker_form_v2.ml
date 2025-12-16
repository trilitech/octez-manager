(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_baker_form_v2"

type dal_selection =
  | Dal_none
  | Dal_instance of string
  | Dal_endpoint of string

type model = {
  instance_name : string;
  parent_node : string;  (* empty = external *)
  node_data_dir : string;
  node_endpoint : string;
  dal : dal_selection;
  base_dir : string;
  delegates : string list;
  liquidity_baking_vote : string;
  service_user : string;
  app_bin_dir : string;
  logging : [`Journald | `File];
  enable_on_boot : bool;
  start_now : bool;
  extra_args : string;
}

let default_service_user () =
  if Common.is_root () then "octez"
  else
    match Unix.getpwuid (Unix.geteuid ()) with
    | pw when String.trim pw.Unix.pw_name <> "" -> pw.Unix.pw_name
    | _ -> "octez"

let initial_model = {
  instance_name = "baker";
  parent_node = "";
  node_data_dir = "";
  node_endpoint = "127.0.0.1:8732";
  dal = Dal_none;
  base_dir = "";
  delegates = [];
  liquidity_baking_vote = "pass";
  service_user = default_service_user ();
  app_bin_dir = "/usr/bin";
  logging = `File;
  enable_on_boot = true;
  start_now = true;
  extra_args = "";
}

let is_nonempty s = String.trim s <> ""
let normalize s = String.lowercase_ascii (String.trim s)

let instance_in_use ~states name =
  let target = normalize name in
  target <> ""
  && List.exists
       (fun (s : Data.Service_state.t) ->
         String.equal target (normalize s.service.Service.instance))
       states

let node_services states =
  states
  |> List.filter (fun (s : Data.Service_state.t) ->
      String.equal s.service.Service.role "node")

let find_node states inst =
  node_services states
  |> List.find_opt (fun (s : Data.Service_state.t) ->
      String.equal (normalize s.service.Service.instance) (normalize inst))

let dal_services states =
  states
  |> List.filter (fun (s : Data.Service_state.t) ->
      let role = normalize s.service.Service.role in
      String.equal role "dal-node" || String.equal role "dal")

let find_dal states inst =
  dal_services states
  |> List.find_opt (fun (s : Data.Service_state.t) ->
      String.equal (normalize s.service.Service.instance) (normalize inst))

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

let service_user_valid ~user =
  if Common.is_root () then true
  else Result.is_ok (System_user.validate_user_for_service ~user)

let baker_node_mode model states =
  match find_node states model.parent_node with
  | Some _ -> `Local
  | None -> if is_nonempty model.node_data_dir then `Local else `Remote

let resolve_node_data_dir model states =
  match find_node states model.parent_node with
  | Some svc -> svc.Data.Service_state.service.Service.data_dir
  | None -> String.trim model.node_data_dir

let resolve_node_endpoint model states =
  match find_node states model.parent_node with
  | Some svc -> node_endpoint_of_service svc.Data.Service_state.service
  | None -> endpoint_with_scheme model.node_endpoint

let resolve_dal_config model states =
  match model.dal with
  | Dal_none -> Dal_disabled
  | Dal_endpoint ep -> Dal_endpoint (endpoint_with_scheme ep)
  | Dal_instance inst -> (
      match find_dal states inst with
      | Some svc ->
          Dal_endpoint (dal_endpoint_of_service svc.Data.Service_state.service)
      | None -> Dal_auto)

let parse_host_port (s : string) : (string * int) option =
  match String.split_on_char ':' s with
  | [host; port] -> (
      try
        let p = int_of_string (String.trim port) in
        if p > 0 && p < 65536 && String.trim host <> "" then Some (host, p)
        else None
      with _ -> None)
  | _ -> None

let endpoint_host_port s =
  let trimmed = String.trim s in
  if
    String.starts_with ~prefix:"http://" (String.lowercase_ascii trimmed)
    || String.starts_with ~prefix:"https://" (String.lowercase_ascii trimmed)
  then
    let prefix_len =
      if String.starts_with ~prefix:"https://" (String.lowercase_ascii trimmed)
      then 8
      else 7
    in
    String.sub trimmed prefix_len (String.length trimmed - prefix_len)
  else trimmed

(* Custom field for DAL selection *)
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
          let states = Data.load_service_states () in
          Option.is_some (find_dal states inst)
      | Dal_endpoint ep ->
          Option.is_some (parse_host_port (endpoint_host_port ep)))
    ~edit:(fun model_ref ->
      let states = Data.load_service_states () in
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
            model_ref := {!model_ref with dal = Dal_instance svc.Service.instance} ;
            (* Maybe use app_bin_dir from DAL service *)
            if has_octez_baker_binary svc.Service.app_bin_dir
               && not (has_octez_baker_binary (!model_ref).app_bin_dir)
            then model_ref := {!model_ref with app_bin_dir = svc.Service.app_bin_dir}
        | `Custom ->
            let initial =
              match (!model_ref).dal with
              | Dal_endpoint ep when ep <> "" -> ep
              | _ -> ""
            in
            Modal_helpers.prompt_validated_text_modal
              ~title:"DAL Node Endpoint (host:port)"
              ~initial
              ~validator:(fun text ->
                match parse_host_port (endpoint_host_port text) with
                | Some _ -> Ok ()
                | None ->
                    Error "Format must be host:port (e.g., 127.0.0.1:10732)")
              ~on_submit:(fun v ->
                model_ref := {!model_ref with dal = Dal_endpoint v})
              ()
      in
      Modal_helpers.open_choice_modal ~title:"DAL Node" ~items ~to_string ~on_select)
    ()

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
  {
    title = " Install Baker ";
    initial_model;
    fields = [
      validated_text
        ~label:"Instance Name"
        ~get:(fun m -> m.instance_name)
        ~set:(fun instance_name m ->
          (* Update base_dir if it was the default for old name *)
          let old = m.instance_name in
          let default_dir = Common.default_role_dir "baker" instance_name in
          let keep_base_dir =
            String.trim m.base_dir <> ""
            && not (String.equal m.base_dir (Common.default_role_dir "baker" old))
          in
          {m with
            instance_name;
            base_dir = (if keep_base_dir then m.base_dir else default_dir)})
        ~validate:(fun m ->
          let states = Data.load_service_states () in
          if not (is_nonempty m.instance_name) then
            Error "Instance name is required"
          else if instance_in_use ~states m.instance_name then
            Error "Instance name already exists"
          else Ok ());

      custom
        ~label:"Parent Node"
        ~get:(fun m -> if m.parent_node = "" then "External" else m.parent_node)
        ~edit:(fun model_ref ->
          let states = Data.load_service_states () in
          let nodes = node_services states in
          let items = `External :: List.map (fun n -> `Node n) nodes in
          let to_string = function
            | `External -> "External/None (use custom endpoint)"
            | `Node n ->
                let svc = n.Data.Service_state.service in
                Printf.sprintf "Node · %s (%s)" svc.Service.instance svc.Service.network
          in
          let on_select = function
            | `External -> model_ref := {!model_ref with parent_node = ""}
            | `Node n ->
                let svc = n.Data.Service_state.service in
                let current_name = normalize (!model_ref).instance_name in
                let should_autoname =
                  current_name = "" || String.equal current_name "baker"
                in
                model_ref := {!model_ref with parent_node = svc.Service.instance} ;
                if should_autoname then
                  let new_name = Printf.sprintf "baker-%s" svc.Service.instance in
                  let default_dir = Common.default_role_dir "baker" new_name in
                  model_ref := {!model_ref with instance_name = new_name; base_dir = default_dir} ;
                (* Maybe use app_bin_dir from node *)
                if has_octez_baker_binary svc.Service.app_bin_dir
                   && not (has_octez_baker_binary (!model_ref).app_bin_dir)
                then model_ref := {!model_ref with app_bin_dir = svc.Service.app_bin_dir}
          in
          Modal_helpers.open_choice_modal ~title:"Parent Node" ~items ~to_string ~on_select)
        ();

      dal_node_field;

      endpoint
        ~label:"Node Endpoint"
        ~get:(fun m -> m.node_endpoint)
        ~set:(fun node_endpoint m -> {m with node_endpoint})
        ();

      node_data_dir
        ~label:"Node Data Dir"
        ~get:(fun m -> m.node_data_dir)
        ~set:(fun node_data_dir m -> {m with node_data_dir})
        ~validate:(fun m ->
          let states = Data.load_service_states () in
          let selected_node = find_node states m.parent_node in
          let node_mode = baker_node_mode m states in
          match node_mode with
          | `Local -> is_nonempty (resolve_node_data_dir m states) || Option.is_some selected_node
          | `Remote -> true)
        ();

      client_base_dir
        ~label:"Baker Base Dir"
        ~get:(fun m -> m.base_dir)
        ~set:(fun base_dir m -> {m with base_dir})
        ~validate:(fun m -> is_nonempty m.base_dir)
        ();

      string_list
        ~label:"Delegates"
        ~get:(fun m -> m.delegates)
        ~set:(fun delegates m -> {m with delegates})
        ~get_suggestions:(fun m ->
          if String.trim m.base_dir = "" then []
          else
            match Keys_reader.read_public_key_hashes ~base_dir:m.base_dir with
            | Ok keys -> List.map (fun k -> k.Keys_reader.value) keys
            | Error _ -> [])
        ();

      choice
        ~label:"Liquidity Baking Vote"
        ~get:(fun m -> m.liquidity_baking_vote)
        ~set:(fun liquidity_baking_vote m -> {m with liquidity_baking_vote})
        ~items:["pass"; "on"; "off"]
        ~to_string:(fun x -> x);

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

      app_bin_dir
        ~label:"App Bin Dir"
        ~get:(fun m -> m.app_bin_dir)
        ~set:(fun app_bin_dir m -> {m with app_bin_dir})
        ~validate:(fun m -> has_octez_baker_binary m.app_bin_dir)
        ();

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

      extra_args
        ~label:"Extra Args"
        ~get_args:(fun m -> m.extra_args)
        ~set_args:(fun extra_args m -> {m with extra_args})
        ~get_bin_dir:(fun m -> m.app_bin_dir)
        ~binary:"octez-baker"
        ~subcommand:["run"]
        ();
    ];

    pre_submit = None;

    on_submit = (fun model ->
      (* Ensure service user and base_dir have defaults *)
      let model =
        if String.trim model.service_user = "" then
          {model with service_user = default_service_user ()}
        else model
      in
      let model =
        if String.trim model.base_dir = "" then
          let default_dir = Common.default_role_dir "baker" model.instance_name in
          {model with base_dir = default_dir}
        else model
      in
      let states = Data.load_service_states () in
      let selected_node = find_node states model.parent_node in
      let node_mode = baker_node_mode model states in
      let node_dir = resolve_node_data_dir model states in
      let node_endpoint = resolve_node_endpoint model states in
      let dal_config = resolve_dal_config model states in

      let logging_mode =
        match model.logging with
        | `Journald -> Logging_mode.Journald
        | `File ->
            let dir =
              Common.default_log_dir ~role:"baker" ~instance:model.instance_name
            in
            let path = Filename.concat dir "baker.log" in
            Logging_mode.File {path; rotate = true}
      in

      let extra_args =
        if model.extra_args = "" then []
        else
          String.split_on_char ' ' model.extra_args
          |> List.filter (fun s -> s <> "")
      in

      let base_dir =
        let trimmed = String.trim model.base_dir in
        if trimmed = "" then Common.default_role_dir "baker" model.instance_name
        else trimmed
      in

      let req = {
        instance = model.instance_name;
        network = None;
        node_instance =
          (match selected_node with
          | Some svc -> Some svc.Data.Service_state.service.Service.instance
          | None -> None);
        node_data_dir =
          (match node_mode with
          | `Local ->
              if Option.is_some selected_node then None
              else if String.trim node_dir = "" then None
              else Some (String.trim node_dir)
          | `Remote -> None);
        node_endpoint = Some node_endpoint;
        node_mode;
        base_dir = Some base_dir;
        delegates = model.delegates;
        dal_config;
        liquidity_baking_vote =
          (if String.trim model.liquidity_baking_vote = "" then None
          else Some (String.trim model.liquidity_baking_vote));
        extra_args;
        service_user = model.service_user;
        app_bin_dir = model.app_bin_dir;
        logging_mode;
        auto_enable = model.enable_on_boot;
      } in

      let* () =
        if Common.is_root () then
          System_user.ensure_service_account ~name:model.service_user
        else Ok ()
      in
      let* (module PM) = require_package_manager () in
      let* _ = PM.install_baker req in
      if model.start_now then
        match Miaou_interfaces.Service_lifecycle.get () with
        | Some sl ->
            Miaou_interfaces.Service_lifecycle.start
              sl
              ~role:"baker"
              ~service:model.instance_name
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
