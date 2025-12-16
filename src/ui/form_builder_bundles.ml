(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Octez_manager_lib
open Form_builder_common

(** {1 Core Service Bundle} *)

let core_service_fields ~get_core ~set_core ~binary ~subcommand
    ?(binary_validator = fun _ -> true) () =
  let open Form_builder in
  [
    (* Instance Name *)
    validated_text
      ~label:"Instance Name"
      ~get:(fun m -> (get_core m).instance_name)
      ~set:(fun instance_name m ->
        let core = get_core m in
        set_core {core with instance_name} m)
      ~validate:(fun m ->
        let states = Data.load_service_states () in
        let name = (get_core m).instance_name in
        if not (is_nonempty name) then
          Error "Instance name is required"
        else if instance_in_use ~states name then
          Error "Instance name already exists"
        else Ok ());

    (* Service User *)
    validated_text
      ~label:"Service User"
      ~get:(fun m -> (get_core m).service_user)
      ~set:(fun service_user m ->
        let core = get_core m in
        set_core {core with service_user} m)
      ~validate:(fun m ->
        let user = (get_core m).service_user in
        if not (is_nonempty user) then
          Error "Service user is required"
        else if not (service_user_valid ~user) then
          Error "Service user does not exist (run as root to create)"
        else Ok ());

    (* App Bin Dir *)
    app_bin_dir
      ~label:"App Bin Dir"
      ~get:(fun m -> (get_core m).app_bin_dir)
      ~set:(fun app_bin_dir m ->
        let core = get_core m in
        set_core {core with app_bin_dir} m)
      ~validate:(fun m -> binary_validator (get_core m).app_bin_dir)
      ();

    (* Logging *)
    choice
      ~label:"Logging"
      ~get:(fun m -> (get_core m).logging)
      ~set:(fun logging m ->
        let core = get_core m in
        set_core {core with logging} m)
      ~items:[`File; `Journald]
      ~to_string:(function `File -> "File" | `Journald -> "Journald");

    (* Enable on Boot *)
    toggle
      ~label:"Enable on Boot"
      ~get:(fun m -> (get_core m).enable_on_boot)
      ~set:(fun enable_on_boot m ->
        let core = get_core m in
        set_core {core with enable_on_boot} m);

    (* Start Now *)
    toggle
      ~label:"Start Now"
      ~get:(fun m -> (get_core m).start_now)
      ~set:(fun start_now m ->
        let core = get_core m in
        set_core {core with start_now} m);

    (* Extra Args *)
    extra_args
      ~label:"Extra Args"
      ~get_args:(fun m -> (get_core m).extra_args)
      ~set_args:(fun extra_args m ->
        let core = get_core m in
        set_core {core with extra_args} m)
      ~get_bin_dir:(fun m -> (get_core m).app_bin_dir)
      ~binary
      ~subcommand
      ();
  ]

(** {1 Client-based Tool Bundle} *)

let client_fields ~get_client ~set_client () =
  let open Form_builder in
  [
    (* Base Directory *)
    client_base_dir
      ~label:"Base Dir"
      ~get:(fun m -> (get_client m).base_dir)
      ~set:(fun base_dir m ->
        let client = get_client m in
        set_client {client with base_dir} m)
      ~validate:(fun m -> is_nonempty (get_client m).base_dir)
      ();

    (* Node Selection - custom field combining service selection + endpoint *)
    custom
      ~label:"Node"
      ~get:(fun m ->
        let client = get_client m in
        match client.node with
        | `None -> "None"
        | `Service inst -> inst
        | `Endpoint ep -> ep)
      ~validate:(fun m ->
        match (get_client m).node with
        | `None -> false
        | `Service inst ->
            let states = Data.load_service_states () in
            List.exists
              (fun (s : Data.Service_state.t) ->
                s.service.Service.role = "node"
                && s.service.Service.instance = inst)
              states
        | `Endpoint ep ->
            is_nonempty ep
            && Option.is_some (parse_host_port ep))
      ~validate_msg:(fun m ->
        match (get_client m).node with
        | `None -> Some "Node selection is required"
        | `Service inst ->
            let states = Data.load_service_states () in
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
            if not (is_nonempty ep) then
              Some "Node endpoint cannot be empty"
            else if Option.is_none (parse_host_port ep) then
              Some "Invalid endpoint format (must be host:port, e.g., 127.0.0.1:8732)"
            else None)
      ~edit:(fun model_ref ->
        let states = Data.load_service_states () in
        let nodes =
          states
          |> List.filter (fun (s : Data.Service_state.t) ->
              s.service.Service.role = "node")
        in
        let items = `External :: List.map (fun n -> `Node n) nodes in
        let to_string = function
          | `External -> "External endpoint"
          | `Node n ->
              let svc = n.Data.Service_state.service in
              Printf.sprintf "Node · %s (%s)" svc.Service.instance svc.Service.network
        in
        let on_select = function
          | `External ->
              Modal_helpers.prompt_text_modal
                ~title:"Node Endpoint (host:port)"
                ~initial:(
                  let client = get_client !model_ref in
                  match client.node with
                  | `Endpoint ep -> ep
                  | _ -> "127.0.0.1:8732")
                ~on_submit:(fun ep ->
                  let client = get_client !model_ref in
                  model_ref := set_client
                    {client with node = `Endpoint ep; node_endpoint = ep}
                    !model_ref)
                ()
          | `Node n ->
              let svc = n.Data.Service_state.service in
              let endpoint =
                let addr = String.trim svc.Service.rpc_addr in
                if String.starts_with ~prefix:"http://" (String.lowercase_ascii addr)
                   || String.starts_with ~prefix:"https://" (String.lowercase_ascii addr)
                then addr
                else "http://" ^ addr
              in
              let client = get_client !model_ref in
              model_ref := set_client
                {client with node = `Service svc.Service.instance; node_endpoint = endpoint}
                !model_ref
        in
        Modal_helpers.open_choice_modal ~title:"Node" ~items ~to_string ~on_select)
      ();
  ]

(** {1 Client-based Tool Bundle with Auto-naming} *)

let client_fields_with_autoname ~role ~binary:_ ~binary_validator
    ~get_core ~set_core ~get_client ~set_client () =
  let open Form_builder in
  [
    (* Node Selection with auto-naming *)
    custom
      ~label:"Node"
      ~get:(fun m ->
        match (get_client m).node with
        | `None -> "None"
        | `Service inst -> inst
        | `Endpoint ep -> if ep = "" then "Custom" else ep)
      ~validate:(fun m ->
        match (get_client m).node with
        | `None -> false
        | `Service inst ->
            let states = Data.load_service_states () in
            List.exists
              (fun (s : Data.Service_state.t) ->
                s.service.Service.role = "node"
                && s.service.Service.instance = inst)
              states
        | `Endpoint ep ->
            is_nonempty ep
            && Option.is_some (parse_host_port ep))
      ~validate_msg:(fun m ->
        match (get_client m).node with
        | `None -> Some "Node selection is required"
        | `Service inst ->
            let states = Data.load_service_states () in
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
            if Option.is_none (parse_host_port ep) then
              Some "Invalid endpoint format (must be host:port, e.g., 127.0.0.1:8732)"
            else None)
      ~edit:(fun model_ref ->
        let states = Data.load_service_states () in
        let nodes =
          List.filter (fun (s : Data.Service_state.t) ->
            s.service.Service.role = "node") states
        in
        let items =
          (nodes |> List.map (fun n -> `Node n)) @ [`Endpoint]
        in
        let to_string = function
          | `Node n ->
              let svc = n.Data.Service_state.service in
              Printf.sprintf "Node · %s (%s)" svc.Service.instance svc.Service.network
          | `Endpoint -> "Custom endpoint (host:port)..."
        in
        let on_select = function
          | `Node n ->
              let svc = n.Data.Service_state.service in
              let current_name = normalize (get_core !model_ref).instance_name in
              let should_autoname =
                current_name = "" || String.equal current_name role
              in
              let endpoint =
                let addr = String.trim svc.Service.rpc_addr in
                if String.starts_with ~prefix:"http://" (String.lowercase_ascii addr)
                   || String.starts_with ~prefix:"https://" (String.lowercase_ascii addr)
                then addr
                else "http://" ^ addr
              in
              let client = get_client !model_ref in
              model_ref := set_client
                {client with node = `Service svc.Service.instance; node_endpoint = endpoint}
                !model_ref ;
              if should_autoname then
                let new_name = Printf.sprintf "%s-%s" role svc.Service.instance in
                let default_dir = Common.default_role_dir role new_name in
                let core = get_core !model_ref in
                let client = get_client !model_ref in
                model_ref := set_core {core with instance_name = new_name} !model_ref ;
                model_ref := set_client {client with base_dir = default_dir} !model_ref ;
              (* Maybe use app_bin_dir from node *)
              if binary_validator svc.Service.app_bin_dir
                 && not (binary_validator (get_core !model_ref).app_bin_dir)
              then
                let core = get_core !model_ref in
                model_ref := set_core {core with app_bin_dir = svc.Service.app_bin_dir} !model_ref
          | `Endpoint ->
              Modal_helpers.prompt_text_modal
                ~title:"Node Endpoint"
                ~placeholder:(Some "host:port (e.g., 127.0.0.1:8732)")
                ~initial:(match (get_client !model_ref).node with `Endpoint ep -> ep | _ -> "")
                ~on_submit:(fun ep ->
                  let client = get_client !model_ref in
                  model_ref := set_client
                    {client with node = `Endpoint ep; node_endpoint = ep}
                    !model_ref)
                ()
        in
        Modal_helpers.open_choice_modal ~title:"Select Node" ~items ~to_string ~on_select)
      ();

    (* Base Directory *)
    client_base_dir
      ~label:"Base Dir"
      ~get:(fun m -> (get_client m).base_dir)
      ~set:(fun base_dir m ->
        let client = get_client m in
        set_client {client with base_dir} m)
      ~validate:(fun m -> is_nonempty (get_client m).base_dir)
      ();
  ]

(** {1 Node-specific Bundle} *)

let node_fields ~get_node ~set_node () =
  let open Form_builder in

  (* Helper for network field - needs special handling for dynamic fetch *)
  let network_field =
    custom
      ~label:"Network"
      ~get:(fun m -> (get_node m).network)
      ~edit:(fun model_ref ->
        (* Simplified - would need full Teztnets integration *)
        Modal_helpers.prompt_text_modal
          ~title:"Network"
          ~initial:(get_node !model_ref).network
          ~on_submit:(fun network ->
            let node = get_node !model_ref in
            model_ref := set_node {node with network} !model_ref)
          ())
      ()
  in

  [
    network_field;

    (* History Mode *)
    choice
      ~label:"History Mode"
      ~get:(fun m -> (get_node m).history_mode)
      ~set:(fun history_mode m ->
        let node = get_node m in
        set_node {node with history_mode} m)
      ~items:["rolling"; "full"; "archive"]
      ~to_string:(fun x -> x);

    (* Data Directory *)
    node_data_dir
      ~label:"Data Dir"
      ~get:(fun m -> (get_node m).data_dir)
      ~set:(fun data_dir m ->
        let node = get_node m in
        set_node {node with data_dir} m)
      ~validate:(fun m ->
        let states = Data.load_service_states () in
        let data_dir = (get_node m).data_dir in
        is_nonempty data_dir
        && not (List.exists
          (fun (st : Data.Service_state.t) ->
            String.equal (String.trim st.service.Service.data_dir) (String.trim data_dir))
          states))
      ();

    (* RPC Address *)
    validated_text
      ~label:"RPC Address"
      ~get:(fun m -> (get_node m).rpc_addr)
      ~set:(fun rpc_addr m ->
        let node = get_node m in
        set_node {node with rpc_addr} m)
      ~validate:(fun m ->
        let addr = (get_node m).rpc_addr in
        match parse_host_port addr with
        | None -> Error "Format must be host:port (e.g., 127.0.0.1:8732)"
        | Some (_host, port) ->
            if port < 1024 || port > 65535 then
              Error "Port must be 1024-65535"
            else Ok ());

    (* P2P Address *)
    validated_text
      ~label:"P2P Address"
      ~get:(fun m -> (get_node m).p2p_addr)
      ~set:(fun p2p_addr m ->
        let node = get_node m in
        set_node {node with p2p_addr} m)
      ~validate:(fun m ->
        let addr = (get_node m).p2p_addr in
        match parse_host_port addr with
        | None -> Error "Format must be host:port (e.g., 0.0.0.0:9732)"
        | Some (_host, port) ->
            if port < 1024 || port > 65535 then
              Error "Port must be 1024-65535"
            else Ok ());
  ]
