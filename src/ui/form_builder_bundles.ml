(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Octez_manager_lib
open Form_builder_common
open Rresult

let ( let* ) = Result.bind

(** {1 Shared helpers} *)

let of_rresult = function Ok v -> Ok v | Error (`Msg msg) -> Error msg

let normalize_string s = String.lowercase_ascii (String.trim s)

let fetch_network_infos_raw () =
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

let dedupe_networks infos =
  let seen = Hashtbl.create 31 in
  infos
  |> List.filter (fun (i : Teztnets.network_info) ->
      let key = normalize_string i.network_url in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key () ;
        true))

(* 5 minutes - networks don't change often, but weeklynet rotates *)
let network_cache =
  Cache.create_result ~name:"networks" ~ttl:300.0 (fun () ->
      let* infos = fetch_network_infos_raw () in
      Ok (dedupe_networks infos))

let get_network_infos () = Cache.get_result network_cache

let network_display_name value =
  let normalized_value = normalize_string value in
  let networks = match get_network_infos () with Ok l -> l | Error _ -> [] in
  match
    List.find_opt
      (fun (info : Teztnets.network_info) ->
        normalize_string info.network_url = normalized_value
        || normalize_string info.alias = normalized_value)
      networks
  with
  | Some info -> info.human_name
  | None -> value

let format_network_choice (info : Teztnets.network_info) =
  let label = info.human_name in
  if normalize_string info.network_url = normalize_string info.alias then label
  else Printf.sprintf "%s · %s" label info.network_url

(** {1 Core Service Bundle} *)

let core_service_fields ~get_core ~set_core ~binary ~subcommand ?baker_mode
    ?(binary_validator = fun _ -> true) ?(skip_instance_name = false) () =
  let open Form_builder in
  let instance_name_field =
    if skip_instance_name then []
    else
      [
        (* Instance Name *)
        validated_text
          ~label:"Instance Name"
          ~get:(fun m -> (get_core m).instance_name)
          ~set:(fun instance_name m ->
            let core = get_core m in
            set_core {core with instance_name} m)
          ~validate:(fun m ->
            let states = Form_builder_common.cached_service_states () in
            let name = (get_core m).instance_name in
            if not (is_nonempty name) then Error "Instance name is required"
            else if instance_in_use ~states name then
              Error "Instance name already exists"
            else Ok ());
      ]
  in
  instance_name_field
  @ [
      (* Service User *)
      validated_text
        ~label:"Service User"
        ~get:(fun m -> (get_core m).service_user)
        ~set:(fun service_user m ->
          let core = get_core m in
          set_core {core with service_user} m)
        ~validate:(fun m ->
          let user = (get_core m).service_user in
          if not (is_nonempty user) then Error "Service user is required"
          else if not (service_user_valid ~user) then
            Error "Service user does not exist (run as root to create)"
          else Ok ())
      |> with_hint
           "Unix user that runs the service. Created automatically if running \
            as root.";
      (* App Bin Dir *)
      app_bin_dir
        ~label:"App Bin Dir"
        ~get:(fun m -> (get_core m).app_bin_dir)
        ~set:(fun app_bin_dir m ->
          let core = get_core m in
          set_core {core with app_bin_dir} m)
        ~validate:(fun m -> binary_validator (get_core m).app_bin_dir)
        ()
      |> with_hint
           "Directory containing octez binaries. Must include the required \
            executable.";
      (* Logging is always journald - octez binaries handle their own file logging *)
      (* Enable on Boot *)
      toggle
        ~label:"Enable on Boot"
        ~get:(fun m -> (get_core m).enable_on_boot)
        ~set:(fun enable_on_boot m ->
          let core = get_core m in
          set_core {core with enable_on_boot} m)
      |> with_hint
           "Auto-start service when system boots. Recommended for production.";
      (* Start Now *)
      toggle
        ~label:"Start Now"
        ~get:(fun m -> (get_core m).start_now)
        ~set:(fun start_now m ->
          let core = get_core m in
          set_core {core with start_now} m)
      |> with_hint "Start service immediately after installation.";
      (* Extra Args *)
      extra_args
        ~label:"Extra Args"
        ~get_args:(fun m -> (get_core m).extra_args)
        ~set_args:(fun extra_args m ->
          let core = get_core m in
          set_core {core with extra_args} m)
        ~get_bin_dir:(fun m -> (get_core m).app_bin_dir)
        ~binary
        ?baker_mode
        ~subcommand
        ()
      |> with_hint
           "Additional command-line arguments passed to the binary. Press \
            Enter to browse options.";
    ]

(** {1 Client-based Tool Bundle with Auto-naming} *)

let client_fields_with_autoname ~role ~binary:_ ~binary_validator ~get_core
    ~set_core ~get_client ~set_client () =
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
            let states = Form_builder_common.cached_service_states () in
            List.exists
              (fun (s : Data.Service_state.t) ->
                s.service.Service.role = "node"
                && s.service.Service.instance = inst)
              states
        | `Endpoint ep -> is_nonempty ep && Option.is_some (parse_host_port ep))
      ~validate_msg:(fun m ->
        match (get_client m).node with
        | `None -> Some "Node selection is required"
        | `Service inst ->
            let states = Form_builder_common.cached_service_states () in
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
              Some
                "Invalid endpoint format (must be host:port, e.g., \
                 127.0.0.1:8732)"
            else None)
      ~edit:(fun model_ref ->
        let states = Form_builder_common.cached_service_states () in
        let nodes =
          List.filter
            (fun (s : Data.Service_state.t) -> s.service.Service.role = "node")
            states
        in
        let items = (nodes |> List.map (fun n -> `Node n)) @ [`Endpoint] in
        let to_string = function
          | `Node n ->
              let svc = n.Data.Service_state.service in
              Printf.sprintf
                "Node · %s (%s)"
                svc.Service.instance
                svc.Service.network
          | `Endpoint -> "Custom endpoint (host:port)..."
        in
        let on_select = function
          | `Node n ->
              let svc = n.Data.Service_state.service in
              let current_name =
                normalize (get_core !model_ref).instance_name
              in
              let should_autoname =
                current_name = "" || String.equal current_name role
              in
              let endpoint =
                let addr = String.trim svc.Service.rpc_addr in
                if
                  String.starts_with
                    ~prefix:"http://"
                    (String.lowercase_ascii addr)
                  || String.starts_with
                       ~prefix:"https://"
                       (String.lowercase_ascii addr)
                then addr
                else "http://" ^ addr
              in
              let client = get_client !model_ref in
              model_ref :=
                set_client
                  {
                    client with
                    node = `Service svc.Service.instance;
                    node_endpoint = endpoint;
                  }
                  !model_ref ;
              if should_autoname then (
                let new_name =
                  Printf.sprintf "%s-%s" role svc.Service.instance
                in
                let default_dir = Common.default_role_dir role new_name in
                let core = get_core !model_ref in
                let client = get_client !model_ref in
                model_ref :=
                  set_core {core with instance_name = new_name} !model_ref ;
                model_ref :=
                  set_client {client with base_dir = default_dir} !model_ref ;
                (* Maybe use app_bin_dir from node *)
                if
                  binary_validator svc.Service.app_bin_dir
                  && not (binary_validator (get_core !model_ref).app_bin_dir)
                then
                  let core = get_core !model_ref in
                  model_ref :=
                    set_core
                      {core with app_bin_dir = svc.Service.app_bin_dir}
                      !model_ref)
          | `Endpoint ->
              Modal_helpers.prompt_text_modal
                ~title:"Node Endpoint"
                ~placeholder:(Some "host:port (e.g., 127.0.0.1:8732)")
                ~initial:
                  (match (get_client !model_ref).node with
                  | `Endpoint ep -> ep
                  | _ -> "")
                ~on_submit:(fun ep ->
                  let client = get_client !model_ref in
                  model_ref :=
                    set_client
                      {client with node = `Endpoint ep; node_endpoint = ep}
                      !model_ref)
                ()
        in
        Modal_helpers.open_choice_modal
          ~title:"Select Node"
          ~items
          ~to_string
          ~on_select)
      ()
    |> with_hint
         "Node providing RPC. Select a managed instance or enter external \
          endpoint.";
    (* Base Directory *)
    client_base_dir
      ~label:"Base Dir"
      ~get:(fun m -> (get_client m).base_dir)
      ~set:(fun base_dir m ->
        let client = get_client m in
        set_client {client with base_dir} m)
      ~validate:(fun m -> is_nonempty (get_client m).base_dir)
      ()
    |> with_hint
         "Client configuration directory. Stores keys and operation receipts.";
  ]

(** {1 Node-specific Bundle} *)

let node_fields ~get_node ~set_node ?(on_network_selected = fun _ -> ()) () =
  let open Form_builder in
  (* Helper for network field - needs special handling for dynamic fetch *)
  let network_field =
    custom
      ~label:"Network"
      ~get:(fun m ->
        let value = (get_node m).network in
        network_display_name value)
      ~edit:(fun model_ref ->
        let fallback () =
          Modal_helpers.prompt_text_modal
            ~title:"Network"
            ~initial:(get_node !model_ref).network
            ~on_submit:(fun network ->
              let node = get_node !model_ref in
              model_ref := set_node {node with network} !model_ref ;
              on_network_selected network)
            ()
        in
        match get_network_infos () with
        | Error msg ->
            Modal_helpers.show_error ~title:"Network" msg ;
            fallback ()
        | Ok nets ->
            let sorted =
              nets
              |> List.sort
                   (fun
                     (a : Teztnets.network_info) (b : Teztnets.network_info) ->
                     String.compare
                       (normalize_string a.human_name)
                       (normalize_string b.human_name))
            in
            let items = (sorted |> List.map (fun n -> `Net n)) @ [`Custom] in
            let to_string = function
              | `Net n -> format_network_choice n
              | `Custom -> "Custom URL or slug..."
            in
            let on_select = function
              | `Net n ->
                  let network = n.Teztnets.network_url in
                  let node = get_node !model_ref in
                  model_ref := set_node {node with network} !model_ref ;
                  on_network_selected network
              | `Custom ->
                  Modal_helpers.prompt_text_modal
                    ~title:"Network"
                    ~initial:(get_node !model_ref).network
                    ~on_submit:(fun network ->
                      let node = get_node !model_ref in
                      model_ref := set_node {node with network} !model_ref ;
                      on_network_selected network)
                    ()
            in
            Modal_helpers.open_choice_modal
              ~title:"Network"
              ~items
              ~to_string
              ~on_select)
      ()
  in
  let parse_port addr =
    match String.split_on_char ':' addr with
    | [_; port_str] -> (
        try Some (int_of_string (String.trim port_str)) with _ -> None)
    | _ -> None
  in

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
  in

  let is_port_in_use (port : int) : bool =
    match
      Miaou_interfaces.Capability.get Manager_interfaces.System_capability.key
    with
    | Some cap ->
        let module Sys = (val cap : Manager_interfaces.System) in
        Sys.is_port_in_use port
    | None -> false
  in

  let validate_port addr states ~label ~example =
    match parse_host_port addr with
    | None ->
        Error (Printf.sprintf "%s must be host:port (e.g., %s)" label example)
    | Some (_host, port) ->
        if port < 1024 || port > 65535 then
          Error (Printf.sprintf "%s port must be 1024-65535" label)
        else
          let rpc_ports, p2p_ports = ports_from_states states in
          if List.mem port rpc_ports || List.mem port p2p_ports then
            Error
              (Printf.sprintf "Port %d is used by another Octez instance" port)
          else if is_port_in_use port then
            Error (Printf.sprintf "Port %d is in use" port)
          else Ok ()
  in

  [
    network_field
    |> with_hint
         "Tezos network to connect to. Mainnet for production, testnets for \
          development.";
    (* History Mode *)
    choice
      ~label:"History Mode"
      ~get:(fun m -> (get_node m).history_mode)
      ~set:(fun history_mode m ->
        let node = get_node m in
        set_node {node with history_mode} m)
      ~items:["rolling"; "full"; "archive"]
      ~to_string:(fun x -> x)
    |> with_hint
         "Rolling: minimal disk (~50GB). Full: all blocks. Archive: all states \
          (1TB+).";
    (* Data Directory *)
    node_data_dir
      ~label:"Data Dir"
      ~get:(fun m -> (get_node m).data_dir)
      ~set:(fun data_dir m ->
        let node = get_node m in
        set_node {node with data_dir} m)
      ~validate:(fun m ->
        let states = Form_builder_common.cached_service_states () in
        let data_dir = (get_node m).data_dir in
        is_nonempty data_dir
        && not
             (List.exists
                (fun (st : Data.Service_state.t) ->
                  String.equal
                    (String.trim st.service.Service.data_dir)
                    (String.trim data_dir))
                states))
      ()
    |> with_hint
         "Directory where blockchain data is stored. Must be writable by \
          service user.";
    (* RPC Address *)
    validated_text
      ~label:"RPC Address"
      ~get:(fun m -> (get_node m).rpc_addr)
      ~set:(fun rpc_addr m ->
        let node = get_node m in
        set_node {node with rpc_addr} m)
      ~validate:(fun m ->
        let addr = (get_node m).rpc_addr in
        let states = Form_builder_common.cached_service_states () in
        validate_port addr states ~label:"RPC Address" ~example:"127.0.0.1:8732")
    |> with_hint
         "RPC endpoint for clients (bakers, wallets). Use 127.0.0.1 for local \
          only.";
    (* P2P Address *)
    validated_text
      ~label:"P2P Address"
      ~get:(fun m -> (get_node m).p2p_addr)
      ~set:(fun p2p_addr m ->
        let node = get_node m in
        set_node {node with p2p_addr} m)
      ~validate:(fun m ->
        let addr = (get_node m).p2p_addr in
        let states = Form_builder_common.cached_service_states () in
        validate_port addr states ~label:"P2P Address" ~example:"0.0.0.0:9732")
    |> with_hint
         "P2P port for peer discovery. Use 0.0.0.0 to accept connections from \
          all interfaces.";
  ]
