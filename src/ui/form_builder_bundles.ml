(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

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
    ?(binary_validator = fun _ -> true) ?(skip_instance_name = false)
    ?(skip_app_bin_dir = false) ?(skip_extra_args = false)
    ?(skip_service_fields = false) ?(edit_mode = false)
    ?(original_instance : string option = None) () =
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
            (* Use non-blocking cache to avoid syscalls during typing *)
            let states =
              Form_builder_common.cached_service_states_nonblocking ()
            in
            let name = (get_core m).instance_name in
            if not (is_nonempty name) then Error "Instance name is required"
            else if
              instance_in_use ~states name
              && not (edit_mode && original_instance = Some name)
            then Error "Instance name already exists"
            else Ok ());
      ]
  in
  (* Service User - readonly in edit mode *)
  let service_user_field =
    if edit_mode then
      readonly ~label:"Service User" ~get:(fun m -> (get_core m).service_user)
      |> with_hint "Service user cannot be changed after creation."
    else
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
            as root."
  in
  let app_bin_dir_field =
    if skip_app_bin_dir then []
    else
      let app_bin_dir_custom =
        custom_field
          ~label:"App Bin Dir"
          ~get:(fun m -> (get_core m).app_bin_dir)
          ~set:(fun app_bin_dir m ->
            let core = get_core m in
            set_core {core with app_bin_dir} m)
          ~validate:(fun m ->
            let app_bin_dir = (get_core m).app_bin_dir in
            let user = (get_core m).service_user in
            (* First check if binary exists *)
            binary_validator app_bin_dir
            && ((not (Common.is_root ()))
               || (not (is_nonempty user))
               ||
               (* In root mode with user, verify service user can access *)
               binary_accessible_to_user ~user ~app_bin_dir ~binary_name:binary
               ))
          ~validate_msg:(fun m ->
            let app_bin_dir = (get_core m).app_bin_dir in
            let user = (get_core m).service_user in
            if not (binary_validator app_bin_dir) then
              Some
                (Printf.sprintf
                   "Binary '%s' not found in %s or not executable"
                   binary
                   app_bin_dir)
            else if
              Common.is_root () && is_nonempty user
              && not
                   (binary_accessible_to_user
                      ~user
                      ~app_bin_dir
                      ~binary_name:binary)
            then
              Some
                (Printf.sprintf
                   "User '%s' cannot access %s in %s. The binary may be in a \
                    directory with restricted permissions (e.g., a user's home \
                    directory). Consider copying binaries to /opt or \
                    /usr/local/bin."
                   user
                   binary
                   app_bin_dir)
            else None)
          ~edit:(fun model_ref ->
            let on_select path =
              let core = get_core !model_ref in
              model_ref := set_core {core with app_bin_dir = path} !model_ref
            in
            Modal_helpers.select_app_bin_dir_modal ~on_select ())
          ()
        |> with_hint
             "Directory containing octez binaries. Must be accessible to the \
              service user."
      in
      [app_bin_dir_custom]
  in
  let extra_args_field =
    if skip_extra_args then []
    else
      [
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
  in
  let service_fields =
    if skip_service_fields then []
    else
      [
        service_user_field;
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
      ]
  in
  app_bin_dir_field @ extra_args_field @ service_fields @ instance_name_field

(** {1 Client-based Tool Bundle with Auto-naming} *)

let client_fields_with_autoname ~role ~binary:_ ~binary_validator ~get_core
    ~set_core ~get_client ~set_client ?(edit_mode = false)
    ?(skip_base_dir = false) () =
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
            (* Use non-blocking cache to avoid syscalls during typing *)
            let states =
              Form_builder_common.cached_service_states_nonblocking ()
            in
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
            (* Use non-blocking cache to avoid syscalls during typing *)
            let states =
              Form_builder_common.cached_service_states_nonblocking ()
            in
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
        (* edit uses blocking version - only called when opening modal *)
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
                (* Strip "node-" prefix from dependency name to avoid "baker-node-mainnet" *)
                let dep_suffix =
                  let inst = svc.Service.instance in
                  if String.starts_with ~prefix:"node-" inst then
                    String.sub inst 5 (String.length inst - 5)
                  else inst
                in
                let new_name = Printf.sprintf "%s-%s" role dep_suffix in
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
          ~on_select
          ())
      ()
    |> with_hint
         "Node providing RPC. Select a managed instance or enter external \
          endpoint.";
  ]
  (* Base Directory - readonly in edit mode, skip for DAL node *)
  @
  if skip_base_dir then []
  else
    [
      (if edit_mode then
         readonly ~label:"Base Dir" ~get:(fun m -> (get_client m).base_dir)
         |> with_hint "Base directory cannot be changed after creation."
       else
         client_base_dir
           ~label:"Base Dir"
           ~get:(fun m -> (get_client m).base_dir)
           ~set:(fun base_dir m ->
             let client = get_client m in
             set_client {client with base_dir} m)
           ~validate:(fun m -> is_nonempty (get_client m).base_dir)
           ()
         |> with_hint
              "Client configuration directory. Stores keys and operation \
               receipts.");
    ]

(** {1 Node-specific Bundle} *)

let node_fields ~get_node ~set_node ?(on_network_selected = fun _ -> ())
    ?(on_history_mode_changed = fun _ -> ()) ?(edit_mode = false)
    ?editing_instance ?(skip_network = false) ?(skip_data_dir = false)
    ?(skip_addresses = false) () =
  let open Form_builder in
  (* Helper for network field - needs special handling for dynamic fetch *)
  let network_field =
    if edit_mode then
      readonly ~label:"Network" ~get:(fun m ->
          let value = (get_node m).network in
          network_display_name value)
    else
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
                       (a : Teztnets.network_info)
                       (b : Teztnets.network_info)
                     ->
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
                ~on_select
                ())
        ()
  in
  (* Use shared port validation from lib *)
  let validate_port addr _states ~label ~example =
    match
      Port_validation.validate_addr
        ~addr
        ?exclude_instance:editing_instance
        ~example
        ()
    with
    | Ok () -> Ok ()
    | Error err ->
        Error (Printf.sprintf "%s: %s" label (Port_validation.pp_error err))
  in

  (* History mode field - read-only in edit mode *)
  let history_mode_field =
    if edit_mode then
      readonly ~label:"History Mode" ~get:(fun m -> (get_node m).history_mode)
    else
      choice
        ~label:"History Mode"
        ~get:(fun m -> (get_node m).history_mode)
        ~set:(fun history_mode m ->
          let node = get_node m in
          let m' = set_node {node with history_mode} m in
          on_history_mode_changed history_mode ;
          m')
        ~items:["rolling"; "full"; "archive"]
        ~to_string:(fun x -> x)
  in
  (* Data dir field - read-only in edit mode *)
  let data_dir_field =
    if edit_mode then
      readonly ~label:"Data Dir" ~get:(fun m -> (get_node m).data_dir)
    else
      node_data_dir
        ~label:"Data Dir"
        ~get:(fun m -> (get_node m).data_dir)
        ~set:(fun data_dir m ->
          let node = get_node m in
          set_node {node with data_dir} m)
        ~validate:(fun m ->
          (* Use non-blocking cache to avoid syscalls during typing *)
          let states =
            Form_builder_common.cached_service_states_nonblocking ()
          in
          let data_dir = (get_node m).data_dir in
          is_nonempty data_dir
          && not
               (List.exists
                  (fun (st : Data.Service_state.t) ->
                    String.equal
                      (String.trim st.service.Service.data_dir)
                      (String.trim data_dir))
                  states))
        ~validate_msg:(fun m ->
          let states =
            Form_builder_common.cached_service_states_nonblocking ()
          in
          let data_dir = String.trim (get_node m).data_dir in
          if data_dir = "" then Some "Data directory is required"
          else
            (* Check if data_dir is already used by another instance *)
            match
              List.find_opt
                (fun (st : Data.Service_state.t) ->
                  String.equal
                    (String.trim st.service.Service.data_dir)
                    data_dir)
                states
            with
            | Some st ->
                Some
                  (Printf.sprintf
                     "Already used by instance '%s'. Choose a different \
                      directory."
                     st.service.Service.instance)
            | None -> None)
        ()
  in
  let network_fields =
    if skip_network then []
    else
      [
        network_field
        |> with_hint
             "Tezos network to connect to. Mainnet for production, testnets \
              for development.";
        history_mode_field
        |> with_hint
             "Rolling: minimal disk (~50GB). Full: all blocks. Archive: all \
              states (1TB+).";
      ]
  in
  let data_dir_fields =
    if skip_data_dir then []
    else
      [
        data_dir_field
        |> with_hint
             "Directory where blockchain data is stored. Must be writable by \
              service user.";
      ]
  in
  let address_fields =
    if skip_addresses then []
    else
      [
        (* RPC Address *)
        validated_text
          ~label:"RPC Address"
          ~get:(fun m -> (get_node m).rpc_addr)
          ~set:(fun rpc_addr m ->
            let node = get_node m in
            set_node {node with rpc_addr} m)
          ~validate:(fun m ->
            let addr = (get_node m).rpc_addr in
            (* Use non-blocking cache to avoid syscalls during typing *)
            let states =
              Form_builder_common.cached_service_states_nonblocking ()
            in
            validate_port
              addr
              states
              ~label:"RPC Address"
              ~example:"127.0.0.1:8732")
        |> with_hint
             "RPC endpoint for clients (bakers, wallets). Use 127.0.0.1 for \
              local only.";
        (* P2P Address *)
        validated_text
          ~label:"P2P Address"
          ~get:(fun m -> (get_node m).p2p_addr)
          ~set:(fun p2p_addr m ->
            let node = get_node m in
            set_node {node with p2p_addr} m)
          ~validate:(fun m ->
            let addr = (get_node m).p2p_addr in
            (* Use non-blocking cache to avoid syscalls during typing *)
            let states =
              Form_builder_common.cached_service_states_nonblocking ()
            in
            validate_port
              addr
              states
              ~label:"P2P Address"
              ~example:"0.0.0.0:9732")
        |> with_hint
             "P2P port for peer discovery. Use 0.0.0.0 to accept connections \
              from all interfaces.";
      ]
  in
  network_fields @ data_dir_fields @ address_fields
