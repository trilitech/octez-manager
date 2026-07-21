(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Baker installation form using field bundles.

    Demonstrates bundle composition for complex forms with custom fields. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_baker_form_v3"

(** {1 Custom Types} *)

type dal_selection =
  | Dal_none
  | Dal_instance of string
  | Dal_endpoint of string

type signer_selection =
  | Signer_local_keys
  | Signer_instance of string
  | Signer_uri of string

(** {1 Model} *)

type model = {
  core : Form_builder_common.core_service_config;
  client : Form_builder_common.client_config;
  (* Baker-specific fields *)
  parent_node : string; (* empty = external *)
  node_data_dir : string;
  dal : dal_selection;
  delegates : string list;
  liquidity_baking_vote : string;
  signer : signer_selection;
  extra_nodes : string list;
  (* extra node instances or endpoints for redundancy *)
  (* Edit mode fields *)
  edit_mode : bool;
  original_instance : string option;
  stopped_dependents : string list;
}

let base_initial_model () =
  {
    core =
      {
        instance_name = "baker";
        service_user = Form_builder_common.default_service_user ();
        app_bin_dir =
          Form_builder_common.default_app_bin_dir ~binary_name:"octez-baker";
        bin_source = None;
        enable_on_boot = true;
        start_now = true;
        extra_args = "";
        group = Context.take_pending_group ();
      };
    client =
      {
        base_dir = Paths.default_role_dir "baker" "baker";
        node = `None;
        node_endpoint = "127.0.0.1:8732";
      };
    parent_node = "";
    node_data_dir = "";
    dal = Dal_none;
    delegates = [];
    liquidity_baking_vote = "pass";
    signer = Signer_local_keys;
    extra_nodes = [];
    edit_mode = false;
    original_instance = None;
    stopped_dependents = [];
  }

let make_initial_model () =
  match Context.take_pending_edit_service () with
  | Some edit_ctx when edit_ctx.service.Service.role = "baker" ->
      let svc = edit_ctx.service in
      (* Read baker env to get delegates and other config *)
      let env =
        match Node_env.read ~inst:svc.Service.instance with
        | Ok pairs -> pairs
        | Error _ -> []
      in
      let lookup key =
        match List.assoc_opt key env with Some v -> String.trim v | None -> ""
      in
      let delegates =
        match lookup "OCTEZ_BAKER_DELEGATES_CSV" with
        | "" -> []
        | csv ->
            csv |> String.split_on_char ',' |> List.map String.trim
            |> List.filter (( <> ) "")
      in
      let node_endpoint = lookup "OCTEZ_NODE_ENDPOINT" in
      let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
      let extra_args = lookup "OCTEZ_BAKER_COMMAND_ARGS" in
      let dal_config = lookup "OCTEZ_DAL_CONFIG" in
      let dal_instance = lookup "OCTEZ_DAL_INSTANCE" in
      let dal =
        if dal_config = "disabled" then Dal_none
        else if dal_instance <> "" then Dal_instance dal_instance
        else if dal_config = "" then Dal_none
        else Dal_endpoint dal_config
      in
      let signer_uri = lookup "OCTEZ_REMOTE_SIGNER_URI" in
      let signer_instance = lookup "OCTEZ_SIGNATORY_INSTANCE" in
      let signer =
        if signer_uri = "" then Signer_local_keys
        else if signer_instance <> "" then Signer_instance signer_instance
        else Signer_uri signer_uri
      in
      let lb_vote = lookup "OCTEZ_BAKER_LB_VOTE" in
      let extra_nodes =
        match lookup "OCTEZ_EXTRA_NODE_ENDPOINTS" with
        | "" -> []
        | csv ->
            csv |> String.split_on_char ',' |> List.map String.trim
            |> List.filter (( <> ) "")
      in
      {
        core =
          {
            instance_name = svc.Service.instance;
            service_user = svc.Service.service_user;
            app_bin_dir = svc.Service.app_bin_dir;
            bin_source = svc.Service.bin_source;
            enable_on_boot = true;
            start_now = false;
            (* Don't auto-start after edit *)
            extra_args;
            group = svc.Service.group;
          };
        client =
          {
            base_dir =
              (if base_dir = "" then
                 Paths.default_role_dir "baker" svc.Service.instance
               else base_dir);
            node =
              (match svc.Service.depends_on with
              | Some inst -> `Service inst
              | None -> `None);
            node_endpoint =
              (if node_endpoint = "" then "127.0.0.1:8732" else node_endpoint);
          };
        parent_node =
          (match svc.Service.depends_on with Some inst -> inst | None -> "");
        node_data_dir = svc.Service.data_dir;
        dal;
        delegates;
        liquidity_baking_vote = (if lb_vote = "" then "pass" else lb_vote);
        signer;
        extra_nodes;
        edit_mode = true;
        original_instance = Some svc.Service.instance;
        stopped_dependents = edit_ctx.stopped_dependents;
      }
  | _ -> base_initial_model ()

(** {1 Helper Functions} *)

let has_octez_baker_binary = Form_builder_common.has_octez_baker_binary

let node_services states =
  states
  |> List.filter (fun (s : Data.Service_state.t) ->
      String.equal s.service.Service.role "node")

let find_node states inst =
  node_services states
  |> List.find_opt (fun (s : Data.Service_state.t) ->
      String.equal
        (Form_builder_common.normalize s.service.Service.instance)
        (Form_builder_common.normalize inst))

let dal_services states =
  states
  |> List.filter (fun (s : Data.Service_state.t) ->
      let role = Form_builder_common.normalize s.service.Service.role in
      String.equal role "dal-node" || String.equal role "dal")

let find_dal states inst =
  dal_services states
  |> List.find_opt (fun (s : Data.Service_state.t) ->
      String.equal
        (Form_builder_common.normalize s.service.Service.instance)
        (Form_builder_common.normalize inst))

let endpoint_with_scheme = Form_builder_common.endpoint_with_scheme

let node_endpoint_of_service (svc : Service.t) =
  Rpc_addr.to_endpoint svc.Service.rpc_addr

let dal_endpoint_of_service (svc : Service.t) =
  Rpc_addr.to_endpoint svc.Service.rpc_addr

let endpoint_host_port ep =
  let ep = endpoint_with_scheme ep in
  try
    let uri = Uri.of_string ep in
    match (Uri.host uri, Uri.port uri) with
    | Some host, Some port -> Printf.sprintf "%s:%d" host port
    | Some host, None -> Printf.sprintf "%s:8732" host
    | _ -> ep
  with _ -> ep

let baker_node_mode model states =
  match find_node states model.parent_node with
  | Some _ -> `Local
  | None ->
      if Form_builder_common.is_nonempty model.node_data_dir then `Local
      else `Remote

let resolve_node_data_dir model states =
  match find_node states model.parent_node with
  | Some svc -> svc.Data.Service_state.service.Service.data_dir
  | None -> String.trim model.node_data_dir

let resolve_node_endpoint model states =
  match find_node states model.parent_node with
  | Some svc -> node_endpoint_of_service svc.Data.Service_state.service
  | None -> endpoint_with_scheme model.client.node_endpoint

let resolve_dal_config model states =
  match model.dal with
  | Dal_none -> Dal_disabled
  | Dal_endpoint ep -> Dal_endpoint (endpoint_with_scheme ep)
  | Dal_instance inst -> (
      match find_dal states inst with
      | Some svc ->
          Dal_endpoint (dal_endpoint_of_service svc.Data.Service_state.service)
      | None -> Dal_auto)

(** {1 Custom Fields} *)

let parent_node_field =
  Form_builder.custom
    ~label:"Parent Node"
    ~get:(fun m -> if m.parent_node = "" then "External" else m.parent_node)
    ~edit:(fun model_ref ->
      let states = Form_builder_common.cached_service_states () in
      let nodes = node_services states in
      let items = `External :: List.map (fun n -> `Node n) nodes in
      let to_string = function
        | `External -> "External/None (use custom endpoint)"
        | `Node n ->
            let svc = n.Data.Service_state.service in
            Printf.sprintf
              "Node · %s (%s)"
              svc.Service.instance
              svc.Service.network
      in
      let on_select = function
        | `External ->
            model_ref :=
              {
                !model_ref with
                parent_node = "";
                client = {!model_ref.client with node = `None};
              }
        | `Node n ->
            let svc = n.Data.Service_state.service in
            let current_name =
              Form_builder_common.normalize !model_ref.core.instance_name
            in
            let should_autoname =
              current_name = "" || String.equal current_name "baker"
            in
            model_ref :=
              {
                !model_ref with
                parent_node = svc.Service.instance;
                (* Derive endpoint + data dir from parent node *)
                client =
                  {
                    !model_ref.client with
                    node = `Service svc.Service.instance;
                    node_endpoint = node_endpoint_of_service svc;
                  };
                node_data_dir = svc.Service.data_dir;
              } ;
            if should_autoname then (
              (* Strip "node-" prefix to avoid "baker-node-shadownet" *)
              let dep_name =
                let inst = svc.Service.instance in
                if String.starts_with ~prefix:"node-" inst then
                  String.sub inst 5 (String.length inst - 5)
                else inst
              in
              let new_name = Printf.sprintf "baker-%s" dep_name in
              let default_dir = Paths.default_role_dir "baker" new_name in
              let new_core = {!model_ref.core with instance_name = new_name} in
              let new_client =
                {!model_ref.client with base_dir = default_dir}
              in
              model_ref :=
                {!model_ref with core = new_core; client = new_client} ;
              (* Maybe use app_bin_dir from node *)
              if
                has_octez_baker_binary svc.Service.app_bin_dir
                && not (has_octez_baker_binary !model_ref.core.app_bin_dir)
              then
                let new_core =
                  {!model_ref.core with app_bin_dir = svc.Service.app_bin_dir}
                in
                model_ref := {!model_ref with core = new_core})
      in
      Modal_helpers.open_choice_modal
        ~title:"Parent Node"
        ~items
        ~to_string
        ~on_select
        ())
    ()

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
          (* Use non-blocking cache to avoid syscalls during typing *)
          let states =
            Form_builder_common.cached_service_states_nonblocking ()
          in
          Option.is_some (find_dal states inst)
      | Dal_endpoint ep ->
          Option.is_some
            (Form_builder_common.parse_host_port (endpoint_host_port ep)))
    ~edit:(fun model_ref ->
      (* edit uses blocking version - only called when opening modal *)
      let states = Form_builder_common.cached_service_states () in
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
            model_ref :=
              {!model_ref with dal = Dal_instance svc.Service.instance} ;
            (* Maybe use app_bin_dir from DAL service *)
            if
              has_octez_baker_binary svc.Service.app_bin_dir
              && not (has_octez_baker_binary !model_ref.core.app_bin_dir)
            then
              let new_core =
                {!model_ref.core with app_bin_dir = svc.Service.app_bin_dir}
              in
              model_ref := {!model_ref with core = new_core}
        | `Custom ->
            Modal_helpers.prompt_text_modal
              ~title:"DAL Endpoint"
              ~placeholder:(Some "host:port (e.g., 127.0.0.1:10732)")
              ~initial:
                (match !model_ref.dal with Dal_endpoint ep -> ep | _ -> "")
              ~on_submit:(fun ep ->
                model_ref := {!model_ref with dal = Dal_endpoint ep})
              ()
      in
      Modal_helpers.open_choice_modal
        ~title:"DAL Node"
        ~items
        ~to_string
        ~on_select
        ())
    ()

let signer_field =
  Form_builder.custom
    ~label:"Remote Signer"
    ~get:(fun m ->
      match m.signer with
      | Signer_local_keys -> "Local keys"
      | Signer_instance inst -> inst
      | Signer_uri uri -> uri)
    ~validate:(fun m ->
      match m.signer with
      | Signer_local_keys -> true
      | Signer_instance inst ->
          (* Validate that instance exists and is a Signatory *)
          let states =
            Form_builder_common.cached_service_states_nonblocking ()
          in
          List.exists
            (fun (s : Data.Service_state.t) ->
              String.equal s.service.Service.instance inst
              && String.equal
                   (String.lowercase_ascii s.service.Service.role)
                   "signatory")
            states
      | Signer_uri uri ->
          (* Basic URI format validation *)
          String.length uri > 0
          && (String.starts_with ~prefix:"http://" uri
             || String.starts_with ~prefix:"https://" uri
             || String.starts_with ~prefix:"unix:" uri))
    ~edit:(fun model_ref ->
      let states = Form_builder_common.cached_service_states () in
      let signatory_services =
        List.filter
          (fun (s : Data.Service_state.t) ->
            String.equal
              (String.lowercase_ascii s.service.Service.role)
              "signatory")
          states
      in
      let items =
        [`Local]
        @ (signatory_services |> List.map (fun n -> `Signatory n))
        @ [`Custom]
      in
      let to_string = function
        | `Local -> "Local keys (default)"
        | `Signatory n ->
            let svc = n.Data.Service_state.service in
            let endpoint = Rpc_addr.to_endpoint svc.Service.rpc_addr in
            Printf.sprintf "Signatory · %s (%s)" svc.Service.instance endpoint
        | `Custom -> "Custom URI"
      in
      let on_select choice =
        match choice with
        | `Local -> model_ref := {!model_ref with signer = Signer_local_keys}
        | `Signatory n ->
            let svc = n.Data.Service_state.service in
            model_ref :=
              {!model_ref with signer = Signer_instance svc.Service.instance}
        | `Custom ->
            Modal_helpers.prompt_text_modal
              ~title:"Remote Signer URI"
              ~placeholder:
                (Some
                   "http://host:port or unix:/path (e.g., \
                    http://127.0.0.1:6732)")
              ~initial:
                (match !model_ref.signer with Signer_uri uri -> uri | _ -> "")
              ~on_submit:(fun uri ->
                model_ref := {!model_ref with signer = Signer_uri uri})
              ()
      in
      Modal_helpers.open_choice_modal
        ~title:"Remote Signer"
        ~items
        ~to_string
        ~on_select
        ())
    ()

let node_data_dir_field =
  Form_builder.custom
    ~label:"Node Data Dir"
    ~get:(fun m -> m.node_data_dir)
    ~validate:(fun m ->
      (* Use non-blocking cache to avoid syscalls during typing *)
      let states = Form_builder_common.cached_service_states_nonblocking () in
      let selected_node = find_node states m.parent_node in
      let node_mode = baker_node_mode m states in
      match node_mode with
      | `Local ->
          Form_builder_common.is_nonempty (resolve_node_data_dir m states)
          || Option.is_some selected_node
      | `Remote -> true)
    ~validate_msg:(fun m ->
      (* Use non-blocking cache to avoid syscalls during typing *)
      let states = Form_builder_common.cached_service_states_nonblocking () in
      let node_mode = baker_node_mode m states in
      match node_mode with
      | `Local -> Some "Node data directory is required for local mode"
      | `Remote -> None)
    ~edit:(fun model_ref ->
      (* edit uses blocking version - only called when opening modal *)
      let states = Form_builder_common.cached_service_states () in
      match find_node states !model_ref.parent_node with
      | Some _ ->
          Modal_helpers.show_error
            ~title:"Node Data Dir"
            "Derived from the selected parent node. Clear Parent Node to edit."
      | None ->
          Modal_helpers.select_node_data_dir_modal
            ~on_select:(fun path ->
              model_ref := {!model_ref with node_data_dir = path})
            ())
    ()

let node_endpoint_field =
  Form_builder.custom
    ~label:"Node Endpoint"
    ~get:(fun m -> m.client.node_endpoint)
    ~validate:(fun m ->
      match
        Form_builder_common.parse_host_port
          (endpoint_host_port m.client.node_endpoint)
      with
      | Some _ -> true
      | None -> false)
    ~validate_msg:(fun _ ->
      Some "Format must be host:port (e.g., 127.0.0.1:8732)")
    ~edit:(fun model_ref ->
      let states = Form_builder_common.cached_service_states () in
      match find_node states !model_ref.parent_node with
      | Some _ ->
          Modal_helpers.show_error
            ~title:"Node Endpoint"
            "Derived from the selected parent node. Clear Parent Node to edit."
      | None ->
          Modal_helpers.prompt_validated_text_modal
            ~title:"Node Endpoint (host:port)"
            ~initial:!model_ref.client.node_endpoint
            ~validator:(fun text ->
              match
                Form_builder_common.parse_host_port (endpoint_host_port text)
              with
              | Some _ -> Ok ()
              | None -> Error "Format must be host:port (e.g., 127.0.0.1:8732)")
            ~on_submit:(fun v ->
              let client = !model_ref.client in
              model_ref :=
                {!model_ref with client = {client with node_endpoint = v}})
            ())
    ()

(** Custom delegates field with signatory key selection integration *)
let delegates_field =
  Form_builder.custom
    ~label:"Delegates"
    ~get:(fun m -> String.concat ", " m.delegates)
    ~validate:(fun _m -> true)
    ~edit:(fun model_ref ->
      (* Check if a signatory instance is selected *)
      let signatory_instance =
        match !model_ref.signer with
        | Signer_instance inst -> Some inst
        | _ -> None
      in
      let get_suggestions () =
        (* Get suggestions from base_dir keys *)
        if String.trim !model_ref.client.base_dir = "" then []
        else
          match
            Keys_reader.read_public_key_hashes
              ~base_dir:!model_ref.client.base_dir
          with
          | Ok keys -> List.map (fun k -> k.Keys_reader.value) keys
          | Error _ -> []
      in
      (* Build alias lookup map for displaying keys with aliases *)
      let alias_map =
        let all_keys = Wallets_page.get_all_keys () in
        List.fold_left
          (fun acc (hash, alias, _base_dir) -> (hash, alias) :: acc)
          []
          all_keys
      in
      (* Build modal items based on signatory selection *)
      let build_items () =
        let current = !model_ref.delegates in
        let suggestions = get_suggestions () in
        let base_items =
          (* Toggle items for current delegates *)
          (current |> List.map (fun d -> `Toggle d))
          (* Add suggestions not in current list *)
          @ (suggestions
            |> List.filter (fun s -> not (List.mem s current))
            |> List.map (fun s -> `Toggle s))
        in
        let signatory_items =
          match signatory_instance with
          | Some inst -> (
              match Signatory.read_authorized_keys inst with
              | Ok keys ->
                  (* Add keys from signatory that aren't already in the list *)
                  keys
                  |> List.filter (fun k ->
                      not
                        (List.mem k current
                        || List.exists
                             (fun (`Toggle d) -> String.equal d k)
                             base_items))
                  |> List.map (fun k -> `Toggle k)
              | Error _ -> [])
          | None -> []
        in
        base_items @ signatory_items @ [`Add; `Clear]
      in
      let to_string = function
        | `Toggle item ->
            let current = !model_ref.delegates in
            let checked = List.mem item current in
            let checkbox = if checked then "[x]" else "[ ]" in
            (* Display as "alias (hash)" if alias exists, otherwise just hash *)
            let display_text =
              match List.assoc_opt item alias_map with
              | Some alias -> Printf.sprintf "%s (%s)" alias item
              | None -> item
            in
            Printf.sprintf "%s %s" checkbox display_text
        | `Add -> "Add key (manual)"
        | `Clear -> "Clear all"
      in
      let on_select = function
        | `Toggle item ->
            let current = !model_ref.delegates in
            let updated =
              if List.mem item current then
                List.filter (fun x -> x <> item) current
              else current @ [item]
            in
            model_ref := {!model_ref with delegates = updated} ;
            `KeepOpen
        | `Add ->
            Modal_helpers.prompt_validated_text_modal
              ~title:"Add Delegate Key"
              ~validator:(fun v ->
                if String.trim v = "" then Error "Cannot be empty"
                else if
                  String.starts_with ~prefix:"tz1" v
                  || String.starts_with ~prefix:"tz2" v
                  || String.starts_with ~prefix:"tz3" v
                  || String.starts_with ~prefix:"tz4" v
                then Ok ()
                else Error "Key must start with tz1, tz2, tz3, or tz4")
              ~on_submit:(fun v ->
                let v = String.trim v in
                let current = !model_ref.delegates in
                if not (List.mem v current) then
                  model_ref := {!model_ref with delegates = current @ [v]})
              () ;
            `KeepOpen
        | `Clear ->
            model_ref := {!model_ref with delegates = []} ;
            `KeepOpen
      in
      let title =
        match signatory_instance with
        | Some inst -> Printf.sprintf "Delegates (Signatory: %s)" inst
        | None -> "Delegates"
      in
      Modal_helpers.open_multiselect_modal
        ~title
        ~items:build_items
        ~to_string
        ~on_select
        ())
    ()

(** {1 Form Specification} *)

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  let baker_mode_for_help model =
    try baker_node_mode model (Form_builder_common.cached_service_states ())
    with _ -> `Remote
  in
  {
    title = " Install Baker ";
    initial_model = make_initial_model;
    fields =
      (fun model ->
        (* Base dir field - readonly in edit mode *)
        let base_dir_field =
          if model.edit_mode then
            readonly ~label:"Baker Base Dir" ~get:(fun m -> m.client.base_dir)
            |> with_hint "Base directory cannot be changed after creation."
          else
            client_base_dir
              ~label:"Baker Base Dir"
              ~get:(fun m -> m.client.base_dir)
              ~set:(fun base_dir m ->
                let m' = {m with client = {m.client with base_dir}} in
                if String.equal base_dir m.client.base_dir then m'
                else {m' with delegates = []})
              ~validate:(fun m ->
                Form_builder_common.is_nonempty m.client.base_dir)
              ()
        in
        (* 1. Dependencies: node and dal node *)
        [parent_node_field; dal_node_field]
        (* 2. Network params - N/A for baker, inherited from node *)
        (* 3. App bin dir *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-baker"
            ~subcommand:["run"]
            ~baker_mode:baker_mode_for_help
            ~binary_validator:has_octez_baker_binary
            ~skip_instance_name:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ()
        (* 4. Base dir *)
        @ [base_dir_field]
        (* 5. Baker params: remote signer, delegates, liquidity baking *)
        @ [
            signer_field;
            delegates_field;
            choice
              ~label:"Liquidity Baking Vote"
              ~get:(fun m -> m.liquidity_baking_vote)
              ~set:(fun liquidity_baking_vote m ->
                {m with liquidity_baking_vote})
              ~items:["pass"; "on"; "off"]
              ~to_string:(fun x -> x);
          ]
        (* 6. Addresses and ports: node endpoint, node data dir *)
        @ [
            node_endpoint_field;
            (if model.edit_mode then
               readonly ~label:"Node Data Dir" ~get:(fun m ->
                   let states =
                     Form_builder_common.cached_service_states_nonblocking ()
                   in
                   resolve_node_data_dir m states)
               |> with_hint
                    "Node data directory cannot be changed after creation."
             else node_data_dir_field);
          ]
        (* 6b. Extra nodes for redundancy *)
        @ [
            string_list
              ~label:"Extra Nodes [experimental]"
              ~get:(fun m -> m.extra_nodes)
              ~set:(fun extra_nodes m -> {m with extra_nodes})
              ~get_suggestions:(fun model ->
                (* Get all available node instances, excluding the parent node *)
                let states = Form_builder_common.cached_service_states () in
                let parent_instance =
                  match model.client.node with
                  | `Service inst -> Some inst
                  | _ -> None
                in
                states
                |> List.filter (fun s ->
                    s.Data.Service_state.service.Service.role = "node")
                |> List.filter (fun s ->
                    (* Exclude parent node from suggestions *)
                    match parent_instance with
                    | Some parent ->
                        s.Data.Service_state.service.Service.instance <> parent
                    | None -> true)
                |> List.map (fun s ->
                    s.Data.Service_state.service.Service.instance))
              ~item_validator:(fun v ->
                (* Accept either instance names or http(s):// endpoints *)
                if
                  String.starts_with ~prefix:"http://" v
                  || String.starts_with ~prefix:"https://" v
                then
                  (* Validate as endpoint - extract host:port from URL *)
                  let url = String.trim v in
                  let after_scheme =
                    if String.starts_with ~prefix:"https://" url then
                      String.sub url 8 (String.length url - 8)
                    else String.sub url 7 (String.length url - 7)
                  in
                  (* Basic validation: must have host:port format *)
                  if String.contains after_scheme ':' then Ok ()
                  else Error "Endpoint must be in format http://host:port"
                else
                  (* Validate as instance name - check if it exists *)
                  let states =
                    Form_builder_common.cached_service_states_nonblocking ()
                  in
                  if
                    List.exists
                      (fun s ->
                        s.Data.Service_state.service.Service.role = "node"
                        && s.Data.Service_state.service.Service.instance = v)
                      states
                  then Ok ()
                  else Error (Printf.sprintf "Node instance '%s' not found" v))
              ()
            |> with_hint
                 "Optional: Select extra node instances or add custom RPC \
                  endpoints for redundancy";
          ]
        (* 7. Extra args *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-baker"
            ~subcommand:["run"]
            ~baker_mode:baker_mode_for_help
            ~binary_validator:has_octez_baker_binary
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ()
        (* 8. Service fields: service user, enable, start *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-baker"
            ~subcommand:["run"]
            ~baker_mode:baker_mode_for_help
            ~binary_validator:has_octez_baker_binary
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_extra_args:true
            ~edit_mode:model.edit_mode
            ()
        (* 9. Instance name *)
        @ [
            validated_text
              ~label:"Instance Name"
              ~get:(fun m -> m.core.instance_name)
              ~set:(fun instance_name m ->
                let old = m.core.instance_name in
                let new_core = {m.core with instance_name} in
                (* In edit mode, never change base_dir - data is already there *)
                if m.edit_mode then {m with core = new_core}
                else
                  let default_dir =
                    Paths.default_role_dir "baker" instance_name
                  in
                  let keep_base_dir =
                    String.trim m.client.base_dir <> ""
                    && not
                         (String.equal
                            m.client.base_dir
                            (Paths.default_role_dir "baker" old))
                  in
                  let new_base_dir =
                    if keep_base_dir then m.client.base_dir else default_dir
                  in
                  let base_dir_changed =
                    not (String.equal new_base_dir m.client.base_dir)
                  in
                  let new_client = {m.client with base_dir = new_base_dir} in
                  let m' = {m with core = new_core; client = new_client} in
                  if base_dir_changed then {m' with delegates = []} else m')
              ~validate:(fun m ->
                let states = Form_builder_common.cached_service_states () in
                match
                  Form_builder_common.validate_instance_name_syntax
                    m.core.instance_name
                with
                | Error e -> Error e
                | Ok () ->
                    if
                      Form_builder_common.instance_in_use
                        ~states
                        m.core.instance_name
                      && not
                           (m.edit_mode
                           && m.original_instance = Some m.core.instance_name)
                    then Error "Instance name already exists"
                    else Ok ());
          ]
        (* 10. Group *)
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
          (* Validate delegates against Signatory authorized keys *)
          match model.signer with
          | Signer_instance inst -> (
              match Signatory.read_authorized_keys inst with
              | Ok authorized_keys ->
                  (* Check if all delegates are authorized *)
                  let unauthorized =
                    List.filter
                      (fun delegate -> not (List.mem delegate authorized_keys))
                      model.delegates
                  in
                  if unauthorized = [] then Ok ()
                  else
                    let message =
                      Printf.sprintf
                        "The following delegate key(s) are NOT in Signatory \
                         '%s' authorized keys:\n\n\
                         %s\n\n\
                         The baker will fail at runtime if these keys are not \
                         configured in the Signatory.\n\n\
                         Continue anyway?"
                        inst
                        (String.concat "\n" unauthorized)
                    in
                    Error
                      (`Modal
                         ( message,
                           fun () ->
                             (* on_continue callback - will trigger submit *)
                             () ))
              | Error _ ->
                  (* If we can't read signatory config, show warning *)
                  let message =
                    Printf.sprintf
                      "Could not read Signatory '%s' configuration to validate \
                       delegate keys.\n\n\
                       Continue anyway?"
                      inst
                  in
                  Error
                    (`Modal
                       ( message,
                         fun () ->
                           (* on_continue callback - will trigger submit *)
                           () )))
          | _ -> Ok ());
    on_init = None;
    on_refresh = None;
    pre_submit_modal = None;
    on_submit =
      (fun model ->
        let states = Form_builder_common.cached_service_states () in
        let selected_node = find_node states model.parent_node in
        let node_endpoint = resolve_node_endpoint model states in
        let dal_config = resolve_dal_config model states in

        (* Always use journald - octez binaries handle their own file logging *)
        let logging_mode = Logging_mode.default in

        let extra_args =
          Form_builder_common.prepare_extra_args model.core.extra_args
        in

        let base_dir =
          let trimmed = String.trim model.client.base_dir in
          if trimmed = "" then
            Paths.default_role_dir "baker" model.core.instance_name
          else trimmed
        in

        (* Extract DAL node instance name if using local DAL *)
        let dal_node =
          match model.dal with Dal_instance inst -> Some inst | _ -> None
        in
        (* Convert signer_selection to signer_mode *)
        let signer_mode =
          match model.signer with
          | Signer_local_keys -> Signer_types.Local_keys
          | Signer_instance inst -> (
              (* Look up instance to get URI *)
              let states = Form_builder_common.cached_service_states () in
              let signatory =
                List.find_opt
                  (fun (s : Data.Service_state.t) ->
                    String.equal s.service.Service.instance inst)
                  states
              in
              match signatory with
              | Some s ->
                  let uri = Rpc_addr.to_endpoint s.service.Service.rpc_addr in
                  Signer_types.Remote_signer {instance = Some inst; uri}
              | None ->
                  (* Fallback if instance not found - should not happen due to validation *)
                  Signer_types.Local_keys)
          | Signer_uri uri -> Signer_types.Remote_signer {instance = None; uri}
        in
        (* Parse extra_nodes: detect if each entry is an instance or endpoint *)
        let extra_nodes =
          List.map
            (fun entry ->
              let entry = String.trim entry in
              if
                String.starts_with ~prefix:"http://" entry
                || String.starts_with ~prefix:"https://" entry
              then Installer_types.Extra_endpoint entry
              else Installer_types.Extra_instance entry)
            model.extra_nodes
        in
        let req =
          {
            Installer_types.instance = model.core.instance_name;
            node_mode =
              (match selected_node with
              | Some svc ->
                  Installer_types.Local_instance
                    svc.Data.Service_state.service.Service.instance
              | None -> Installer_types.Remote_endpoint node_endpoint);
            base_dir = Some base_dir;
            delegates = model.delegates;
            dal_config;
            dal_node;
            liquidity_baking_vote =
              (if String.trim model.liquidity_baking_vote = "" then None
               else Some (String.trim model.liquidity_baking_vote));
            signer_mode;
            extra_args;
            extra_env = [];
            service_user = model.core.service_user;
            app_bin_dir = model.core.app_bin_dir;
            bin_source = model.core.bin_source;
            logging_mode;
            auto_enable = model.core.enable_on_boot;
            preserve_data = model.edit_mode;
            extra_nodes;
          }
        in

        (* In edit mode, stop the service before applying changes *)
        let* () =
          if model.edit_mode then
            (* Use original instance name when stopping (may be different if renaming) *)
            let stop_instance =
              Option.value
                ~default:model.core.instance_name
                model.original_instance
            in
            match
              Lifecycle.stop_service ~quiet:true ~instance:stop_instance ()
            with
            | Ok () -> Ok ()
            | Error (`Msg _) ->
                Ok () (* Continue anyway - service might be stopped *)
          else Ok ()
        in
        let* () =
          if Paths.is_root () then
            System_user.ensure_service_account
              ~quiet:true
              ~name:model.core.service_user
              ()
          else Ok ()
        in
        let* (module PM) = Form_builder_common.require_package_manager () in
        let* _ = PM.install_baker ~quiet:true req in
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
        (* Refresh caches so UI shows updated data *)
        Delegate_scheduler.invalidate_config ~instance:model.core.instance_name ;
        Baker_highwatermarks.refresh ~instance:model.core.instance_name ;
        System_metrics_scheduler.invalidate_version
          ~role:"baker"
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
                ~role:"baker"
                ~service:model.core.instance_name
              |> Result.map_error (fun e -> `Msg e)
          | None -> Error (`Msg "Service lifecycle capability not available")
        else Ok ());
  }

module Page = Form_builder.Make (struct
  type nonrec model = model

  let spec = spec
end)

module For_tests = struct
  let initial_model = make_initial_model

  let baker_node_mode = baker_node_mode
end

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
