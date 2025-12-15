(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Table_widget = Miaou_widgets_display.Table_widget
module Keys = Miaou.Core.Keys
open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_baker_form"

type dal_selection =
  | Dal_none
  | Dal_instance of string
  | Dal_endpoint of string

type form_state = {
  instance_name : string;
  parent_node : string;
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

type state = {
  form : form_state;
  cursor : int;
  next_page : string option;
  service_states : Data.Service_state.t list;
}

type msg = unit

let default_service_user () =
  if Common.is_root () then "octez"
  else
    match Unix.getpwuid (Unix.geteuid ()) with
    | pw when String.trim pw.Unix.pw_name <> "" -> pw.Unix.pw_name
    | _ -> "octez"

let normalize s = String.lowercase_ascii (String.trim s)

let is_nonempty s = String.trim s <> ""

let default_form =
  {
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

let form_ref = ref default_form

let push_help_hint ?short ?long () =
  Miaou.Core.Help_hint.clear () ;
  match (short, long) with
  | None, None -> ()
  | _ -> Miaou.Core.Help_hint.push ?short ?long ()

let update_form_ref f = form_ref := f !form_ref

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

let endpoint_with_scheme rpc_addr =
  let trimmed = String.trim rpc_addr in
  if trimmed = "" then "http://127.0.0.1:8732"
  else if
    String.starts_with ~prefix:"http://" (String.lowercase_ascii trimmed)
    || String.starts_with ~prefix:"https://" (String.lowercase_ascii trimmed)
  then trimmed
  else "http://" ^ trimmed

let pad_with_right cols left right =
  if right = "" then left
  else
    let space =
      let raw = cols - String.length left - String.length right in
      if raw < 1 then 1 else raw
    in
    left ^ String.make space ' ' ^ right

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

let maybe_use_app_bin_dir candidate =
  let trimmed = String.trim candidate in
  if trimmed = "" then ()
  else
    let candidate_ok = has_octez_baker_binary trimmed in
    let current_ok = has_octez_baker_binary !form_ref.app_bin_dir in
    if
      candidate_ok
      && ((not current_ok) || not (is_nonempty !form_ref.app_bin_dir))
    then update_form_ref (fun f -> {f with app_bin_dir = trimmed})

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

let node_endpoint_of_service (svc : Service.t) =
  endpoint_with_scheme svc.Service.rpc_addr

let dal_services states =
  states
  |> List.filter (fun (s : Data.Service_state.t) ->
      let role = normalize s.service.Service.role in
      String.equal role "dal-node" || String.equal role "dal")

let find_dal states inst =
  dal_services states
  |> List.find_opt (fun (s : Data.Service_state.t) ->
      String.equal (normalize s.service.Service.instance) (normalize inst))

let dal_endpoint_of_service (svc : Service.t) =
  endpoint_with_scheme svc.Service.rpc_addr

let delegate_label delegates =
  match delegates with [] -> "(none)" | lst -> String.concat ", " lst

let baker_node_mode s =
  match find_node s.service_states s.form.parent_node with
  | Some _ -> `Local
  | None -> if is_nonempty s.form.node_data_dir then `Local else `Remote

let ensure_service_user_initialized () =
  let current = !form_ref in
  if String.trim current.service_user = "" then
    update_form_ref (fun f -> {f with service_user = default_service_user ()})

let ensure_base_dir_initialized () =
  let current = !form_ref in
  let default_dir = Common.default_role_dir "baker" current.instance_name in
  if String.trim current.base_dir = "" then
    update_form_ref (fun f -> {f with base_dir = default_dir})

let ensure_defaults () =
  ensure_service_user_initialized () ;
  ensure_base_dir_initialized ()

let field_hint (f : form_state) cursor : string option * string option =
  let mk heading body =
    let short = Printf.sprintf "**%s** — %s" heading body in
    let long = Printf.sprintf "### %s\n\n%s" heading body in
    (Some short, Some long)
  in
  match cursor with
  | 0 ->
      mk
        "Instance Name"
        (Printf.sprintf
           "Unique label (e.g., `%s`). Used in service names and baker base \
            directories."
           f.instance_name)
  | 1 ->
      mk
        "Parent Node"
        "Choose a managed node or use an external endpoint. Selecting a node \
         auto-fills its data dir, RPC endpoint, and app bin dir when it has \
         octez-baker."
  | 2 ->
      mk
        "Node Mode"
        (let mode =
           if is_nonempty f.parent_node || is_nonempty f.node_data_dir then
             `Local
           else `Remote
         in
         match mode with
         | `Local -> "Runs with a local node data directory."
         | `Remote -> "Runs remotely against the RPC endpoint (no data dir).")
  | 3 ->
      mk
        "DAL Node"
        "Optional DAL node. Choose none, reuse a managed dal-node instance, or \
         enter a custom endpoint."
  | 4 ->
      mk
        "Node Endpoint"
        (Printf.sprintf
           "RPC endpoint for the baker (host:port). Derived from the selected \
            node; override for external nodes. Current: `%s`."
           f.node_endpoint)
  | 5 ->
      mk
        "Node Data Dir"
        (Printf.sprintf
           "Path to the node data directory. Required when using a local \
            external node. Leave empty to run remotely. Current: `%s`."
           f.node_data_dir)
  | 6 ->
      mk
        "Baker Base Dir"
        (Printf.sprintf
           "Wallet base directory for the baker (stores keys/config). Current: \
            `%s`."
           f.base_dir)
  | 7 ->
      mk
        "Delegates"
        "Optional delegate aliases/addresses. Add/remove multiple entries."
  | 8 ->
      mk
        "Liquidity Baking Vote"
        "Vote on liquidity baking subsidy: `on`, `off`, or `pass`. Required \
         for baker to start."
  | 9 ->
      mk
        "Service User"
        (Printf.sprintf
           "Account running the service (current: `%s`). If root and missing, \
            the installer will create it."
           f.service_user)
  | 10 ->
      mk
        "App Bin Dir"
        (Printf.sprintf
           "Directory containing `octez-baker`. Current: `%s`."
           f.app_bin_dir)
  | 11 ->
      mk
        "Logging"
        "File writes baker.log under the instance log dir; Journald uses \
         systemd journal."
  | 12 -> mk "Enable on Boot" "Runs systemctl enable so the baker auto-starts."
  | 13 -> mk "Start Now" "Start the baker service immediately after install."
  | 14 -> mk "Extra Args" "Additional octez-baker run flags (space-separated)."
  | 15 ->
      mk
        "Confirm & Install"
        "Runs the installer with current values. All required fields must be \
         valid."
  | _ -> (None, None)

let apply_field_hint s =
  let short, long = field_hint s.form s.cursor in
  push_help_hint ?short ?long ()

let move s delta =
  let max_cursor = 15 in
  (* Number of fields + confirm *)
  let cursor = max 0 (min max_cursor (s.cursor + delta)) in
  {s with cursor}

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

let open_binary_help s =
  let default_mode = baker_node_mode s in
  let choices =
    match default_mode with
    | `Local -> [`Local; `Remote]
    | `Remote -> [`Remote; `Local]
  in
  let label = function
    | `Local -> "Local node help (run with local node)"
    | `Remote -> "Remote node help (run remotely)"
  in
  let open_for mode =
    Binary_help_explorer.open_baker_run_help
      ~app_bin_dir:s.form.app_bin_dir
      ~mode
      ~on_apply:(fun tokens ->
        let arg_str = String.concat " " tokens in
        update_form_ref (fun f -> {f with extra_args = arg_str}))
  in
  Modal_helpers.open_choice_modal
    ~title:"Baker Flags"
    ~items:choices
    ~to_string:label
    ~on_select:open_for ;
  s

let update_instance_name new_name =
  let old = !form_ref.instance_name in
  let default_dir = Common.default_role_dir "baker" new_name in
  update_form_ref (fun f ->
      let keep_base_dir =
        String.trim f.base_dir <> ""
        && not (String.equal f.base_dir (Common.default_role_dir "baker" old))
      in
      {
        f with
        instance_name = new_name;
        base_dir = (if keep_base_dir then f.base_dir else default_dir);
      })

let resolve_node_data_dir s =
  match find_node s.service_states s.form.parent_node with
  | Some svc -> svc.Data.Service_state.service.Service.data_dir
  | None -> String.trim s.form.node_data_dir

let resolve_node_endpoint s =
  match find_node s.service_states s.form.parent_node with
  | Some svc -> node_endpoint_of_service svc.Data.Service_state.service
  | None -> endpoint_with_scheme s.form.node_endpoint

let resolve_dal_endpoint s =
  match s.form.dal with
  | Dal_none -> None
  | Dal_endpoint ep -> Some (endpoint_with_scheme ep)
  | Dal_instance inst -> (
      match find_dal s.service_states inst with
      | Some svc ->
          Some (dal_endpoint_of_service svc.Data.Service_state.service)
      | None -> None)

let init () =
  ensure_defaults () ;
  let service_states = Data.load_service_states () in
  {form = !form_ref; cursor = 0; next_page = None; service_states}

let update s _ = s

let refresh s =
  ensure_defaults () ;
  let s =
    {s with form = !form_ref; service_states = Data.load_service_states ()}
  in
  match Context.consume_navigation () with
  | Some p -> {s with next_page = Some p}
  | None -> {s with next_page = None}

let edit_field s =
  let open Modal_helpers in
  match s.cursor with
  | 0 ->
      (* Instance Name *)
      prompt_text_modal
        ~title:"Instance Name"
        ~initial:!form_ref.instance_name
        ~on_submit:(fun v -> update_instance_name v)
        () ;
      s
  | 1 ->
      (* Parent Node *)
      let nodes = node_services s.service_states in
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
        | `External -> update_form_ref (fun f -> {f with parent_node = ""})
        | `Node n ->
            let svc = n.Data.Service_state.service in
            let current_name = normalize !form_ref.instance_name in
            let should_autoname =
              current_name = "" || String.equal current_name "baker"
            in
            update_form_ref (fun f ->
                {f with parent_node = svc.Service.instance}) ;
            if should_autoname then
              update_instance_name
                (Printf.sprintf "baker-%s" svc.Service.instance) ;
            maybe_use_app_bin_dir svc.Service.app_bin_dir
      in
      open_choice_modal ~title:"Parent Node" ~items ~to_string ~on_select ;
      s
  | 2 ->
      (* Node Mode (derived) *)
      show_error
        ~title:"Node Mode"
        "Mode is derived automatically: Local when a parent node or data \
         directory is provided, Remote otherwise." ;
      s
  | 3 ->
      (* DAL Node *)
      let dal_nodes = dal_services s.service_states in
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
        | `None -> update_form_ref (fun f -> {f with dal = Dal_none})
        | `Dal n ->
            let svc = n.Data.Service_state.service in
            update_form_ref (fun f ->
                {f with dal = Dal_instance svc.Service.instance}) ;
            maybe_use_app_bin_dir svc.Service.app_bin_dir
        | `Custom ->
            let initial =
              match !form_ref.dal with
              | Dal_endpoint ep when ep <> "" -> ep
              | _ -> ""
            in
            prompt_validated_text_modal
              ~title:"DAL Node Endpoint (host:port)"
              ~initial
              ~validator:(fun text ->
                match parse_host_port (endpoint_host_port text) with
                | Some _ -> Ok ()
                | None ->
                    Error "Format must be host:port (e.g., 127.0.0.1:10732)")
              ~on_submit:(fun v ->
                update_form_ref (fun f -> {f with dal = Dal_endpoint v}))
              ()
      in
      open_choice_modal ~title:"DAL Node" ~items ~to_string ~on_select ;
      s
  | 4 ->
      (* Node Endpoint *)
      prompt_validated_text_modal
        ~title:"Node Endpoint (host:port)"
        ~initial:!form_ref.node_endpoint
        ~validator:(fun text ->
          match parse_host_port (endpoint_host_port text) with
          | Some _ -> Ok ()
          | None -> Error "Format must be host:port (e.g., 127.0.0.1:8732)")
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with node_endpoint = v}))
        () ;
      s
  | 5 ->
      (* Node Data Dir *)
      prompt_validated_text_modal
        ~title:"Node Data Directory"
        ~initial:!form_ref.node_data_dir
        ~validator:(fun _ -> Ok ())
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with node_data_dir = v}))
        () ;
      s
  | 6 ->
      (* Baker Base Dir *)
      prompt_text_modal
        ~title:"Baker Base Directory"
        ~initial:!form_ref.base_dir
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with base_dir = v}))
        () ;
      s
  | 7 ->
      (* Delegates *)
      let delegates = !form_ref.delegates in
      let removable = List.map (fun d -> `Remove d) delegates in
      let items =
        let base = if delegates = [] then removable else `Clear :: removable in
        `Add :: base
      in
      let to_string = function
        | `Add -> "Add delegate"
        | `Clear -> "Clear all"
        | `Remove d -> "Remove: " ^ d
      in
      let on_select = function
        | `Add ->
            prompt_validated_text_modal
              ~title:"Add Delegate"
              ~validator:(fun v ->
                if String.trim v = "" then Error "Delegate cannot be empty"
                else Ok ())
              ~on_submit:(fun v ->
                let v = String.trim v in
                update_form_ref (fun f ->
                    if List.mem v f.delegates then f
                    else {f with delegates = f.delegates @ [v]}))
              ()
        | `Clear -> update_form_ref (fun f -> {f with delegates = []})
        | `Remove d ->
            update_form_ref (fun f ->
                {f with delegates = List.filter (fun x -> x <> d) f.delegates})
      in
      open_choice_modal ~title:"Delegates" ~items ~to_string ~on_select ;
      s
  | 8 ->
      (* Liquidity Baking Vote *)
      let items = ["pass"; "on"; "off"] in
      open_choice_modal
        ~title:"Liquidity Baking Vote"
        ~items
        ~to_string:(fun x -> x)
        ~on_select:(fun v ->
          update_form_ref (fun f -> {f with liquidity_baking_vote = v})) ;
      s
  | 9 ->
      (* Service User *)
      prompt_text_modal
        ~title:"Service User"
        ~initial:!form_ref.service_user
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with service_user = v}))
        () ;
      s
  | 10 ->
      (* App Bin Dir *)
      prompt_text_modal
        ~title:"App Bin Directory"
        ~initial:!form_ref.app_bin_dir
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with app_bin_dir = v}))
        () ;
      s
  | 11 ->
      (* Logging *)
      let items = ["File"; "Journald"] in
      open_choice_modal
        ~title:"Logging"
        ~items
        ~to_string:(fun x -> x)
        ~on_select:(fun v ->
          let logging = if v = "File" then `File else `Journald in
          update_form_ref (fun f -> {f with logging})) ;
      s
  | 12 ->
      (* Enable on Boot *)
      update_form_ref (fun f -> {f with enable_on_boot = not f.enable_on_boot}) ;
      s
  | 13 ->
      (* Start Now *)
      update_form_ref (fun f -> {f with start_now = not f.start_now}) ;
      s
  | 14 ->
      (* Extra Args -> open flag explorer *)
      open_binary_help s
  | 15 -> (
      (* Confirm *)
      let f = !form_ref in
      let selected_node = find_node s.service_states f.parent_node in
      let instance = String.trim f.instance_name in
      let app_bin_ok = has_octez_baker_binary f.app_bin_dir in
      let node_dir = resolve_node_data_dir s in
      let node_mode = baker_node_mode s in
      let node_dir_ok =
        match node_mode with
        | `Local -> is_nonempty node_dir || Option.is_some selected_node
        | `Remote -> true
      in
      let node_endpoint = resolve_node_endpoint s in
      let dal_endpoint = resolve_dal_endpoint s in
      if instance = "" then (
        show_error ~title:"Error" "Instance name is required." ;
        s)
      else if instance_in_use ~states:s.service_states instance then (
        show_error ~title:"Error" "Instance name already exists." ;
        s)
      else if Option.is_some selected_node = false && not node_dir_ok then (
        show_error
          ~title:"Error"
          "Provide a node data directory when using a local external node." ;
        s)
      else if
        match f.dal with
        | Dal_none -> false
        | Dal_instance inst -> Option.is_none (find_dal s.service_states inst)
        | Dal_endpoint ep ->
            Option.is_none (parse_host_port (endpoint_host_port ep))
      then (
        show_error
          ~title:"Error"
          "Select a DAL node instance or enter a valid DAL endpoint." ;
        s)
      else if not (service_user_valid ~user:f.service_user) then (
        show_error
          ~title:"Error"
          "Service user does not exist and cannot be created (run as root or \
           choose an existing user)." ;
        s)
      else if not app_bin_ok then (
        show_error
          ~title:"Error"
          "octez-baker not found/executable in app bin dir." ;
        s)
      else
        let logging_mode =
          match f.logging with
          | `Journald -> Logging_mode.Journald
          | `File ->
              let dir =
                Common.default_log_dir ~role:"baker" ~instance:f.instance_name
              in
              let path = Filename.concat dir "baker.log" in
              Logging_mode.File {path; rotate = true}
        in
        let extra_args =
          if f.extra_args = "" then []
          else
            String.split_on_char ' ' f.extra_args
            |> List.filter (fun s -> s <> "")
        in
        let base_dir =
          let trimmed = String.trim f.base_dir in
          if trimmed = "" then Common.default_role_dir "baker" instance
          else trimmed
        in
        let req =
          {
            instance;
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
            delegates = f.delegates;
            dal_endpoint;
            liquidity_baking_vote =
              (if String.trim f.liquidity_baking_vote = "" then None
              else Some (String.trim f.liquidity_baking_vote));
            extra_args;
            service_user = f.service_user;
            app_bin_dir = f.app_bin_dir;
            logging_mode;
            auto_enable = f.enable_on_boot;
          }
        in
        let res =
          let* () =
            if Common.is_root () then
              System_user.ensure_service_account ~name:f.service_user
            else Ok ()
          in
          let* (module PM) = require_package_manager () in
          let* _ = PM.install_baker req in
          if f.start_now then
            match Miaou_interfaces.Service_lifecycle.get () with
            | Some sl ->
                Miaou_interfaces.Service_lifecycle.start
                  sl
                  ~role:"baker"
                  ~service:instance
                |> Result.map_error (fun e -> `Msg e)
            | None -> Error (`Msg "Service lifecycle capability not available")
          else Ok ()
        in
        match res with
        | Ok _ ->
            Context.mark_instances_dirty () ;
            {s with next_page = Some "instances"}
        | Error (`Msg e) ->
            show_error ~title:"Installation Failed" e ;
            s)
  | _ -> s

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  refresh s

let handle_key s key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    refresh s)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") ->
        push_help_hint () ;
        {s with next_page = Some "__BACK__"}
    | Some Keys.Up -> move s (-1)
    | Some Keys.Down -> move s 1
    | Some (Keys.Char "?") -> open_binary_help s |> refresh
    | Some Keys.Enter -> edit_field s |> refresh
    | _ -> s

let view s ~focus:_ ~size =
  apply_field_hint s ;
  let f = s.form in
  let selected_node = find_node s.service_states f.parent_node in
  let node_data_dir = resolve_node_data_dir s in
  let node_endpoint = resolve_node_endpoint s in
  let node_mode = baker_node_mode s in
  let dal_label, valid_dal =
    match f.dal with
    | Dal_none -> ("None", true)
    | Dal_endpoint ep ->
        let label = if ep = "" then "Custom" else ep in
        (label, Option.is_some (parse_host_port (endpoint_host_port ep)))
    | Dal_instance inst -> (
        match find_dal s.service_states inst with
        | Some svc ->
            let endpoint =
              dal_endpoint_of_service svc.Data.Service_state.service
            in
            ( Printf.sprintf
                "%s (%s)"
                svc.Data.Service_state.service.Service.instance
                endpoint,
              true )
        | None -> (Printf.sprintf "%s (missing)" inst, false))
  in
  let valid_instance =
    is_nonempty f.instance_name
    && not (instance_in_use ~states:s.service_states f.instance_name)
  in
  let valid_parent_node = f.parent_node = "" || Option.is_some selected_node in
  let valid_node_dir =
    match node_mode with
    | `Local -> Option.is_some selected_node || is_nonempty node_data_dir
    | `Remote -> true
  in
  let valid_node_endpoint =
    Option.is_some selected_node
    || Option.is_some (parse_host_port (endpoint_host_port f.node_endpoint))
  in
  let valid_base_dir = is_nonempty f.base_dir in
  let valid_delegates = List.for_all is_nonempty f.delegates in
  let valid_lb_vote =
    let vote = String.lowercase_ascii (String.trim f.liquidity_baking_vote) in
    vote = "on" || vote = "off" || vote = "pass"
  in
  let valid_service_user =
    is_nonempty f.service_user && service_user_valid ~user:f.service_user
  in
  let valid_app_bin_dir = has_octez_baker_binary f.app_bin_dir in
  let all_ok =
    valid_instance && valid_node_dir && valid_node_endpoint && valid_base_dir
    && valid_delegates && valid_lb_vote && valid_service_user
    && valid_app_bin_dir && valid_dal
  in
  let status ok = if ok then "✓" else "✗" in
  let items =
    [
      ("Instance Name", f.instance_name, valid_instance);
      ( "Parent Node",
        (if f.parent_node = "" then "External" else f.parent_node),
        valid_parent_node );
      ( "Node Mode",
        (match node_mode with `Local -> "Local" | `Remote -> "Remote"),
        true );
      ("DAL Node", dal_label, valid_dal);
      ("Node Endpoint", node_endpoint, valid_node_endpoint);
      ("Node Data Dir", node_data_dir, valid_node_dir);
      ("Baker Base Dir", f.base_dir, valid_base_dir);
      ("Delegates", delegate_label f.delegates, valid_delegates);
      ("Liquidity Baking Vote", f.liquidity_baking_vote, valid_lb_vote);
      ("Service User", f.service_user, valid_service_user);
      ("App Bin Dir", f.app_bin_dir, valid_app_bin_dir);
      ( "Logging",
        (match f.logging with `File -> "File" | `Journald -> "Journald"),
        true );
      ("Enable on Boot", string_of_bool f.enable_on_boot, true);
      ("Start Now", string_of_bool f.start_now, true);
      ( "Extra Args",
        (if f.extra_args = "" then "(none)" else f.extra_args),
        true );
      ("Confirm & Install", (if all_ok then "Ready" else "Incomplete"), all_ok);
    ]
  in
  let rows =
    List.map
      (fun (label, value, ok) ->
        let value = if ok then value else Widgets.fg 214 (Widgets.bold value) in
        (label, value, status ok))
      items
  in
  let columns =
    [
      {
        Miaou_widgets_display.Table_widget.Table.header = "Parameter";
        to_string = (fun (l, _, _) -> l);
      };
      {header = "Value"; to_string = (fun (_, v, _) -> v)};
      {header = "S"; to_string = (fun (_, _, s) -> s)};
    ]
  in
  let table =
    Table_widget.Table.create ~cols:size.LTerm_geom.cols ~columns ~rows ()
  in
  let table = Table_widget.Table.move_cursor table s.cursor in
  let status_banner =
    if all_ok then
      Widgets.bg 22 (Widgets.fg 15 " ✓ Form is valid - ready to install! ")
    else
      let msg = " ⚠ Form incomplete - fill required fields " in
      Widgets.bg 160 (Widgets.fg 15 (Widgets.bold msg))
  in
  let warning_banner =
    if not valid_app_bin_dir then
      Widgets.bg
        220
        (Widgets.fg 0 (Widgets.bold " ⚠ octez-baker missing/executable? "))
    else ""
  in
  let title_line = Widgets.title_highlight " Install Baker " in
  let header_line =
    pad_with_right size.LTerm_geom.cols status_banner warning_banner
  in
  let header = [title_line; header_line] in
  let footer =
    [Widgets.dim "↑/↓ navigate, Enter to edit, ? baker flags, Esc back"]
  in
  Miaou_widgets_layout.Vsection.render ~size ~header ~footer ~child:(fun _ ->
      Table_widget.Table.render table)

let next_page s = s.next_page

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let enter s = edit_field s

  let service_select _ _ = failwith "not used"

  let service_cycle s _ = refresh s

  let back _ = failwith "not used"

  let handled_keys () = Miaou.Core.Keys.[Up; Down; Enter]

  let keymap (_ : state) =
    [
      ("Up", (fun s -> move s (-1)), "up");
      ("Down", (fun s -> move s 1), "down");
      ("Enter", (fun s -> enter s), "open");
      ("?", open_binary_help, "baker flags");
    ]

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let next_page = next_page

  let has_modal = has_modal
end

module Page = Monitored_page.Make(Page_Impl)(struct
  let page_name = "install_baker"
end)

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
