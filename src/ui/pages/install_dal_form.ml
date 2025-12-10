(*
*****************************************************************************
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
*****************************************************************************
*)

module Widgets = Miaou_widgets_display.Widgets
module Table_widget = Miaou_widgets_display.Table_widget
module Keys = Miaou.Core.Keys
open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_dal_form"

type endpoint_selection = Manual of string | Node_instance of string

type profile = No_profile | Bootstrap

type form_state = {
  instance_name : string;
  network : string;
  endpoint : endpoint_selection;
  data_dir : string;
  rpc_addr : string;
  net_addr : string;
  public_addr : string;
  peers : string;
  metrics_addr : string;
  history_mode : string;
  profile : profile;
  slots_backup_uri : string;
  trust_slots_backup : bool;
  service_user : string;
  app_bin_dir : string;
  logging : [`Journald | `File];
  enable_on_boot : bool;
  extra_args : string;
}

type state = {
  form : form_state;
  cursor : int;
  next_page : string option;
  service_states : Data.Service_state.t list;
}

type msg = unit

let normalize s = String.lowercase_ascii (String.trim s)

let is_nonempty s = String.trim s <> ""

let default_service_user () =
  if Common.is_root () then "octez"
  else
    match Unix.getpwuid (Unix.geteuid ()) with
    | pw when String.trim pw.Unix.pw_name <> "" -> pw.Unix.pw_name
    | _ -> "octez"

let default_form =
  {
    instance_name = "dal";
    network = "mainnet";
    endpoint = Manual "http://127.0.0.1:8732";
    data_dir = "";
    rpc_addr = "127.0.0.1:10732";
    net_addr = "";
    public_addr = "";
    peers = "";
    metrics_addr = "";
    history_mode = "";
    profile = No_profile;
    slots_backup_uri = "";
    trust_slots_backup = false;
    service_user = default_service_user ();
    app_bin_dir = "/usr/bin";
    logging = `File;
    enable_on_boot = true;
    extra_args = "";
  }

let form_ref = ref default_form

let update_form_ref f = form_ref := f !form_ref

let push_help_hint ?short ?long () =
  Miaou.Core.Help_hint.clear () ;
  match (short, long) with
  | None, None -> ()
  | _ -> Miaou.Core.Help_hint.push ?short ?long ()

let parse_host_port (s : string) : (string * int) option =
  match String.split_on_char ':' s with
  | [host; port] -> (
      try
        let p = int_of_string (String.trim port) in
        if p > 0 && p < 65536 && String.trim host <> "" then Some (host, p)
        else None
      with _ -> None)
  | _ -> None

let endpoint_with_scheme rpc_addr =
  let trimmed = String.trim rpc_addr in
  if trimmed = "" then "http://127.0.0.1:8732"
  else if
    String.starts_with ~prefix:"http://" (String.lowercase_ascii trimmed)
    || String.starts_with ~prefix:"https://" (String.lowercase_ascii trimmed)
  then trimmed
  else "http://" ^ trimmed

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

let ensure_defaults () =
  let current = !form_ref in
  if String.trim current.service_user = "" then
    update_form_ref (fun f -> {f with service_user = default_service_user ()}) ;
  if String.trim current.data_dir = "" then
    update_form_ref (fun f ->
        {
          f with
          data_dir = Common.default_role_dir "dal-node" current.instance_name;
        })

let move s delta =
  let max_cursor = 18 in
  let cursor = max 0 (min max_cursor (s.cursor + delta)) in
  {s with cursor}

let node_services states =
  states
  |> List.filter (fun (s : Data.Service_state.t) ->
      String.equal (normalize s.service.Service.role) "node")

let find_node states inst =
  node_services states
  |> List.find_opt (fun (s : Data.Service_state.t) ->
      String.equal (normalize s.service.Service.instance) (normalize inst))

let instance_in_use ~states name =
  let target = normalize name in
  target <> ""
  && List.exists
       (fun (s : Data.Service_state.t) ->
         String.equal target (normalize s.service.Service.instance))
       states

let data_dir_in_use ~states dir =
  let target = normalize dir in
  target <> ""
  && List.exists
       (fun (s : Data.Service_state.t) ->
         String.equal target (normalize s.service.Service.data_dir))
       states

let append_extra_args tokens =
  if tokens = [] then ()
  else
    let current = !form_ref in
    let existing =
      if String.trim current.extra_args = "" then []
      else
        String.split_on_char ' ' current.extra_args
        |> List.filter (fun s -> String.trim s <> "")
    in
    let merged = existing @ tokens in
    update_form_ref (fun f -> {f with extra_args = String.concat " " merged})

let open_binary_help s =
  let app_bin_dir = String.trim s.form.app_bin_dir in
  Binary_help_explorer.open_dal_run_help ~app_bin_dir ~on_apply:(fun tokens ->
      let arg_str = String.concat " " tokens in
      update_form_ref (fun f -> {f with extra_args = arg_str})) ;
  s

let init () =
  ensure_defaults () ;
  let service_states = Data.load_service_states () in
  {form = !form_ref; cursor = 0; next_page = None; service_states}

let update s _ = s

let refresh s =
  ensure_defaults () ;
  let s = {s with form = !form_ref} in
  match Context.consume_navigation () with
  | Some p -> {s with next_page = Some p}
  | None -> {s with next_page = None}

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

let prompt_endpoint_choice s =
  let nodes = node_services s.service_states in
  let items =
    (`Manual, "Manual URI", endpoint_with_scheme "http://127.0.0.1:8732")
    :: List.map
         (fun st ->
           ( `Node st.Data.Service_state.service.Service.instance,
             st.Data.Service_state.service.Service.instance,
             endpoint_with_scheme st.Data.Service_state.service.Service.rpc_addr
           ))
         nodes
  in
  Modal_helpers.open_choice_modal
    ~title:"Endpoint"
    ~items
    ~to_string:(fun (_, label, value) -> Printf.sprintf "%s (%s)" label value)
    ~on_select:(fun (choice, _, value) ->
      match choice with
      | `Manual ->
          Modal_helpers.prompt_validated_text_modal
            ~title:"Endpoint"
            ~initial:value
            ~validator:(fun v ->
              if Option.is_some (parse_host_port (endpoint_host_port v)) then
                Ok ()
              else Error "Endpoint must be host:port (with optional http[s]://)")
            ~on_submit:(fun v ->
              update_form_ref (fun f -> {f with endpoint = Manual v}))
            ()
      | `Node inst ->
          update_form_ref (fun f -> {f with endpoint = Node_instance inst}))

let edit_field s =
  let open Modal_helpers in
  match s.cursor with
  | 0 ->
      prompt_validated_text_modal
        ~title:"Instance Name"
        ~initial:!form_ref.instance_name
        ~validator:(fun v ->
          let trimmed = String.trim v in
          if trimmed = "" then Error "Instance name cannot be empty"
          else if instance_in_use ~states:s.service_states trimmed then
            Error "Instance already exists"
          else Ok ())
        ~on_submit:(fun v ->
          update_form_ref (fun f ->
              {
                f with
                instance_name = v;
                data_dir =
                  (if f.data_dir = "" then Common.default_role_dir "dal-node" v
                   else f.data_dir);
              }))
        () ;
      s
  | 1 -> (
      match get_network_infos () with
      | Ok nets ->
          let sorted =
            nets
            |> List.sort
                 (fun (a : Teztnets.network_info) (b : Teztnets.network_info) ->
                   String.compare
                     (normalize (Option.value ~default:a.alias a.human_name))
                     (normalize (Option.value ~default:b.alias b.human_name)))
          in
          open_choice_modal
            ~title:"Network"
            ~items:sorted
            ~to_string:(fun info ->
              let label = Option.value ~default:info.alias info.human_name in
              if normalize info.network_url = normalize info.alias then label
              else Printf.sprintf "%s · %s" label info.network_url)
            ~on_select:(fun info ->
              update_form_ref (fun f -> {f with network = info.network_url})) ;
          s
      | Error msg ->
          show_error ~title:"Network" msg ;
          s)
  | 2 ->
      prompt_endpoint_choice s ;
      s
  | 3 ->
      prompt_validated_text_modal
        ~title:"Data Directory"
        ~initial:!form_ref.data_dir
        ~validator:(fun v ->
          let trimmed = String.trim v in
          if trimmed = "" then Error "Data directory cannot be empty"
          else if data_dir_in_use ~states:s.service_states trimmed then
            Error "This data directory is already used"
          else Ok ())
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with data_dir = v}))
        () ;
      s
  | 4 ->
      prompt_validated_text_modal
        ~title:"RPC Address (host:port)"
        ~initial:!form_ref.rpc_addr
        ~validator:(fun v ->
          if Option.is_some (parse_host_port v) then Ok ()
          else Error "RPC address must be host:port")
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with rpc_addr = v}))
        () ;
      s
  | 5 ->
      prompt_validated_text_modal
        ~title:"P2P Bind (host:port)"
        ~initial:!form_ref.net_addr
        ~validator:(fun v ->
          if String.trim v = "" then Ok ()
          else if Option.is_some (parse_host_port v) then Ok ()
          else Error "P2P address must be host:port (or leave empty)")
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with net_addr = v}))
        () ;
      s
  | 6 ->
      prompt_text_modal
        ~title:"Peers"
        ~initial:!form_ref.peers
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with peers = v}))
        () ;
      s
  | 7 ->
      prompt_text_modal
        ~title:"Public Address"
        ~initial:!form_ref.public_addr
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with public_addr = v}))
        () ;
      s
  | 8 ->
      prompt_text_modal
        ~title:"Metrics Address"
        ~initial:!form_ref.metrics_addr
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with metrics_addr = v}))
        () ;
      s
  | 9 ->
      prompt_text_modal
        ~title:"History Mode"
        ~initial:!form_ref.history_mode
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with history_mode = v}))
        () ;
      s
  | 10 ->
      open_choice_modal
        ~title:"Profile"
        ~items:[No_profile; Bootstrap]
        ~to_string:(function No_profile -> "None" | Bootstrap -> "Bootstrap")
        ~on_select:(fun p -> update_form_ref (fun f -> {f with profile = p})) ;
      s
  | 11 ->
      prompt_text_modal
        ~title:"Slots Backup URI"
        ~initial:!form_ref.slots_backup_uri
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with slots_backup_uri = v}))
        () ;
      s
  | 12 ->
      open_choice_modal
        ~title:"Trust Backup URIs"
        ~items:[true; false]
        ~to_string:(fun b -> if b then "Enable" else "Disable")
        ~on_select:(fun v ->
          update_form_ref (fun f -> {f with trust_slots_backup = v})) ;
      s
  | 13 ->
      prompt_validated_text_modal
        ~title:"Service User"
        ~initial:!form_ref.service_user
        ~validator:(fun v ->
          if v = "" then Error "Service user cannot be empty"
          else if service_user_valid ~user:v then Ok ()
          else Error "Service user is not allowed")
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with service_user = v}))
        () ;
      s
  | 14 ->
      prompt_validated_text_modal
        ~title:"App Bin Directory"
        ~initial:!form_ref.app_bin_dir
        ~validator:(fun v ->
          if has_octez_baker_binary v then Ok ()
          else Error "octez-baker not found/executable in this directory")
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with app_bin_dir = v}))
        () ;
      s
  | 15 ->
      open_choice_modal
        ~title:"Logging"
        ~items:[`File; `Journald]
        ~to_string:(function `File -> "File" | `Journald -> "Journald")
        ~on_select:(fun v -> update_form_ref (fun f -> {f with logging = v})) ;
      s
  | 16 ->
      open_choice_modal
        ~title:"Enable on Boot"
        ~items:[true; false]
        ~to_string:(fun b -> if b then "Yes" else "No")
        ~on_select:(fun v ->
          update_form_ref (fun f -> {f with enable_on_boot = v})) ;
      s
  | 17 ->
      prompt_text_modal
        ~title:"Extra Args"
        ~initial:!form_ref.extra_args
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with extra_args = v}))
        () ;
      s
  | _ -> s

let logging_mode = function
  | `Journald -> Logging_mode.Journald
  | `File ->
      let dir =
        Common.default_log_dir
          ~role:"dal-node"
          ~instance:!form_ref.instance_name
      in
      let path = Filename.concat dir "dal-node.log" in
      Logging_mode.File {path; rotate = true}

let endpoint_uri s =
  match s.form.endpoint with
  | Manual uri -> endpoint_with_scheme uri
  | Node_instance inst -> (
      match find_node s.service_states inst with
      | Some svc ->
          endpoint_with_scheme svc.Data.Service_state.service.Service.rpc_addr
      | None -> "http://127.0.0.1:8732")

let build_service_args s =
  let f = s.form in
  let base =
    [
      "run";
      "dal";
      "--data-dir";
      f.data_dir;
      "--rpc-addr";
      f.rpc_addr;
      "--endpoint";
      endpoint_uri s;
    ]
  in
  let optional =
    let acc = [] in
    let acc =
      if String.trim f.net_addr <> "" then acc @ ["--net-addr"; f.net_addr]
      else acc
    in
    let acc =
      if String.trim f.public_addr <> "" then
        acc @ ["--public-addr"; f.public_addr]
      else acc
    in
    let acc =
      if String.trim f.peers <> "" then acc @ ["--peers"; f.peers] else acc
    in
    let acc =
      if String.trim f.metrics_addr <> "" then
        acc @ ["--metrics-addr"; f.metrics_addr]
      else acc
    in
    let acc =
      if String.trim f.history_mode <> "" then
        acc @ ["--history-mode"; f.history_mode]
      else acc
    in
    let acc =
      match f.profile with
      | Bootstrap -> acc @ ["--bootstrap-profile"]
      | No_profile -> acc
    in
    let acc =
      if String.trim f.slots_backup_uri <> "" then
        acc @ ["--slots-backup-uri"; f.slots_backup_uri]
      else acc
    in
    let acc =
      if f.trust_slots_backup then acc @ ["--trust-slots-backup-uris"] else acc
    in
    acc
  in
  let extra =
    if f.extra_args = "" then []
    else
      String.split_on_char ' ' f.extra_args
      |> List.filter (fun s -> String.trim s <> "")
  in
  base @ optional @ extra

let install s =
  let f = s.form in
  let valid_instance =
    is_nonempty f.instance_name
    && not (instance_in_use ~states:s.service_states f.instance_name)
  in
  let valid_data_dir =
    is_nonempty f.data_dir
    && not (data_dir_in_use ~states:s.service_states f.data_dir)
  in
  let valid_rpc = Option.is_some (parse_host_port f.rpc_addr) in
  let valid_net =
    f.net_addr = "" || Option.is_some (parse_host_port f.net_addr)
  in
  let valid_endpoint =
    match f.endpoint with
    | Manual uri ->
        let with_scheme = endpoint_with_scheme uri in
        Option.is_some (parse_host_port (endpoint_host_port with_scheme))
    | Node_instance inst -> Option.is_some (find_node s.service_states inst)
  in
  let valid_user =
    is_nonempty f.service_user && service_user_valid ~user:f.service_user
  in
  let valid_bin = has_octez_baker_binary f.app_bin_dir in
  if
    not
      (valid_instance && valid_data_dir && valid_rpc && valid_net
     && valid_endpoint && valid_user && valid_bin)
  then
    Modal_helpers.show_error
      ~title:"Installation"
      "Please fix invalid fields before installing"
  else
    let req : daemon_request =
      {
        role = "dal-node";
        instance = f.instance_name;
        network = f.network;
        history_mode = History_mode.default;
        data_dir = f.data_dir;
        rpc_addr = f.rpc_addr;
        net_addr = f.net_addr;
        service_user = f.service_user;
        app_bin_dir = f.app_bin_dir;
        logging_mode = logging_mode f.logging;
        service_args = build_service_args s;
        extra_env = [];
        extra_paths = [f.data_dir];
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
      PM.install_daemon req
    in
    match res with
    | Ok _ ->
        Context.mark_instances_dirty () ;
        Context.navigate "instances"
    | Error (`Msg e) -> Modal_helpers.show_error ~title:"Installation Failed" e

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  refresh s

let handle_key s key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    refresh s)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") -> {s with next_page = Some "__BACK__"}
    | Some Keys.Up -> move s (-1)
    | Some Keys.Down -> move s 1
    | Some (Keys.Char "?") -> open_binary_help s |> refresh
    | Some Keys.Enter ->
        if s.cursor >= 18 then (
          install s ;
          refresh s)
        else edit_field s |> refresh
    | Some (Keys.Char "m") ->
        Modal_helpers.show_menu_modal () ;
        s
    | _ -> s

let view s ~focus:_ ~size =
  push_help_hint ~short:"Enter to edit · ? for flags" () ;
  let f = s.form in
  let endpoint_label =
    match f.endpoint with
    | Manual uri -> endpoint_with_scheme uri
    | Node_instance inst -> (
        match find_node s.service_states inst with
        | Some svc ->
            Printf.sprintf
              "%s (%s)"
              svc.Data.Service_state.service.Service.instance
              (endpoint_with_scheme
                 svc.Data.Service_state.service.Service.rpc_addr)
        | None -> Printf.sprintf "%s (missing)" inst)
  in
  let valid_instance =
    is_nonempty f.instance_name
    && not (instance_in_use ~states:s.service_states f.instance_name)
  in
  let valid_data_dir =
    is_nonempty f.data_dir
    && not (data_dir_in_use ~states:s.service_states f.data_dir)
  in
  let valid_rpc = Option.is_some (parse_host_port f.rpc_addr) in
  let valid_net =
    f.net_addr = "" || Option.is_some (parse_host_port f.net_addr)
  in
  let valid_endpoint =
    match f.endpoint with
    | Manual uri ->
        Option.is_some
          (parse_host_port (endpoint_host_port (endpoint_with_scheme uri)))
    | Node_instance inst -> Option.is_some (find_node s.service_states inst)
  in
  let valid_user =
    is_nonempty f.service_user && service_user_valid ~user:f.service_user
  in
  let valid_bin = has_octez_baker_binary f.app_bin_dir in
  let status ok = if ok then "OK" else "ERR" in
  let items =
    [
      ("Instance", f.instance_name, valid_instance);
      ("Network", f.network, true);
      ("Endpoint", endpoint_label, valid_endpoint);
      ("Data Dir", f.data_dir, valid_data_dir);
      ("RPC Addr", f.rpc_addr, valid_rpc);
      ( "P2P Bind",
        (if f.net_addr = "" then "(default)" else f.net_addr),
        valid_net );
      ("Peers", (if f.peers = "" then "(none)" else f.peers), true);
      ( "Public Addr",
        (if f.public_addr = "" then "(default)" else f.public_addr),
        true );
      ( "Metrics Addr",
        (if f.metrics_addr = "" then "(default)" else f.metrics_addr),
        true );
      ( "History Mode",
        (if f.history_mode = "" then "(default)" else f.history_mode),
        true );
      ( "Profile",
        (match f.profile with No_profile -> "None" | Bootstrap -> "Bootstrap"),
        true );
      ( "Slots Backup",
        (if f.slots_backup_uri = "" then "(none)" else f.slots_backup_uri),
        true );
      ("Trust Backup", (if f.trust_slots_backup then "true" else "false"), true);
      ("Service User", f.service_user, valid_user);
      ("App Bin Dir", f.app_bin_dir, valid_bin);
      ( "Logging",
        (match f.logging with `File -> "File" | `Journald -> "Journald"),
        true );
      ("Enable on Boot", string_of_bool f.enable_on_boot, true);
      ( "Extra Args",
        (if f.extra_args = "" then "(none)" else f.extra_args),
        true );
      ( "Confirm & Install",
        (if
           valid_instance && valid_data_dir && valid_rpc && valid_net
           && valid_endpoint && valid_user && valid_bin
         then "Ready"
         else "Incomplete"),
        valid_instance && valid_data_dir && valid_rpc && valid_net
        && valid_endpoint && valid_user && valid_bin );
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
        Miaou_widgets_display.Table_widget.Table.header = "Field";
        to_string = (fun (l, _, _) -> l);
      };
      {header = "Value"; to_string = (fun (_, v, _) -> v)};
      {header = ""; to_string = (fun (_, _, s) -> s)};
    ]
  in
  let table =
    Table_widget.Table.create ~cols:size.LTerm_geom.cols ~columns ~rows ()
  in
  let table = Table_widget.Table.move_cursor table s.cursor in
  let header = [Widgets.title_highlight " Install DAL Node "] in
  let footer = [Widgets.dim "↑/↓ navigate, Enter to edit, ? flags, Esc back"] in
  Miaou_widgets_layout.Vsection.render ~size ~header ~footer ~child:(fun _ ->
      Table_widget.Table.render table)

let next_page s = s.next_page

module Page :
  Miaou.Core.Tui_page.PAGE_SIG with type state = state and type msg = msg =
struct
  type nonrec state = state

  type nonrec msg = msg

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let enter s =
    if s.cursor >= 18 then (
      install s ;
      refresh s)
    else edit_field s |> refresh

  let service_select s _ = s

  let service_cycle s _ = s

  let back s = {s with next_page = Some "__BACK__"}

  let keymap _ = [("Esc", back, "Back"); ("?", open_binary_help, "Flags")]

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let next_page = next_page

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
