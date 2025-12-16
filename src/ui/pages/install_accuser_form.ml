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

let name = "install_accuser_form"

type node_selection = Node_instance of string | Node_endpoint of string

type form_state = {
  instance_name : string;
  node : node_selection;
  base_dir : string;
  service_user : string;
  app_bin_dir : string;
  logging : [`Journald | `File];
  enable_on_boot : bool;
  start_now : bool;
  extra_args : string;
}

type state = {
  form : form_state; [@warning "-69"]
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

let default_form =
  {
    instance_name = "accuser";
    node = Node_endpoint "127.0.0.1:8732";
    base_dir = "";
    service_user = default_service_user ();
    app_bin_dir = "/usr/bin";
    logging = `File;
    enable_on_boot = true;
    start_now = true;
    extra_args = "";
  }

let form_ref = ref default_form

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
  match parse_host_port trimmed with
  | Some (h, p) -> (h, p)
  | None -> ("127.0.0.1", 8732)

let node_label ~states node_selection =
  match node_selection with
  | Node_instance inst -> (
      match List.find_opt (fun s -> s.Data.Service_state.service.Service.instance = inst) states with
      | Some s -> Printf.sprintf "Node: %s (%s)" inst s.service.network
      | None -> Printf.sprintf "Node: %s (not found)" inst)
  | Node_endpoint ep -> Printf.sprintf "Endpoint: %s" ep

let is_form_valid form ~states =
  let has_instance_name = String.trim form.instance_name <> "" in
  let has_valid_node =
    match form.node with
    | Node_instance inst ->
        List.exists
          (fun s ->
            s.Data.Service_state.service.Service.instance = inst
            && s.service.role = "node")
          states
    | Node_endpoint ep -> parse_host_port ep <> None
  in
  let has_base_dir = String.trim form.base_dir <> "" in
  has_instance_name && has_valid_node && has_base_dir

let validation_message form ~states =
  let issues = ref [] in
  if String.trim form.instance_name = "" then
    issues := "Instance name required" :: !issues ;
  (match form.node with
  | Node_instance inst ->
      if
        not
          (List.exists
             (fun s ->
               s.Data.Service_state.service.Service.instance = inst
               && s.service.role = "node")
             states)
      then issues := "Valid node instance required" :: !issues
  | Node_endpoint ep ->
      if parse_host_port ep = None then
        issues := "Valid endpoint (host:port) required" :: !issues) ;
  if String.trim form.base_dir = "" then
    issues := "Base directory required" :: !issues ;
  match !issues with
  | [] -> ""
  | lst -> String.concat ", " (List.rev lst)

let find_node states inst =
  List.find_opt
    (fun s ->
      s.Data.Service_state.service.Service.instance = inst
      && s.service.role = "node")
    states

let init () =
  let services = Data.load_service_states () in
  form_ref := default_form ;
  {form = !form_ref; cursor = 0; next_page = None; service_states = services}

let update s _ = s

let node_endpoint_of_service (svc : Service.t) =
  let host, port = endpoint_host_port svc.Service.rpc_addr in
  Printf.sprintf "http://%s:%d" host port

let refresh_state s =
  {s with service_states = Data.load_service_states ()}

let view s ~focus:_ ~size =
  let form = !form_ref in
  let has_instance_name = String.trim form.instance_name <> "" in
  let valid_node =
    match form.node with
    | Node_instance inst ->
        List.exists
          (fun st ->
            st.Data.Service_state.service.Service.instance = inst
            && st.service.role = "node")
          s.service_states
    | Node_endpoint ep -> parse_host_port ep <> None
  in
  let has_base_dir = String.trim form.base_dir <> "" in
  let all_ok = has_instance_name && valid_node && has_base_dir in

  let status ok = if ok then "✓" else "✗" in
  let items =
    [
      ("Instance Name", form.instance_name, has_instance_name);
      ("Node", node_label ~states:s.service_states form.node, valid_node);
      ("Base Dir", form.base_dir, has_base_dir);
      ("Service User", form.service_user, true);
      ("App Bin Dir", form.app_bin_dir, true);
      ( "Logging",
        (match form.logging with `Journald -> "Journald" | `File -> "File"),
        true );
      ("Enable on Boot", string_of_bool form.enable_on_boot, true);
      ("Start Now", string_of_bool form.start_now, true);
      ("Extra Args", (if form.extra_args = "" then "(none)" else form.extra_args), true);
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
      let msg = " ⚠ Form incomplete: " ^ validation_message form ~states:s.service_states ^ " " in
      Widgets.bg 160 (Widgets.fg 15 (Widgets.bold msg))
  in
  let title_line = Widgets.title_highlight " Install Accuser " in
  let header = [title_line; status_banner] in
  let footer =
    [Widgets.dim "↑/↓ navigate, Enter to edit, Esc back"]
  in
  Miaou_widgets_layout.Vsection.render ~size ~header ~footer ~child:(fun _ ->
      Table_widget.Table.render table)

let move s delta =
  let max_cursor = 9 in
  (* Number of fields + confirm *)
  let cursor = max 0 (min max_cursor (s.cursor + delta)) in
  {s with cursor}

let rec edit_field s =
  let open Modal_helpers in
  match s.cursor with
  | 0 ->
      (* Instance Name *)
      prompt_text_modal
        ~title:"Accuser Instance Name"
        ~initial:!form_ref.instance_name
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with instance_name = v}))
        () ;
      s
  | 1 ->
      (* Node selection *)
      let nodes =
        List.filter
          (fun st -> st.Data.Service_state.service.Service.role = "node")
          s.service_states
      in
      let node_items = List.map (fun st -> `Node st) nodes in
      let items = node_items @ [`External] in
      let to_string = function
        | `Node st ->
            Printf.sprintf "%s (%s)"
              st.Data.Service_state.service.Service.instance
              st.Data.Service_state.service.Service.network
        | `External -> "External endpoint..."
      in
      let on_select = function
        | `Node st ->
            update_form_ref (fun f ->
                {f with node = Node_instance st.Data.Service_state.service.Service.instance})
        | `External ->
            prompt_text_modal
              ~title:"Node Endpoint"
              ~initial:
                (match !form_ref.node with
                | Node_endpoint ep -> ep
                | Node_instance _ -> "127.0.0.1:8732")
              ~on_submit:(fun ep ->
                update_form_ref (fun f -> {f with node = Node_endpoint ep}))
              ()
      in
      open_choice_modal ~title:"Select Node" ~items ~to_string ~on_select ;
      s
  | 2 ->
      (* Base Dir *)
      Modal_helpers.select_client_base_dir_modal
        ~on_select:(fun path ->
          update_form_ref (fun f -> {f with base_dir = path}))
        () ;
      s
  | 3 ->
      (* Service User *)
      prompt_text_modal
        ~title:"Service User"
        ~initial:!form_ref.service_user
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with service_user = v}))
        () ;
      s
  | 4 ->
      (* App Bin Dir *)
      Modal_helpers.select_app_bin_dir_modal
        ~on_select:(fun path ->
          update_form_ref (fun f -> {f with app_bin_dir = path}))
        () ;
      s
  | 5 ->
      (* Logging *)
      open_choice_modal
        ~title:"Logging Mode"
        ~items:[`Journald; `File]
        ~to_string:(function `Journald -> "Journald" | `File -> "File")
        ~on_select:(fun mode ->
          update_form_ref (fun f -> {f with logging = mode})) ;
      s
  | 6 ->
      (* Enable on Boot *)
      update_form_ref (fun f -> {f with enable_on_boot = not f.enable_on_boot}) ;
      s
  | 7 ->
      (* Start Now *)
      update_form_ref (fun f -> {f with start_now = not f.start_now}) ;
      s
  | 8 ->
      (* Extra Args *)
      prompt_text_modal
        ~title:"Extra Arguments"
        ~initial:!form_ref.extra_args
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with extra_args = v}))
        () ;
      s
  | 9 ->
      (* Confirm & Install *)
      submit_form s
  | _ -> s

and submit_form s =
  let form = !form_ref in
  if not (is_form_valid form ~states:s.service_states) then (
    Modal_helpers.show_error
      ~title:"Form Incomplete"
      (validation_message form ~states:s.service_states) ;
    s)
  else
    (* Build installation request *)
    let network, endpoint =
      match form.node with
      | Node_instance inst -> (
          match find_node s.service_states inst with
          | Some node_state ->
              let network = node_state.service.network in
              let endpoint = node_endpoint_of_service node_state.service in
              (network, endpoint)
          | None ->
              Modal_helpers.show_error ~title:"Error" "Selected node not found" ;
              failwith "node not found")
      | Node_endpoint ep ->
          let host, port = endpoint_host_port ep in
          ("mainnet", Printf.sprintf "http://%s:%d" host port)
    in
    let extra_args_list =
      if String.trim form.extra_args = "" then []
      else String.split_on_char ' ' form.extra_args
    in
    let service_args =
      ["run"; "--endpoint"; endpoint; "--base-dir"; form.base_dir]
      @ extra_args_list
    in
    let logging_mode =
      match form.logging with
      | `Journald -> Logging_mode.Journald
      | `File ->
          let dir =
            Common.default_log_dir ~role:"accuser" ~instance:form.instance_name
          in
          let path = Filename.concat dir "accuser.log" in
          Logging_mode.File {path; rotate = true}
    in
    let req : daemon_request =
      {
        role = "accuser";
        instance = form.instance_name;
        network;
        history_mode = History_mode.default;
        data_dir = Common.default_role_dir "accuser" form.instance_name;
        rpc_addr = endpoint;
        net_addr = "";
        service_user = form.service_user;
        app_bin_dir = form.app_bin_dir;
        logging_mode;
        service_args;
        extra_env = [];
        extra_paths = [];
        auto_enable = form.enable_on_boot;
      }
    in
    (* Call installer *)
    match Installer.install_daemon req with
    | Ok service ->
        (* Register base_dir in directory registry *)
        let _ =
          Directory_registry.add
            ~path:form.base_dir
            ~dir_type:Client_base_dir
            ~linked_services:[service.Service.instance]
        in
        (* Start if requested *)
        if form.start_now then (
          match Systemd.start ~role:"accuser" ~instance:form.instance_name with
          | Ok () -> ()
          | Error _ -> ()) ;
        Modal_helpers.show_success
          ~title:"Success"
          (Printf.sprintf "Accuser %s installed successfully" form.instance_name) ;
        Context.mark_instances_dirty () ;
        {s with next_page = Some "instances"}
    | Error (`Msg msg) ->
        Modal_helpers.show_error ~title:"Installation Failed" msg ;
        s

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG with type state = state and type msg = msg = struct
  type nonrec state = state

  type nonrec msg = msg

  let init = init

  let update = update

  let view = view

  let move s delta = move s delta

  let enter s = edit_field s

  let refresh s = refresh_state s

  let service_select s _ = s

  let service_cycle s _ = s

  let back s = {s with next_page = Some "instances"}

  let keymap _ =
    [
      ("Enter", (fun s -> s), "Edit field / Submit");
      ("Up", (fun s -> s), "Up");
      ("Down", (fun s -> s), "Down");
      ("Esc", (fun s -> s), "Back to instances");
    ]

  let handled_keys () = Keys.[Enter; Up; Down; Char "Esc"; Char "Escape"]

  let handle_modal_key s key ~size:_ =
    Miaou.Core.Modal_manager.handle_key key ;
    refresh_state s

  let handle_key s key ~size:_ =
    if Miaou.Core.Modal_manager.has_active () then (
      Miaou.Core.Modal_manager.handle_key key ;
      refresh_state s)
    else
      match key with
      | "Up" -> move s (-1)
      | "Down" -> move s 1
      | "Enter" -> edit_field s
      | "Esc" | "Escape" -> {s with next_page = Some "instances"}
      | _ -> s

  let next_page s = s.next_page

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()
end

module Page =
  Monitored_page.Make
    (Page_Impl)
    (struct
      let page_name = "install_accuser_form"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
