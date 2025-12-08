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

type form_state = {
  instance_name : string;
  parent_node : string;
  delegates : string;
  service_user : string;
  app_bin_dir : string;
  logging : [`Journald | `File];
  enable_on_boot : bool;
  start_now : bool;
  extra_args : string;
}

type state = {form : form_state; cursor : int; next_page : string option}

type msg = unit

let default_form =
  {
    instance_name = "";
    parent_node = "";
    delegates = "";
    service_user = "octez";
    app_bin_dir = "/usr/bin";
    logging = `File;
    enable_on_boot = true;
    start_now = true;
    extra_args = "";
  }

let form_ref = ref default_form

let update_form_ref f = form_ref := f !form_ref

let init () = {form = !form_ref; cursor = 0; next_page = None}

let update s _ = s

let refresh s =
  let s = {s with form = !form_ref} in
  match Context.consume_navigation () with
  | Some p -> {s with next_page = Some p}
  | None -> {s with next_page = None}

let move s delta =
  let max_cursor = 9 in
  (* Number of fields + confirm *)
  let cursor = max 0 (min max_cursor (s.cursor + delta)) in
  {s with cursor}

let get_available_nodes () =
  let services = Data.load_service_states () in
  services
  |> List.filter (fun s -> s.Data.Service_state.service.Service.role = "node")
  |> List.map (fun s -> s.Data.Service_state.service.Service.instance)

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

let edit_field s =
  let open Modal_helpers in
  match s.cursor with
  | 0 ->
      (* Instance Name *)
      prompt_text_modal
        ~title:"Instance Name"
        ~initial:!form_ref.instance_name
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with instance_name = v}))
        () ;
      s
  | 1 ->
      (* Parent Node *)
      let nodes = get_available_nodes () in
      if nodes = [] then (
        show_error ~title:"Error" "No nodes available. Create a node first." ;
        s)
      else (
        open_choice_modal
          ~title:"Parent Node"
          ~items:nodes
          ~to_string:(fun x -> x)
          ~on_select:(fun v ->
            update_form_ref (fun f ->
                {
                  f with
                  parent_node = v;
                  instance_name =
                    (if f.instance_name = "" then "baker-" ^ v
                     else f.instance_name);
                })) ;
        s)
  | 2 ->
      (* Delegates *)
      prompt_text_modal
        ~title:"Delegates (comma separated)"
        ~initial:!form_ref.delegates
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with delegates = v}))
        () ;
      s
  | 3 ->
      (* Service User *)
      prompt_text_modal
        ~title:"Service User"
        ~initial:!form_ref.service_user
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with service_user = v}))
        () ;
      s
  | 4 ->
      (* App Bin Dir *)
      prompt_text_modal
        ~title:"App Bin Directory"
        ~initial:!form_ref.app_bin_dir
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with app_bin_dir = v}))
        () ;
      s
  | 5 ->
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
        ~title:"Extra Args"
        ~initial:!form_ref.extra_args
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with extra_args = v}))
        () ;
      s
  | 9 -> (
      (* Confirm *)
      let f = !form_ref in
      if f.instance_name = "" then (
        show_error ~title:"Error" "Instance name is required." ;
        s)
      else if f.parent_node = "" then (
        show_error ~title:"Error" "Parent node is required." ;
        s)
      else if f.delegates = "" then (
        show_error ~title:"Error" "At least one delegate is required." ;
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
        let delegates =
          String.split_on_char ',' f.delegates
          |> List.map String.trim
          |> List.filter (( <> ) "")
        in
        let extra_args =
          if f.extra_args = "" then []
          else
            String.split_on_char ' ' f.extra_args
            |> List.filter (fun s -> s <> "")
        in
        let req =
          {
            instance = f.instance_name;
            network = None;
            node_instance = Some f.parent_node;
            node_data_dir = None;
            node_endpoint = None;
            base_dir = None;
            delegates;
            extra_args;
            service_user = f.service_user;
            app_bin_dir = f.app_bin_dir;
            logging_mode;
            auto_enable = f.enable_on_boot;
          }
        in
        let res =
          let* (module PM) = require_package_manager () in
          let* _ = PM.install_baker req in
          if f.start_now then
            match Miaou_interfaces.Service_lifecycle.get () with
            | Some sl ->
                Miaou_interfaces.Service_lifecycle.start
                  sl
                  ~role:"baker"
                  ~service:f.instance_name
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
    | Some (Keys.Char "Esc") -> {s with next_page = Some "__BACK__"}
    | Some Keys.Up -> move s (-1)
    | Some Keys.Down -> move s 1
    | Some Keys.Enter -> edit_field s |> refresh
    | _ -> s

let view s ~focus:_ ~size =
  let f = s.form in
  let items =
    [
      ("Instance Name", f.instance_name);
      ("Parent Node", f.parent_node);
      ("Delegates", f.delegates);
      ("Service User", f.service_user);
      ("App Bin Dir", f.app_bin_dir);
      ( "Logging",
        match f.logging with `File -> "File" | `Journald -> "Journald" );
      ("Enable on Boot", string_of_bool f.enable_on_boot);
      ("Start Now", string_of_bool f.start_now);
      ("Extra Args", f.extra_args);
      ( "Confirm & Install",
        if f.instance_name <> "" && f.parent_node <> "" && f.delegates <> ""
        then "Ready"
        else "Incomplete" );
    ]
  in
  let rows = items in
  let columns =
    [
      {
        Miaou_widgets_display.Table_widget.Table.header = "Parameter";
        to_string = (fun (l, _) -> l);
      };
      {header = "Value"; to_string = (fun (_, v) -> v)};
    ]
  in
  let table =
    Table_widget.Table.create ~cols:size.LTerm_geom.cols ~columns ~rows ()
  in
  let table = Table_widget.Table.move_cursor table s.cursor in
  let header = [Widgets.title_highlight " Install Baker "] in
  let footer = [Widgets.dim "↑/↓ navigate, Enter to edit, Esc back"] in
  Miaou_widgets_layout.Vsection.render ~size ~header ~footer ~child:(fun _ ->
      Table_widget.Table.render table)

let next_page s = s.next_page

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page : Miaou.Core.Tui_page.PAGE_SIG = struct
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

  let keymap (_ : state) =
    [
      ("Up", (fun s -> move s (-1)), "up");
      ("Down", (fun s -> move s 1), "down");
      ("Enter", (fun s -> enter s), "open");
    ]

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let next_page = next_page

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
