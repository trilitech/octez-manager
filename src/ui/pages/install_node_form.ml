module Widgets = Miaou_widgets_display.Widgets
module Table_widget = Miaou_widgets_display.Table_widget
module Keys = Miaou.Core.Keys
open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_node_form"

type form_state = {
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
  snapshot : [`None | `Url of string];
  extra_args : string;
}

type state = {form : form_state; cursor : int; next_page : string option}

type msg = unit

let default_form =
  {
    instance_name = "";
    network = "mainnet";
    history_mode = "rolling";
    data_dir = "";
    app_bin_dir = "/usr/bin";
    rpc_addr = "127.0.0.1:8732";
    p2p_addr = "0.0.0.0:9732";
    service_user = "octez";
    logging = `File;
    enable_on_boot = true;
    start_now = true;
    snapshot = `None;
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
  let max_cursor = 13 in
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

let edit_field s =
  let open Modal_helpers in
  match s.cursor with
  | 0 ->
      (* Instance Name *)
      prompt_text_modal
        ~title:"Instance Name"
        ~initial:!form_ref.instance_name
        ~on_submit:(fun v ->
          update_form_ref (fun f ->
              {
                f with
                instance_name = v;
                data_dir =
                  (if f.data_dir = "" then Common.default_data_dir v
                   else f.data_dir);
              }))
        () ;
      s
  | 1 ->
      (* Network *)
      let networks = ["mainnet"; "ghostnet"; "weeklynet"] in
      (* TODO: fetch from Teztnets *)
      open_choice_modal
        ~title:"Network"
        ~items:networks
        ~to_string:(fun x -> x)
        ~on_select:(fun v -> update_form_ref (fun f -> {f with network = v})) ;
      s
  | 2 ->
      (* History Mode *)
      let modes = ["rolling"; "full"; "archive"] in
      open_choice_modal
        ~title:"History Mode"
        ~items:modes
        ~to_string:(fun x -> x)
        ~on_select:(fun v ->
          update_form_ref (fun f -> {f with history_mode = v})) ;
      s
  | 3 ->
      (* Data Dir *)
      prompt_text_modal
        ~title:"Data Directory"
        ~initial:!form_ref.data_dir
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with data_dir = v}))
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
      (* RPC Address *)
      prompt_text_modal
        ~title:"RPC Address"
        ~initial:!form_ref.rpc_addr
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with rpc_addr = v}))
        () ;
      s
  | 6 ->
      (* P2P Address *)
      prompt_text_modal
        ~title:"P2P Address"
        ~initial:!form_ref.p2p_addr
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with p2p_addr = v}))
        () ;
      s
  | 7 ->
      (* Service User *)
      prompt_text_modal
        ~title:"Service User"
        ~initial:!form_ref.service_user
        ~on_submit:(fun v ->
          update_form_ref (fun f -> {f with service_user = v}))
        () ;
      s
  | 8 ->
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
  | 9 ->
      (* Enable on Boot *)
      update_form_ref (fun f -> {f with enable_on_boot = not f.enable_on_boot}) ;
      s
  | 10 ->
      (* Start Now *)
      update_form_ref (fun f -> {f with start_now = not f.start_now}) ;
      s
  | 11 ->
      (* Snapshot *)
      let items = ["None"; "Url"] in
      open_choice_modal
        ~title:"Snapshot"
        ~items
        ~to_string:(fun x -> x)
        ~on_select:(fun v ->
          if v = "None" then
            update_form_ref (fun f -> {f with snapshot = `None})
          else
            prompt_text_modal
              ~title:"Snapshot URL"
              ~initial:""
              ~on_submit:(fun url ->
                update_form_ref (fun f -> {f with snapshot = `Url url}))
              ()) ;
      s
  | 12 ->
      (* Extra Args *)
      prompt_text_modal
        ~title:"Extra Args"
        ~initial:!form_ref.extra_args
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with extra_args = v}))
        () ;
      s
  | 13 -> (
      (* Confirm *)
      (* Trigger install *)
      let f = !form_ref in
      if f.instance_name = "" then (
        show_error ~title:"Error" "Instance name is required." ;
        s)
      else
        let history_mode =
          match History_mode.of_string f.history_mode with
          | Ok m -> m
          | Error _ -> History_mode.Rolling (* Default fallback *)
        in
        let logging_mode =
          match f.logging with
          | `Journald -> Logging_mode.Journald
          | `File ->
              let dir =
                Common.default_log_dir ~role:"node" ~instance:f.instance_name
              in
              let path = Filename.concat dir "node.log" in
              Logging_mode.File {path; rotate = true}
        in
        let bootstrap =
          match f.snapshot with
          | `None -> Genesis
          | `Url u -> Snapshot {src = Some u; kind = None}
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
            network = f.network;
            history_mode;
            data_dir = (if f.data_dir = "" then None else Some f.data_dir);
            rpc_addr = f.rpc_addr;
            net_addr = f.p2p_addr;
            service_user = f.service_user;
            app_bin_dir = f.app_bin_dir;
            logging_mode;
            extra_args;
            auto_enable = f.enable_on_boot;
            bootstrap;
          }
        in
        let res =
          let* (module I) = require_package_manager () in
          I.install_node req
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
  let snapshot_str = match f.snapshot with `None -> "None" | `Url u -> u in
  let items =
    [
      ("Instance Name", f.instance_name);
      ("Network", f.network);
      ("History Mode", f.history_mode);
      ("Data Dir", f.data_dir);
      ("App Bin Dir", f.app_bin_dir);
      ("RPC Address", f.rpc_addr);
      ("P2P Address", f.p2p_addr);
      ("Service User", f.service_user);
      ( "Logging",
        match f.logging with `File -> "File" | `Journald -> "Journald" );
      ("Enable on Boot", string_of_bool f.enable_on_boot);
      ("Start Now", string_of_bool f.start_now);
      ("Snapshot", snapshot_str);
      ("Extra Args", f.extra_args);
      ( "Confirm & Install",
        if f.instance_name <> "" then "Ready" else "Incomplete" );
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
  let header = [Widgets.title_highlight " Install Node "] in
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

  let enter _ = failwith "not used"

  let service_select _ _ = failwith "not used"

  let service_cycle s _ = refresh s

  let back _ = failwith "not used"

  let keymap _ = []

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
