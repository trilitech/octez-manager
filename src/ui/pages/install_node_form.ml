module Widgets = Miaou_widgets_display.Widgets
module Table_widget = Miaou_widgets_display.Table_widget
module Keys = Miaou.Core.Keys
open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_node_form"

type tzinit_snapshot = {
  network_slug : string;
  kind_slug : string;
  label : string;
}

type snapshot_selection = [`None | `Url of string | `Tzinit of tzinit_snapshot]

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
  snapshot : snapshot_selection;
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

let network_cache : Teztnets.network_info list ref = ref []

let snapshot_cache : (string, Snapshots.entry list) Hashtbl.t = Hashtbl.create 7

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
    network_cache := infos ;
    Ok infos

let normalize_string s = String.lowercase_ascii (String.trim s)

let network_display_name value =
  let normalized_value = normalize_string value in
  match
    List.find_opt
      (fun (info : Teztnets.network_info) ->
        normalize_string info.network_url = normalized_value
        || normalize_string info.alias = normalized_value)
      !network_cache
  with
  | Some info -> Option.value ~default:info.alias info.human_name
  | None -> value

let format_network_choice (info : Teztnets.network_info) =
  let label = Option.value ~default:info.alias info.human_name in
  if normalize_string info.network_url = normalize_string info.alias then label
  else Printf.sprintf "%s · %s" label info.network_url

let fetch_snapshot_list slug =
  let fallback () = of_rresult (Snapshots.list ~network_slug:slug) in
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Snapshot_provider_capability.key
  with
  | Some cap -> (
      let module P = (val cap : Manager_interfaces.Snapshot_provider) in
      match of_rresult (P.list ~network_slug:slug) with
      | Ok entries -> Ok entries
      | Error _ -> fallback ())
  | None -> fallback ()

let get_snapshot_entries network =
  match Snapshots.slug_of_network network with
  | None ->
      let trimmed = String.trim network in
      if trimmed = "" then Error "Select a network before choosing a snapshot."
      else
        Error
          (Printf.sprintf "Unable to derive a tzinit slug from '%s'." trimmed)
  | Some slug -> (
      match Hashtbl.find_opt snapshot_cache slug with
      | Some entries -> Ok (slug, entries)
      | None -> (
          match fetch_snapshot_list slug with
          | Ok entries ->
              Hashtbl.replace snapshot_cache slug entries ;
              Ok (slug, entries)
          | Error msg -> Error msg))

type snapshot_choice =
  | Snapshot_none
  | Snapshot_custom
  | Snapshot_entry of Snapshots.entry

let snapshot_choice_label = function
  | Snapshot_none -> "None (start from genesis)"
  | Snapshot_custom -> "Custom snapshot URL"
  | Snapshot_entry entry ->
      let hm =
        match entry.history_mode with
        | Some mode when String.trim mode <> "" -> " · " ^ mode
        | _ -> ""
      in
      Printf.sprintf "%s (%s%s)" entry.label entry.slug hm

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

let parse_host_port (s : string) : (string * int) option =
  match String.split_on_char ':' s with
  | [host; port] -> (
      try
        let p = int_of_string (String.trim port) in
        if p > 0 && p < 65536 && String.trim host <> "" then Some (host, p)
        else None
      with _ -> None)
  | _ -> None

let is_port_in_use (port : int) : bool =
  match
    Miaou_interfaces.Capability.get Manager_interfaces.System_capability.key
  with
  | Some cap ->
      let module Sys = (val cap : Manager_interfaces.System) in
      Sys.is_port_in_use port
  | None -> false

let next_free_port ~start ~avoid =
  let rec loop p =
    if
      p >= 1024 && p <= 65535
      && (not (List.mem p avoid))
      && not (is_port_in_use p)
    then p
    else loop (p + 1)
  in
  loop start

let get_registered_ports () =
  match Data.load_service_states () with
  | states ->
      let parse_port addr =
        match String.split_on_char ':' addr with
        | [_; port_str] -> (
            try Some (int_of_string (String.trim port_str)) with _ -> None)
        | _ -> None
      in
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
  | 1 -> (
      (* Network *)
      match get_network_infos () with
      | Error msg ->
          show_error ~title:"Network" msg ;
          s
      | Ok nets ->
          let sorted =
            nets
            |> List.sort
                 (fun (a : Teztnets.network_info) (b : Teztnets.network_info) ->
                   String.compare
                     (normalize_string
                        (Option.value ~default:a.alias a.human_name))
                     (normalize_string
                        (Option.value ~default:b.alias b.human_name)))
          in
          open_choice_modal
            ~title:"Network"
            ~items:sorted
            ~to_string:format_network_choice
            ~on_select:(fun info ->
              update_form_ref (fun f ->
                  {f with network = info.network_url; snapshot = `None})) ;
          s)
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
      let rpc_ports, p2p_ports = get_registered_ports () in
      let avoid = rpc_ports @ p2p_ports in
      prompt_validated_text_modal
        ~title:"RPC Address (host:port)"
        ~initial:!form_ref.rpc_addr
        ~validator:(fun text ->
          match parse_host_port text with
          | None -> Error "Format must be host:port (e.g., 127.0.0.1:8732)"
          | Some (_host, port) ->
              if port < 1024 || port > 65535 then
                Error "Port must be 1024-65535"
              else if List.mem port rpc_ports || List.mem port p2p_ports then
                let sugg = next_free_port ~start:(port + 1) ~avoid in
                Error
                  (Printf.sprintf
                     "Port %d is used by another Octez instance. Try: %d"
                     port
                     sugg)
              else if is_port_in_use port then
                let sugg = next_free_port ~start:(port + 1) ~avoid in
                Error (Printf.sprintf "Port %d is in use. Try: %d" port sugg)
              else Ok ())
        ~on_submit:(fun v -> update_form_ref (fun f -> {f with rpc_addr = v}))
        () ;
      s
  | 6 ->
      (* P2P Address *)
      let rpc_ports, p2p_ports = get_registered_ports () in
      let avoid = rpc_ports @ p2p_ports in
      prompt_validated_text_modal
        ~title:"P2P Address (host:port)"
        ~initial:!form_ref.p2p_addr
        ~validator:(fun text ->
          match parse_host_port text with
          | None -> Error "Format must be host:port (e.g., 0.0.0.0:9732)"
          | Some (_host, port) ->
              if port < 1024 || port > 65535 then
                Error "Port must be 1024-65535"
              else if List.mem port rpc_ports || List.mem port p2p_ports then
                let sugg = next_free_port ~start:(port + 1) ~avoid in
                Error
                  (Printf.sprintf
                     "Port %d is used by another Octez instance. Try: %d"
                     port
                     sugg)
              else if is_port_in_use port then
                let sugg = next_free_port ~start:(port + 1) ~avoid in
                Error (Printf.sprintf "Port %d is in use. Try: %d" port sugg)
              else Ok ())
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
      let current_network = String.trim !form_ref.network in
      let base_choices = [Snapshot_none; Snapshot_custom] in
      let choices =
        if current_network = "" then (
          show_error
            ~title:"Snapshots"
            "Select a network to browse tzinit snapshots." ;
          base_choices)
        else
          match get_snapshot_entries current_network with
          | Ok (_slug, entries) ->
              base_choices @ List.map (fun e -> Snapshot_entry e) entries
          | Error msg ->
              show_error ~title:"Snapshots" msg ;
              base_choices
      in
      open_choice_modal
        ~title:"Snapshot"
        ~items:choices
        ~to_string:snapshot_choice_label
        ~on_select:(function
        | Snapshot_none -> update_form_ref (fun f -> {f with snapshot = `None})
        | Snapshot_custom ->
            prompt_text_modal
              ~title:"Snapshot URL"
              ~initial:(match !form_ref.snapshot with `Url u -> u | _ -> "")
              ~on_submit:(fun url ->
                update_form_ref (fun f -> {f with snapshot = `Url url}))
              ()
        | Snapshot_entry entry ->
            update_form_ref (fun f ->
                {
                  f with
                  snapshot =
                    `Tzinit
                      {
                        network_slug = entry.Snapshots.network;
                        kind_slug = entry.Snapshots.slug;
                        label = entry.Snapshots.label;
                      };
                })) ;
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
          | `Tzinit choice ->
              Snapshot {src = None; kind = Some choice.kind_slug}
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
  let network_value =
    if !network_cache = [] then f.network else network_display_name f.network
  in
  let snapshot_str =
    match f.snapshot with
    | `None -> "None"
    | `Url u -> u
    | `Tzinit sel -> Printf.sprintf "tzinit · %s (%s)" sel.label sel.kind_slug
  in
  (* Validation helpers *)
  let is_nonempty s = String.trim s <> "" in
  let valid_rpc =
    match parse_host_port f.rpc_addr with
    | None -> false
    | Some (_host, port) ->
        if port < 1024 || port > 65535 then false
        else
          let rpc_ports, p2p_ports = get_registered_ports () in
          let avoid = rpc_ports @ p2p_ports in
          not (List.mem port avoid || is_port_in_use port)
  in
  let valid_p2p =
    match parse_host_port f.p2p_addr with
    | None -> false
    | Some (_host, port) ->
        if port < 1024 || port > 65535 then false
        else
          let rpc_ports, p2p_ports = get_registered_ports () in
          let avoid = rpc_ports @ p2p_ports in
          not (List.mem port avoid || is_port_in_use port)
  in
  let valid_snapshot =
    match f.snapshot with
    | `None -> true
    | `Url u -> is_nonempty u
    | `Tzinit _ -> true
  in
  let all_ok =
    is_nonempty f.instance_name
    && is_nonempty f.network && is_nonempty f.history_mode
    && is_nonempty f.data_dir && is_nonempty f.app_bin_dir && valid_rpc
    && valid_p2p && is_nonempty f.service_user && valid_snapshot
  in
  let status ok = if ok then "✓" else "✗" in
  let items =
    [
      ("Instance Name", f.instance_name, is_nonempty f.instance_name);
      ("Network", network_value, is_nonempty f.network);
      ("History Mode", f.history_mode, is_nonempty f.history_mode);
      ("Data Dir", f.data_dir, is_nonempty f.data_dir);
      ("App Bin Dir", f.app_bin_dir, is_nonempty f.app_bin_dir);
      ("RPC Address", f.rpc_addr, valid_rpc);
      ("P2P Address", f.p2p_addr, valid_p2p);
      ("Service User", f.service_user, is_nonempty f.service_user);
      ( "Logging",
        (match f.logging with `File -> "File" | `Journald -> "Journald"),
        true );
      ("Enable on Boot", string_of_bool f.enable_on_boot, true);
      ("Start Now", string_of_bool f.start_now, true);
      ("Snapshot", snapshot_str, valid_snapshot);
      ("Extra Args", f.extra_args, true);
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
  (* Add overall form status banner *)
  let status_banner =
    if all_ok then
      Widgets.bg 22 (Widgets.fg 15 " ✓ Form is valid - ready to install! ")
    else
      let invalid_fields =
        [
          (not (is_nonempty f.instance_name), "Instance Name");
          (not (is_nonempty f.network), "Network");
          (not (is_nonempty f.history_mode), "History Mode");
          (not (is_nonempty f.data_dir), "Data Dir");
          (not (is_nonempty f.app_bin_dir), "App Bin Dir");
          (not valid_rpc, "RPC Address");
          (not valid_p2p, "P2P Address");
          (not (is_nonempty f.service_user), "Service User");
          (not valid_snapshot, "Snapshot");
        ]
        |> List.filter fst |> List.map snd
      in
      let count = List.length invalid_fields in
      let msg =
        if count = 0 then " ✓ Form is valid - ready to install! "
        else
          Printf.sprintf
            " ⚠ Form incomplete: %d field%s need attention "
            count
            (if count > 1 then "s" else "")
      in
      Widgets.bg 160 (Widgets.fg 15 (Widgets.bold msg))
  in
  let header =
    [Widgets.title_highlight " Install Node "; ""; status_banner; ""]
  in
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
