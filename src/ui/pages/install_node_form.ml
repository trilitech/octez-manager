(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Table_widget = Miaou_widgets_display.Table_widget
module Keys = Miaou.Core.Keys
module Bg = Background_runner
module Str_map = Map.Make (String)
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
  preserve_data : [`Auto | `Keep | `Refresh];
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

let default_form =
  {
    instance_name = "node";
    network = "mainnet";
    history_mode = "rolling";
    data_dir = "";
    app_bin_dir = "/home/mathias/dev/tezos/tezos";
    rpc_addr = "127.0.0.1:8732";
    p2p_addr = "0.0.0.0:9732";
    service_user = default_service_user ();
    logging = `File;
    enable_on_boot = true;
    start_now = true;
    snapshot = `None;
    extra_args = "";
    preserve_data = `Auto;
  }

let form_ref = ref default_form

let network_cache : Teztnets.network_info list ref = ref []

let snapshot_cache : (string, Snapshots.entry list * float) Hashtbl.t =
  Hashtbl.create 7

let snapshot_inflight : (string, unit) Hashtbl.t = Hashtbl.create 7

let snapshot_cache_ttl = 300. (* seconds *)

let of_rresult = function Ok v -> Ok v | Error (`Msg msg) -> Error msg

let push_help_hint ?short ?long () =
  Miaou.Core.Help_hint.clear () ;
  match (short, long) with
  | None, None -> ()
  | _ -> Miaou.Core.Help_hint.push ?short ?long ()

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
    let normalize s = String.lowercase_ascii (String.trim s) in
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

let cache_snapshot slug entries =
  Hashtbl.replace snapshot_cache slug (entries, Unix.gettimeofday ())

let schedule_snapshot_fetch slug =
  if not (Hashtbl.mem snapshot_inflight slug) then (
    Hashtbl.add snapshot_inflight slug () ;
    Bg.submit_blocking (fun () ->
        Fun.protect
          ~finally:(fun () -> Hashtbl.remove snapshot_inflight slug)
          (fun () ->
            match fetch_snapshot_list slug with
            | Ok entries -> cache_snapshot slug entries
            | Error msg ->
                prerr_endline
                  (Printf.sprintf "snapshot fetch failed for %s: %s" slug msg))))

let snapshot_entries_from_cache slug =
  match Hashtbl.find_opt snapshot_cache slug with
  | Some (entries, ts) ->
      if Unix.gettimeofday () -. ts > snapshot_cache_ttl then
        schedule_snapshot_fetch slug ;
      Some entries
  | None -> None

let ensure_snapshot_entries slug =
  match snapshot_entries_from_cache slug with
  | Some entries -> Ok entries
  | None -> (
      match fetch_snapshot_list slug with
      | Ok entries ->
          cache_snapshot slug entries ;
          Ok entries
      | Error msg -> Error msg)

let prefetch_snapshot_list network =
  match Snapshots.slug_of_network network with
  | Some slug -> schedule_snapshot_fetch slug
  | None -> ()

let get_snapshot_entries network =
  match Snapshots.slug_of_network network with
  | None ->
      let trimmed = String.trim network in
      if trimmed = "" then Error "Select a network before choosing a snapshot."
      else
        Error
          (Printf.sprintf "Unable to derive a tzinit slug from '%s'." trimmed)
  | Some slug -> (
      match ensure_snapshot_entries slug with
      | Ok entries -> Ok (slug, entries)
      | Error msg -> Error msg)

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

let has_octez_node_binary dir =
  let trimmed = String.trim dir in
  if trimmed = "" then false
  else
    let candidate = Filename.concat trimmed "octez-node" in
    Sys.file_exists candidate
    &&
      try
        Unix.access candidate [Unix.X_OK] ;
        true
      with Unix.Unix_error _ -> false

let move s delta =
  let max_cursor = 13 in
  (* Number of fields + confirm *)
  let cursor = max 0 (min max_cursor (s.cursor + delta)) in
  {s with cursor}

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
           "Unique label (e.g., `%s`); used in service names and data/log \
            directories. Must be unique per host."
           f.instance_name)
  | 1 ->
      mk
        "Network"
        (Printf.sprintf
           "Target Tezos network (e.g., `%s`). Mainnet/ghostnet are preloaded; \
            custom URLs work if they expose bootstrap peers."
           f.network)
  | 2 ->
      mk
        "History Mode"
        (Printf.sprintf
           "Rolling (light) vs full/archive (heavier). Must match any snapshot \
            you import. Current: `%s`."
           f.history_mode)
  | 3 ->
      mk
        "Data Dir"
        (Printf.sprintf
           "Where chain data lives (e.g., /var/lib/octez/node-<name>). \
            Current: `%s`. Existing dirs trigger refresh/keep prompt."
           f.data_dir)
  | 4 ->
      mk
        "App Bin Dir"
        (Printf.sprintf
           "Directory containing octez binaries; expects `octez-node` inside. \
            Current: `%s`."
           f.app_bin_dir)
  | 5 ->
      mk
        "RPC Address"
        (Printf.sprintf
           "Host:port for RPC (default 127.0.0.1:8732). Must be free. Current: \
            `%s`."
           f.rpc_addr)
  | 6 ->
      mk
        "P2P Address"
        (Printf.sprintf
           "Host:port to accept peers (default 0.0.0.0:9732). Port must be \
            free. Current: `%s`."
           f.p2p_addr)
  | 7 ->
      mk
        "Service User"
        (Printf.sprintf
           "Account running the service (current: `%s`). If root and missing, \
            installer will create it."
           f.service_user)
  | 8 ->
      mk
        "Logging"
        (Printf.sprintf
           "`File` writes node.log under the instance log dir; `Journald` uses \
            systemd journal. Current: `%s`."
           (match f.logging with `File -> "File" | `Journald -> "Journald"))
  | 9 -> mk "Enable on Boot" "Runs `systemctl enable` so the node auto-starts."
  | 10 -> mk "Start Now" "Start the node service immediately after install."
  | 11 ->
      mk
        "Snapshot"
        (match f.snapshot with
        | `None ->
            "Bootstrap from genesis. Importing a snapshot speeds sync; pick \
             one matching the history mode."
        | `Url u ->
            Printf.sprintf
              "Custom snapshot URL: `%s`. Ensure it matches the network and \
               history mode."
              u
        | `Tzinit sel ->
            Printf.sprintf
              "tzinit preset: %s (%s). Matches %s history mode."
              sel.label
              sel.kind_slug
              sel.kind_slug)
  | 12 ->
      mk
        "Extra Args"
        "Additional `octez-node run` flags. Press ? to browse supported \
         options; leave blank for defaults."
  | 13 ->
      mk
        "Confirm & Install"
        "Runs the installer with current values. All required fields must be \
         valid before proceeding."
  | _ -> (None, None)

let apply_field_hint s =
  let short, long = field_hint s.form s.cursor in
  push_help_hint ?short ?long ()

let parse_port addr =
  match String.split_on_char ':' addr with
  | [_; port_str] -> (
      try Some (int_of_string (String.trim port_str)) with _ -> None)
  | _ -> None

let data_dir_in_use ~states path =
  let trimmed = String.trim path in
  if trimmed = "" then false
  else
    List.exists
      (fun (st : Data.Service_state.t) ->
        String.equal (String.trim st.service.Service.data_dir) trimmed)
      states

let data_dir_conflict_instance ~states path =
  let trimmed = String.trim path in
  if trimmed = "" then None
  else
    states
    |> List.find_map (fun (st : Data.Service_state.t) ->
        let svc_dir = String.trim st.service.Service.data_dir in
        if String.equal svc_dir trimmed then Some st.service.Service.instance
        else None)

let data_dir_nonempty path =
  let trimmed = String.trim path in
  if trimmed = "" then false
  else
    try
      if not (Sys.is_directory trimmed) then false
      else
        Sys.readdir trimmed |> Array.to_list
        |> List.filter (fun e -> e <> "." && e <> "..")
        |> function
        | [] -> false
        | _ -> true
    with _ -> false

(* Check if history mode conflicts with selected snapshot *)
let history_snapshot_conflict ~history_mode ~snapshot ~network =
  match snapshot with
  | `None | `Url _ -> false (* Can't validate these *)
  | `Tzinit tz -> (
      (* Look up the snapshot entry to get its history mode *)
      match Snapshots.slug_of_network network with
      | None -> false
      | Some slug -> (
          match snapshot_entries_from_cache slug with
          | None -> false (* Cache not ready *)
          | Some entries -> (
              match
                List.find_opt
                  (fun (e : Snapshots.entry) ->
                    e.network = tz.network_slug && e.slug = tz.kind_slug)
                  entries
              with
              | None -> false (* Entry not found *)
              | Some entry -> (
                  match entry.history_mode with
                  | Some snap_mode when String.trim snap_mode <> "" -> (
                      match History_mode.of_string history_mode with
                      | Ok requested ->
                          not
                            (Installer.For_tests.history_mode_matches
                               ~requested
                               ~snapshot_mode:snap_mode)
                      | Error _ -> true)
                  | _ -> false (* No history mode metadata *)))))

let pad_with_right cols left right =
  if right = "" then left
  else
    let space =
      let raw = cols - String.length left - String.length right in
      if raw < 1 then 1 else raw
    in
    left ^ String.make space ' ' ^ right

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

let get_registered_ports ?states () =
  let states =
    match states with Some s -> s | None -> Data.load_service_states ()
  in
  ports_from_states states

let ensure_ports_initialized () =
  let rpc_ports, p2p_ports = get_registered_ports () in
  let avoid = ref (rpc_ports @ p2p_ports) in
  let ensure current ~default_host ~start_port setter =
    let needs_new =
      match parse_host_port current with
      | Some (_host, port) ->
          port < 1024 || port > 65535 || List.mem port !avoid
          || is_port_in_use port
      | None -> true
    in
    if needs_new then (
      let port = next_free_port ~start:start_port ~avoid:!avoid in
      setter (Printf.sprintf "%s:%d" default_host port) ;
      avoid := port :: !avoid)
    else
      match parse_host_port current with
      | Some (_host, port) -> avoid := port :: !avoid
      | None -> ()
  in
  let current = !form_ref in
  ensure
    current.rpc_addr
    ~default_host:"127.0.0.1"
    ~start_port:8732
    (fun value -> update_form_ref (fun f -> {f with rpc_addr = value})) ;
  let current = !form_ref in
  ensure current.p2p_addr ~default_host:"0.0.0.0" ~start_port:9732 (fun value ->
      update_form_ref (fun f -> {f with p2p_addr = value}))

let ensure_service_user_initialized () =
  let current = !form_ref in
  if String.trim current.service_user = "" then
    update_form_ref (fun f -> {f with service_user = default_service_user ()})

let effective_data_dir (f : form_state) =
  if String.trim f.data_dir = "" then Common.default_data_dir f.instance_name
  else f.data_dir

let dir_nonempty path =
  let trimmed = String.trim path in
  if trimmed = "" then false
  else
    try
      let st = Unix.stat trimmed in
      st.Unix.st_kind = Unix.S_DIR
      &&
      let entries = Sys.readdir trimmed in
      Array.exists (fun e -> e <> "." && e <> "..") entries
    with Unix.Unix_error _ | Sys_error _ -> false

let normalized s = String.lowercase_ascii (String.trim s)

let service_user_valid ~user =
  if Common.is_root () then true
  else Result.is_ok (System_user.validate_user_for_service ~user)

let instance_in_use ?states name =
  let target = normalized name in
  let states =
    match states with Some s -> s | None -> Data.load_service_states ()
  in
  target <> ""
  && List.exists
       (fun (s : Data.Service_state.t) ->
         String.equal target (normalized s.service.Service.instance))
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
  Binary_help_explorer.open_node_run_help ~app_bin_dir ~on_apply:(fun tokens ->
      let arg_str = String.concat " " tokens in
      update_form_ref (fun f -> {f with extra_args = arg_str})) ;
  s

let init () =
  ensure_service_user_initialized () ;
  ensure_ports_initialized () ;
  prefetch_snapshot_list !form_ref.network ;
  let service_states = Data.load_service_states () in
  {form = !form_ref; cursor = 0; next_page = None; service_states}

let update s _ = s

let refresh s =
  ensure_service_user_initialized () ;
  ensure_ports_initialized () ;
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
                preserve_data = `Auto;
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
                  {f with network = info.network_url; snapshot = `None}) ;
              prefetch_snapshot_list info.network_url) ;
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
      prompt_validated_text_modal
        ~title:"Data Directory"
        ~initial:!form_ref.data_dir
        ~validator:(fun v ->
          let trimmed = String.trim v in
          if trimmed = "" then Error "Data directory cannot be empty"
          else if data_dir_in_use ~states:s.service_states trimmed then
            Error "This data directory is already used by a registered instance"
          else Ok ())
        ~on_submit:(fun v ->
          update_form_ref (fun f ->
              {f with data_dir = v; preserve_data = `Auto}))
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
      let rpc_ports, p2p_ports =
        get_registered_ports ~states:s.service_states ()
      in
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
      let rpc_ports, p2p_ports =
        get_registered_ports ~states:s.service_states ()
      in
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
              Context.toast_info msg ;
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
      (* Extra Args -> open flag explorer *)
      open_binary_help s
  | 13 ->
      (* Confirm *)
      (* Trigger install *)
      let f = !form_ref in
      if f.instance_name = "" then (
        show_error ~title:"Error" "Instance name is required." ;
        s)
      else if instance_in_use ~states:s.service_states f.instance_name then (
        show_error ~title:"Error" "Instance name already exists." ;
        s)
      else if
        data_dir_conflict_instance ~states:s.service_states f.data_dir
        |> Option.is_some
      then (
        let conflict =
          data_dir_conflict_instance ~states:s.service_states f.data_dir
        in
        let msg =
          match conflict with
          | Some inst ->
              Printf.sprintf
                "Data directory is already used by instance '%s'. Remove or \
                 change that instance before installing."
                inst
          | None ->
              "Data directory is already used by another instance. Remove it \
               first or pick a new path."
        in
        show_error ~title:"Error" msg ;
        s)
      else if
        (not (Common.is_root ()))
        && Result.is_error
             (System_user.validate_user_for_service ~user:f.service_user)
      then (
        show_error
          ~title:"Error"
          "Service user does not exist and cannot be created (run as root or \
           choose an existing user)." ;
        s)
      else
        let data_dir = effective_data_dir f in
        let needs_choice = dir_nonempty data_dir && f.preserve_data = `Auto in
        let run_install ~preserve_data =
          update_form_ref (fun f -> {f with data_dir; preserve_data}) ;
          let f = !form_ref in
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
          let req : Installer_types.node_request =
            {
              instance = f.instance_name;
              network = f.network;
              history_mode;
              data_dir =
                (if f.data_dir = "" then Some data_dir else Some data_dir);
              rpc_addr = f.rpc_addr;
              net_addr = f.p2p_addr;
              service_user = f.service_user;
              app_bin_dir = f.app_bin_dir;
              logging_mode;
              extra_args;
              auto_enable = f.enable_on_boot;
              bootstrap;
              preserve_data = preserve_data = `Keep;
            }
          in
          let res =
            let* () =
              if Common.is_root () then
                System_user.ensure_service_account ~name:f.service_user
              else Ok ()
            in
            let* (module I) = require_package_manager () in
            I.install_node req
          in
          match res with
          | Ok _ ->
              Context.mark_instances_dirty () ;
              Context.navigate "instances" ;
              s
          | Error (`Msg e) ->
              show_error ~title:"Installation Failed" e ;
              s
        in
        if needs_choice then (
          open_choice_modal
            ~title:"Data directory exists"
            ~items:[`Refresh; `Keep]
            ~to_string:(function
              | `Refresh -> "Refresh (wipe and import)"
              | `Keep -> "Keep existing data")
            ~on_select:(function
              | `Refresh -> ignore (run_install ~preserve_data:`Refresh)
              | `Keep -> ignore (run_install ~preserve_data:`Keep)) ;
          s)
        else run_install ~preserve_data:f.preserve_data
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
  let network_value =
    if !network_cache = [] then f.network else network_display_name f.network
  in
  let snapshot_str =
    match f.snapshot with
    | `None -> "None"
    | `Url u -> u
    | `Tzinit sel -> Printf.sprintf "tzinit · %s (%s)" sel.label sel.kind_slug
  in
  let data_dir_risky =
    match f.preserve_data with
    | `Keep -> false
    | _ -> data_dir_nonempty f.data_dir
  in
  (* Validation helpers *)
  let is_nonempty s = String.trim s <> "" in
  let valid_instance =
    is_nonempty f.instance_name
    && not (instance_in_use ~states:s.service_states f.instance_name)
  in
  let valid_service_user =
    is_nonempty f.service_user && service_user_valid ~user:f.service_user
  in
  let data_dir_conflict = data_dir_in_use ~states:s.service_states f.data_dir in
  let has_history_conflict =
    is_nonempty f.history_mode
    && history_snapshot_conflict
         ~history_mode:f.history_mode
         ~snapshot:f.snapshot
         ~network:f.network
  in
  let valid_history_mode =
    is_nonempty f.history_mode && not has_history_conflict
  in
  let valid_rpc =
    match parse_host_port f.rpc_addr with
    | None -> false
    | Some (_host, port) ->
        if port < 1024 || port > 65535 then false
        else
          let rpc_ports, p2p_ports =
            get_registered_ports ~states:s.service_states ()
          in
          let avoid = rpc_ports @ p2p_ports in
          not (List.mem port avoid || is_port_in_use port)
  in
  let valid_p2p =
    match parse_host_port f.p2p_addr with
    | None -> false
    | Some (_host, port) ->
        if port < 1024 || port > 65535 then false
        else
          let rpc_ports, p2p_ports =
            get_registered_ports ~states:s.service_states ()
          in
          let avoid = rpc_ports @ p2p_ports in
          not (List.mem port avoid || is_port_in_use port)
  in
  let valid_app_bin_dir = has_octez_node_binary f.app_bin_dir in
  let valid_snapshot =
    (match f.snapshot with
      | `None -> true
      | `Url u -> is_nonempty u
      | `Tzinit _ -> true)
    && not has_history_conflict
  in
  let all_ok =
    valid_instance && is_nonempty f.network && valid_history_mode
    && is_nonempty f.data_dir && (not data_dir_conflict) && valid_app_bin_dir
    && valid_rpc && valid_p2p && valid_service_user && valid_snapshot
  in
  let status ok = if ok then "✓" else "✗" in
  let items =
    [
      ("Instance Name", f.instance_name, valid_instance);
      ("Network", network_value, is_nonempty f.network);
      ("History Mode", f.history_mode, valid_history_mode);
      ("Data Dir", f.data_dir, is_nonempty f.data_dir && not data_dir_conflict);
      ("App Bin Dir", f.app_bin_dir, valid_app_bin_dir);
      ("RPC Address", f.rpc_addr, valid_rpc);
      ("P2P Address", f.p2p_addr, valid_p2p);
      ("Service User", f.service_user, valid_service_user);
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
          ( not valid_history_mode,
            if has_history_conflict then
              "History Mode (conflicts with snapshot)"
            else "History Mode" );
          ((not (is_nonempty f.data_dir)) || data_dir_conflict, "Data Dir");
          (not valid_app_bin_dir, "App Bin Dir");
          (not valid_rpc, "RPC Address");
          (not valid_p2p, "P2P Address");
          (not valid_service_user, "Service User");
          ( not valid_snapshot,
            if has_history_conflict then "Snapshot (history mode mismatch)"
            else "Snapshot" );
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
  let warning_banner =
    if data_dir_conflict then
      let msg =
        Printf.sprintf
          " ⚠ Data dir already used by another instance: %s "
          f.data_dir
      in
      Widgets.bg 220 (Widgets.fg 0 (Widgets.bold msg))
    else if data_dir_risky then
      let msg =
        Printf.sprintf
          " ⚠ Data dir not empty: %s (install may overwrite existing data) "
          f.data_dir
      in
      Widgets.bg 220 (Widgets.fg 0 (Widgets.bold msg))
    else ""
  in
  let title_line = Widgets.title_highlight " Install Node " in
  let header_line =
    if warning_banner = "" then status_banner
    else pad_with_right size.LTerm_geom.cols status_banner warning_banner
  in
  let header = [title_line; header_line] in
  let footer =
    [Widgets.dim "↑/↓ navigate, Enter to edit, ? node flags, Esc back"]
  in
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
      ("?", open_binary_help, "node flags");
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
