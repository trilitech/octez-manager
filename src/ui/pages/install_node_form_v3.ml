(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Node installation form using field bundles.

    Demonstrates bundle composition for the most complex form with snapshot
    import, caching, and preserve_data flow. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let of_rresult = function Ok v -> Ok v | Error (`Msg msg) -> Error msg

let name = "install_node_form_v3"

(** {1 Custom Types} *)

type tzinit_snapshot = {
  network_slug : string;
  kind_slug : string;
  label : string;
}

type snapshot_selection = [`None | `Url of string | `Tzinit of tzinit_snapshot]

type preserve_data = [`Auto | `Keep | `Refresh]

(** {1 Model} *)

type model = {
  core : Form_builder_common.core_service_config;
  node : Form_builder_common.node_config;
  (* Node-specific fields *)
  snapshot : snapshot_selection;
  preserve_data : preserve_data;
  tmp_dir : string option; (* Custom directory for snapshot download *)
  keep_snapshot : bool; (* Keep snapshot file after import *)
  check_snapshot : bool; (* Verify snapshot integrity during import *)
  (* Edit mode *)
  edit_mode : bool;
  original_instance : string option;
  stopped_dependents : string list;
}

(** Generate instance name from network and history mode.
    Format: node-{network} for rolling, node-{network}-{history_mode} for full/archive *)
let generate_instance_name ~network ~history_mode =
  (* Extract short network name (e.g., "mainnet" from URL or slug) *)
  let network_short =
    let n = String.lowercase_ascii (String.trim network) in
    (* Handle URL format like https://teztnets.com/mainnet *)
    let base =
      match String.rindex_opt n '/' with
      | Some i -> String.sub n (i + 1) (String.length n - i - 1)
      | None -> n
    in
    (* Truncate long names *)
    if String.length base > 15 then String.sub base 0 15 else base
  in
  match String.lowercase_ascii history_mode with
  | "rolling" -> Printf.sprintf "node-%s" network_short
  | mode -> Printf.sprintf "node-%s-%s" network_short mode

let snapshot_provider () =
  Miaou_interfaces.Capability.get
    Manager_interfaces.Snapshot_provider_capability.key

let slug_of_network network =
  let fallback () = Snapshots.slug_of_network network in
  match snapshot_provider () with
  | Some cap -> (
      let module P = (val cap : Manager_interfaces.Snapshot_provider) in
      match P.slug_of_network network with
      | Some slug -> Some slug
      | None -> fallback ())
  | None -> fallback ()

(** Check if a snapshot is an auto-generated snapshot by looking at its label *)
let is_auto_snapshot = function
  | `Tzinit {label; _} -> String.starts_with ~prefix:"Auto (" label
  | _ -> false

(** Create a default snapshot placeholder for auto-resolution by the installer.
    This avoids synchronous I/O while still defaulting to snapshot download.
    Archive mode returns `None since snapshots cannot be imported for archive nodes. *)
let create_default_snapshot ~network ~history_mode =
  match String.lowercase_ascii (String.trim history_mode) with
  | "archive" -> `None (* Archive nodes cannot import snapshots *)
  | _ -> (
      match slug_of_network network with
      | Some network_slug ->
          let kind_slug =
            match String.lowercase_ascii (String.trim history_mode) with
            | "rolling" -> "rolling"
            | "full" -> "full"
            | _ -> "rolling"
          in
          `Tzinit
            {
              network_slug;
              kind_slug;
              label = Printf.sprintf "Auto (%s)" kind_slug;
            }
      | None -> `None)

let base_initial_model () =
  let network = "mainnet" in
  let history_mode = "rolling" in
  let instance_name = generate_instance_name ~network ~history_mode in
  {
    core =
      {
        instance_name;
        service_user = Form_builder_common.default_service_user ();
        app_bin_dir =
          Form_builder_common.default_app_bin_dir ~binary_name:"octez-node";
        enable_on_boot = true;
        start_now = true;
        extra_args = "";
      };
    node =
      {
        network;
        history_mode;
        data_dir = Common.default_role_dir "node" instance_name;
        rpc_addr = "127.0.0.1:8732";
        p2p_addr = "0.0.0.0:9732";
        (* All interfaces for peer reachability *)
      };
    snapshot = create_default_snapshot ~network ~history_mode;
    preserve_data = `Auto;
    tmp_dir = None;
    keep_snapshot = false;
    check_snapshot = true;
    edit_mode = false;
    original_instance = None;
    stopped_dependents = [];
  }

(** {1 Helper Functions} *)

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

(** {1 Snapshot Cache} *)

let snapshot_cache = Cache.create_safe_keyed ~name:"snapshots" ~ttl:60.0 ()

let snapshot_inflight : (string, unit) Hashtbl.t = Hashtbl.create 7

let snapshot_inflight_lock = Mutex.create ()

let fetch_snapshot_list slug =
  let fallback () = of_rresult (Snapshots.list ~network_slug:slug) in
  match snapshot_provider () with
  | Some cap -> (
      let module P = (val cap : Manager_interfaces.Snapshot_provider) in
      match of_rresult (P.list ~network_slug:slug) with
      | Ok entries -> Ok entries
      | Error _ -> fallback ())
  | None -> fallback ()

let cache_snapshot slug entries =
  Cache.set_safe_keyed snapshot_cache slug entries

let schedule_snapshot_fetch slug =
  let should_fetch =
    Mutex.protect snapshot_inflight_lock (fun () ->
        if not (Hashtbl.mem snapshot_inflight slug) then (
          Hashtbl.add snapshot_inflight slug () ;
          true)
        else false)
  in
  if should_fetch then
    Background_runner.submit_blocking
      (* ~on_complete:(fun () -> Context.invalidate_cache ()) *)
      (fun () ->
        Fun.protect
          ~finally:(fun () ->
            Mutex.protect snapshot_inflight_lock (fun () ->
                Hashtbl.remove snapshot_inflight slug))
          (fun () ->
            match fetch_snapshot_list slug with
            | Ok entries -> cache_snapshot slug entries
            | Error msg ->
                Common.append_debug_log
                  (Printf.sprintf "Background snapshot fetch failed: %s" msg)))

let snapshot_entries_from_cache slug =
  Cache.get_safe_keyed_cached snapshot_cache slug

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
  match slug_of_network network with
  | Some slug -> (
      match snapshot_entries_from_cache slug with
      | Some _ -> ()
      | None -> schedule_snapshot_fetch slug)
  | None -> ()

(** {1 Snapshot Space Check} *)

let snapshot_size_cache =
  Cache.create_safe_keyed ~name:"snapshot_sizes" ~ttl:60.0 ()

let snapshot_size_inflight : (string, unit) Hashtbl.t = Hashtbl.create 7

let snapshot_size_inflight_lock = Mutex.create ()

let format_bytes bytes =
  let kb = 1024L in
  let mb = Int64.mul kb 1024L in
  let gb = Int64.mul mb 1024L in
  if bytes >= gb then
    Printf.sprintf "%.1f GB" (Int64.to_float bytes /. Int64.to_float gb)
  else if bytes >= mb then
    Printf.sprintf "%.0f MB" (Int64.to_float bytes /. Int64.to_float mb)
  else Printf.sprintf "%Ld bytes" bytes

(** Get the download URL for a snapshot selection *)
let get_snapshot_url ~network snapshot =
  match snapshot with
  | `None -> None
  | `Url url ->
      if
        String.length url > 4
        && String.sub (String.lowercase_ascii url) 0 4 = "http"
      then Some url
      else None (* Local file, no space check needed *)
  | `Tzinit snap -> (
      match slug_of_network network with
      | None -> None
      | Some network_slug -> (
          match snapshot_entries_from_cache network_slug with
          | None -> None
          | Some entries -> (
              match
                List.find_opt
                  (fun (e : Snapshots.entry) ->
                    e.network = snap.network_slug && e.slug = snap.kind_slug)
                  entries
              with
              | None -> None
              | Some entry -> entry.download_url)))

(** Check if there's enough space for the snapshot download and import.
    Returns [Ok ()] if space is sufficient or can't be determined.
    Returns [Error msg] if space is definitely insufficient.

    Checks both:
    - tmp_dir: needs snapshot_size * 1.1 for download
    - data_dir: needs snapshot_size * 1.2 for imported data

    If both are on the same filesystem, checks combined requirement. *)
let check_snapshot_space ~network ~snapshot ~tmp_dir ~data_dir =
  match get_snapshot_url ~network snapshot with
  | None -> Ok () (* No URL to check, or local file *)
  | Some url -> (
      match Cache.get_safe_keyed_cached snapshot_size_cache url with
      | Some size_opt -> (
          match size_opt with
          | None -> Ok () (* Size unknown, proceed *)
          | Some snapshot_size -> (
              let tmp_path =
                Option.value ~default:(Filename.get_temp_dir_name ()) tmp_dir
              in
              (* Space needed: download (1.1x) + imported data (1.2x) *)
              let download_required =
                Int64.add snapshot_size (Int64.div snapshot_size 10L)
              in
              let storage_required =
                Int64.add snapshot_size (Int64.div snapshot_size 5L)
              in
              (* Check if tmp and data_dir are on same filesystem *)
              let data_path =
                (* Use parent dir if data_dir doesn't exist yet *)
                if Sys.file_exists data_dir then data_dir
                else Filename.dirname data_dir
              in
              let same_fs =
                Common.same_filesystem tmp_path data_path
                |> Option.value ~default:false
              in
              if same_fs then
                (* Same filesystem: need space for both download and storage *)
                let total_required =
                  Int64.add download_required storage_required
                in
                match Common.get_available_space tmp_path with
                | None -> Ok ()
                | Some available ->
                    if available >= total_required then Ok ()
                    else
                      Error
                        (Printf.sprintf
                           "Need %s total (%s for download + %s for storage) \
                            but %s only has %s"
                           (format_bytes total_required)
                           (format_bytes download_required)
                           (format_bytes storage_required)
                           tmp_path
                           (format_bytes available))
              else
                (* Different filesystems: check each separately *)
                let* () =
                  match Common.get_available_space tmp_path with
                  | None -> Ok ()
                  | Some available ->
                      if available >= download_required then Ok ()
                      else
                        Error
                          (Printf.sprintf
                             "Need %s for download but %s only has %s"
                             (format_bytes download_required)
                             tmp_path
                             (format_bytes available))
                in
                match Common.get_available_space data_path with
                | None -> Ok ()
                | Some available ->
                    if available >= storage_required then Ok ()
                    else
                      Error
                        (Printf.sprintf
                           "Need %s for node storage but %s only has %s"
                           (format_bytes storage_required)
                           data_dir
                           (format_bytes available))))
      | None ->
          (* Trigger background fetch *)
          let should_fetch =
            Mutex.protect snapshot_size_inflight_lock (fun () ->
                if not (Hashtbl.mem snapshot_size_inflight url) then (
                  Hashtbl.add snapshot_size_inflight url () ;
                  true)
                else false)
          in
          if should_fetch then
            Background_runner.submit_blocking (fun () ->
                Fun.protect
                  ~finally:(fun () ->
                    Mutex.protect snapshot_size_inflight_lock (fun () ->
                        Hashtbl.remove snapshot_size_inflight url))
                  (fun () ->
                    let size = Common.get_remote_file_size url in
                    Cache.set_safe_keyed snapshot_size_cache url size)) ;
          Ok ())

let parse_port addr =
  match String.split_on_char ':' addr with
  | [_; port_str] -> (
      try Some (int_of_string (String.trim port_str)) with _ -> None)
  | _ -> None

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

let ensure_ports_initialized model_ref =
  let states =
    try Form_builder_common.cached_service_states () with _ -> []
    (* In tests/early init, capability may be absent; default to empty. *)
  in
  let rpc_ports, p2p_ports = ports_from_states states in
  let avoid = ref (rpc_ports @ p2p_ports) in
  let ensure current ~default_host ~start_port setter =
    let needs_new =
      match Form_builder_common.parse_host_port current with
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
      match Form_builder_common.parse_host_port current with
      | Some (_host, port) -> avoid := port :: !avoid
      | None -> ()
  in
  let current = !model_ref in
  ensure
    current.node.rpc_addr
    ~default_host:"127.0.0.1"
    ~start_port:8732
    (fun value ->
      model_ref :=
        {
          current with
          node = Form_builder_common.{current.node with rpc_addr = value};
        }) ;
  let current = !model_ref in
  ensure
    current.node.p2p_addr
    ~default_host:"0.0.0.0"
    ~start_port:9732
    (fun value ->
      model_ref :=
        {
          current with
          node = Form_builder_common.{current.node with p2p_addr = value};
        })

let make_initial_model () =
  match Context.take_pending_edit_service () with
  | Some edit_ctx when edit_ctx.service.Service.role = "node" ->
      let svc = edit_ctx.service in
      {
        core =
          {
            instance_name = svc.Service.instance;
            service_user = svc.Service.service_user;
            app_bin_dir = svc.Service.app_bin_dir;
            enable_on_boot = true;
            start_now = false;
            (* Don't auto-start after edit *)
            extra_args = String.concat " " svc.Service.extra_args;
          };
        node =
          {
            network = svc.Service.network;
            history_mode = History_mode.to_string svc.Service.history_mode;
            data_dir = svc.Service.data_dir;
            rpc_addr = svc.Service.rpc_addr;
            p2p_addr = svc.Service.net_addr;
          };
        snapshot = `None;
        preserve_data = `Keep;
        (* Always preserve data in edit mode *)
        tmp_dir = None;
        keep_snapshot = false;
        check_snapshot = true;
        edit_mode = true;
        original_instance = Some svc.Service.instance;
        stopped_dependents = edit_ctx.stopped_dependents;
      }
  | _ ->
      let model_ref = ref (base_initial_model ()) in
      ensure_ports_initialized model_ref ;
      !model_ref

(** Check if a snapshot entry matches the requested history mode *)
let snapshot_entry_matches_history_mode entry ~history_mode =
  match entry.Snapshots.history_mode with
  | Some snap_mode when String.trim snap_mode <> "" -> (
      (* Use explicit history mode from metadata *)
      match History_mode.of_string history_mode with
      | Ok requested ->
          Installer.For_tests.history_mode_matches
            ~requested
            ~snapshot_mode:snap_mode
      | Error _ -> false)
  | _ -> (
      (* No explicit history mode in metadata - try to infer from slug *)
      let slug_lower = String.lowercase_ascii entry.Snapshots.slug in
      let mode_lower = String.lowercase_ascii (String.trim history_mode) in
      (* Check if slug starts with or contains the history mode *)
      String.starts_with ~prefix:mode_lower slug_lower
      ||
      match mode_lower with
      | "full" ->
          (* "full" matches "full", "full50", etc. *)
          String.starts_with ~prefix:"full" slug_lower
      | _ -> false)

let history_snapshot_conflict ~history_mode ~snapshot ~network =
  match snapshot with
  | `None | `Url _ -> false
  | `Tzinit tz -> (
      match slug_of_network network with
      | None -> false
      | Some slug -> (
          match snapshot_entries_from_cache slug with
          | None -> false
          | Some entries -> (
              match
                List.find_opt
                  (fun (e : Snapshots.entry) ->
                    e.network = tz.network_slug && e.slug = tz.kind_slug)
                  entries
              with
              | None -> false
              | Some entry ->
                  not (snapshot_entry_matches_history_mode entry ~history_mode))
          ))

(** {1 Custom Fields} *)

let snapshot_field =
  Form_builder.custom
    ~label:"Snapshot Import"
    ~get:(fun m ->
      match m.snapshot with
      | `None -> "None"
      | `Url url -> if url = "" then "Custom URL..." else url
      | `Tzinit snap ->
          Printf.sprintf "tzinit · %s (%s)" snap.label snap.kind_slug)
    ~edit:(fun model_ref ->
      let snapshots_opt =
        match slug_of_network !model_ref.node.network with
        | Some slug -> (
            (* Check cache *)
            match snapshot_entries_from_cache slug with
            | Some entries -> Some entries
            | None ->
                (* Cache miss or expired:
                   1. Trigger a background fetch if not already in flight
                   2. Return None for now (will re-render when fetch completes,
                      assuming UI handles async updates, or on next interaction) *)
                schedule_snapshot_fetch slug ;
                None)
        | None -> None
      in

      (* Filter snapshots to only show those matching the selected history mode
         and exclude full50 snapshots *)
      let filtered_snapshots =
        match snapshots_opt with
        | Some entries ->
            let matches =
              entries
              |> List.filter (fun e ->
                  e.Snapshots.slug <> "full50"
                  && snapshot_entry_matches_history_mode
                       e
                       ~history_mode:!model_ref.node.history_mode)
            in
            if matches = [] then `NoMatches else `Entries matches
        | None -> `Loading
      in

      (* When no snapshots match, only None and Custom are offered.
         This allows users to either sync from genesis or provide a custom URL. *)
      let items =
        match filtered_snapshots with
        | `Loading -> [`Loading]
        | `NoMatches -> [`None; `Custom]
        | `Entries entries ->
            (`None :: (entries |> List.map (fun e -> `Tzinit e))) @ [`Custom]
      in

      let to_string = function
        | `Loading -> Context.render_spinner "Loading snapshots..."
        | `None -> "None (manual sync)"
        | `Custom -> "Custom URL..."
        | `Tzinit e ->
            Printf.sprintf "%s (%s)" e.Snapshots.label e.Snapshots.slug
      in

      let on_select choice =
        match choice with
        | `Loading -> () (* Do nothing on select if loading *)
        | `None -> model_ref := {!model_ref with snapshot = `None}
        | `Tzinit e ->
            let snap =
              {
                network_slug = e.Snapshots.network;
                kind_slug = e.Snapshots.slug;
                label = e.Snapshots.label;
              }
            in
            model_ref := {!model_ref with snapshot = `Tzinit snap}
        | `Custom ->
            Modal_helpers.prompt_text_modal
              ~title:"Snapshot URL"
              ~placeholder:(Some "https://...")
              ~initial:(match !model_ref.snapshot with `Url u -> u | _ -> "")
              ~on_submit:(fun url ->
                model_ref := {!model_ref with snapshot = `Url url})
              ()
      in
      Modal_helpers.open_choice_modal
        ~title:"Import Snapshot"
        ~items
        ~to_string
        ~on_tick:Context.tick_spinner
        ~on_select
        ())
    ~validate:(fun m ->
      let history_conflict =
        history_snapshot_conflict
          ~history_mode:m.node.history_mode
          ~snapshot:m.snapshot
          ~network:m.node.network
      in
      if history_conflict then false
      else
        match m.snapshot with
        | `None -> true
        | `Url u -> String.trim u <> ""
        | `Tzinit _ -> true)
    ~validate_msg:(fun m ->
      let history_conflict =
        history_snapshot_conflict
          ~history_mode:m.node.history_mode
          ~snapshot:m.snapshot
          ~network:m.node.network
      in
      if history_conflict then
        Some "Snapshot history mode does not match selected history mode"
      else
        match m.snapshot with
        | `None -> None
        | `Url u when String.trim u = "" -> Some "Snapshot URL cannot be empty"
        | _ -> None)
    ()

let tmp_dir_field =
  Form_builder.custom
    ~label:"Download Directory"
    ~get:(fun m ->
      match m.tmp_dir with None -> "/tmp (default)" | Some dir -> dir)
    ~edit:(fun model_ref ->
      Modal_helpers.open_file_browser_modal
        ?initial_path:!model_ref.tmp_dir
        ~dirs_only:true
        ~require_writable:true
        ~on_select:(fun dir ->
          let dir = String.trim dir in
          model_ref :=
            {!model_ref with tmp_dir = (if dir = "" then None else Some dir)})
        ())
    ~validate:(fun m ->
      (* First check directory validity *)
      let dir_valid =
        match m.tmp_dir with
        | None -> true
        | Some dir ->
            String.trim dir <> ""
            && ((not (Sys.file_exists dir)) || Sys.is_directory dir)
      in
      if not dir_valid then false
      else
        (* Then check available space *)
        match
          check_snapshot_space
            ~network:m.node.network
            ~snapshot:m.snapshot
            ~tmp_dir:m.tmp_dir
            ~data_dir:m.node.data_dir
        with
        | Ok () -> true
        | Error _ -> false)
    ~validate_msg:(fun m ->
      match m.tmp_dir with
      | Some dir when String.trim dir = "" ->
          Some "Directory path cannot be empty"
      | Some dir when Sys.file_exists dir && not (Sys.is_directory dir) ->
          Some "Path exists but is not a directory"
      | _ -> (
          (* Check available space *)
          match
            check_snapshot_space
              ~network:m.node.network
              ~snapshot:m.snapshot
              ~tmp_dir:m.tmp_dir
              ~data_dir:m.node.data_dir
          with
          | Ok () -> None
          | Error msg -> Some msg))
    ()

let keep_snapshot_field =
  Form_builder.toggle
    ~label:"Keep Snapshot"
    ~get:(fun m -> m.keep_snapshot)
    ~set:(fun keep_snapshot m -> {m with keep_snapshot})

let check_snapshot_field =
  Form_builder.toggle
    ~label:"Check Snapshot"
    ~get:(fun m -> m.check_snapshot)
    ~set:(fun check_snapshot m -> {m with check_snapshot})

(** {1 Form Specification} *)

(** Auto-update instance name, data_dir, and snapshot when network/history_mode changes *)
let set_node_with_autoname node m =
  if m.edit_mode then {m with node}
  else
    (* Check if network or history_mode changed *)
    let old_name =
      generate_instance_name
        ~network:m.node.network
        ~history_mode:m.node.history_mode
    in
    let new_name =
      generate_instance_name
        ~network:node.network
        ~history_mode:node.history_mode
    in
    (* Only auto-update if instance name matches the old generated name *)
    if String.equal m.core.instance_name old_name then
      let new_data_dir = Common.default_role_dir "node" new_name in
      let old_default_dir = Common.default_role_dir "node" old_name in
      let should_update_data_dir =
        String.equal (String.trim m.node.data_dir) old_default_dir
      in
      (* Auto-update snapshot if:
         1. Current snapshot is an auto snapshot and network or history_mode changed, OR
         2. Current snapshot is None and we're moving FROM archive mode to non-archive mode *)
      let old_is_archive =
        String.(lowercase_ascii (trim m.node.history_mode)) = "archive"
      in
      let new_is_archive =
        String.(lowercase_ascii (trim node.history_mode)) = "archive"
      in
      let new_snapshot =
        if
          (is_auto_snapshot m.snapshot
          || (m.snapshot = `None && old_is_archive && not new_is_archive))
          && ((not (String.equal m.node.network node.network))
             || not (String.equal m.node.history_mode node.history_mode))
        then
          create_default_snapshot
            ~network:node.network
            ~history_mode:node.history_mode
        else m.snapshot
      in
      {
        m with
        node =
          {
            node with
            data_dir =
              (if should_update_data_dir then new_data_dir else node.data_dir);
          };
        core = {m.core with instance_name = new_name};
        snapshot = new_snapshot;
      }
    else {m with node}

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  {
    title = " Install Node ";
    initial_model = make_initial_model;
    fields =
      (fun model ->
        (* 1. Dependencies - none for node *)
        (* 2. Network params: network, history mode, snapshot, tmp_dir, keep_snapshot *)
        node_fields
          ~get_node:(fun m -> m.node)
          ~set_node:set_node_with_autoname
          ~on_network_selected:prefetch_snapshot_list
          ~edit_mode:model.edit_mode
          ?editing_instance:model.original_instance
          ~skip_data_dir:true
          ~skip_addresses:true
          ()
        (* Snapshot field only shown for new installs, not edit mode, and not for archive mode *)
        @ (if
             model.edit_mode
             || String.(lowercase_ascii (trim model.node.history_mode))
                = "archive"
           then []
           else
             [
               snapshot_field
               |> with_hint
                    "Import a snapshot for faster sync. None = sync from \
                     genesis (slow).";
             ])
        (* Download directory field, only shown when snapshot is selected *)
        @ (if model.edit_mode || model.snapshot = `None then []
           else
             [
               tmp_dir_field
               |> with_hint
                    "Custom directory for large snapshot downloads. Use if \
                     /tmp has insufficient space.";
             ])
        (* Keep snapshot field, only shown when snapshot is selected *)
        @ (if model.edit_mode || model.snapshot = `None then []
           else
             [
               keep_snapshot_field
               |> with_hint
                    "Keep the downloaded snapshot file after import instead of \
                     deleting it.";
             ])
        (* Check snapshot field, only shown when snapshot is selected *)
        @ (if model.edit_mode || model.snapshot = `None then []
           else
             [
               check_snapshot_field
               |> with_hint
                    "Verify snapshot integrity during import. Disable with \
                     --no-check for faster import (not recommended).";
             ])
        (* 3. App bin dir *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-node"
            ~subcommand:["run"]
            ~binary_validator:has_octez_node_binary
            ~skip_instance_name:true
            ~skip_extra_args:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ()
        (* 4. Data dir *)
        @ node_fields
            ~get_node:(fun m -> m.node)
            ~set_node:(fun node m -> {m with node})
            ~edit_mode:model.edit_mode
            ?editing_instance:model.original_instance
            ~skip_network:true
            ~skip_addresses:true
            ()
        (* 5. Role-specific - none for node *)
        (* 6. Addresses and ports *)
        @ node_fields
            ~get_node:(fun m -> m.node)
            ~set_node:(fun node m -> {m with node})
            ~edit_mode:model.edit_mode
            ?editing_instance:model.original_instance
            ~skip_network:true
            ~skip_data_dir:true
            ()
        (* 7. Extra args *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-node"
            ~subcommand:["run"]
            ~binary_validator:has_octez_node_binary
            ~skip_instance_name:true
            ~skip_app_bin_dir:true
            ~skip_service_fields:true
            ~edit_mode:model.edit_mode
            ()
        (* 8. Service fields: service user, enable on boot, start now *)
        @ core_service_fields
            ~get_core:(fun m -> m.core)
            ~set_core:(fun core m -> {m with core})
            ~binary:"octez-node"
            ~subcommand:["run"]
            ~binary_validator:has_octez_node_binary
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
                (* In edit mode, never change data_dir - data is already there *)
                if m.edit_mode then {m with core = new_core}
                else
                  let default_dir =
                    Common.default_role_dir "node" instance_name
                  in
                  let keep_data_dir =
                    String.trim m.node.data_dir <> ""
                    && not
                         (String.equal
                            m.node.data_dir
                            (Common.default_role_dir "node" old))
                  in
                  let new_node =
                    {
                      m.node with
                      data_dir =
                        (if keep_data_dir then m.node.data_dir else default_dir);
                    }
                  in
                  {m with core = new_core; node = new_node})
              ~validate:(fun m ->
                (* Use non-blocking cache to avoid syscalls during typing *)
                let states =
                  Form_builder_common.cached_service_states_nonblocking ()
                in
                if not (Form_builder_common.is_nonempty m.core.instance_name)
                then Error "Instance name is required"
                else if
                  Form_builder_common.instance_in_use
                    ~states
                    m.core.instance_name
                  && not
                       (m.edit_mode
                       && m.original_instance = Some m.core.instance_name)
                then Error "Instance name already exists"
                else Ok ())
            |> with_hint
                 "Unique identifier for this node. Used in systemd unit and \
                  default paths.";
          ]);
    pre_submit = None;
    on_init = Some (fun model -> prefetch_snapshot_list model.node.network);
    on_refresh = None;
    pre_submit_modal =
      Some
        (fun model ->
          (* Only show modal if user hasn't already made a choice *)
          match model.preserve_data with
          | `Keep | `Refresh ->
              None (* User already chose, proceed with submit *)
          | `Auto ->
              (* Check if data_dir exists and has content *)
              let trimmed_dir = String.trim model.node.data_dir in
              let effective_dir =
                if trimmed_dir = "" then
                  Common.default_data_dir model.core.instance_name
                else trimmed_dir
              in
              let data_exists = Sys.file_exists effective_dir in
              if data_exists && Sys.is_directory effective_dir then
                let is_empty =
                  try
                    let entries = Sys.readdir effective_dir in
                    Array.length entries = 0
                  with _ -> false
                in
                if is_empty then None
                else
                  Some
                    (Form_builder.PreSubmitModal
                       {
                         title = "Data Directory Exists";
                         message =
                           Some
                             (Printf.sprintf
                                "Directory '%s' already exists with data."
                                effective_dir);
                         choices = [`Refresh; `Keep];
                         to_string =
                           (function
                           | `Refresh -> "Refresh (import snapshot)"
                           | `Keep -> "Keep (preserve existing)");
                         on_choice =
                           (fun (choice : [`Refresh | `Keep]) model ->
                             {
                               model with
                               preserve_data = (choice :> preserve_data);
                             });
                       })
              else None);
    on_submit =
      (fun model ->
        let history_mode =
          match History_mode.of_string model.node.history_mode with
          | Ok hm -> hm
          | Error _ -> History_mode.default
        in

        (* Always use journald - octez binaries handle their own file logging *)
        let logging_mode = Logging_mode.default in

        let extra_args =
          Form_builder_common.prepare_extra_args model.core.extra_args
        in

        let data_dir =
          let trimmed = String.trim model.node.data_dir in
          if trimmed = "" then Common.default_data_dir model.core.instance_name
          else trimmed
        in

        (* Resolve bootstrap method *)
        let bootstrap =
          match model.snapshot with
          | `None -> Genesis
          | `Url url -> Snapshot {src = Some url}
          | `Tzinit _snap -> Snapshot {src = None}
        in

        let req : Installer_types.node_request =
          {
            instance = model.core.instance_name;
            network = model.node.network;
            history_mode;
            data_dir = Some data_dir;
            rpc_addr = model.node.rpc_addr;
            net_addr = model.node.p2p_addr;
            service_user = model.core.service_user;
            app_bin_dir = model.core.app_bin_dir;
            logging_mode;
            extra_args;
            auto_enable = model.core.enable_on_boot;
            bootstrap;
            preserve_data = model.preserve_data = `Keep;
            snapshot_no_check = not model.check_snapshot;
            tmp_dir = model.tmp_dir;
            keep_snapshot = model.keep_snapshot;
          }
        in

        let description =
          if model.edit_mode then
            Printf.sprintf "Edit node %s" model.core.instance_name
          else Printf.sprintf "Install node %s" model.core.instance_name
        in
        Job_manager.submit
          ~description
          (fun ~append_log () ->
            (* In edit mode, stop the service and dependents before applying changes *)
            let* () =
              if model.edit_mode then (
                (* Use original instance name when stopping (may be different if renaming) *)
                let stop_instance =
                  Option.value
                    ~default:model.core.instance_name
                    model.original_instance
                in
                append_log
                  (Printf.sprintf
                     "Stopping service %s before applying changes...\n"
                     stop_instance) ;
                let stop_result =
                  try
                    Installer.stop_service
                      ~quiet:true
                      ~instance:stop_instance
                      ()
                  with exn ->
                    append_log
                      (Printf.sprintf
                         "Exception in stop_service: %s\n"
                         (Printexc.to_string exn)) ;
                    Ok () (* Continue anyway *)
                in
                match stop_result with
                | Ok () ->
                    append_log "Stop service completed OK\n" ;
                    Ok ()
                | Error (`Msg msg) ->
                    append_log (Printf.sprintf "Warning: %s\n" msg) ;
                    Ok ()
                (* Continue anyway - service might already be stopped *))
              else Ok ()
            in
            append_log "Ensuring service account...\n" ;
            let* () =
              if Common.is_root () then
                System_user.ensure_service_account
                  ~quiet:true
                  ~name:model.core.service_user
                  ()
              else Ok ()
            in
            append_log "Getting package manager...\n" ;
            let* (module PM) = require_package_manager () in
            append_log "Starting install_node...\n" ;
            let result = PM.install_node ~quiet:true ~on_log:append_log req in
            (match result with
            | Ok _ -> append_log "install_node succeeded\n"
            | Error (`Msg e) ->
                append_log (Printf.sprintf "install_node failed: %s\n" e)) ;
            let* _service = result in
            (* Handle rename: clean up old instance if name changed *)
            let* () =
              match (model.edit_mode, model.original_instance) with
              | true, Some old_name when old_name <> model.core.instance_name ->
                  append_log
                    (Printf.sprintf
                       "Renaming instance from %s to %s...\n"
                       old_name
                       model.core.instance_name) ;
                  Installer.cleanup_renamed_instance
                    ~quiet:true
                    ~old_instance:old_name
                    ~new_instance:model.core.instance_name
                    ()
              | _ -> Ok ()
            in
            (* Queue restart dependents for modal on instances page *)
            if model.edit_mode && model.stopped_dependents <> [] then
              Context.set_pending_restart_dependents model.stopped_dependents ;
            (* Start the service if requested, even if not enabling on boot *)
            if model.core.start_now && not model.core.enable_on_boot then
              match Miaou_interfaces.Service_lifecycle.get () with
              | Some sl ->
                  Miaou_interfaces.Service_lifecycle.start
                    sl
                    ~role:"node"
                    ~service:model.core.instance_name
                  |> Result.map_error (fun e -> `Msg e)
              | None ->
                  Error (`Msg "Service lifecycle capability not available")
            else Ok ())
          ~on_complete:(fun status ->
            match status with
            | Job_manager.Succeeded ->
                Context.toast_success
                  (Printf.sprintf
                     "Node %s installed successfully"
                     model.core.instance_name) ;
                (* Invalidate caches and mark instances dirty to refresh UI *)
                Context.mark_instances_dirty ()
            | Job_manager.Failed msg ->
                (* Log to debug file for troubleshooting *)
                let log_msg =
                  match Job_manager.get_latest_job () with
                  | Some job -> String.concat "" (List.rev job.log)
                  | None -> "(no job log)"
                in
                Common.append_debug_log
                  (Printf.sprintf
                     "Install failed: %s\nJob log:\n%s"
                     msg
                     log_msg) ;
                Context.toast_error
                  (Printf.sprintf
                     "Failed to install node %s: %s"
                     model.core.instance_name
                     (if msg = "" then log_msg else msg))
            | _ -> ()) ;
        Ok ());
  }

module Page = Form_builder.Make (struct
  type nonrec model = model

  let spec = spec
end)

module For_tests = struct
  let clear_snapshot_cache () =
    Cache.invalidate_safe_keyed snapshot_cache ;
    Mutex.protect snapshot_inflight_lock (fun () ->
        Hashtbl.reset snapshot_inflight)

  let set_snapshot_cache ~network ~entries =
    match slug_of_network network with
    | None -> ()
    | Some slug -> cache_snapshot slug entries

  let snapshot_entry_matches_history_mode = snapshot_entry_matches_history_mode

  let history_snapshot_conflict = history_snapshot_conflict
end

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
