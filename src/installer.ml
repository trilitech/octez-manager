(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types

let ( let* ) = Result.bind

let invalid_instance_name_chars_msg =
  "Only alphanumeric characters (a-z, A-Z, 0-9), hyphens (-), underscores (_), \
   and dots (.) are allowed."

let backup_file_if_exists path =
  let normalized = String.trim path in
  if normalized = "" then Ok None
  else if Sys.file_exists normalized then (
    let tmp = Filename.temp_file "octez-manager.backup" ".tmp" in
    match Common.copy_file normalized tmp with
    | Ok () -> Ok (Some {tmp_path = tmp; original_path = normalized})
    | Error _ as e ->
        Common.remove_path tmp ;
        e)
  else Ok None

let ensure_backup_parent ~owner ~group path =
  let dir = Filename.dirname path in
  if dir = "" || dir = "." then Ok ()
  else if Sys.file_exists dir then Ok ()
  else Common.ensure_dir_path ~owner ~group ~mode:0o755 dir

let restore_backup ~owner ~group backup_opt =
  match backup_opt with
  | None -> Ok ()
  | Some backup -> (
      let* () = ensure_backup_parent ~owner ~group backup.original_path in
      match Common.copy_file backup.tmp_path backup.original_path with
      | Ok () ->
          Common.remove_path backup.tmp_path ;
          Ok ()
      | Error _ as e -> e)

let normalize_optional_string = function
  | Some s ->
      let trimmed = String.trim s in
      if trimmed = "" then None else Some trimmed
  | None -> None

(* Logging is via journald - no logrotate needed *)
let logrotate_specs_of (_services : Service.t list) = []

let is_http_url s =
  let trimmed = String.trim s |> String.lowercase_ascii in
  String.starts_with ~prefix:"http://" trimmed
  || String.starts_with ~prefix:"https://" trimmed

let is_file_uri s =
  let trimmed = String.trim s |> String.lowercase_ascii in
  String.starts_with ~prefix:"file://" trimmed

let strip_file_uri s =
  if is_file_uri s then
    let prefix_len = String.length "file://" in
    Some (String.sub s prefix_len (String.length s - prefix_len))
  else None

let history_mode_matches ~requested ~snapshot_mode =
  let requested_str = History_mode.to_string requested in
  let requested_lower = String.lowercase_ascii requested_str in
  let snapshot_lower = String.lowercase_ascii (String.trim snapshot_mode) in
  requested_lower = snapshot_lower

let resolve_snapshot_download ~network ~history_mode =
  let* network_slug =
    match Snapshots.slug_of_network network with
    | Some slug -> Ok slug
    | None ->
        R.error_msg
          "Unable to infer a tzinit network slug from --network. Provide \
           either a known alias (mainnet/ghostnet/...) or a teztnets JSON URL."
  in
  let* kind_slug, kind_label =
    match history_mode with
    | History_mode.Rolling -> Ok ("rolling", "rolling")
    | History_mode.Full -> Ok ("full", "full")
    | History_mode.Archive ->
        R.error_msg
          "Snapshots are not provided for archive history mode. Download a \
           full archive manually, extract it into your target data directory, \
           then rerun octez-manager without --snapshot but with --data-dir \
           pointing to that directory."
  in
  let* entry_opt =
    Snapshots.fetch_entry ~network_slug ~slug:kind_slug ~label:kind_label
  in
  match entry_opt with
  | None ->
      R.error_msgf
        "Snapshot kind '%s' is not advertised for %s. Run `octez-manager \
         list-snapshots --network %s` to inspect available downloads."
        kind_label
        network
        network
  | Some entry -> (
      match entry.history_mode with
      | Some snapshot_mode when String.trim snapshot_mode <> "" -> (
          if not (history_mode_matches ~requested:history_mode ~snapshot_mode)
          then
            R.error_msgf
              "Snapshot '%s' has history mode '%s' but requested history mode \
               is '%s'. Please select a snapshot with the correct history mode \
               or change your history mode selection to match."
              entry.label
              snapshot_mode
              (History_mode.to_string history_mode)
          else
            match entry.download_url with
            | Some url ->
                Ok {download_url = String.trim url; network_slug; kind_slug}
            | None ->
                R.error_msgf
                  "Snapshot kind '%s' for %s exposes no HTTPS download on \
                   tzinit."
                  entry.label
                  network)
      | Some _ | None -> (
          (* If history mode is not available in metadata, proceed with caution *)
          match entry.download_url with
          | Some url ->
              Ok {download_url = String.trim url; network_slug; kind_slug}
          | None ->
              R.error_msgf
                "Snapshot kind '%s' for %s exposes no HTTPS download on tzinit."
                entry.label
                network))

type snapshot_file = {path : string; cleanup : bool}

type snapshot_progress = {
  on_download_progress : (int -> int option -> unit) option;
}

let download_snapshot ?(quiet = false) ?on_log ?progress ?tmp_dir src =
  let tmp =
    match tmp_dir with
    | Some dir ->
        let name =
          Printf.sprintf "octez-manager.snapshot.%d.snap" (Unix.getpid ())
        in
        Filename.concat dir name
    | None -> Filename.temp_file "octez-manager.snapshot" ".snap"
  in
  let last_log_time = ref 0. in
  let res =
    match (progress, on_log) with
    | Some {on_download_progress}, _ ->
        Common.download_file_with_progress
          ~url:src
          ~dest_path:tmp
          ~on_progress:(fun pct _ ->
            match on_download_progress with
            | Some f -> f pct (Some 100)
            | None -> ())
    | None, Some log ->
        (* Use progress download and convert to log messages *)
        Common.download_file_with_progress
          ~url:src
          ~dest_path:tmp
          ~on_progress:(fun pct _ ->
            (* Log every 5 seconds to avoid flooding *)
            let now = Unix.gettimeofday () in
            if now -. !last_log_time >= 5. then (
              last_log_time := now ;
              log (Printf.sprintf "Download progress: %d%%\n" pct)))
    | None, None -> Common.download_file ~quiet ~url:src ~dest_path:tmp ()
  in
  match res with
  | Ok () -> Ok {path = tmp; cleanup = true}
  | Error _ as e ->
      Common.remove_path tmp ;
      e

let prepare_snapshot_source ?(quiet = false) ?on_log ?progress ?tmp_dir src =
  let trimmed = String.trim src in
  if trimmed = "" then R.error_msg "Snapshot URI is empty"
  else
    match strip_file_uri trimmed with
    | Some path -> Ok {path; cleanup = false}
    | None when not (is_http_url trimmed) ->
        if Sys.file_exists trimmed then Ok {path = trimmed; cleanup = false}
        else R.error_msgf "Snapshot file %s does not exist" trimmed
    | _ -> download_snapshot ~quiet ?on_log ?progress ?tmp_dir trimmed

let snapshot_plan_of_request request =
  match request.bootstrap with
  | Genesis -> Ok No_snapshot
  | Snapshot {src} -> (
      match normalize_optional_string src with
      | Some uri -> Ok (Direct_snapshot {uri})
      | None ->
          resolve_snapshot_download
            ~network:request.network
            ~history_mode:request.history_mode
          |> Result.map (fun res -> Tzinit_snapshot res))

let snapshot_metadata_of_plan ~no_check = function
  | No_snapshot ->
      {
        auto = false;
        uri = None;
        network_slug = None;
        kind_slug = None;
        no_check;
      }
  | Direct_snapshot {uri} ->
      {
        auto = true;
        uri = Some uri;
        network_slug = None;
        kind_slug = None;
        no_check;
      }
  | Tzinit_snapshot res ->
      {
        auto = true;
        uri = None;
        network_slug = Some res.network_slug;
        kind_slug = Some res.kind_slug;
        no_check;
      }

let import_snapshot ?(quiet = false) ?on_log ~app_bin_dir ~data_dir
    ~snapshot_path ~no_check () =
  let octez_node = Filename.concat app_bin_dir "octez-node" in
  let args =
    let base =
      [octez_node; "snapshot"; "import"; "--data-dir"; data_dir; snapshot_path]
    in
    if no_check then base @ ["--no-check"] else base
  in
  (* Use streaming for real-time output when on_log is provided *)
  match on_log with
  | Some log ->
      (* Wrap log to detect and announce phases *)
      let current_phase = ref "" in
      let phase_log line =
        let announce phase =
          if !current_phase <> phase then (
            current_phase := phase ;
            log (Printf.sprintf "\n=== %s ===\n" phase))
        in
        (if String.length line > 0 then
           let lower = String.lowercase_ascii line in
           if
             String.sub lower 0 (min 10 (String.length lower)) |> fun s ->
             String.ends_with ~suffix:"retrieving" s
           then announce "Importing context"
           else if
             String.ends_with ~suffix:"integrity_check." lower
             || String.ends_with ~suffix:"integrity check" lower
           then announce "Integrity check"
           else if String.starts_with ~prefix:"storing floating" lower then
             announce "Storing blocks") ;
        log line
      in
      Common.run_streaming ~on_log:phase_log args
  | None -> Common.run ~quiet args

let import_snapshot_file ?(quiet = false) ?on_log ~app_bin_dir ~data_dir
    ~snapshot_file ~no_check () =
  import_snapshot
    ~quiet
    ?on_log
    ~app_bin_dir
    ~data_dir
    ~snapshot_path:snapshot_file.path
    ~no_check
    ()

let perform_snapshot_plan ?(quiet = false) ?on_log ?tmp_dir
    ?(keep_snapshot = false) ~plan ~app_bin_dir ~data_dir ~no_check () =
  let log msg = match on_log with Some f -> f msg | None -> () in
  let should_cleanup snapshot_file =
    snapshot_file.cleanup && not keep_snapshot
  in
  match plan with
  | No_snapshot -> Ok ()
  | Direct_snapshot {uri} ->
      log "\n=== Downloading snapshot ===\n" ;
      log (Printf.sprintf "From: %s\n" uri) ;
      let* snapshot_file =
        prepare_snapshot_source ~quiet ?on_log ?tmp_dir uri
      in
      log "\n=== Importing snapshot ===\n" ;
      Fun.protect
        ~finally:(fun () ->
          if should_cleanup snapshot_file then
            Common.remove_path snapshot_file.path)
        (fun () ->
          import_snapshot_file
            ~quiet
            ?on_log
            ~app_bin_dir
            ~data_dir
            ~snapshot_file
            ~no_check
            ())
  | Tzinit_snapshot res ->
      log "\n=== Downloading snapshot ===\n" ;
      log (Printf.sprintf "From: %s\n" res.download_url) ;
      let* snapshot_file =
        download_snapshot ~quiet ?on_log ?tmp_dir res.download_url
      in
      log "\n=== Importing snapshot ===\n" ;
      Fun.protect
        ~finally:(fun () ->
          if should_cleanup snapshot_file then
            Common.remove_path snapshot_file.path)
        (fun () ->
          import_snapshot_file
            ~quiet
            ?on_log
            ~app_bin_dir
            ~data_dir
            ~snapshot_file
            ~no_check
            ())

let perform_bootstrap ?(quiet = false) ?on_log ?tmp_dir ~plan
    ~(request : node_request) ~data_dir () =
  perform_snapshot_plan
    ~quiet
    ?on_log
    ?tmp_dir
    ~keep_snapshot:request.keep_snapshot
    ~plan
    ~app_bin_dir:request.app_bin_dir
    ~data_dir
    ~no_check:request.snapshot_no_check
    ()

let ensure_node_config ?(quiet = false) ~app_bin_dir ~data_dir ~network
    ~history_mode () =
  let config_path = Filename.concat data_dir "config.json" in
  if Sys.file_exists config_path then Ok ()
  else
    let octez_node = Filename.concat app_bin_dir "octez-node" in
    Common.run
      ~quiet
      [
        octez_node;
        "config";
        "init";
        "--network";
        network;
        "--data-dir";
        data_dir;
        "--history-mode";
        History_mode.to_string history_mode;
      ]

let normalize_data_dir instance = function
  | Some dir when dir <> "" -> dir
  | _ -> Common.default_data_dir instance

let endpoint_of_rpc rpc_addr =
  let trimmed = String.trim rpc_addr in
  if trimmed = "" then "http://127.0.0.1:8732"
  else if
    String.starts_with ~prefix:"http://" (String.lowercase_ascii trimmed)
    || String.starts_with ~prefix:"https://" (String.lowercase_ascii trimmed)
  then trimmed
  else "http://" ^ trimmed

let lookup_node_service instance =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | Some svc when String.equal (String.lowercase_ascii svc.Service.role) "node"
    ->
      Ok svc
  | Some svc ->
      R.error_msgf
        "Instance '%s' is a %s service, expected a node"
        instance
        svc.role
  | None -> R.error_msgf "Unknown instance '%s'" instance

let build_run_args ~network ~history_mode ~rpc_addr ~net_addr ~extra_args
    ~logging_mode:_ =
  let base =
    [
      "--network=" ^ network;
      "--history-mode=" ^ History_mode.to_string history_mode;
      "--rpc-addr=" ^ rpc_addr;
      "--net-addr=" ^ net_addr;
    ]
  in
  (* Logging is via journald - octez binaries handle their own file logging *)
  String.concat " " (base @ extra_args)

let prepare_logging ~instance:_ ~role:_ ~logging_mode:_ =
  (* Always use journald *)
  Logging_mode.default

(* Logging is via journald - no file setup needed *)
let ensure_logging_destination ~service_user:_ _logging_mode = Ok ()

let ensure_logging_base_directory ~owner:_ ~group:_ _logging_mode = Ok ()

let remove_logging_artifacts _logging_mode = Ok ()

let should_drop_service_user ~user ~remaining_services =
  let trimmed = String.trim user in
  if trimmed = "" then false
  else
    not
      (List.exists
         (fun (svc : Service.t) ->
           String.equal trimmed (String.trim svc.Service.service_user))
         remaining_services)

(* Logging is via journald - no file setup needed *)
let ensure_runtime_log_directory ~owner:_ ~group:_ _logging_mode = Ok ()

let ensure_directories ~owner ~group paths =
  let filtered =
    paths
    |> List.filter (fun p -> String.trim p <> "")
    |> List.sort_uniq String.compare
  in
  List.fold_left
    (fun acc dir ->
      let* () = acc in
      Common.ensure_dir_path ~owner ~group ~mode:0o755 dir)
    (Ok ())
    filtered

let reown_runtime_paths ~owner ~group ~paths ~logging_mode:_ =
  let normalize =
    paths
    |> List.filter (fun p -> String.trim p <> "")
    |> List.sort_uniq String.compare
  in
  (* Logging is via journald - no file ownership needed *)
  List.fold_left
    (fun acc dir ->
      let* () = acc in
      Common.ensure_tree_owner ~owner ~group dir)
    (Ok ())
    normalize

let is_valid_instance_char c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
  | _ -> false

let validate_instance_name_chars ~instance =
  if String.length instance = 0 then
    R.error_msgf "Instance name cannot be empty."
  else if not (String.for_all is_valid_instance_char instance) then
    R.error_msgf
      "Instance name '%s' contains invalid characters. %s"
      instance
      invalid_instance_name_chars_msg
  else Ok ()

let validate_instance_name_unique ~instance =
  let* services = Service_registry.list () in
  let existing =
    List.find_opt
      (fun svc -> String.equal svc.Service.instance instance)
      services
  in
  match existing with
  | Some svc ->
      R.error_msgf
        "Instance name '%s' is already in use by a %s service. Please choose a \
         different name."
        instance
        svc.Service.role
  | None -> Ok ()

let validate_instance_name ?(allow_existing = false) ~instance () =
  let* () = validate_instance_name_chars ~instance in
  if allow_existing then Ok () else validate_instance_name_unique ~instance

let resolve_from_data_dir data_dir =
  let ( let* ) = Result.bind in
  let config_path = Filename.concat data_dir "config.json" in
  if not (Sys.file_exists config_path) then Ok (`Path data_dir)
  else
    let json = Yojson.Safe.from_file config_path in
    let open Yojson.Safe.Util in
    let fail field =
      Error (Format.sprintf "Cannot read %S from %S" field config_path)
    in
    let* network =
      try
        match member "network" json |> member "chain_name" with
        | `String s ->
            let* network =
              Result.map_error (fun (`Msg s) -> s)
              @@ Teztnets.resolve_network_from_node_chain s
            in
            Ok network.alias
        | _ -> fail ".network.chain_name"
      with _ -> Ok "mainnet"
    in
    let* history_mode =
      try
        match member "shell" json |> member "history_mode" with
        | `String s ->
            Result.map_error (fun (`Msg s) -> s) @@ History_mode.of_string s
        | `Assoc [("full", _)] -> Ok History_mode.Full
        | `Assoc [("rolling", _)] -> Ok History_mode.Rolling
        | `Assoc [("archive", _)] -> Ok History_mode.Archive
        | _ -> fail ".p2p.shell.history_mode"
      with _ -> Ok History_mode.default
    in
    let* rpc_addr =
      try
        match member "rpc" json |> member "listen-addrs" with
        | `List [`String s] -> Ok s
        | `List _ ->
            Error
              (Format.sprintf
                 "Multiple rpc addresses listed in %S are not supported"
                 config_path)
        | _ -> Ok "127.0.0.1:8732"
      with _ -> Ok "127.0.0.1:8732"
    in
    let net_addr =
      match member "p2p" json |> member "listen-addr" with
      | `String s -> s
      | _ -> "0.0.0.0:9732"
    in
    Ok (`Data_dir {network; history_mode; rpc_addr; net_addr})

(** Update endpoint references in dependent services when a service's RPC address changes.
    For nodes: updates OCTEZ_NODE_ENDPOINT in baker/accuser/dal-node env files.
    For DAL nodes: updates OCTEZ_DAL_CONFIG in baker env files. *)
let update_dependent_endpoints ~instance ~role ~new_rpc_addr () =
  let new_endpoint = endpoint_of_rpc new_rpc_addr in
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> Ok ()
  | Some svc ->
      List.fold_left
        (fun acc dep_inst ->
          let* () = acc in
          match Node_env.read ~inst:dep_inst with
          | Ok pairs ->
              let updated_pairs =
                List.map
                  (fun (k, v) ->
                    (* For node: update OCTEZ_NODE_ENDPOINT in dependents *)
                    if role = "node" && k = "OCTEZ_NODE_ENDPOINT" then
                      (k, new_endpoint)
                      (* For DAL node: update OCTEZ_DAL_CONFIG in baker dependents *)
                    else if
                      (role = "dal-node" || role = "dal")
                      && k = "OCTEZ_DAL_CONFIG"
                    then (k, new_endpoint)
                    else (k, v))
                  pairs
              in
              Node_env.write_pairs ~inst:dep_inst updated_pairs
          | Error _ -> Ok ())
        (Ok ())
        svc.dependents

let install_node ?(quiet = false) ?on_log (request : node_request) =
  let log msg = match on_log with Some f -> f msg | None -> () in
  log "Validating instance name...\n" ;
  let* () =
    validate_instance_name
      ~allow_existing:request.preserve_data
      ~instance:request.instance
      ()
  in
  log "Resolving network...\n" ;
  let* resolved_network =
    Teztnets.resolve_network_for_octez_node request.network
  in
  let data_dir = normalize_data_dir request.instance request.data_dir in
  let logging_mode =
    prepare_logging
      ~instance:request.instance
      ~role:"node"
      ~logging_mode:request.logging_mode
  in
  let data_dir_nonempty =
    let trimmed = String.trim data_dir in
    if trimmed = "" || not (Sys.file_exists trimmed) then false
    else
      try
        let st = Unix.stat trimmed in
        st.Unix.st_kind = Unix.S_DIR
        &&
        let entries = Sys.readdir trimmed in
        Array.exists (fun e -> e <> "." && e <> "..") entries
      with Unix.Unix_error _ | Sys_error _ -> false
  in
  log "Computing snapshot plan...\n" ;
  let* snapshot_plan = snapshot_plan_of_request request in
  let snapshot_meta =
    snapshot_metadata_of_plan ~no_check:request.snapshot_no_check snapshot_plan
  in
  log "Ensuring service account...\n" ;
  let* () =
    System_user.ensure_service_account ~quiet ~name:request.service_user ()
  in
  log "Ensuring system directories...\n" ;
  let* () =
    if Common.is_root () then
      System_user.ensure_system_directories
        ~user:request.service_user
        ~group:request.service_user
        ()
    else Ok ()
  in
  log "Ensuring logging destination...\n" ;
  let* () =
    ensure_logging_destination ~service_user:request.service_user logging_mode
  in
  let run_args =
    build_run_args
      ~network:resolved_network
      ~history_mode:request.history_mode
      ~rpc_addr:request.rpc_addr
      ~net_addr:request.net_addr
      ~extra_args:request.extra_args
      ~logging_mode
  in
  log "Validating user for service...\n" ;
  let* () = System_user.validate_user_for_service ~user:request.service_user in
  let owner, group =
    if Common.is_root () then (request.service_user, request.service_user)
    else Common.current_user_group_names ()
  in
  log (Printf.sprintf "Owner: %s, Group: %s\n" owner group) ;
  let* () =
    if data_dir_nonempty && not request.preserve_data then (
      log "Removing existing data directory...\n" ;
      Common.remove_tree data_dir)
    else Ok ()
  in
  log "Ensuring directories...\n" ;
  let* () = ensure_directories ~owner ~group [data_dir] in
  log "Ensuring logging base directory...\n" ;
  let* () = ensure_logging_base_directory ~owner ~group logging_mode in
  log "Ensuring runtime log directory...\n" ;
  let* () = ensure_runtime_log_directory ~owner ~group logging_mode in
  log "Ensuring node config...\n" ;
  let* () =
    ensure_node_config
      ~quiet
      ~app_bin_dir:request.app_bin_dir
      ~data_dir
      ~network:resolved_network
      ~history_mode:request.history_mode
      ()
  in
  log "Performing bootstrap...\n" ;
  let* () =
    if request.preserve_data then (
      log "Skipping bootstrap (preserve_data=true)\n" ;
      Ok ())
    else
      perform_bootstrap ~quiet ?on_log ~plan:snapshot_plan ~request ~data_dir ()
  in
  log "Reowning runtime paths...\n" ;
  let* () = reown_runtime_paths ~owner ~group ~paths:[data_dir] ~logging_mode in
  log "Creating service record...\n" ;
  (* In edit mode, preserve existing dependents list *)
  let existing_dependents =
    if request.preserve_data then
      match Service_registry.find ~instance:request.instance with
      | Ok (Some existing) -> existing.Service.dependents
      | _ -> []
    else []
  in
  let service =
    Service.make
      ~instance:request.instance
      ~role:"node"
      ~network:request.network
      ~history_mode:request.history_mode
      ~data_dir
      ~rpc_addr:request.rpc_addr
      ~net_addr:request.net_addr
      ~service_user:request.service_user
      ~app_bin_dir:request.app_bin_dir
      ~logging_mode
      ~snapshot_auto:snapshot_meta.auto
      ~snapshot_uri:snapshot_meta.uri
      ~snapshot_network_slug:snapshot_meta.network_slug
      ~snapshot_no_check:snapshot_meta.no_check
      ~extra_args:request.extra_args
      ~dependents:existing_dependents
      ()
  in
  log "Installing systemd unit...\n" ;
  let* () =
    Systemd.install_unit
      ~quiet
      ~role:"node"
      ~app_bin_dir:request.app_bin_dir
      ~user:request.service_user
      ()
  in
  log "Building extra env...\n" ;
  let extra_env =
    [
      ("OCTEZ_NETWORK", request.network);
      ("OCTEZ_HISTORY_MODE", History_mode.to_string request.history_mode);
      ("OCTEZ_SNAPSHOT_AUTO", if snapshot_meta.auto then "1" else "0");
      ("OCTEZ_SNAPSHOT_URI", Option.value ~default:"" snapshot_meta.uri);
      ( "OCTEZ_SNAPSHOT_NETWORK_SLUG",
        Option.value ~default:"" snapshot_meta.network_slug );
      ("OCTEZ_SNAPSHOT_KIND", Option.value ~default:"" snapshot_meta.kind_slug);
      ("OCTEZ_SNAPSHOT_NO_CHECK", if snapshot_meta.no_check then "1" else "0");
    ]
  in
  log "Writing node env...\n" ;
  let* () =
    Node_env.write ~inst:request.instance ~data_dir ~run_args ~extra_env
  in
  log "Writing systemd dropin...\n" ;
  let* () =
    Systemd.write_dropin_node
      ~quiet
      ~inst:request.instance
      ~data_dir
      ~logging_mode
      ()
  in
  log "Writing service registry...\n" ;
  let* () = Service_registry.write service in
  (* In edit mode, update dependent endpoints if RPC address changed *)
  let* () =
    if request.preserve_data then (
      log "Updating dependent endpoints...\n" ;
      update_dependent_endpoints
        ~instance:request.instance
        ~role:"node"
        ~new_rpc_addr:request.rpc_addr
        ())
    else Ok ()
  in
  log "Listing services for logrotate...\n" ;
  let* services = Service_registry.list () in
  log "Syncing logrotate...\n" ;
  let* () = Systemd.sync_logrotate (logrotate_specs_of services) in
  log "Enabling service...\n" ;
  let* () =
    if request.auto_enable then
      Systemd.enable
        ~quiet
        ~role:"node"
        ~instance:request.instance
        ~start_now:true
        ()
    else Ok ()
  in
  log "Install complete!\n" ;
  Ok service

let install_daemon ?(quiet = false) (request : daemon_request) =
  let* () =
    validate_instance_name
      ~allow_existing:request.preserve_data
      ~instance:request.instance
      ()
  in
  let logging_mode =
    prepare_logging
      ~instance:request.instance
      ~role:request.role
      ~logging_mode:request.logging_mode
  in
  let* () =
    System_user.ensure_service_account ~quiet ~name:request.service_user ()
  in
  let* () =
    if Common.is_root () then
      System_user.ensure_system_directories
        ~user:request.service_user
        ~group:request.service_user
        ()
    else Ok ()
  in
  let* () =
    ensure_logging_destination ~service_user:request.service_user logging_mode
  in
  let* () = System_user.validate_user_for_service ~user:request.service_user in
  let owner, group =
    if Common.is_root () then (request.service_user, request.service_user)
    else Common.current_user_group_names ()
  in
  let directories = request.data_dir :: request.extra_paths in
  let* () = ensure_directories ~owner ~group directories in
  let* () = ensure_logging_base_directory ~owner ~group logging_mode in
  let* () = ensure_runtime_log_directory ~owner ~group logging_mode in
  let extra_env =
    let service_args = String.concat " " request.service_args |> String.trim in
    let args_entry =
      if service_args = "" then [] else [("OCTEZ_SERVICE_ARGS", service_args)]
    in
    ("OCTEZ_DATA_DIR", request.data_dir)
    :: ("OCTEZ_NETWORK", request.network)
    :: args_entry
    @ request.extra_env
  in
  let* () = Node_env.write_pairs ~inst:request.instance extra_env in
  let* () =
    Systemd.install_unit
      ~quiet
      ~role:request.role
      ~app_bin_dir:request.app_bin_dir
      ~user:request.service_user
      ()
  in
  (* Resolve depends_on to (parent_role, parent_instance) tuple for systemd *)
  let depends_on_for_systemd =
    match request.depends_on with
    | None -> None
    | Some parent_instance -> (
        match Service_registry.find ~instance:parent_instance with
        | Ok (Some parent_svc) -> Some (parent_svc.Service.role, parent_instance)
        | _ -> None)
  in
  let* () =
    Systemd.write_dropin
      ~role:request.role
      ~inst:request.instance
      ~data_dir:request.data_dir
      ~logging_mode
      ~extra_paths:request.extra_paths
      ?depends_on:depends_on_for_systemd
      ()
  in
  let* () =
    reown_runtime_paths ~owner ~group ~paths:directories ~logging_mode
  in
  (* In edit mode, preserve existing dependents list *)
  let existing_dependents =
    if request.preserve_data then
      match Service_registry.find ~instance:request.instance with
      | Ok (Some existing) -> existing.Service.dependents
      | _ -> []
    else []
  in
  let service =
    Service.make
      ~instance:request.instance
      ~role:request.role
      ~network:request.network
      ~history_mode:request.history_mode
      ~data_dir:request.data_dir
      ~rpc_addr:request.rpc_addr
      ~net_addr:request.net_addr
      ~service_user:request.service_user
      ~app_bin_dir:request.app_bin_dir
      ~logging_mode
      ~extra_args:request.service_args
      ~depends_on:request.depends_on
      ~dependents:existing_dependents
      ()
  in
  let* () = Service_registry.write service in
  (* In edit mode, update dependent endpoints if RPC address changed (for DAL nodes) *)
  let* () =
    if
      request.preserve_data
      && (request.role = "dal-node" || request.role = "dal")
    then
      update_dependent_endpoints
        ~instance:request.instance
        ~role:request.role
        ~new_rpc_addr:request.rpc_addr
        ()
    else Ok ()
  in
  (* Register as dependent on parent if depends_on is set *)
  let* () =
    match request.depends_on with
    | Some parent_instance -> (
        match Service_registry.find ~instance:parent_instance with
        | Ok (Some parent_svc) ->
            (* Only add if not already in dependents list *)
            if List.mem request.instance parent_svc.dependents then Ok ()
            else
              let updated_parent =
                {
                  parent_svc with
                  dependents = request.instance :: parent_svc.dependents;
                }
              in
              Service_registry.write updated_parent
        | Ok None -> Ok () (* Parent not found, skip *)
        | Error _ -> Ok () (* Error finding parent, skip *))
    | None -> Ok ()
  in
  let* services = Service_registry.list () in
  let* () = Systemd.sync_logrotate (logrotate_specs_of services) in
  let* () =
    if request.auto_enable then
      Systemd.enable
        ~quiet
        ~role:request.role
        ~instance:request.instance
        ~start_now:true
        ()
    else Ok ()
  in
  Ok service

let install_baker ?(quiet = false) (request : baker_request) =
  let* node_mode : Installer_types.resolved_baker_node_mode =
    match request.node_mode with
    | Remote_endpoint endpoint -> Ok (Remote endpoint)
    | Local_instance inst ->
        let* svc = lookup_node_service inst in
        Ok (Local svc)
  in
  let node_data_dir =
    match node_mode with Remote _ -> "" | Local svc -> svc.Service.data_dir
  in
  let history_mode =
    match node_mode with
    | Local svc -> svc.Service.history_mode
    | Remote _ -> History_mode.default
  in
  let node_endpoint =
    match node_mode with
    | Remote endpoint -> endpoint_of_rpc endpoint
    | Local svc -> endpoint_of_rpc svc.Service.rpc_addr
  in
  let* network =
    match node_mode with
    | Local svc -> Ok svc.Service.network
    | Remote _ -> Teztnets.resolve_octez_node_chain ~endpoint:node_endpoint
  in
  let base_dir =
    match request.base_dir with
    | Some dir when String.trim dir <> "" -> dir
    | _ -> Common.default_role_dir "baker" request.instance
  in
  let dal_config =
    match request.dal_config with
    | Dal_endpoint ep when String.trim ep <> "" ->
        Dal_endpoint (endpoint_of_rpc ep)
    | Dal_disabled -> Dal_disabled
    | _ -> Dal_auto
  in
  let* liquidity_baking_vote =
    match request.liquidity_baking_vote with
    | Some vote when String.trim vote <> "" ->
        let normalized = String.lowercase_ascii (String.trim vote) in
        if normalized = "on" || normalized = "off" || normalized = "pass" then
          Ok normalized
        else
          R.error_msg
            (Printf.sprintf
               "Invalid liquidity baking vote '%s'. Must be 'on', 'off', or \
                'pass'."
               vote)
    | _ ->
        R.error_msg
          "Liquidity baking vote is required. Use --liquidity-baking-vote with \
           'on', 'off', or 'pass'."
  in
  let node_mode_env =
    match node_mode with Local _ -> "local" | Remote _ -> "remote"
  in
  (* Delegates are positional arguments, not --delegate flags *)
  let delegate_args = String.concat " " request.delegates |> String.trim in
  let extra_args_str = String.concat " " request.extra_args |> String.trim in
  let depends_on =
    match node_mode with
    | Local svc -> Some svc.Service.instance
    | Remote _ -> None
  in
  let* service =
    install_daemon
      ~quiet
      {
        role = "baker";
        instance = request.instance;
        network;
        history_mode;
        data_dir = node_data_dir;
        rpc_addr = node_endpoint;
        net_addr = "";
        service_user = request.service_user;
        app_bin_dir = request.app_bin_dir;
        logging_mode = request.logging_mode;
        service_args = [];
        extra_env =
          [
            ("OCTEZ_BAKER_BASE_DIR", base_dir);
            ("OCTEZ_NODE_ENDPOINT", node_endpoint);
            ( "OCTEZ_NODE_INSTANCE",
              match node_mode with
              | Local svc -> svc.Service.instance
              | Remote _ -> "" );
            ("OCTEZ_BAKER_NODE_MODE", node_mode_env);
            ( "OCTEZ_DAL_CONFIG",
              match dal_config with
              | Dal_disabled -> "disabled"
              | Dal_endpoint ep -> ep
              | Dal_auto -> "" );
            ("OCTEZ_DAL_INSTANCE", Option.value ~default:"" request.dal_node);
            ("OCTEZ_BAKER_DELEGATES_ARGS", delegate_args);
            ("OCTEZ_BAKER_DELEGATES_CSV", String.concat "," request.delegates);
            ("OCTEZ_BAKER_LB_VOTE", liquidity_baking_vote);
            ("OCTEZ_BAKER_EXTRA_ARGS", extra_args_str);
          ];
        extra_paths = [base_dir];
        auto_enable = request.auto_enable;
        depends_on;
        preserve_data = request.preserve_data;
      }
  in
  (* Register as dependent on parent node (avoid duplicates) *)
  let* () =
    match node_mode with
    | Local parent_svc ->
        if List.mem request.instance parent_svc.dependents then Ok ()
        else
          let updated_parent =
            {
              parent_svc with
              dependents = request.instance :: parent_svc.dependents;
            }
          in
          Service_registry.write updated_parent
    | Remote _ -> Ok ()
  in
  (* Register as dependent on DAL node if using local DAL (avoid duplicates) *)
  let* () =
    match request.dal_node with
    | Some dal_inst -> (
        match Service_registry.find ~instance:dal_inst with
        | Ok (Some dal_svc) ->
            if List.mem request.instance dal_svc.dependents then Ok ()
            else
              let updated_dal =
                {
                  dal_svc with
                  dependents = request.instance :: dal_svc.dependents;
                }
              in
              Service_registry.write updated_dal
        | _ -> Ok ())
    | None -> Ok ()
  in
  Ok service

let install_accuser ?(quiet = false) (request : accuser_request) =
  let* node_mode : Installer_types.resolved_baker_node_mode =
    match request.node_mode with
    | Remote_endpoint endpoint -> Ok (Remote endpoint)
    | Local_instance inst ->
        let* svc = lookup_node_service inst in
        Ok (Local svc)
  in
  let node_data_dir =
    match node_mode with Remote _ -> "" | Local svc -> svc.Service.data_dir
  in
  let history_mode =
    match node_mode with
    | Local svc -> svc.Service.history_mode
    | Remote _ -> History_mode.default
  in
  let node_endpoint =
    match node_mode with
    | Remote endpoint -> endpoint_of_rpc endpoint
    | Local svc -> endpoint_of_rpc svc.Service.rpc_addr
  in
  let* network =
    match node_mode with
    | Local svc -> Ok svc.Service.network
    | Remote _ -> Teztnets.resolve_octez_node_chain ~endpoint:node_endpoint
  in
  let base_dir =
    match request.base_dir with
    | Some dir when String.trim dir <> "" -> dir
    | _ -> Common.default_role_dir "accuser" request.instance
  in
  let extra_args_str = String.concat " " request.extra_args |> String.trim in
  let depends_on =
    match node_mode with
    | Local svc -> Some svc.Service.instance
    | Remote _ -> None
  in
  let* service =
    install_daemon
      ~quiet
      {
        role = "accuser";
        instance = request.instance;
        network;
        history_mode;
        data_dir = node_data_dir;
        rpc_addr = node_endpoint;
        net_addr = "";
        service_user = request.service_user;
        app_bin_dir = request.app_bin_dir;
        logging_mode = request.logging_mode;
        service_args = [];
        extra_env =
          [
            ("OCTEZ_CLIENT_BASE_DIR", base_dir);
            ("OCTEZ_NODE_ENDPOINT", node_endpoint);
            ( "OCTEZ_NODE_INSTANCE",
              match node_mode with
              | Local svc -> svc.Service.instance
              | Remote _ -> "" );
            ("OCTEZ_BAKER_EXTRA_ARGS", extra_args_str);
          ];
        extra_paths = [base_dir];
        auto_enable = request.auto_enable;
        depends_on;
        preserve_data = request.preserve_data;
      }
  in
  (* Register as dependent on parent node (avoid duplicates) *)
  let* () =
    match node_mode with
    | Local parent_svc ->
        if List.mem request.instance parent_svc.dependents then Ok ()
        else
          let updated_parent =
            {
              parent_svc with
              dependents = request.instance :: parent_svc.dependents;
            }
          in
          Service_registry.write updated_parent
    | Remote _ -> Ok ()
  in
  Ok service

let start_service ?quiet ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Check parent dependency is running *)
      let* () =
        match svc.depends_on with
        | None -> Ok ()
        | Some parent_instance -> (
            match Service_registry.find ~instance:parent_instance with
            | Ok (Some parent) -> (
                match
                  Systemd.is_active ~role:parent.role ~instance:parent_instance
                with
                | Ok true -> Ok ()
                | Ok false ->
                    R.error_msgf
                      "Cannot start %s: dependency '%s' is not running.\n\
                       Start it first with: octez-manager instance %s start"
                      instance
                      parent_instance
                      parent_instance
                | Error _ ->
                    R.error_msgf
                      "Cannot start %s: dependency '%s' is not running.\n\
                       Start it first with: octez-manager instance %s start"
                      instance
                      parent_instance
                      parent_instance)
            | _ ->
                (* Parent not found in registry, skip check *)
                Ok ())
      in
      Systemd.start ?quiet ~role:svc.role ~instance ()

let stop_service_cascade ?quiet ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Stop dependents first *)
      let* () =
        if svc.dependents <> [] then (
          if not (Option.value ~default:false quiet) then
            Printf.printf
              "Stopping dependents: %s\n"
              (String.concat ", " svc.dependents) ;
          List.fold_left
            (fun acc dep ->
              let* () = acc in
              (* Silently ignore missing dependents during cascade *)
              match Service_registry.find ~instance:dep with
              | Ok (Some dep_svc) ->
                  Systemd.stop ?quiet ~role:dep_svc.role ~instance:dep ()
              | _ -> Ok ())
            (Ok ())
            svc.dependents)
        else Ok ()
      in
      Systemd.stop ?quiet ~role:svc.role ~instance ()

let stop_service ?quiet ~instance () = stop_service_cascade ?quiet ~instance ()

let get_stopped_dependencies ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some _svc ->
      (* Collect all stopped parent dependencies *)
      let rec collect_deps acc inst =
        match Service_registry.find ~instance:inst with
        | Ok (Some s) -> (
            match s.depends_on with
            | None -> Ok acc
            | Some parent_inst -> (
                match Service_registry.find ~instance:parent_inst with
                | Ok (Some parent) -> (
                    match
                      Systemd.is_active ~role:parent.role ~instance:parent_inst
                    with
                    | Ok true -> collect_deps acc parent_inst
                    | Ok false | Error _ ->
                        (* Parent is stopped, add it and check its dependencies *)
                        collect_deps (parent :: acc) parent_inst)
                | _ -> Ok acc))
        | _ -> Ok acc
      in
      let* deps = collect_deps [] instance in
      (* Return in order: topmost parent first *)
      Ok (List.rev deps)

let get_stopped_dependents ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Collect all stopped dependents *)
      let stopped =
        List.filter_map
          (fun dep_inst ->
            match Service_registry.find ~instance:dep_inst with
            | Ok (Some dep) -> (
                match Systemd.is_active ~role:dep.role ~instance:dep_inst with
                | Ok true -> None
                | Ok false | Error _ -> Some dep)
            | _ -> None)
          svc.dependents
      in
      Ok stopped

let restart_service ?quiet ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | Some svc -> Systemd.restart ?quiet ~role:svc.role ~instance ()
  | None -> R.error_msgf "Instance '%s' not found" instance

let remove_service ?(quiet = false) ~delete_data_dir ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Stop dependents first (cascade stop) *)
      let* () =
        if svc.dependents <> [] then (
          if not quiet then
            Format.printf
              "Stopping dependents: %s@."
              (String.concat ", " svc.dependents) ;
          List.fold_left
            (fun acc dep ->
              let* () = acc in
              match Service_registry.find ~instance:dep with
              | Ok (Some dep_svc) ->
                  Systemd.stop ~quiet ~role:dep_svc.role ~instance:dep ()
              | _ -> Ok ())
            (Ok ())
            svc.dependents)
        else Ok ()
      in
      (* Unregister from parent's dependents list *)
      let* () =
        match svc.depends_on with
        | None -> Ok ()
        | Some parent_instance -> (
            match Service_registry.find ~instance:parent_instance with
            | Ok (Some parent) ->
                let updated_deps =
                  List.filter (( <> ) instance) parent.dependents
                in
                let updated_parent = {parent with dependents = updated_deps} in
                Service_registry.write updated_parent
            | _ -> Ok ())
      in
      let* () =
        Systemd.disable ~quiet ~role:svc.role ~instance ~stop_now:true ()
      in
      Systemd.remove_dropin ~role:svc.role ~instance ;
      let* () =
        match delete_data_dir with
        | true -> Common.remove_tree svc.data_dir
        | false -> Ok ()
      in
      let* () = Service_registry.remove ~instance in
      let* services = Service_registry.list () in
      Systemd.sync_logrotate (logrotate_specs_of services)

let purge_service ?(quiet = false) ~prompt_yes_no ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      let* () = remove_service ~quiet ~delete_data_dir:true ~instance () in
      let* () =
        let is_baker = svc.role = "baker" in
        let is_accuser = svc.role = "accuser" in
        if is_baker || is_accuser then
          let env =
            match Node_env.read ~inst:svc.instance with
            | Ok pairs -> pairs
            | Error _ -> []
          in
          let base_dir =
            List.assoc
              (if is_baker then "OCTEZ_BAKER_BASE_DIR"
               else "OCTEZ_CLIENT_BASE_DIR")
              env
          in
          let remove_base_dir =
            prompt_yes_no
              (Format.sprintf "Purge base-dir %S?" base_dir)
              ~default:false
          in
          if remove_base_dir then Common.remove_tree base_dir else Ok ()
        else Ok ()
      in
      (* Also remove per-instance env files under XDG_CONFIG_HOME or /etc when purging *)
      let env_dir =
        Filename.concat (Common.env_instances_base_dir ()) instance
      in
      let _ =
        (* Best-effort: don't fail purge if env removal fails *)
        match Common.remove_tree env_dir with
        | Ok () -> ()
        | Error _ -> ()
      in
      let* () = remove_logging_artifacts svc.logging_mode in
      let* remaining = Service_registry.list () in
      if
        should_drop_service_user
          ~user:svc.service_user
          ~remaining_services:remaining
      then System_user.remove_service_account ~quiet ~name:svc.service_user ()
      else Ok ()

let list_services () = Service_registry.list ()

(** Clean up old instance after rename.
    Removes service registry entry and systemd dropin but preserves data. *)
let cleanup_renamed_instance ?(quiet = false) ~old_instance ~new_instance () =
  let* svc_opt = Service_registry.find ~instance:old_instance in
  match svc_opt with
  | None -> Ok () (* Old instance already gone, nothing to clean *)
  | Some old_svc ->
      (* Stop the old service if still running *)
      let _ = Systemd.stop ~role:old_svc.role ~instance:old_instance () in
      (* Disable old service *)
      let* () =
        Systemd.disable
          ~quiet
          ~role:old_svc.role
          ~instance:old_instance
          ~stop_now:true
          ()
      in
      (* Remove old dropin *)
      Systemd.remove_dropin ~role:old_svc.role ~instance:old_instance ;
      (* Update dependents to point to new instance *)
      let* () =
        List.fold_left
          (fun acc dep_inst ->
            let* () = acc in
            (* Update OCTEZ_NODE_INSTANCE and OCTEZ_DAL_INSTANCE in dependent's env file *)
            match Node_env.read ~inst:dep_inst with
            | Ok pairs ->
                let updated_pairs =
                  List.map
                    (fun (k, v) ->
                      if
                        (k = "OCTEZ_NODE_INSTANCE" || k = "OCTEZ_DAL_INSTANCE")
                        && String.trim v = old_instance
                      then (k, new_instance)
                      else (k, v))
                    pairs
                in
                Node_env.write_pairs ~inst:dep_inst updated_pairs
            | Error _ -> Ok () (* Skip if can't read env *))
          (Ok ())
          old_svc.dependents
      in
      (* Remove old instance from parent's dependents list *)
      let* () =
        match old_svc.depends_on with
        | None -> Ok ()
        | Some parent_inst -> (
            match Service_registry.find ~instance:parent_inst with
            | Ok (Some parent) ->
                let updated_deps =
                  List.filter (( <> ) old_instance) parent.dependents
                in
                let updated_parent = {parent with dependents = updated_deps} in
                Service_registry.write updated_parent
            | _ -> Ok ())
      in
      (* Also check if old instance was in DAL node's dependents via env file *)
      let* () =
        match Node_env.read ~inst:old_instance with
        | Ok pairs -> (
            let dal_inst =
              List.assoc_opt "OCTEZ_DAL_INSTANCE" pairs
              |> Option.map String.trim |> Option.value ~default:""
            in
            if dal_inst = "" then Ok ()
            else
              match Service_registry.find ~instance:dal_inst with
              | Ok (Some dal_svc) ->
                  let updated_deps =
                    List.filter (( <> ) old_instance) dal_svc.dependents
                  in
                  let updated_dal = {dal_svc with dependents = updated_deps} in
                  Service_registry.write updated_dal
              | _ -> Ok ())
        | Error _ -> Ok ()
      in
      (* Transfer dependents to new service (deduplicate) *)
      let* new_svc_opt = Service_registry.find ~instance:new_instance in
      let* () =
        match new_svc_opt with
        | Some new_svc ->
            let merged_deps =
              old_svc.dependents @ new_svc.dependents
              |> List.sort_uniq String.compare
            in
            let updated_new = {new_svc with dependents = merged_deps} in
            Service_registry.write updated_new
        | None -> Ok ()
      in
      (* Remove old registry entry *)
      let* () = Service_registry.remove ~instance:old_instance in
      (* Remove old env files directory *)
      let old_env_dir =
        Filename.concat (Common.env_instances_base_dir ()) old_instance
      in
      let _ = Common.remove_tree old_env_dir in
      (* Sync logrotate *)
      let* services = Service_registry.list () in
      Systemd.sync_logrotate (logrotate_specs_of services)

let cleanup_dependencies () =
  let* services = Service_registry.list () in
  let all_instances =
    List.map (fun svc -> svc.Service.instance) services
    |> List.sort_uniq String.compare
  in
  let is_valid_dependent dep = List.mem dep all_instances in
  let updates =
    List.filter_map
      (fun svc ->
        let valid_deps =
          List.filter is_valid_dependent svc.Service.dependents
        in
        let stale_deps =
          List.filter (fun d -> not (is_valid_dependent d)) svc.dependents
        in
        if stale_deps <> [] then Some (svc, valid_deps, stale_deps) else None)
      services
  in
  let* cleaned_count =
    List.fold_left
      (fun acc (svc, valid_deps, stale_deps) ->
        let* count = acc in
        Printf.printf
          "Cleaning %s: removing stale dependents: %s\n"
          svc.Service.instance
          (String.concat ", " stale_deps) ;
        let updated_svc : Service.t = {svc with dependents = valid_deps} in
        let* () = Service_registry.write updated_svc in
        Ok (count + List.length stale_deps))
      (Ok 0)
      updates
  in
  Ok cleaned_count

let find_orphan_directories () =
  let* services = Service_registry.list () in
  let registered_dirs =
    List.map (fun svc -> svc.Service.data_dir) services
    |> List.sort_uniq String.compare
  in
  (* Base directories to scan for orphans *)
  let octez_data_base =
    if Common.is_root () then "/var/lib/octez"
    else Filename.concat (Common.xdg_data_home ()) "octez"
  in
  let octez_log_base =
    if Common.is_root () then "/var/log/octez"
    else Filename.concat (Common.xdg_state_home ()) "octez/logs"
  in
  let list_subdirs dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      Sys.readdir dir |> Array.to_list
      |> List.map (fun name -> Filename.concat dir name)
      |> List.filter Sys.is_directory
    else []
  in
  let list_files dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      Sys.readdir dir |> Array.to_list
      |> List.map (fun name -> Filename.concat dir name)
      |> List.filter (fun p -> Sys.file_exists p && not (Sys.is_directory p))
    else []
  in
  (* Find orphan data directories *)
  let all_data_dirs = list_subdirs octez_data_base in
  let orphan_data_dirs =
    List.filter (fun d -> not (List.mem d registered_dirs)) all_data_dirs
  in
  (* Find orphan log files *)
  let orphan_log_files = list_files octez_log_base in
  Ok (orphan_data_dirs, orphan_log_files)

let cleanup_orphans ~dry_run =
  let* orphan_dirs, orphan_logs = find_orphan_directories () in
  let removed = ref [] in
  let errors = ref [] in
  let process_path path =
    if dry_run then removed := path :: !removed
    else
      match Common.remove_tree path with
      | Ok () -> removed := path :: !removed
      | Error (`Msg msg) -> errors := (path, msg) :: !errors
  in
  List.iter process_path orphan_dirs ;
  List.iter process_path orphan_logs ;
  Ok (List.rev !removed, List.rev !errors)

let backup_file_if_exists_for_tests = backup_file_if_exists

let restore_backup_for_tests = restore_backup

module For_tests = struct
  type nonrec file_backup = file_backup

  let validate_instance_name_chars = validate_instance_name_chars

  let validate_instance_name_unique = validate_instance_name_unique

  let validate_instance_name = validate_instance_name

  let ensure_logging_base_directory = ensure_logging_base_directory

  let remove_logging_artifacts = remove_logging_artifacts

  let should_drop_service_user = should_drop_service_user

  let backup_file_if_exists ~path = backup_file_if_exists_for_tests path

  let restore_backup ~owner ~group backup =
    restore_backup_for_tests ~owner ~group backup

  let normalize_data_dir = normalize_data_dir

  let build_run_args = build_run_args

  let snapshot_plan_of_request = snapshot_plan_of_request

  let snapshot_metadata_of_plan = snapshot_metadata_of_plan

  let strip_file_uri = strip_file_uri

  let is_http_url = is_http_url

  let is_file_uri = is_file_uri

  let resolve_snapshot_download = resolve_snapshot_download

  let history_mode_matches = history_mode_matches
end
