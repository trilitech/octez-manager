open Rresult
open Installer_types

let ( let* ) = Result.bind

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

module Path_set = Set.Make (String)

let logrotate_specs_of services =
  let table = Hashtbl.create 7 in
  let add_path role path =
    let current =
      match Hashtbl.find_opt table role with
      | Some s -> s
      | None -> Path_set.empty
    in
    Hashtbl.replace table role (Path_set.add path current)
  in
  List.iter
    (fun (svc : Service.t) ->
      match svc.logging_mode with
      | Logging_mode.File {path; rotate} when rotate ->
          let trimmed = String.trim path in
          if trimmed <> "" then add_path svc.role trimmed
      | _ -> ())
    services ;
  Hashtbl.fold
    (fun role paths acc ->
      let values = Path_set.elements paths in
      if values = [] then acc else {Systemd.role; paths = values} :: acc)
    table
    []

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
  let snapshot_lower =
    String.lowercase_ascii (String.trim snapshot_mode)
  in
  requested_lower = snapshot_lower

let resolve_snapshot_download ~network ~history_mode ~snapshot_kind =
  let* network_slug =
    match Snapshots.slug_of_network network with
    | Some slug -> Ok slug
    | None ->
        R.error_msg
          "Unable to infer a tzinit network slug from --network. Provide \
           either a known alias (mainnet/ghostnet/...) or a teztnets JSON URL."
  in
  let* kind_slug, kind_label =
    match snapshot_kind with
    | Some raw -> (
        match Snapshots.sanitize_kind_input raw with
        | Some slug -> Ok (slug, raw)
        | None -> R.error_msgf "Snapshot kind '%s' cannot be parsed." raw)
    | None -> (
        match history_mode with
        | History_mode.Rolling -> Ok ("rolling", "rolling")
        | History_mode.Full -> Ok ("full", "full")
        | History_mode.Archive ->
            R.error_msg
              "Snapshots are not provided for archive history mode. Download a \
               full archive manually, extract it into your target data \
               directory, then rerun octez-manager without --snapshot but with \
               --data-dir pointing to that directory.")
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
      | Some snapshot_mode when String.trim snapshot_mode <> "" ->
          if not (history_mode_matches ~requested:history_mode ~snapshot_mode)
          then
            R.error_msgf
              "Snapshot '%s' has history mode '%s' but requested history mode \
               is '%s'. Please select a snapshot with the correct history mode \
               or change your history mode selection to match."
              entry.label
              snapshot_mode
              (History_mode.to_string history_mode)
          else (
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

type install_progress = {
  on_download_progress : (int -> int option -> unit) option;
  on_step_complete : (string -> unit) option;
}

let download_snapshot_to_tmp ?progress src =
  let tmp = Filename.temp_file "octez-manager.snapshot" ".snap" in
  let res =
    match progress with
    | Some {on_download_progress} ->
        Common.download_file_with_progress
          ~url:src
          ~dest_path:tmp
          ~on_progress:(fun pct _ ->
            match on_download_progress with
            | Some f -> f pct (Some 100)
            | None -> ())
    | None -> Common.download_file ~url:src ~dest_path:tmp
  in
  match res with
  | Ok () -> Ok {path = tmp; cleanup = true}
  | Error _ as e ->
      Common.remove_path tmp ;
      e

let prepare_snapshot_source ?progress src =
  let trimmed = String.trim src in
  if trimmed = "" then R.error_msg "Snapshot URI is empty"
  else
    match strip_file_uri trimmed with
    | Some path -> Ok {path; cleanup = false}
    | None when not (is_http_url trimmed) ->
        if Sys.file_exists trimmed then Ok {path = trimmed; cleanup = false}
        else R.error_msgf "Snapshot file %s does not exist" trimmed
    | _ -> download_snapshot_to_tmp ?progress trimmed

let snapshot_plan_of_request request =
  match request.bootstrap with
  | Genesis -> Ok No_snapshot
  | Snapshot {src; kind} -> (
      match normalize_optional_string src with
      | Some uri -> Ok (Direct_snapshot {uri})
      | None ->
          resolve_snapshot_download
            ~network:request.network
            ~history_mode:request.history_mode
            ~snapshot_kind:kind
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

let import_snapshot ~app_bin_dir ~data_dir ~snapshot_path ~no_check =
  let octez_node = Filename.concat app_bin_dir "octez-node" in
  let args =
    let base =
      [octez_node; "snapshot"; "import"; "--data-dir"; data_dir; snapshot_path]
    in
    if no_check then base @ ["--no-check"] else base
  in
  Common.run args

let materialize_snapshot_plan ?progress ~plan () :
    (snapshot_file option, R.msg) result =
  match plan with
  | No_snapshot -> Ok None
  | Direct_snapshot {uri} ->
      prepare_snapshot_source ?progress uri |> Result.map Option.some
  | Tzinit_snapshot res ->
      download_snapshot_to_tmp ?progress res.download_url
      |> Result.map Option.some

let import_snapshot_file ~app_bin_dir ~data_dir ~snapshot_file ~no_check =
  import_snapshot
    ~app_bin_dir
    ~data_dir
    ~snapshot_path:snapshot_file.path
    ~no_check

let perform_snapshot_plan ?progress ~plan ~app_bin_dir ~data_dir ~no_check =
  match plan with
  | No_snapshot -> Ok ()
  | Direct_snapshot {uri} ->
      Option.iter (fun p -> Option.iter (fun f -> f "download") p.on_step_complete) progress ;
      let snapshot_progress =
        match progress with
        | Some {on_download_progress; _} -> Some {on_download_progress}
        | None -> None
      in
      let* snapshot_file = prepare_snapshot_source ?progress:snapshot_progress uri in
      Option.iter (fun p -> Option.iter (fun f -> f "import") p.on_step_complete) progress ;
      Fun.protect
        ~finally:(fun () ->
          if snapshot_file.cleanup then Common.remove_path snapshot_file.path)
        (fun () ->
          import_snapshot_file ~app_bin_dir ~data_dir ~snapshot_file ~no_check)
  | Tzinit_snapshot res ->
      Option.iter (fun p -> Option.iter (fun f -> f "download") p.on_step_complete) progress ;
      let snapshot_progress =
        match progress with
        | Some {on_download_progress; _} -> Some {on_download_progress}
        | None -> None
      in
      let* snapshot_file = download_snapshot_to_tmp ?progress:snapshot_progress res.download_url in
      Option.iter (fun p -> Option.iter (fun f -> f "import") p.on_step_complete) progress ;
      Fun.protect
        ~finally:(fun () ->
          if snapshot_file.cleanup then Common.remove_path snapshot_file.path)
        (fun () ->
          import_snapshot_file ~app_bin_dir ~data_dir ~snapshot_file ~no_check)

let perform_bootstrap ?progress ~plan ~(request : node_request) ~data_dir =
  perform_snapshot_plan
    ?progress
    ~plan
    ~app_bin_dir:request.app_bin_dir
    ~data_dir
    ~no_check:request.snapshot_no_check

let ensure_node_config ~app_bin_dir ~data_dir ~network ~history_mode =
  let config_path = Filename.concat data_dir "config.json" in
  if Sys.file_exists config_path then Ok ()
  else
    let octez_node = Filename.concat app_bin_dir "octez-node" in
    Common.run
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
    ~logging_mode =
  let base =
    [
      "--network=" ^ network;
      "--history-mode=" ^ History_mode.to_string history_mode;
      "--rpc-addr=" ^ rpc_addr;
      "--net-addr=" ^ net_addr;
    ]
  in
  let logging_args =
    match logging_mode with
    | Logging_mode.Journald -> []
    | Logging_mode.File {path; _} -> ["--log-output=" ^ path]
  in
  String.concat " " (base @ logging_args @ extra_args)

let prepare_logging ~instance ~role ~logging_mode =
  match logging_mode with
  | Logging_mode.Journald -> Logging_mode.Journald
  | Logging_mode.File ({path; _} as file) when String.trim path <> "" ->
      Logging_mode.File file
  | Logging_mode.File _ -> Logging_mode.default_for ~instance ~role

let ensure_logging_destination ~service_user = function
  | Logging_mode.Journald -> Ok ()
  | Logging_mode.File {path; _} when String.trim path = "" -> Ok ()
  | Logging_mode.File {path; _} ->
      let dir = Filename.dirname path in
      if dir = "" || dir = "." then Ok ()
      else
        let owner, group =
          if Common.is_root () then (service_user, service_user)
          else Common.current_user_group_names ()
        in
        Common.ensure_dir_path ~owner ~group ~mode:0o755 dir

let ensure_logging_base_directory ~owner ~group = function
  | Logging_mode.Journald -> Ok ()
  | Logging_mode.File {path; _} ->
      let trimmed = String.trim path in
      if trimmed = "" then Ok ()
      else
        let dir = Filename.dirname trimmed in
        if dir = "" || dir = "." then Ok ()
        else Common.ensure_dir_path ~owner ~group ~mode:0o755 dir

let remove_logging_artifacts = function
  | Logging_mode.Journald -> Ok ()
  | Logging_mode.File {path; _} ->
      let trimmed = String.trim path in
      if trimmed = "" then Ok () else Common.remove_tree trimmed

let should_drop_service_user ~user ~remaining_services =
  let trimmed = String.trim user in
  if trimmed = "" then false
  else
    not
      (List.exists
         (fun (svc : Service.t) ->
           String.equal trimmed (String.trim svc.Service.service_user))
         remaining_services)

let ensure_runtime_log_directory ~owner ~group = function
  | Logging_mode.Journald -> Ok ()
  | Logging_mode.File {path; _} when String.trim path = "" -> Ok ()
  | Logging_mode.File {path; _} ->
      let dir = Filename.dirname path in
      let* () =
        if dir = "" || dir = "." then Ok ()
        else Common.ensure_dir_path ~owner ~group ~mode:0o755 dir
      in
      if Sys.file_exists path then Ok ()
      else
        let fd =
          Unix.openfile path [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND] 0o644
        in
        Unix.close fd ;
        Ok ()

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

let reown_runtime_paths ~owner ~group ~paths ~logging_mode =
  let normalize =
    paths
    |> List.filter (fun p -> String.trim p <> "")
    |> List.sort_uniq String.compare
  in
  let* () =
    List.fold_left
      (fun acc dir ->
        let* () = acc in
        Common.ensure_tree_owner ~owner ~group dir)
      (Ok ())
      normalize
  in
  match logging_mode with
  | Logging_mode.Journald -> Ok ()
  | Logging_mode.File {path; _} ->
      let trimmed = String.trim path in
      if trimmed = "" then Ok ()
      else
        let dir = Filename.dirname trimmed in
        let* () =
          if dir = "" || dir = "." then Ok ()
          else Common.ensure_tree_owner ~owner ~group dir
        in
        if Sys.file_exists trimmed then
          Common.ensure_tree_owner ~owner ~group trimmed
        else Ok ()

let snapshot_plan_for_service ~(service : Service.t) ~history_mode
    ~network_override ~snapshot_uri_override ~snapshot_kind_override =
  let chosen_uri =
    match normalize_optional_string snapshot_uri_override with
    | Some uri -> Some uri
    | None -> service.snapshot_uri
  in
  match chosen_uri with
  | Some uri -> Ok (Direct_snapshot {uri})
  | None ->
      let network =
        match normalize_optional_string network_override with
        | Some net -> net
        | None -> (
            match service.snapshot_network_slug with
            | Some slug when String.trim slug <> "" -> slug
            | _ -> service.network)
      in
      let snapshot_kind =
        match normalize_optional_string snapshot_kind_override with
        | Some kind -> Some kind
        | None -> service.snapshot_kind
      in
      resolve_snapshot_download ~network ~history_mode ~snapshot_kind
      |> Result.map (fun res -> Tzinit_snapshot res)

let import_snapshot_for_instance ~(instance : string) ?snapshot_uri
    ?snapshot_kind ?network ?history_mode ~no_check () =
  let* service = lookup_node_service instance in
  let history_mode =
    match history_mode with Some hm -> hm | None -> service.history_mode
  in
  let* plan =
    snapshot_plan_for_service
      ~service
      ~history_mode
      ~network_override:network
      ~snapshot_uri_override:snapshot_uri
      ~snapshot_kind_override:snapshot_kind
  in
  let* was_active = Systemd.is_active ~role:"node" ~instance:service.instance in
  let* () = Systemd.stop ~role:"node" ~instance:service.instance in
  let* () =
    perform_snapshot_plan
      ~plan
      ~app_bin_dir:service.app_bin_dir
      ~data_dir:service.data_dir
      ~no_check
  in
  let owner, group =
    if Common.is_root () then (service.service_user, service.service_user)
    else Common.current_user_group_names ()
  in
  let* () =
    reown_runtime_paths
      ~owner
      ~group
      ~paths:[service.data_dir]
      ~logging_mode:service.logging_mode
  in
  let* () =
    if was_active then Systemd.start ~role:"node" ~instance:service.instance
    else Ok ()
  in
  Ok ()

let refresh_instance_from_snapshot ~(instance : string) ?snapshot_uri
    ?snapshot_kind ?network ?history_mode ?on_download_progress ~no_check () =
  let* service = lookup_node_service instance in
  let history_mode =
    match history_mode with Some hm -> hm | None -> service.history_mode
  in
  let* plan =
    snapshot_plan_for_service
      ~service
      ~history_mode
      ~network_override:network
      ~snapshot_uri_override:snapshot_uri
      ~snapshot_kind_override:snapshot_kind
  in
  let no_check_flag = no_check || service.snapshot_no_check in
  let progress = {on_download_progress} in
  let* snapshot_file_opt = materialize_snapshot_plan ~progress ~plan () in
  let identity_path = Filename.concat service.data_dir "identity.json" in
  let log_path =
    match service.logging_mode with
    | Logging_mode.File {path; _} ->
        let trimmed = String.trim path in
        if trimmed = "" then None else Some trimmed
    | Logging_mode.Journald -> None
  in
  let* identity_backup = backup_file_if_exists identity_path in
  let* log_backup =
    match log_path with
    | Some path -> backup_file_if_exists path
    | None -> Ok None
  in
  let owner, group =
    if Common.is_root () then (service.service_user, service.service_user)
    else Common.current_user_group_names ()
  in
  let restore_once =
    let restored = ref false in
    fun () ->
      if !restored then Ok ()
      else
        let* () = restore_backup ~owner ~group identity_backup in
        let* () = restore_backup ~owner ~group log_backup in
        restored := true ;
        Ok ()
  in
  let* was_active = Systemd.is_active ~role:"node" ~instance:service.instance in
  let* () = Systemd.stop ~role:"node" ~instance:service.instance in
  let result =
    Fun.protect
      ~finally:(fun () ->
        match snapshot_file_opt with
        | Some file when file.cleanup -> Common.remove_path file.path
        | _ -> ())
      (fun () ->
        let* () = Common.remove_tree service.data_dir in
        let* () = ensure_directories ~owner ~group [service.data_dir] in
        let* () =
          ensure_logging_base_directory ~owner ~group service.logging_mode
        in
        let* () =
          ensure_runtime_log_directory ~owner ~group service.logging_mode
        in
        let* () =
          ensure_node_config
            ~app_bin_dir:service.app_bin_dir
            ~data_dir:service.data_dir
            ~network:service.network
            ~history_mode:service.history_mode
        in
        let* () =
          match snapshot_file_opt with
          | None -> Ok ()
          | Some snapshot_file ->
              import_snapshot_file
                ~app_bin_dir:service.app_bin_dir
                ~data_dir:service.data_dir
                ~snapshot_file
                ~no_check:no_check_flag
        in
        let* () = restore_once () in
        let* () =
          reown_runtime_paths
            ~owner
            ~group
            ~paths:[service.data_dir]
            ~logging_mode:service.logging_mode
        in
        let* () =
          if was_active then
            Systemd.start ~role:"node" ~instance:service.instance
          else Ok ()
        in
        Ok ())
  in
  match result with
  | Ok () -> Ok ()
  | Error _ as e ->
      let (_ : (unit, _) result) = restore_once () in
      e

let install_node ?on_download_progress ?on_step_complete (request : node_request) =
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
  let* snapshot_plan = snapshot_plan_of_request request in
  let snapshot_meta =
    snapshot_metadata_of_plan ~no_check:request.snapshot_no_check snapshot_plan
  in
  Option.iter (fun f -> f "setup") on_step_complete ;
  let* () = System_user.ensure_service_account ~name:request.service_user in
  let* () =
    if Common.is_root () then
      System_user.ensure_system_directories
        ~user:request.service_user
        ~group:request.service_user
    else Ok ()
  in
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
  let* () = System_user.validate_user_for_service ~user:request.service_user in
  let owner, group =
    if Common.is_root () then (request.service_user, request.service_user)
    else Common.current_user_group_names ()
  in
  let* () =
    if data_dir_nonempty && not request.preserve_data then
      Common.remove_tree data_dir
    else Ok ()
  in
  let* () = ensure_directories ~owner ~group [data_dir] in
  let* () = ensure_logging_base_directory ~owner ~group logging_mode in
  let* () = ensure_runtime_log_directory ~owner ~group logging_mode in
  let* () =
    ensure_node_config
      ~app_bin_dir:request.app_bin_dir
      ~data_dir
      ~network:resolved_network
      ~history_mode:request.history_mode
  in
  let progress =
    match (on_download_progress, on_step_complete) with
    | None, None -> None
    | _ -> Some {on_download_progress; on_step_complete}
  in
  let* () =
    if request.preserve_data then Ok ()
    else perform_bootstrap ?progress ~plan:snapshot_plan ~request ~data_dir
  in
  Option.iter (fun f -> f "configure") on_step_complete ;
  let* () = reown_runtime_paths ~owner ~group ~paths:[data_dir] ~logging_mode in
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
      ~snapshot_kind:snapshot_meta.kind_slug
      ~snapshot_no_check:snapshot_meta.no_check
      ~extra_args:request.extra_args
      ()
  in
  let* () =
    Systemd.install_unit
      ~role:"node"
      ~app_bin_dir:request.app_bin_dir
      ~user:request.service_user
  in
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
  let* () =
    Node_env.write ~inst:request.instance ~data_dir ~run_args ~extra_env
  in
  let* () =
    Systemd.write_dropin_node ~inst:request.instance ~data_dir ~logging_mode
  in
  let* () = Service_registry.write service in
  let* services = Service_registry.list () in
  let* () = Systemd.sync_logrotate (logrotate_specs_of services) in
  let* () =
    if request.auto_enable then
      Systemd.enable ~role:"node" ~instance:request.instance ~start_now:true
    else Ok ()
  in
  Ok service

let install_daemon (request : daemon_request) =
  let logging_mode =
    prepare_logging
      ~instance:request.instance
      ~role:request.role
      ~logging_mode:request.logging_mode
  in
  let* () = System_user.ensure_service_account ~name:request.service_user in
  let* () =
    if Common.is_root () then
      System_user.ensure_system_directories
        ~user:request.service_user
        ~group:request.service_user
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
      ~role:request.role
      ~app_bin_dir:request.app_bin_dir
      ~user:request.service_user
  in
  let* () =
    Systemd.write_dropin
      ~role:request.role
      ~inst:request.instance
      ~data_dir:request.data_dir
      ~logging_mode
      ~extra_paths:request.extra_paths
      ()
  in
  let* () =
    reown_runtime_paths ~owner ~group ~paths:directories ~logging_mode
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
      ()
  in
  let* () = Service_registry.write service in
  let* services = Service_registry.list () in
  let* () = Systemd.sync_logrotate (logrotate_specs_of services) in
  let* () =
    if request.auto_enable then
      Systemd.enable
        ~role:request.role
        ~instance:request.instance
        ~start_now:true
    else Ok ()
  in
  Ok service

let install_baker (request : baker_request) =
  let* node_service_opt =
    match request.node_instance with
    | None -> Ok None
    | Some inst ->
        let* svc = lookup_node_service inst in
        Ok (Some svc)
  in
  let resolved_node_mode =
    match request.node_mode with
    | `Local -> `Local
    | `Remote -> `Remote
    | `Auto -> (
        match node_service_opt with
        | Some _ -> `Local
        | None -> (
            match request.node_data_dir with
            | Some dir when String.trim dir <> "" -> `Local
            | _ -> `Remote))
  in
  let* node_data_dir_opt =
    match resolved_node_mode with
    | `Remote -> Ok None
    | `Local -> (
        match (node_service_opt, request.node_data_dir) with
        | Some svc, _ -> Ok (Some svc.Service.data_dir)
        | None, Some dir when String.trim dir <> "" -> Ok (Some dir)
        | None, _ ->
            R.error_msg "--node-data-dir is required when using a local node")
  in
  let node_data_dir_opt =
    match node_data_dir_opt with
    | Some dir when String.trim dir <> "" -> Some (String.trim dir)
    | _ -> None
  in
  let data_dir_for_service =
    match node_data_dir_opt with
    | Some dir -> dir
    | None -> Common.default_role_dir "baker" request.instance ^ "/remote-node"
  in
  let network =
    match (request.network, node_service_opt) with
    | Some net, _ when String.trim net <> "" -> net
    | None, Some svc -> svc.Service.network
    | _ -> "mainnet"
  in
  let history_mode =
    match node_service_opt with
    | Some svc -> svc.Service.history_mode
    | None -> History_mode.default
  in
  let node_endpoint =
    match request.node_endpoint with
    | Some endpoint when String.trim endpoint <> "" -> endpoint
    | _ -> (
        match node_service_opt with
        | Some svc -> endpoint_of_rpc svc.Service.rpc_addr
        | None -> "http://127.0.0.1:8732")
  in
  let base_dir =
    match request.base_dir with
    | Some dir when String.trim dir <> "" -> dir
    | _ -> Common.default_role_dir "baker" request.instance
  in
  let dal_endpoint =
    match request.dal_endpoint with
    | Some ep when String.trim ep <> "" -> Some (endpoint_of_rpc ep)
    | _ -> None
  in
  let node_mode_env =
    match resolved_node_mode with `Local -> "local" | `Remote -> "remote"
  in
  let delegate_flags =
    request.delegates |> List.concat_map (fun d -> ["--delegate"; d])
  in
  let delegate_args = String.concat " " delegate_flags |> String.trim in
  let extra_args_str = String.concat " " request.extra_args |> String.trim in
  install_daemon
    {
      role = "baker";
      instance = request.instance;
      network;
      history_mode;
      data_dir = data_dir_for_service;
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
          ("OCTEZ_BAKER_NODE_MODE", node_mode_env);
          ( "OCTEZ_DAL_ENDPOINT",
            match dal_endpoint with Some ep -> ep | None -> "" );
          ("OCTEZ_BAKER_DELEGATES_ARGS", delegate_args);
          ("OCTEZ_BAKER_DELEGATES_CSV", String.concat "," request.delegates);
          ("OCTEZ_BAKER_EXTRA_ARGS", extra_args_str);
        ];
      extra_paths = [base_dir];
      auto_enable = request.auto_enable;
    }

let add_authorized_keys ~app_bin_dir ~base_dir ~service_user entries =
  let signer_bin = Filename.concat app_bin_dir "octez-signer" in
  let apply acc (name_opt, key_raw) =
    let* () = acc in
    let key = String.trim key_raw in
    if key = "" then Ok ()
    else
      let base_cmd =
        [signer_bin; "--base-dir"; base_dir; "add"; "authorized"; "key"; key]
      in
      let cmd =
        match name_opt with
        | Some name when String.trim name <> "" -> base_cmd @ ["--name"; name]
        | _ -> base_cmd
      in
      Common.run_as ~user:service_user cmd
  in
  List.fold_left apply (Ok ()) entries

let install_signer (request : signer_request) =
  let base_dir =
    match request.base_dir with
    | Some dir when String.trim dir <> "" -> dir
    | _ -> Common.default_role_dir "signer" request.instance
  in
  let address =
    let trimmed = String.trim request.address in
    if trimmed = "" then "127.0.0.1" else trimmed
  in
  let port = if request.port <= 0 then 6732 else request.port in
  let endpoint = Printf.sprintf "tcp://%s:%d" address port in
  let auth_flag =
    if request.require_auth then ["--require-authentication"] else []
  in
  let password_flag =
    match request.password_file with
    | Some path when String.trim path <> "" -> ["--password-filename"; path]
    | _ -> []
  in
  let service_args =
    auth_flag @ password_flag
    @ [
        "--base-dir";
        base_dir;
        "launch";
        "socket";
        "signer";
        "--address";
        address;
        "--port";
        string_of_int port;
      ]
  in
  let daemon_request : daemon_request =
    {
      role = "signer";
      instance = request.instance;
      network = request.network;
      history_mode = History_mode.default;
      data_dir = base_dir;
      rpc_addr = endpoint;
      net_addr = endpoint;
      service_user = request.service_user;
      app_bin_dir = request.app_bin_dir;
      logging_mode = request.logging_mode;
      service_args;
      extra_env =
        [
          ("OCTEZ_SIGNER_ENDPOINT", endpoint);
          ("OCTEZ_SIGNER_BASE_DIR", base_dir);
          ( "OCTEZ_SIGNER_REQUIRE_AUTH",
            if request.require_auth then "1" else "0" );
        ];
      extra_paths = [base_dir];
      auto_enable = false;
    }
  in
  let* service = install_daemon daemon_request in
  let* () =
    add_authorized_keys
      ~app_bin_dir:request.app_bin_dir
      ~base_dir
      ~service_user:request.service_user
      request.authorized_keys
  in
  let* () =
    if request.auto_enable then
      Systemd.enable ~role:"signer" ~instance:request.instance ~start_now:true
    else Ok ()
  in
  Ok service

let start_service ~instance ~role =
  let* svc_opt = Service_registry.find ~instance in
  let role = match svc_opt with Some svc -> svc.role | None -> role in
  Systemd.start ~role ~instance

let stop_service ~instance ~role =
  let* svc_opt = Service_registry.find ~instance in
  let role = match svc_opt with Some svc -> svc.role | None -> role in
  Systemd.stop ~role ~instance

let restart_service ~instance ~role =
  let* svc_opt = Service_registry.find ~instance in
  let role = match svc_opt with Some svc -> svc.role | None -> role in
  Systemd.restart ~role ~instance

let remove_service ~delete_data_dir ~instance ~role =
  let* svc_opt = Service_registry.find ~instance in
  let role = match svc_opt with Some svc -> svc.role | None -> role in
  let data_dir =
    match svc_opt with Some svc -> Some svc.data_dir | None -> None
  in
  let* () =
    match svc_opt with
    | Some _ -> Systemd.disable ~role ~instance ~stop_now:true
    | None -> Ok ()
  in
  Systemd.remove_dropin ~role ~instance ;
  let* () =
    match (delete_data_dir, data_dir) with
    | true, Some dir -> Common.remove_tree dir
    | _ -> Ok ()
  in
  let* () = Service_registry.remove ~instance in
  let* services = Service_registry.list () in
  Systemd.sync_logrotate (logrotate_specs_of services)

let purge_service ~instance ~role =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> remove_service ~delete_data_dir:true ~instance ~role
  | Some svc ->
      let* () = remove_service ~delete_data_dir:true ~instance ~role in
      let* () = remove_logging_artifacts svc.logging_mode in
      let* remaining = Service_registry.list () in
      if
        should_drop_service_user
          ~user:svc.service_user
          ~remaining_services:remaining
      then System_user.remove_service_account ~name:svc.service_user
      else Ok ()

let list_services () = Service_registry.list ()

let schedule_refresh ~instance ~frequency ~snapshot_kind ~no_check =
  let* service = lookup_node_service instance in
  let manager_bin =
    if Filename.is_relative Sys.executable_name then
      match Common.which "octez-manager" with
      | Some path -> path
      | None -> Sys.executable_name (* Fallback *)
    else Sys.executable_name
  in
  let cmd =
    Printf.sprintf
      "%s instance %s refresh-from-new-snapshot --snapshot-kind=%s%s"
      manager_bin
      instance
      snapshot_kind
      (if no_check then " --snapshot-no-check" else "")
  in
  Systemd.install_refresh_timer
    ~instance
    ~frequency
    ~cmd
    ~user:service.service_user

let unschedule_refresh ~instance = Systemd.remove_refresh_timer ~instance

let generate_secret_key ~instance ~alias =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | Some svc when String.equal svc.role "signer" ->
      let signer_bin = Filename.concat svc.app_bin_dir "octez-signer" in
      Common.run_as
        ~user:svc.service_user
        [
          signer_bin;
          "--base-dir";
          svc.data_dir;
          "generate";
          "secret";
          "key";
          alias;
        ]
  | Some svc ->
      R.error_msgf "Instance '%s' is a %s, expected a signer" instance svc.role
  | None -> R.error_msgf "Unknown instance '%s'" instance

let list_keys ~instance =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | Some svc when String.equal svc.role "signer" ->
      let signer_bin = Filename.concat svc.app_bin_dir "octez-signer" in
      Common.run_out
        [signer_bin; "--base-dir"; svc.data_dir; "list"; "known"; "keys"]
  | Some svc ->
      R.error_msgf "Instance '%s' is a %s, expected a signer" instance svc.role
  | None -> R.error_msgf "Unknown instance '%s'" instance

let add_authorized_key ~instance ~key ~name =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | Some svc when String.equal svc.role "signer" ->
      add_authorized_keys
        ~app_bin_dir:svc.app_bin_dir
        ~base_dir:svc.data_dir
        ~service_user:svc.service_user
        [(name, key)]
  | Some svc ->
      R.error_msgf "Instance '%s' is a %s, expected a signer" instance svc.role
  | None -> R.error_msgf "Unknown instance '%s'" instance

let backup_file_if_exists_for_tests = backup_file_if_exists

let restore_backup_for_tests = restore_backup

module For_tests = struct
  type nonrec file_backup = file_backup

  let ensure_logging_base_directory = ensure_logging_base_directory

  let remove_logging_artifacts = remove_logging_artifacts

  let should_drop_service_user = should_drop_service_user

  let backup_file_if_exists ~path = backup_file_if_exists_for_tests path

  let restore_backup ~owner ~group backup =
    restore_backup_for_tests ~owner ~group backup

  let normalize_data_dir = normalize_data_dir

  let endpoint_of_rpc = endpoint_of_rpc

  let build_run_args = build_run_args

  let snapshot_plan_of_request = snapshot_plan_of_request

  let snapshot_metadata_of_plan = snapshot_metadata_of_plan

  let strip_file_uri = strip_file_uri

  let is_http_url = is_http_url

  let is_file_uri = is_file_uri

  let resolve_snapshot_download = resolve_snapshot_download

  let history_mode_matches = history_mode_matches
end
