open Rresult

let ( let* ) = Result.bind

let role_binary role =
  match String.lowercase_ascii role with
  | "node" -> "octez-node"
  | "baker" -> "octez-baker"
  | "accuser" -> "octez-accuser"
  | "dal" | "dal-node" -> "octez-dal-node"
  | other -> "octez-" ^ other

let system_unit_path role =
  Printf.sprintf "/etc/systemd/system/octez-%s@.service" role

let user_unit_path role =
  let dir = Filename.concat (Common.xdg_config_home ()) "systemd/user" in
  Filename.concat dir (Printf.sprintf "octez-%s@.service" role)

let unit_path role =
  if Common.is_root () then system_unit_path role else user_unit_path role

let dropin_dir role inst =
  if Common.is_root () then
    Printf.sprintf "/etc/systemd/system/octez-%s@%s.service.d" role inst
  else
    let base = Filename.concat (Common.xdg_config_home ()) "systemd/user" in
    Filename.concat base (Printf.sprintf "octez-%s@%s.service.d" role inst)

let dropin_path role inst =
  Filename.concat (dropin_dir role inst) "override.conf"

let unit_name role inst = Printf.sprintf "octez-%s@%s" role inst

let systemctl_cmd () =
  if Common.is_root () then ["systemctl"] else ["systemctl"; "--user"]

let run_systemctl args = Common.run (systemctl_cmd () @ args)

let run_systemctl_timeout args =
  (* Keep systemctl calls bounded to avoid UI stalls. Shorten to 2s. *)
  Common.run (("timeout" :: "2s" :: systemctl_cmd ()) @ args)

let run_systemctl_out_timeout args =
  (* Keep systemctl calls bounded to avoid UI stalls. Shorten to 2s. *)
  Common.run_out (("timeout" :: "2s" :: systemctl_cmd ()) @ args)

let run_systemctl_out args = Common.run_out (systemctl_cmd () @ args)

let cat_unit ~role ~instance =
  run_systemctl_out ["cat"; unit_name role instance]

let status ~role ~instance =
  (* systemctl can hang if the user bus is unavailable; cap to 5s. *)
  run_systemctl_out_timeout ["status"; "--no-pager"; unit_name role instance]

let is_enabled ~role ~instance =
  (* Cap to 5s to avoid blocking the UI on systemd hiccups. *)
  run_systemctl_out_timeout ["is-enabled"; unit_name role instance]

let is_active ~role ~instance =
  let unit = unit_name role instance in
  (* Cap to 5s to avoid hangs when the user bus/systemd is slow or unavailable. *)
  match run_systemctl_out_timeout ["show"; "--property=ActiveState"; unit] with
  | Ok line ->
      let state =
        match String.split_on_char '=' line with
        | [_; value] -> String.trim value
        | _ -> String.trim line
      in
      Ok (String.equal state "active")
  | Error _ as e -> e

let env_file_template user_mode =
  let base =
    if user_mode then Common.env_instances_base_dir ()
    else "/etc/octez/instances"
  in
  Filename.concat base "%i/node.env"

let exec_line role =
  match String.lowercase_ascii role with
  | "baker" ->
      "ExecStart=/bin/sh -lc 'MODE=${OCTEZ_BAKER_NODE_MODE:-local}; \
       CMD=\"${APP_BIN_DIR}/octez-baker\"; CMD=\"$CMD --base-dir \
       \\\"${OCTEZ_BAKER_BASE_DIR}\\\" --endpoint \
       \\\"${OCTEZ_NODE_ENDPOINT}\\\"\"; if [ \"$MODE\" = \"remote\" ]; then \
       CMD=\"$CMD run with remote node \\\"${OCTEZ_NODE_ENDPOINT}\\\"\"; else \
       CMD=\"$CMD run with local node \\\"${OCTEZ_DATA_DIR}\\\"\"; fi; if [ -n \
       \"${OCTEZ_DAL_ENDPOINT:-}\" ]; then CMD=\"$CMD --dal-node \
       \\\"${OCTEZ_DAL_ENDPOINT}\\\"\"; fi; exec $CMD \
       ${OCTEZ_BAKER_DELEGATES_ARGS:-} ${OCTEZ_BAKER_EXTRA_ARGS:-}'"
  | "node" ->
      "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/octez-node\" run \
       --data-dir=\"${OCTEZ_DATA_DIR}\" ${OCTEZ_NODE_ARGS:-}'"
  | other ->
      Printf.sprintf
        "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/octez-%s\" \
         ${OCTEZ_SERVICE_ARGS:-}'"
        other

let prestart_hooks_dir () =
  let base =
    if Common.is_root () then "/usr/lib/octez-manager"
    else Filename.concat (Common.xdg_data_home ()) "octez-manager"
  in
  Filename.concat base "hooks"

let prestart_script_path role =
  Filename.concat
    (prestart_hooks_dir ())
    (Printf.sprintf "octez-%s-prestart.sh" role)

let node_prestart_script_body =
  "#!/bin/sh\n" ^ "set -eu\n\n"
  ^ "NODE_BIN=\"${APP_BIN_DIR:-/usr/bin}/octez-node\"\n"
  ^ "DATA_DIR=\"${OCTEZ_DATA_DIR:?OCTEZ_DATA_DIR must be set}\"\n"
  ^ "STORE_DIR=\"$DATA_DIR/store\"\n"
  ^ "VERSION_FILE=\"$DATA_DIR/version.json\"\n"
  ^ "LMDB_TOREMOVE=\"$DATA_DIR/lmdb_store_to_remove\"\n\n"
  ^ "LOCK_FILE=\"$DATA_DIR/lock\"\n" ^ "if [ -d \"$STORE_DIR\" ]; then\n"
  ^ "  if [ ! -f \"$VERSION_FILE\" ]; then\n"
  ^ "    echo \"octez-manager prestart: missing version.json, wiping store and \
     reimporting\" >&2\n"
  ^ "    rm -rf \"$STORE_DIR\" \"$DATA_DIR/context\" \"$DATA_DIR/version.json\"\n"
  ^ "  else\n" ^ "    \"$NODE_BIN\" upgrade storage --data-dir \"$DATA_DIR\"\n"
  ^ "    if [ -d \"$LMDB_TOREMOVE\" ]; then\n"
  ^ "      rm -rf \"$LMDB_TOREMOVE\"\n" ^ "    fi\n" ^ "    exit 0\n" ^ "  fi\n"
  ^ "fi\n\n" ^ "if [ -f \"$LOCK_FILE\" ]; then\n" ^ "  rm -f \"$LOCK_FILE\"\n"
  ^ "fi\n\n" ^ "AUTO=\"${OCTEZ_SNAPSHOT_AUTO:-0}\"\n"
  ^ "if [ \"$AUTO\" != \"1\" ]; then\n" ^ "  exit 0\n" ^ "fi\n\n"
  ^ "TMP=$(mktemp /tmp/octez-manager.snapshot.XXXXXX)\n" ^ "cleanup() {\n"
  ^ "  rm -f \"$TMP\"\n" ^ "}\n" ^ "trap cleanup EXIT INT TERM\n\n"
  ^ "NETWORK=\"${OCTEZ_NETWORK:-${OCTEZ_SNAPSHOT_NETWORK_SLUG:-}}\"\n"
  ^ "HISTORY_MODE=\"${OCTEZ_HISTORY_MODE:-}\"\n"
  ^ "CONFIG=\"$DATA_DIR/config.json\"\n\n" ^ "ensure_config() {\n"
  ^ "  if [ ! -f \"$CONFIG\" ] && [ -n \"$NETWORK\" ]; then\n"
  ^ "    set -- --network \"$NETWORK\" --data-dir \"$DATA_DIR\"\n"
  ^ "    if [ -n \"$HISTORY_MODE\" ]; then\n"
  ^ "      set -- \"$@\" --history-mode \"$HISTORY_MODE\"\n" ^ "    fi\n"
  ^ "    \"$NODE_BIN\" config init \"$@\"\n" ^ "  fi\n" ^ "}\n\n"
  ^ "fetch_snapshot() {\n" ^ "  URI=\"${OCTEZ_SNAPSHOT_URI:-}\"\n"
  ^ "  if [ -n \"$URI\" ]; then\n" ^ "    case \"$URI\" in\n"
  ^ "      http://*|https://*) curl -fSL \"$URI\" -o \"$TMP\" ;;\n"
  ^ "      file://*) cp \"${URI#file://}\" \"$TMP\" ;;\n" ^ "      *)\n"
  ^ "        if [ ! -f \"$URI\" ]; then\n"
  ^ "          echo \"octez-manager prestart: snapshot source $URI not found\" \
     >&2\n" ^ "          return 1\n" ^ "        fi\n"
  ^ "        cp \"$URI\" \"$TMP\" ;;\n" ^ "    esac\n" ^ "    return 0\n"
  ^ "  fi\n\n" ^ "  SNAPSHOT_NETWORK=\"${OCTEZ_SNAPSHOT_NETWORK_SLUG:-}\"\n"
  ^ "  SNAPSHOT_KIND=\"${OCTEZ_SNAPSHOT_KIND:-}\"\n"
  ^ "  if [ -z \"$SNAPSHOT_NETWORK\" ]; then\n"
  ^ "    echo \"octez-manager prestart: no network slug available for snapshot \
     download\" >&2\n" ^ "    return 1\n" ^ "  fi\n"
  ^ "  if [ -z \"$SNAPSHOT_KIND\" ]; then\n"
  ^ "    SNAPSHOT_KIND=\"${OCTEZ_HISTORY_MODE:-rolling}\"\n" ^ "  fi\n"
  ^ "  curl -fSL \
     \"https://snapshots.tzinit.org/$SNAPSHOT_NETWORK/$SNAPSHOT_KIND\" -o \
     \"$TMP\"\n" ^ "}\n\n" ^ "if fetch_snapshot; then\n" ^ "  ensure_config\n"
  ^ "  EXTRA=\"${OCTEZ_SNAPSHOT_NO_CHECK:-0}\"\n"
  ^ "  if [ \"$EXTRA\" = \"1\" ]; then\n"
  ^ "    \"$NODE_BIN\" snapshot import --no-check --force --data-dir \
     \"$DATA_DIR\" \"$TMP\"\n" ^ "  else\n"
  ^ "    \"$NODE_BIN\" snapshot import --force --data-dir \"$DATA_DIR\" \"$TMP\"\n"
  ^ "  fi\n" ^ "else\n"
  ^ "  echo \"octez-manager prestart: snapshot fetch skipped\" >&2\n" ^ "fi\n"

let write_prestart_script role =
  match String.lowercase_ascii role with
  | "node" ->
      let path = prestart_script_path role in
      let owner, group =
        if Common.is_root () then ("root", "root")
        else Common.current_user_group_names ()
      in
      let* () =
        Common.write_file
          ~mode:0o755
          ~owner
          ~group
          path
          node_prestart_script_body
      in
      Ok (Some path)
  | _ -> Ok None

let unit_template ~user_mode ~role ~app_bin_dir ~user ?prestart () =
  let bin_dir = if app_bin_dir = "" then "/usr/bin" else app_bin_dir in
  let env_file = env_file_template user_mode in
  let header_common =
    Printf.sprintf
      "[Unit]\n\
       Description=Octez %s (%%i)\n\
       After=network-online.target\n\
       Wants=network-online.target\n\n\
       [Service]\n\
       Environment=APP_BIN_DIR=%s\n\
       Environment=ROLE=%s\n\
       EnvironmentFile=-%s\n"
      role
      bin_dir
      role
      env_file
  in
  let header =
    if user_mode then header_common
    else header_common ^ Printf.sprintf "User=%s\nGroup=%s\n" user user
  in
  let prestart_block =
    match prestart with
    | Some cmd -> [Printf.sprintf "ExecStartPre=%s" cmd]
    | None -> []
  in
  let common_hardening =
    "Restart=on-failure\n\
     NoNewPrivileges=yes\n\
     PrivateTmp=yes\n\
     ProtectSystem=strict\n\
     ProtectHome=false"
  in
  let exec_block = exec_line role in
  let install_section =
    if user_mode then "\n[Install]\nWantedBy=default.target\n"
    else "\n[Install]\nWantedBy=multi-user.target\n"
  in
  String.concat
    "\n"
    ([header] @ prestart_block @ [exec_block; common_hardening; install_section])

let validate_bin_dir ~user ~app_bin_dir ~role =
  let bin_dir = if app_bin_dir = "" then "/usr/bin" else app_bin_dir in
  let binary = Filename.concat bin_dir (role_binary role) in
  if not (Sys.file_exists binary) then
    R.error_msgf "Binary not found: %s" binary
  else if Common.is_root () then
    match Common.run ["sudo"; "-n"; "-u"; user; "test"; "-x"; binary] with
    | Ok () -> Ok ()
    | Error _ -> (
        let cmd = Printf.sprintf "test -x %s" (Common.sh_quote binary) in
        match Common.run ["su"; "-s"; "/bin/sh"; "-c"; cmd; user] with
        | Ok () -> Ok ()
        | Error (`Msg m) ->
            R.error_msgf
              "User %s cannot execute %s: %s. Adjust permissions or pick a \
               different service user."
              user
              binary
              m)
  else
    match Common.run ["test"; "-x"; binary] with
    | Ok () -> Ok ()
    | Error (`Msg m) -> R.error_msgf "Cannot execute %s: %s" binary m

let install_unit ~role ~app_bin_dir ~user =
  let path = unit_path role in
  let owner, group =
    if Common.is_root () then ("root", "root")
    else Common.current_user_group_names ()
  in
  let* () =
    Common.ensure_dir_path ~owner ~group ~mode:0o755 (Filename.dirname path)
  in
  let* () = validate_bin_dir ~user ~app_bin_dir ~role in
  let* prestart = write_prestart_script role in
  let body =
    unit_template
      ~user_mode:(not (Common.is_root ()))
      ~role
      ~app_bin_dir
      ~user
      ?prestart
      ()
  in
  let* () = Common.write_file ~mode:0o644 ~owner ~group path body in
  let* () = run_systemctl_timeout ["daemon-reload"] in
  Ok ()

module StringSet = Set.Make (String)

type logrotate_spec = {role : string; paths : string list}

let managed_logrotate_header = "# Managed by octez-manager"

let logrotate_template log_path =
  Printf.sprintf
    "%s {\n\
    \  daily\n\
    \  missingok\n\
    \  rotate 5\n\
    \  compress\n\
    \  delaycompress\n\
    \  notifempty\n\
    \  copytruncate\n\
     }\n"
    log_path

let logrotate_body paths =
  match paths with
  | [] -> ""
  | _ ->
      let entries = List.map logrotate_template paths in
      String.concat "\n" (managed_logrotate_header :: entries)

let file_is_managed path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        try
          let line = input_line ic in
          String.starts_with ~prefix:managed_logrotate_header line
        with End_of_file -> false)
  with Sys_error _ -> false

let remove_file_if_managed path =
  if Sys.file_exists path && file_is_managed path then
    try
      Sys.remove path ;
      Ok ()
    with Sys_error msg -> R.error_msg msg
  else Ok ()

let system_logrotate_config_path role =
  Printf.sprintf "/etc/logrotate.d/octez-%s" role

let write_system_logrotate role paths =
  if paths = [] then remove_file_if_managed (system_logrotate_config_path role)
  else
    let owner, group = ("root", "root") in
    let body = logrotate_body paths ^ "\n" in
    Common.write_file
      ~mode:0o644
      ~owner
      ~group
      (system_logrotate_config_path role)
      body

let cleanup_system_logrotate active_roles =
  let dir = "/etc/logrotate.d" in
  if not (Sys.file_exists dir) then Ok ()
  else
    let active =
      List.fold_left
        (fun acc role -> StringSet.add role acc)
        StringSet.empty
        active_roles
    in
    Sys.readdir dir |> Array.to_list
    |> List.fold_left
         (fun acc file ->
           let* () = acc in
           if String.starts_with ~prefix:"octez-" file then
             let role = String.sub file 6 (String.length file - 6) in
             if StringSet.mem role active then Ok ()
             else remove_file_if_managed (Filename.concat dir file)
           else Ok ())
         (Ok ())

let sync_system_logrotate specs =
  let* () =
    List.fold_left
      (fun acc {role; paths} ->
        let* () = acc in
        write_system_logrotate role (List.sort String.compare paths))
      (Ok ())
      specs
  in
  let active_roles = List.map (fun spec -> spec.role) specs in
  cleanup_system_logrotate active_roles

let user_logrotate_root () =
  Filename.concat (Common.xdg_config_home ()) "octez-manager"

let user_logrotate_include_dir () =
  Filename.concat (user_logrotate_root ()) "logrotate.d"

let user_logrotate_main_config () =
  Filename.concat (user_logrotate_root ()) "logrotate.conf"

let user_logrotate_role_config role =
  Filename.concat
    (user_logrotate_include_dir ())
    (Printf.sprintf "%s.conf" role)

let user_logrotate_state_file () =
  Filename.concat
    (Filename.concat (Common.xdg_state_home ()) "octez")
    "logrotate.state"

let user_systemd_dir () =
  Filename.concat (Common.xdg_config_home ()) "systemd/user"

let user_logrotate_service_path () =
  Filename.concat (user_systemd_dir ()) "octez-manager-logrotate.service"

let user_logrotate_timer_path () =
  Filename.concat (user_systemd_dir ()) "octez-manager-logrotate.timer"

let user_logrotate_unit = "octez-manager-logrotate"

let ensure_user_logrotate_main_config ~owner ~group =
  let body = Printf.sprintf "include %s\n" (user_logrotate_include_dir ()) in
  Common.write_file
    ~mode:0o644
    ~owner
    ~group
    (user_logrotate_main_config ())
    body

let write_user_logrotate_spec ~owner ~group {role; paths} =
  if paths = [] then remove_file_if_managed (user_logrotate_role_config role)
  else
    let body = logrotate_body (List.sort String.compare paths) ^ "\n" in
    Common.write_file
      ~mode:0o644
      ~owner
      ~group
      (user_logrotate_role_config role)
      body

let remove_unused_user_role_configs active_roles =
  let dir = user_logrotate_include_dir () in
  if not (Sys.file_exists dir) then Ok ()
  else
    let active =
      List.fold_left
        (fun acc role -> StringSet.add role acc)
        StringSet.empty
        active_roles
    in
    Sys.readdir dir |> Array.to_list
    |> List.fold_left
         (fun acc file ->
           let* () = acc in
           if Filename.check_suffix file ".conf" then
             let role = Filename.chop_suffix file ".conf" in
             if StringSet.mem role active then Ok ()
             else remove_file_if_managed (Filename.concat dir file)
           else Ok ())
         (Ok ())

let logrotate_binary () =
  match Common.which "logrotate" with
  | Some path -> Ok path
  | None ->
      R.error_msg
        "logrotate binary not found in PATH. Install the 'logrotate' package \
         to enable file log rotation."

let ensure_user_logrotate_timer ~owner ~group ~logrotate_bin =
  let service_body =
    Printf.sprintf
      "[Unit]\n\
       Description=Rotate Octez logs\n\n\
       [Service]\n\
       Type=oneshot\n\
       ExecStart=%s -s %s %s\n"
      logrotate_bin
      (user_logrotate_state_file ())
      (user_logrotate_main_config ())
  in
  let timer_body =
    "[Unit]\n\
     Description=Rotate Octez logs daily\n\n\
     [Timer]\n\
     OnCalendar=daily\n\
     Persistent=true\n\
     AccuracySec=1h\n\n\
     [Install]\n\
     WantedBy=timers.target\n"
  in
  let* () =
    Common.write_file
      ~mode:0o644
      ~owner
      ~group
      (user_logrotate_service_path ())
      service_body
  in
  let* () =
    Common.write_file
      ~mode:0o644
      ~owner
      ~group
      (user_logrotate_timer_path ())
      timer_body
  in
  let* () = run_systemctl_timeout ["daemon-reload"] in
  run_systemctl ["enable"; "--now"; user_logrotate_unit ^ ".timer"]

let disable_user_logrotate_timer () =
  let timer = user_logrotate_unit ^ ".timer" in
  let service = user_logrotate_unit ^ ".service" in
  (* Only attempt to disable units if their files exist to avoid spurious errors *)
  if Sys.file_exists (user_logrotate_timer_path ()) then
    ignore (run_systemctl ["disable"; "--now"; timer]) ;
  if Sys.file_exists (user_logrotate_service_path ()) then
    ignore (run_systemctl ["disable"; "--now"; service]) ;
  ignore (run_systemctl_timeout ["daemon-reload"]) ;
  ()

let cleanup_user_logrotate_files () =
  disable_user_logrotate_timer () ;
  List.iter
    Common.remove_path
    [
      user_logrotate_service_path ();
      user_logrotate_timer_path ();
      user_logrotate_main_config ();
      user_logrotate_state_file ();
    ] ;
  let _ = Common.remove_tree (user_logrotate_include_dir ()) in
  ()

let sync_user_logrotate specs =
  let owner, group = Common.current_user_group_names () in
  if specs = [] then (
    cleanup_user_logrotate_files () ;
    Ok ())
  else
    let* logrotate_bin = logrotate_binary () in
    let* () =
      Common.ensure_dir_path
        ~owner
        ~group
        ~mode:0o755
        (user_logrotate_include_dir ())
    in
    let* () = ensure_user_logrotate_main_config ~owner ~group in
    let* () =
      List.fold_left
        (fun acc spec ->
          let* () = acc in
          write_user_logrotate_spec ~owner ~group spec)
        (Ok ())
        specs
    in
    let active_roles = List.map (fun spec -> spec.role) specs in
    let* () = remove_unused_user_role_configs active_roles in
    let state_dir = Filename.dirname (user_logrotate_state_file ()) in
    let* () = Common.ensure_dir_path ~owner ~group ~mode:0o755 state_dir in
    ensure_user_logrotate_timer ~owner ~group ~logrotate_bin

let sync_logrotate specs =
  if Common.is_root () then sync_system_logrotate specs
  else sync_user_logrotate specs

type logging_resources = {extra_lines : string list; extra_paths : string list}

let logging_resources ~role:_ ~logging_mode =
  match logging_mode with
  | Logging_mode.Journald ->
      {
        extra_lines = ["StandardOutput=journal"; "StandardError=journal"];
        extra_paths = [];
      }
  | Logging_mode.File {path; _} ->
      let trimmed = String.trim path in
      if trimmed = "" then {extra_lines = []; extra_paths = []}
      else
        let dir = Filename.dirname trimmed in
        {
          extra_lines = [Printf.sprintf "Environment=OCTEZ_LOG_PATH=%s" trimmed];
          extra_paths = (if dir = "" || dir = "." then [] else [dir]);
        }

let unique_non_empty paths =
  paths
  |> List.filter (fun p -> String.trim p <> "")
  |> List.sort_uniq String.compare

let read_write_paths_for ~data_dir ~logging_paths ~extra_paths =
  let base =
    if Common.is_root () then [data_dir; "/var/log/octez"] else [data_dir]
  in
  unique_non_empty (base @ logging_paths @ extra_paths)

let write_dropin_body ~role ~data_dir ~logging_mode ~extra_paths =
  let resources = logging_resources ~role ~logging_mode in
  let rw_paths =
    read_write_paths_for
      ~data_dir
      ~logging_paths:resources.extra_paths
      ~extra_paths
  in
  let header =
    let base = ref ["[Service]"] in
    if Common.is_root () then base := !base @ ["PermissionsStartOnly=true"] ;
    !base @ resources.extra_lines
  in
  String.concat
    "\n"
    (header
    @ [Printf.sprintf "Environment=OCTEZ_DATA_DIR=%s" data_dir]
    @ List.map (fun p -> Printf.sprintf "ReadWritePaths=%s" p) rw_paths)
  ^ "\n"

let write_dropin ~role ~inst ~data_dir ~logging_mode ?(extra_paths = []) () =
  let dir = dropin_dir role inst in
  let path = dropin_path role inst in
  let owner, group =
    if Common.is_root () then ("root", "root")
    else Common.current_user_group_names ()
  in
  let* () = Common.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  let body = write_dropin_body ~role ~data_dir ~logging_mode ~extra_paths in
  let* () = Common.write_file ~mode:0o644 ~owner ~group path body in
  run_systemctl_timeout ["daemon-reload"]

let write_dropin_node ~inst ~data_dir ~logging_mode =
  write_dropin ~role:"node" ~inst ~data_dir ~logging_mode ()

let render_logging_lines logging_mode =
  (logging_resources ~role:"node" ~logging_mode).extra_lines

let enable ~role ~instance ~start_now =
  let unit = unit_name role instance in
  let action = if start_now then ["enable"; "--now"] else ["enable"] in
  run_systemctl (action @ [unit])

let disable ~role ~instance ~stop_now =
  let unit = unit_name role instance in
  let action = if stop_now then ["disable"; "--now"] else ["disable"] in
  run_systemctl (action @ [unit])

let start ~role ~instance = run_systemctl ["start"; unit_name role instance]

let stop ~role ~instance = run_systemctl ["stop"; unit_name role instance]

let restart ~role ~instance = run_systemctl ["restart"; unit_name role instance]

let remove_dropin ~role ~instance =
  let path = dropin_dir role instance in
  let _ = Common.remove_tree path in
  ()

let refresh_unit_name instance = Printf.sprintf "octez-refresh-node@%s" instance

let install_refresh_timer ~instance ~frequency ~cmd ~user =
  let service_name = refresh_unit_name instance in
  let service_path =
    if Common.is_root () then
      Printf.sprintf "/etc/systemd/system/%s.service" service_name
    else
      Filename.concat
        (Filename.concat (Common.xdg_config_home ()) "systemd/user")
        (service_name ^ ".service")
  in
  let timer_path =
    if Common.is_root () then
      Printf.sprintf "/etc/systemd/system/%s.timer" service_name
    else
      Filename.concat
        (Filename.concat (Common.xdg_config_home ()) "systemd/user")
        (service_name ^ ".timer")
  in
  let owner, group =
    if Common.is_root () then ("root", "root")
    else Common.current_user_group_names ()
  in
  let service_body =
    Printf.sprintf
      "[Unit]\n\
       Description=Refresh Octez node %s from snapshot\n\
       After=network-online.target\n\
       Wants=network-online.target\n\n\
       [Service]\n\
       Type=oneshot\n\
       User=%s\n\
       ExecStart=%s\n"
      instance
      user
      cmd
  in
  let timer_body =
    Printf.sprintf
      "[Unit]\n\
       Description=Refresh Octez node %s from snapshot %s\n\n\
       [Timer]\n\
       OnCalendar=%s\n\
       Persistent=true\n\
       AccuracySec=1h\n\n\
       [Install]\n\
       WantedBy=timers.target\n"
      instance
      frequency
      frequency
  in
  let* () =
    Common.write_file ~mode:0o644 ~owner ~group service_path service_body
  in
  let* () = Common.write_file ~mode:0o644 ~owner ~group timer_path timer_body in
  let* () = run_systemctl_timeout ["daemon-reload"] in
  run_systemctl ["enable"; "--now"; service_name ^ ".timer"]

let remove_refresh_timer ~instance =
  let service_name = refresh_unit_name instance in
  let timer_unit = service_name ^ ".timer" in
  let service_unit = service_name ^ ".service" in
  let service_path =
    if Common.is_root () then
      Printf.sprintf "/etc/systemd/system/%s.service" service_name
    else
      Filename.concat
        (Filename.concat (Common.xdg_config_home ()) "systemd/user")
        (service_name ^ ".service")
  in
  let timer_path =
    if Common.is_root () then
      Printf.sprintf "/etc/systemd/system/%s.timer" service_name
    else
      Filename.concat
        (Filename.concat (Common.xdg_config_home ()) "systemd/user")
        (service_name ^ ".timer")
  in
  (* Only attempt to disable units if their files exist to avoid spurious errors *)
  if Sys.file_exists timer_path then
    ignore (run_systemctl ["disable"; "--now"; timer_unit]) ;
  if Sys.file_exists service_path then
    ignore (run_systemctl ["disable"; "--now"; service_unit]) ;
  Common.remove_path service_path ;
  Common.remove_path timer_path ;
  let _ = run_systemctl_timeout ["daemon-reload"] in
  ()

module For_tests = struct
  let role_binary = role_binary

  let unit_path = unit_path

  let dropin_dir = dropin_dir

  let dropin_path = dropin_path

  let unit_template ~role ~app_bin_dir ~user ?prestart () =
    unit_template
      ~user_mode:(not (Common.is_root ()))
      ~role
      ~app_bin_dir
      ~user
      ?prestart
      ()

  let render_logging_lines = render_logging_lines

  let disable_user_logrotate_timer = disable_user_logrotate_timer
end
