(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

let role_binary role =
  match String.lowercase_ascii role with
  | "node" -> "octez-node"
  | "baker" -> "octez-baker"
  | "accuser" -> "octez-baker"
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

let run_systemctl_timeout ?quiet ?(duration = "2s") args =
  (* Keep systemctl calls bounded to avoid UI stalls. *)
  Common.run ?quiet (("timeout" :: duration :: systemctl_cmd ()) @ args)

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

type unit_state = {
  active_state : string; (* active, inactive, failed, etc. *)
  sub_state : string; (* running, dead, failed, etc. *)
  result : string option; (* exit-code, signal, timeout, etc. *)
  exit_status : int option; (* actual exit code if available *)
}

let get_unit_state ~role ~instance =
  let unit = unit_name role instance in
  (* Get ActiveState, SubState, Result, and ExecMainStatus properties *)
  match
    run_systemctl_out_timeout
      ["show"; "--property=ActiveState,SubState,Result,ExecMainStatus"; unit]
  with
  | Ok output ->
      let lines = String.split_on_char '\n' output in
      let parse_prop prefix line =
        if
          String.length line > String.length prefix
          && String.sub line 0 (String.length prefix) = prefix
        then
          Some
            (String.sub
               line
               (String.length prefix)
               (String.length line - String.length prefix)
            |> String.trim)
        else None
      in
      let active_state = ref "unknown" in
      let sub_state = ref "unknown" in
      let result = ref None in
      let exit_status = ref None in
      List.iter
        (fun line ->
          (match parse_prop "ActiveState=" line with
          | Some v -> active_state := v
          | None -> ()) ;
          (match parse_prop "SubState=" line with
          | Some v -> sub_state := v
          | None -> ()) ;
          (match parse_prop "Result=" line with
          | Some v when v <> "" && v <> "success" -> result := Some v
          | _ -> ()) ;
          match parse_prop "ExecMainStatus=" line with
          | Some v -> exit_status := int_of_string_opt v
          | None -> ())
        lines ;
      Ok
        {
          active_state = !active_state;
          sub_state = !sub_state;
          result = !result;
          exit_status = !exit_status;
        }
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
      (* Order: binary, global opts, subcommand, delegates (positional), command opts *)
      (* Global args (like -f for password file) must come before the subcommand *)
      "ExecStart=/bin/sh -lc 'MODE=${OCTEZ_BAKER_NODE_MODE:-local}; \
       CMD=\"${APP_BIN_DIR}/octez-baker ${OCTEZ_BAKER_GLOBAL_ARGS:-} \
       --base-dir \\\"${OCTEZ_BAKER_BASE_DIR}\\\" --endpoint \
       \\\"${OCTEZ_NODE_ENDPOINT}\\\"\"; if [ \"$MODE\" = \"remote\" ]; then \
       CMD=\"$CMD run remotely\"; else CMD=\"$CMD run with local node \
       \\\"${OCTEZ_DATA_DIR}\\\"\"; fi; CMD=\"$CMD \
       ${OCTEZ_BAKER_DELEGATES_ARGS:-}\"; DAL_CFG=\"${OCTEZ_DAL_CONFIG:-}\"; \
       if [ \"$DAL_CFG\" = \"disabled\" ]; then CMD=\"$CMD --without-dal\"; \
       elif [ -n \"$DAL_CFG\" ]; then CMD=\"$CMD --dal-node \
       \\\"$DAL_CFG\\\"\"; fi; CMD=\"$CMD --liquidity-baking-toggle-vote \
       \\\"${OCTEZ_BAKER_LB_VOTE}\\\"\"; exec $CMD \
       ${OCTEZ_BAKER_COMMAND_ARGS:-}'"
  | "node" ->
      "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/octez-node\" run \
       --data-dir=\"${OCTEZ_DATA_DIR}\" ${OCTEZ_NODE_ARGS:-}'"
  | "accuser" ->
      (* Accuser is a subcommand of octez-baker: octez-baker [global] run accuser [opts] *)
      (* Global args (like -f for password file) must come before 'run accuser' *)
      "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/octez-baker\" \
       ${OCTEZ_BAKER_GLOBAL_ARGS:-} --base-dir \"${OCTEZ_CLIENT_BASE_DIR}\" \
       --endpoint \"${OCTEZ_NODE_ENDPOINT}\" run accuser \
       ${OCTEZ_BAKER_COMMAND_ARGS:-}'"
  | "dal-node" | "dal" ->
      (* DAL node uses octez-dal-node binary directly *)
      "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/octez-dal-node\" run \
       --endpoint \"${OCTEZ_NODE_ENDPOINT}\" --data-dir \
       \"${OCTEZ_DAL_DATA_DIR}\" --rpc-addr \"${OCTEZ_DAL_RPC_ADDR}\" \
       --net-addr \"${OCTEZ_DAL_NET_ADDR}\" ${OCTEZ_SERVICE_ARGS:-}'"
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
  ^ "LMDB_TOREMOVE=\"$DATA_DIR/lmdb_store_to_remove\"\n"
  ^ "LOCK_FILE=\"$DATA_DIR/lock\"\n\n"
  ^ "# Check if store has actual chain data (not just empty directory)\n"
  ^ "has_chain_data() {\n" ^ "  for d in \"$STORE_DIR\"/chain_*; do\n"
  ^ "    [ -d \"$d\" ] && return 0\n" ^ "  done\n" ^ "  return 1\n" ^ "}\n\n"
  ^ "if [ -d \"$STORE_DIR\" ]; then\n" ^ "  if has_chain_data; then\n"
  ^ "    # Store has data - check version.json\n"
  ^ "    if [ ! -f \"$VERSION_FILE\" ]; then\n"
  ^ "      # Missing version.json but store looks valid (dev builds may not \
     create it)\n"
  ^ "      echo \"octez-manager prestart: missing version.json, creating \
     default for existing store\" >&2\n"
  ^ "      echo '{ \"version\": \"3.2\" }' > \"$VERSION_FILE\"\n" ^ "    fi\n"
  ^ "    \"$NODE_BIN\" upgrade storage --data-dir \"$DATA_DIR\"\n"
  ^ "    if [ -d \"$LMDB_TOREMOVE\" ]; then\n"
  ^ "      rm -rf \"$LMDB_TOREMOVE\"\n" ^ "    fi\n" ^ "    exit 0\n"
  ^ "  else\n" ^ "    # Store directory exists but is empty/corrupt - wipe it\n"
  ^ "    echo \"octez-manager prestart: store exists but has no chain data, \
     wiping\" >&2\n"
  ^ "    rm -rf \"$STORE_DIR\" \"$DATA_DIR/context\" \"$VERSION_FILE\"\n"
  ^ "  fi\n" ^ "fi\n\n" ^ "if [ -f \"$LOCK_FILE\" ]; then\n"
  ^ "  rm -f \"$LOCK_FILE\"\n" ^ "fi\n\n"
  ^ "AUTO=\"${OCTEZ_SNAPSHOT_AUTO:-0}\"\n" ^ "if [ \"$AUTO\" != \"1\" ]; then\n"
  ^ "  exit 0\n" ^ "fi\n\n"
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
       Wants=network-online.target\n\
       StartLimitBurst=10\n\
       StartLimitIntervalSec=300s\n\n\
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
     RestartSec=5s\n\
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

(** Validate that service user can access a binary by path.
    This is a convenience wrapper that accepts the full binary path directly
    instead of deriving it from a role. *)
let validate_binary_access ~user ~binary_path =
  if not (Sys.file_exists binary_path) then
    R.error_msgf "Binary not found: %s" binary_path
  else if Common.is_root () then
    match Common.run ["sudo"; "-n"; "-u"; user; "test"; "-x"; binary_path] with
    | Ok () -> Ok ()
    | Error _ -> (
        let cmd = Printf.sprintf "test -x %s" (Common.sh_quote binary_path) in
        match Common.run ["su"; "-s"; "/bin/sh"; "-c"; cmd; user] with
        | Ok () -> Ok ()
        | Error (`Msg m) ->
            R.error_msgf
              "User %s cannot execute %s: %s. Adjust permissions or pick a \
               different service user."
              user
              binary_path
              m)
  else
    match Common.run ["test"; "-x"; binary_path] with
    | Ok () -> Ok ()
    | Error (`Msg m) -> R.error_msgf "Cannot execute %s: %s" binary_path m

let install_unit ?(quiet = false) ~role ~app_bin_dir ~user () =
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
  let* () = run_systemctl_timeout ~quiet ["daemon-reload"] in
  Ok ()

type logging_resources = {extra_lines : string list; extra_paths : string list}

let logging_resources ~role:_ ~logging_mode:_ =
  (* Always use journald - octez binaries handle their own file logging *)
  {
    extra_lines = ["StandardOutput=journal"; "StandardError=journal"];
    extra_paths = [];
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

let write_dropin_body ~role ~data_dir ~logging_mode ~extra_paths ?depends_on ()
    =
  let resources = logging_resources ~role ~logging_mode in
  let rw_paths =
    read_write_paths_for
      ~data_dir
      ~logging_paths:resources.extra_paths
      ~extra_paths
  in
  (* Add dependency directives if depends_on is set *)
  let unit_section =
    match depends_on with
    | Some (parent_role, parent_instance) ->
        let parent_unit =
          Printf.sprintf "octez-%s@%s.service" parent_role parent_instance
        in
        Printf.sprintf
          "[Unit]\nBindsTo=%s\nAfter=%s\n\n"
          parent_unit
          parent_unit
    | None -> ""
  in
  let header =
    let base = ref ["[Service]"] in
    if Common.is_root () then base := !base @ ["PermissionsStartOnly=true"] ;
    !base @ resources.extra_lines
  in
  unit_section
  ^ String.concat
      "\n"
      (header
      @ [Printf.sprintf "Environment=OCTEZ_DATA_DIR=%s" data_dir]
      @ List.map (fun p -> Printf.sprintf "ReadWritePaths=%s" p) rw_paths)
  ^ "\n"

let write_dropin ?(quiet = false) ~role ~inst ~data_dir ~logging_mode
    ?(extra_paths = []) ?depends_on () =
  let dir = dropin_dir role inst in
  let path = dropin_path role inst in
  let owner, group =
    if Common.is_root () then ("root", "root")
    else Common.current_user_group_names ()
  in
  let* () = Common.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  let body =
    write_dropin_body ~role ~data_dir ~logging_mode ~extra_paths ?depends_on ()
  in
  let* () = Common.write_file ~mode:0o644 ~owner ~group path body in
  run_systemctl_timeout ~quiet ["daemon-reload"]

let write_dropin_node ?quiet ~inst ~data_dir ~logging_mode () =
  write_dropin ?quiet ~role:"node" ~inst ~data_dir ~logging_mode ()

let render_logging_lines logging_mode =
  (logging_resources ~role:"node" ~logging_mode).extra_lines

let enable ?quiet:_ ~role ~instance ~start_now () =
  let unit = unit_name role instance in
  let action = if start_now then ["enable"; "--now"] else ["enable"] in
  (* Enable can trigger start, which might take time if deps are slow. *)
  (* Force quiet=false so output is captured in logs if running in background job *)
  run_systemctl_timeout ~quiet:false ~duration:"30s" (action @ [unit])

let disable ?quiet:_ ~role ~instance ~stop_now () =
  let unit = unit_name role instance in
  let action = if stop_now then ["disable"; "--now"] else ["disable"] in
  (* Disable with --now triggers stop, which can take time for node shutdown. *)
  (* Force quiet=false so output is captured in logs if running in background job *)
  run_systemctl_timeout ~quiet:false ~duration:"30s" (action @ [unit])

let start_unit ~unit_name =
  (* Start can take time (e.g. node initialization/upgrade). *)
  run_systemctl_timeout ~quiet:false ~duration:"30s" ["start"; unit_name]

let stop_unit ~unit_name =
  (* Stop needs time for graceful shutdown. *)
  run_systemctl_timeout ~quiet:false ~duration:"30s" ["stop"; unit_name]

let restart_unit ~unit_name =
  (* Restart = stop + start. *)
  run_systemctl_timeout ~quiet:false ~duration:"60s" ["restart"; unit_name]

let enable_unit unit_name =
  run_systemctl_timeout ~quiet:false ~duration:"10s" ["enable"; unit_name]

let disable_unit unit_name =
  run_systemctl_timeout ~quiet:false ~duration:"10s" ["disable"; unit_name]

let start ?quiet:_ ~role ~instance () =
  start_unit ~unit_name:(unit_name role instance)

let stop ?quiet:_ ~role ~instance () =
  stop_unit ~unit_name:(unit_name role instance)

let restart ?quiet:_ ~role ~instance () =
  restart_unit ~unit_name:(unit_name role instance)

let remove_dropin ~role ~instance =
  let path = dropin_dir role instance in
  let _ = Common.remove_tree path in
  ()

module For_tests = struct
  let role_binary = role_binary

  let unit_name = unit_name

  let system_unit_path = system_unit_path

  let user_unit_path = user_unit_path

  let unit_path = unit_path

  let dropin_dir = dropin_dir

  let dropin_path = dropin_path

  let systemctl_cmd = systemctl_cmd

  let env_file_template = env_file_template

  let prestart_hooks_dir = prestart_hooks_dir

  let prestart_script_path = prestart_script_path

  let unit_template ~role ~app_bin_dir ~user ?prestart () =
    unit_template
      ~user_mode:(not (Common.is_root ()))
      ~role
      ~app_bin_dir
      ~user
      ?prestart
      ()

  let render_logging_lines = render_logging_lines

  let exec_line = exec_line

  (** Parse systemd show output for unit state (for testing) *)
  let parse_unit_state_output output =
    let lines = String.split_on_char '\n' output in
    let parse_prop prefix line =
      if
        String.length line > String.length prefix
        && String.sub line 0 (String.length prefix) = prefix
      then
        Some
          (String.sub
             line
             (String.length prefix)
             (String.length line - String.length prefix)
          |> String.trim)
      else None
    in
    let active_state = ref "unknown" in
    let sub_state = ref "unknown" in
    let result = ref None in
    let exit_status = ref None in
    List.iter
      (fun line ->
        (match parse_prop "ActiveState=" line with
        | Some v -> active_state := v
        | None -> ()) ;
        (match parse_prop "SubState=" line with
        | Some v -> sub_state := v
        | None -> ()) ;
        (match parse_prop "Result=" line with
        | Some v when v <> "" && v <> "success" -> result := Some v
        | _ -> ()) ;
        match parse_prop "ExecMainStatus=" line with
        | Some v -> exit_status := int_of_string_opt v
        | None -> ())
      lines ;
    {
      active_state = !active_state;
      sub_state = !sub_state;
      result = !result;
      exit_status = !exit_status;
    }
end

let get_service_paths ~role ~instance =
  let unit_file = unit_path role in
  (* Unit file is template, but helpful to know *)
  let dropin = dropin_path role instance in
  let env_file =
    let tmpl = env_file_template (not (Common.is_root ())) in
    (* Replace %i with instance name *)
    let len = String.length tmpl in
    let buf = Buffer.create len in
    let rec loop i =
      if i >= len then ()
      else if i + 1 < len && tmpl.[i] = '%' && tmpl.[i + 1] = 'i' then (
        Buffer.add_string buf instance ;
        loop (i + 2))
      else (
        Buffer.add_char buf tmpl.[i] ;
        loop (i + 1))
    in
    loop 0 ;
    Buffer.contents buf
  in
  [
    ("Service Unit", unit_file);
    ("Drop-in Override", dropin);
    ("Environment File", env_file);
  ]
