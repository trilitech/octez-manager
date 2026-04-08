(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Systemd unit file template generation and installation.

    This module handles generating systemd service unit files, prestart scripts,
    and binary validation. It is extracted from {!Systemd} to keep that module
    focused on service lifecycle and queries. *)

open Rresult

let ( let* ) = Result.bind

(** Map a service role name to its binary name. *)
let role_binary role =
  match String.lowercase_ascii role with
  | "node" -> "octez-node"
  | "baker" -> "octez-baker"
  | "accuser" -> "octez-baker"
  | "dal" | "dal-node" -> "octez-dal-node"
  | "signatory" -> "signatory"
  | "index" -> "octez-index"
  | other -> "octez-" ^ other

let env_file_template user_mode =
  let base =
    if user_mode then Paths.env_instances_base_dir ()
    else "/etc/octez/instances"
  in
  Filename.concat base "%i/node.env"

let exec_line role =
  match String.lowercase_ascii role with
  | "baker" ->
      (* Order: binary, global opts, subcommand, delegates (positional), command opts *)
      (* Global args (like -f for password file) must come before the subcommand *)
      (* --extra-node flags must come AFTER the run command, not before *)
      "ExecStart=/bin/sh -lc 'MODE=${OCTEZ_BAKER_NODE_MODE:-local}; \
       CMD=\"${APP_BIN_DIR}/octez-baker ${OCTEZ_BAKER_GLOBAL_ARGS:-} \
       --base-dir \\\"${OCTEZ_BAKER_BASE_DIR}\\\" --endpoint \
       \\\"${OCTEZ_NODE_ENDPOINT}\\\"\"; if [ \"$MODE\" = \"remote\" ]; then \
       CMD=\"$CMD run remotely\"; else CMD=\"$CMD run with local node \
       \\\"${OCTEZ_DATA_DIR}\\\"\"; fi; CMD=\"$CMD \
       ${OCTEZ_BAKER_DELEGATES_ARGS:-}\"; \
       EXTRA_ENDPOINTS=\"${OCTEZ_EXTRA_NODE_ENDPOINTS:-}\"; if [ -n \
       \"$EXTRA_ENDPOINTS\" ]; then IFS=\",\"; for ep in $EXTRA_ENDPOINTS; do \
       CMD=\"$CMD --extra-node \\\"$ep\\\"\"; done; unset IFS; fi; \
       DAL_CFG=\"${OCTEZ_DAL_CONFIG:-}\"; if [ \"$DAL_CFG\" = \"disabled\" ]; \
       then CMD=\"$CMD --without-dal\"; elif [ -n \"$DAL_CFG\" ]; then \
       CMD=\"$CMD --dal-node \\\"$DAL_CFG\\\"\"; fi; CMD=\"$CMD \
       --liquidity-baking-toggle-vote \\\"${OCTEZ_BAKER_LB_VOTE}\\\"\"; exec \
       $CMD ${OCTEZ_BAKER_COMMAND_ARGS:-}'"
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
      (* DAL node uses octez-dal-node binary directly.
         --rpc-addr and --net-addr are only included when the corresponding
         env vars are non-empty (${VAR:+...} expands to empty when VAR is unset
         or empty).  This preserves the original config.json when those flags
         were not present in the imported service's ExecStart. *)
      "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/octez-dal-node\" run \
       --endpoint \"${OCTEZ_NODE_ENDPOINT}\" --data-dir \
       \"${OCTEZ_DAL_DATA_DIR}\" ${OCTEZ_DAL_RPC_ADDR:+--rpc-addr \
       \"${OCTEZ_DAL_RPC_ADDR}\"} ${OCTEZ_DAL_NET_ADDR:+--net-addr \
       \"${OCTEZ_DAL_NET_ADDR}\"} ${OCTEZ_SERVICE_ARGS:-}'"
  | "signatory" ->
      (* Signatory remote signer uses signatory binary with config file *)
      "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/signatory\" serve \
       --base-dir \"${OCTEZ_DATA_DIR}\" --config \"${SIGNATORY_CONFIG_PATH}\"'"
  | "index" ->
      (* octez-index run --base-dir ... --endpoint ...
         --rpc-addr is only added when OCTEZ_INDEX_RPC_ADDR is non-empty.
         OCTEZ_SERVICE_ARGS carries --watched-address and --db-name flags. *)
      "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/octez-index\" run \
       --base-dir \"${OCTEZ_INDEXER_DIR}\" --endpoint \
       \"${OCTEZ_NODE_ENDPOINT}\" ${OCTEZ_INDEX_RPC_ADDR:+--rpc-addr \
       \"${OCTEZ_INDEX_RPC_ADDR}\"} ${OCTEZ_SERVICE_ARGS:-}'"
  | other ->
      Printf.sprintf
        "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/octez-%s\" \
         ${OCTEZ_SERVICE_ARGS:-}'"
        other

let prestart_hooks_dir () =
  let base =
    if Paths.is_root () then "/usr/lib/octez-manager"
    else Filename.concat (Paths.xdg_data_home ()) "octez-manager"
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
     \"$TMP\"\n" ^ "}\n\n" ^ "import_snapshot() {\n"
  ^ "  EXTRA=\"${OCTEZ_SNAPSHOT_NO_CHECK:-0}\"\n" ^ "  IMPORT_OUTPUT=$(\n"
  ^ "    if [ \"$EXTRA\" = \"1\" ]; then\n"
  ^ "      \"$NODE_BIN\" snapshot import --no-check --force --data-dir \
     \"$DATA_DIR\" \"$TMP\" 2>&1\n" ^ "    else\n"
  ^ "      \"$NODE_BIN\" snapshot import --force --data-dir \"$DATA_DIR\" \
     \"$TMP\" 2>&1\n" ^ "    fi\n" ^ "  ) && return 0\n"
  ^ "  # Check if the failure is a chain name mismatch (stale snapshot from\n"
  ^ "  # a previous weeklynet rotation). If so, wipe and let the node sync\n"
  ^ "  # from genesis instead of failing the service start.\n"
  ^ "  LOWER=$(echo \"$IMPORT_OUTPUT\" | tr '[:upper:]' '[:lower:]')\n"
  ^ "  case \"$LOWER\" in\n"
  ^ "    *\"not consistent\"*\"chain name\"*|*\"chain name\"*\"not \
     consistent\"*)\n"
  ^ "      echo \"octez-manager prestart: snapshot is from a different chain \
     (stale weeklynet?), syncing from genesis\" >&2\n"
  ^ "      rm -rf \"$STORE_DIR\" \"$DATA_DIR/context\" \"$VERSION_FILE\" \
     \"$LOCK_FILE\"\n"
  ^ "      # Re-create config since data dir may be in a bad state\n"
  ^ "      rm -f \"$CONFIG\"\n" ^ "      ensure_config\n" ^ "      return 0\n"
  ^ "      ;;\n" ^ "    *)\n" ^ "      echo \"$IMPORT_OUTPUT\" >&2\n"
  ^ "      return 1\n" ^ "      ;;\n" ^ "  esac\n" ^ "}\n\n"
  ^ "if fetch_snapshot; then\n" ^ "  ensure_config\n" ^ "  import_snapshot\n"
  ^ "else\n" ^ "  echo \"octez-manager prestart: snapshot fetch skipped\" >&2\n"
  ^ "fi\n"

let write_prestart_script role =
  match String.lowercase_ascii role with
  | "node" ->
      let path = prestart_script_path role in
      let owner, group =
        if Paths.is_root () then ("root", "root")
        else Paths.current_user_group_names ()
      in
      let* () =
        File_ops.write_file
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
  else if Paths.is_root () then
    match Cmd_runner.run ["sudo"; "-n"; "-u"; user; "test"; "-x"; binary] with
    | Ok () -> Ok ()
    | Error _ -> (
        let cmd = Printf.sprintf "test -x %s" (Cmd_runner.sh_quote binary) in
        match Cmd_runner.run ["runuser"; "-s"; "/bin/sh"; "-c"; cmd; user] with
        | Ok () -> Ok ()
        | Error (`Msg m) ->
            R.error_msgf
              "User %s cannot execute %s: %s. Adjust permissions or pick a \
               different service user."
              user
              binary
              m)
  else
    match Cmd_runner.run ["test"; "-x"; binary] with
    | Ok () -> Ok ()
    | Error (`Msg m) -> R.error_msgf "Cannot execute %s: %s" binary m

(** Validate that service user can access a binary by path.
    This is a convenience wrapper that accepts the full binary path directly
    instead of deriving it from a role. *)
let validate_binary_access ~user ~binary_path =
  if not (Sys.file_exists binary_path) then
    R.error_msgf "Binary not found: %s" binary_path
  else if Paths.is_root () then
    match
      Cmd_runner.run ["sudo"; "-n"; "-u"; user; "test"; "-x"; binary_path]
    with
    | Ok () -> Ok ()
    | Error _ -> (
        let cmd =
          Printf.sprintf "test -x %s" (Cmd_runner.sh_quote binary_path)
        in
        match Cmd_runner.run ["runuser"; "-s"; "/bin/sh"; "-c"; cmd; user] with
        | Ok () -> Ok ()
        | Error (`Msg m) ->
            R.error_msgf
              "User %s cannot execute %s: %s. Adjust permissions or pick a \
               different service user."
              user
              binary_path
              m)
  else
    match Cmd_runner.run ["test"; "-x"; binary_path] with
    | Ok () -> Ok ()
    | Error (`Msg m) -> R.error_msgf "Cannot execute %s: %s" binary_path m

let install_unit ?(quiet = false) ~unit_path ~daemon_reload ~role ~app_bin_dir
    ~user () =
  let path = unit_path role in
  let owner, group =
    if Paths.is_root () then ("root", "root")
    else Paths.current_user_group_names ()
  in
  let* () =
    File_ops.ensure_dir_path ~owner ~group ~mode:0o755 (Filename.dirname path)
  in
  let* () = validate_bin_dir ~user ~app_bin_dir ~role in
  let* prestart = write_prestart_script role in
  let body =
    unit_template
      ~user_mode:(not (Paths.is_root ()))
      ~role
      ~app_bin_dir
      ~user
      ?prestart
      ()
  in
  let* () = File_ops.write_file ~mode:0o644 ~owner ~group path body in
  let* () = daemon_reload ~quiet in
  Ok ()
