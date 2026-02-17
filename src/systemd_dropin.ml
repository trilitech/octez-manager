(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Systemd drop-in override generation and installation.

    This module handles generating systemd drop-in configuration files for
    per-instance overrides (data directory, logging, read-write paths, etc.).
    It is extracted from {!Systemd} to keep that module focused on service
    lifecycle and queries. *)

let ( let* ) = Result.bind

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
    if Paths.is_root () then [data_dir; "/var/log/octez"] else [data_dir]
  in
  unique_non_empty (base @ logging_paths @ extra_paths)

let write_dropin_body ~role ~data_dir ~logging_mode ~extra_paths ?app_bin_dir
    ?depends_on () =
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
    | Some dependencies when dependencies <> [] ->
        let binds_to_lines =
          List.map
            (fun (parent_role, parent_instance) ->
              Printf.sprintf "octez-%s@%s.service" parent_role parent_instance)
            dependencies
        in
        let after_lines = binds_to_lines in
        Printf.sprintf
          "[Unit]\n%s\n%s\n\n"
          (String.concat
             "\n"
             (List.map (Printf.sprintf "BindsTo=%s") binds_to_lines))
          (String.concat
             "\n"
             (List.map (Printf.sprintf "After=%s") after_lines))
    | _ -> ""
  in
  let header =
    let base = ["[Service]"] in
    let base =
      if Paths.is_root () then base @ ["PermissionsStartOnly=true"] else base
    in
    base @ resources.extra_lines
  in
  let env_lines =
    [Printf.sprintf "Environment=OCTEZ_DATA_DIR=%s" data_dir]
    @
    match app_bin_dir with
    | Some dir -> [Printf.sprintf "Environment=APP_BIN_DIR=%s" dir]
    | None -> []
  in
  unit_section
  ^ String.concat
      "\n"
      (header @ env_lines
      @ List.map (fun p -> Printf.sprintf "ReadWritePaths=%s" p) rw_paths)
  ^ "\n"

let write_dropin ?(quiet = false) ~dropin_dir ~dropin_path ~daemon_reload ~role
    ~inst ~data_dir ~logging_mode ?(extra_paths = []) ?app_bin_dir ?depends_on
    () =
  let dir = dropin_dir role inst in
  let path = dropin_path role inst in
  let owner, group =
    if Paths.is_root () then ("root", "root")
    else Paths.current_user_group_names ()
  in
  let* () = File_ops.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  let body =
    write_dropin_body
      ~role
      ~data_dir
      ~logging_mode
      ~extra_paths
      ?app_bin_dir
      ?depends_on
      ()
  in
  let* () = File_ops.write_file ~mode:0o644 ~owner ~group path body in
  daemon_reload ~quiet

let write_dropin_node ?quiet ~dropin_dir ~dropin_path ~daemon_reload ~inst
    ~data_dir ~logging_mode ?app_bin_dir () =
  write_dropin
    ?quiet
    ~dropin_dir
    ~dropin_path
    ~daemon_reload
    ~role:"node"
    ~inst
    ~data_dir
    ~logging_mode
    ?app_bin_dir
    ()

let render_logging_lines logging_mode =
  (logging_resources ~role:"node" ~logging_mode).extra_lines
