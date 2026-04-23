(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

let system_unit_path role =
  (* Signatory uses "signatory@.service", other services use "octez-<role>@.service" *)
  match role with
  | "signatory" -> "/etc/systemd/system/signatory@.service"
  | _ -> Printf.sprintf "/etc/systemd/system/octez-%s@.service" role

let user_unit_path role =
  let dir = Filename.concat (Paths.xdg_config_home ()) "systemd/user" in
  (* Signatory uses "signatory@.service", other services use "octez-<role>@.service" *)
  let service_name =
    match role with
    | "signatory" -> "signatory@.service"
    | _ -> Printf.sprintf "octez-%s@.service" role
  in
  Filename.concat dir service_name

let unit_path role =
  if Paths.is_root () then system_unit_path role else user_unit_path role

let dropin_dir role inst =
  (* Signatory uses "signatory@", other services use "octez-<role>@" *)
  let service_prefix =
    match role with "signatory" -> "signatory" | _ -> "octez-" ^ role
  in
  if Paths.is_root () then
    Printf.sprintf "/etc/systemd/system/%s@%s.service.d" service_prefix inst
  else
    let base = Filename.concat (Paths.xdg_config_home ()) "systemd/user" in
    Filename.concat base (Printf.sprintf "%s@%s.service.d" service_prefix inst)

let dropin_path role inst =
  Filename.concat (dropin_dir role inst) "override.conf"

let unit_name role inst =
  (* Signatory uses "signatory@" prefix, other services use "octez-<role>@" *)
  match role with
  | "signatory" -> Printf.sprintf "signatory@%s" inst
  | _ -> Printf.sprintf "octez-%s@%s" role inst

let systemctl_cmd () =
  if Paths.is_root () then ["systemctl"] else ["systemctl"; "--user"]

let run_systemctl_timeout ?quiet ?(duration = "2s") args =
  (* Keep systemctl calls bounded to avoid UI stalls. *)
  Cmd_runner.run ?quiet (("timeout" :: duration :: systemctl_cmd ()) @ args)

let run_systemctl_out_timeout args =
  (* Keep systemctl calls bounded to avoid UI stalls. Shorten to 2s. *)
  Cmd_runner.run_out (("timeout" :: "2s" :: systemctl_cmd ()) @ args)

let run_systemctl_out args = Cmd_runner.run_out (systemctl_cmd () @ args)

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

let validate_bin_dir = Systemd_unit_template.validate_bin_dir

let validate_binary_access = Systemd_unit_template.validate_binary_access

let install_unit ?(quiet = false) ~role ~app_bin_dir ~user () =
  Systemd_unit_template.install_unit
    ~quiet
    ~unit_path
    ~daemon_reload:(fun ~quiet ->
      run_systemctl_timeout ~quiet ["daemon-reload"])
    ~role
    ~app_bin_dir
    ~user
    ()

let write_dropin ?quiet ~role ~inst ~data_dir ~logging_mode ?extra_paths
    ?app_bin_dir ?depends_on () =
  Systemd_dropin.write_dropin
    ?quiet
    ~dropin_dir
    ~dropin_path
    ~daemon_reload:(fun ~quiet ->
      run_systemctl_timeout ~quiet ["daemon-reload"])
    ~role
    ~inst
    ~data_dir
    ~logging_mode
    ?extra_paths
    ?app_bin_dir
    ?depends_on
    ()

let write_dropin_node ?quiet ~inst ~data_dir ~logging_mode ?app_bin_dir () =
  Systemd_dropin.write_dropin_node
    ?quiet
    ~dropin_dir
    ~dropin_path
    ~daemon_reload:(fun ~quiet ->
      run_systemctl_timeout ~quiet ["daemon-reload"])
    ~inst
    ~data_dir
    ~logging_mode
    ?app_bin_dir
    ()

let render_logging_lines = Systemd_dropin.render_logging_lines

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

let reset_failed_unit ~unit_name =
  run_systemctl_timeout ~quiet:true ~duration:"5s" ["reset-failed"; unit_name]

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

let reset_failed ~role ~instance () =
  reset_failed_unit ~unit_name:(unit_name role instance)

let restart ?quiet:_ ~role ~instance () =
  (* Clear StartLimitHit before restarting so a prior failure doesn't block. *)
  let _ = reset_failed ~role ~instance () in
  restart_unit ~unit_name:(unit_name role instance)

let remove_dropin ~role ~instance =
  let path = dropin_dir role instance in
  let _ = File_ops.remove_tree path in
  ()

module For_tests = struct
  let role_binary = Systemd_unit_template.role_binary

  let unit_name = unit_name

  let system_unit_path = system_unit_path

  let user_unit_path = user_unit_path

  let unit_path = unit_path

  let dropin_dir = dropin_dir

  let dropin_path = dropin_path

  let systemctl_cmd = systemctl_cmd

  let env_file_template = Systemd_unit_template.env_file_template

  let prestart_hooks_dir = Systemd_unit_template.prestart_hooks_dir

  let prestart_script_path = Systemd_unit_template.prestart_script_path

  let unit_template ~role ~app_bin_dir ~user ?prestart () =
    Systemd_unit_template.unit_template
      ~user_mode:(not (Paths.is_root ()))
      ~role
      ~app_bin_dir
      ~user
      ?prestart
      ()

  let render_logging_lines = render_logging_lines

  let exec_line = Systemd_unit_template.exec_line

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
    let tmpl =
      Systemd_unit_template.env_file_template (not (Paths.is_root ()))
    in
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

(* ── Payout Timer Management ──────────────────────────────── *)

let ( let* ) = Result.bind

(** Validate instance name to prevent injection attacks in unit files.
    Instance names must match [^[a-zA-Z0-9._-]+$]. *)
let validate_instance_name instance =
  let is_valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' -> true
    | _ -> false
  in
  if String.length instance = 0 then
    Error (`Msg "Instance name cannot be empty")
  else if not (String.for_all is_valid_char instance) then
    Error
      (`Msg
         (Printf.sprintf
            "Invalid instance name '%s': must contain only alphanumeric \
             characters, dots, underscores, and hyphens"
            instance))
  else Ok ()

let payout_unit_name instance =
  Printf.sprintf "octez-manager-payout@%s" instance

let payout_service_path instance =
  if Paths.is_root () then
    Printf.sprintf "/etc/systemd/system/%s.service" (payout_unit_name instance)
  else
    let base = Filename.concat (Paths.xdg_config_home ()) "systemd/user" in
    Filename.concat
      base
      (Printf.sprintf "%s.service" (payout_unit_name instance))

let payout_timer_path instance =
  if Paths.is_root () then
    Printf.sprintf "/etc/systemd/system/%s.timer" (payout_unit_name instance)
  else
    let base = Filename.concat (Paths.xdg_config_home ()) "systemd/user" in
    Filename.concat base (Printf.sprintf "%s.timer" (payout_unit_name instance))

let write_payout_service ~instance ~octez_manager_bin ~service_user () =
  let* () = validate_instance_name instance in
  let path = payout_service_path instance in
  let owner, group =
    if Paths.is_root () then ("root", "root")
    else Paths.current_user_group_names ()
  in
  let user_line =
    match service_user with
    | Some user when Paths.is_root () -> Printf.sprintf "User=%s\n" user
    | _ -> ""
  in
  let content =
    Printf.sprintf
      "[Unit]\n\
       Description=octez-manager payout for %s\n\
       After=network-online.target\n\n\
       [Service]\n\
       Type=oneshot\n\
       %sExecStart=\"%s\" rewards continual run --baker %s\n"
      instance
      user_line
      octez_manager_bin
      instance
  in
  (* Ensure parent directory exists *)
  let dir = Filename.dirname path in
  let* () = File_ops.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  let* () = File_ops.write_file ~mode:0o644 ~owner ~group path content in
  (* Reload systemd *)
  run_systemctl_timeout ~quiet:true ["daemon-reload"]

let write_payout_timer ~instance () =
  let* () = validate_instance_name instance in
  let path = payout_timer_path instance in
  let owner, group =
    if Paths.is_root () then ("root", "root")
    else Paths.current_user_group_names ()
  in
  let content =
    Printf.sprintf
      "[Unit]\n\
       Description=octez-manager payout timer for %s\n\n\
       [Timer]\n\
       OnCalendar=*:0/5\n\
       Persistent=true\n\
       RandomizedDelaySec=60\n\n\
       [Install]\n\
       WantedBy=timers.target\n"
      instance
  in
  (* Ensure parent directory exists *)
  let dir = Filename.dirname path in
  let* () = File_ops.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  let* () = File_ops.write_file ~mode:0o644 ~owner ~group path content in
  (* Reload systemd *)
  run_systemctl_timeout ~quiet:true ["daemon-reload"]

let enable_payout_timer ~instance =
  let unit = payout_unit_name instance ^ ".timer" in
  run_systemctl_timeout ~quiet:false ~duration:"10s" ["enable"; "--now"; unit]

let disable_payout_timer ~instance =
  let unit = payout_unit_name instance ^ ".timer" in
  run_systemctl_timeout ~quiet:false ~duration:"10s" ["disable"; "--now"; unit]

let remove_payout_units ~instance =
  let service_path = payout_service_path instance in
  let timer_path = payout_timer_path instance in
  File_ops.remove_path service_path ;
  File_ops.remove_path timer_path ;
  let _ = run_systemctl_timeout ~quiet:true ["daemon-reload"] in
  ()

let is_payout_timer_active ~instance =
  let unit = payout_unit_name instance ^ ".timer" in
  match run_systemctl_out_timeout ["show"; "--property=ActiveState"; unit] with
  | Ok line ->
      let state =
        match String.split_on_char '=' line with
        | [_; value] -> String.trim value
        | _ -> String.trim line
      in
      String.equal state "active"
  | Error _ -> false

let payout_timer_status ~instance =
  let unit = payout_unit_name instance ^ ".timer" in
  match run_systemctl_out_timeout ["status"; "--no-pager"; unit] with
  | Ok output -> Some output
  | Error _ -> None
