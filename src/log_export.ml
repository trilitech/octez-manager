(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Export logs and diagnostic information for an instance. *)

let ( let* ) = Result.bind

(** Get binary version from app_bin_dir *)
let get_binary_version ~app_bin_dir ~binary =
  let binary_path = Filename.concat app_bin_dir binary in
  if Sys.file_exists binary_path then
    match Common.run_out [binary_path; "--version"] with
    | Ok version -> Some (String.trim version)
    | Error _ -> None
  else None

(** Get the daily logs directory for a service *)
let get_daily_logs_dir ~role ~instance =
  match Service_registry.find ~instance with
  | Error _ | Ok None -> None
  | Ok (Some svc) -> (
      let env =
        match Node_env.read ~inst:instance with
        | Ok pairs -> pairs
        | Error _ -> []
      in
      let lookup key =
        match List.assoc_opt key env with
        | Some v when String.trim v <> "" -> Some (String.trim v)
        | _ -> None
      in
      match role with
      | "node" -> Some (Filename.concat svc.Service.data_dir "daily_logs")
      | "baker" ->
          let base =
            Option.value
              (lookup "OCTEZ_BAKER_BASE_DIR")
              ~default:svc.Service.data_dir
          in
          Some (Filename.concat (Filename.concat base "logs") "octez-baker")
      | "accuser" ->
          let base =
            Option.value
              (lookup "OCTEZ_CLIENT_BASE_DIR")
              ~default:svc.Service.data_dir
          in
          Some (Filename.concat (Filename.concat base "logs") "octez-accuser")
      | "dal-node" | "dal" ->
          let base =
            Option.value
              (lookup "OCTEZ_DAL_DATA_DIR")
              ~default:svc.Service.data_dir
          in
          Some (Filename.concat base "daily_logs")
      | _ -> Some (Filename.concat svc.Service.data_dir "daily_logs"))

(** Collect daily log files from the last N days *)
let collect_daily_logs ~role ~instance ~days =
  match get_daily_logs_dir ~role ~instance with
  | None -> []
  | Some dir ->
      if not (Sys.file_exists dir) then []
      else
        let now = Unix.time () in
        let day_seconds = 86400.0 in
        let files = ref [] in
        for i = 0 to days - 1 do
          let target_time = now -. (float_of_int i *. day_seconds) in
          let tm = Unix.localtime target_time in
          let filename =
            Printf.sprintf
              "daily-%04d%02d%02d.log"
              (1900 + tm.tm_year)
              (tm.tm_mon + 1)
              tm.tm_mday
          in
          let path = Filename.concat dir filename in
          if Sys.file_exists path then files := path :: !files
        done ;
        List.rev !files

(** Export journald logs for the last N days *)
let export_journald_logs ~role ~instance ~days ~output_file =
  let user_flag = if Common.is_root () then "" else "--user " in
  let unit = Printf.sprintf "octez-%s@%s" role instance in
  let since = Printf.sprintf "--since='%d days ago'" days in
  let cmd =
    Printf.sprintf
      "journalctl %s-u %s %s --no-pager > %s 2>/dev/null"
      user_flag
      unit
      since
      (Filename.quote output_file)
  in
  let _ = Sys.command cmd in
  if Sys.file_exists output_file then Some output_file else None

(** Get instance details as a string *)
let get_instance_details ~svc =
  let buf = Buffer.create 1024 in
  let add fmt = Printf.bprintf buf fmt in
  add "Instance Details\n" ;
  add "================\n\n" ;
  add "Instance      : %s\n" svc.Service.instance ;
  add "Role          : %s\n" svc.Service.role ;
  add "Network       : %s\n" svc.Service.network ;
  add "History mode  : %s\n" (History_mode.to_string svc.Service.history_mode) ;
  add "Data dir      : %s\n" svc.Service.data_dir ;
  add "RPC addr      : %s\n" svc.Service.rpc_addr ;
  add "P2P addr      : %s\n" svc.Service.net_addr ;
  add "Service user  : %s\n" svc.Service.service_user ;
  add "App bin dir   : %s\n" svc.Service.app_bin_dir ;
  add "Created at    : %s\n" svc.Service.created_at ;
  add
    "Depends on    : %s\n"
    (Option.value ~default:"(none)" svc.Service.depends_on) ;
  add
    "Dependents    : %s\n"
    (if svc.Service.dependents = [] then "(none)"
     else String.concat ", " svc.Service.dependents) ;
  Buffer.contents buf

(** Get version information *)
let get_version_info ~svc =
  let buf = Buffer.create 512 in
  let add fmt = Printf.bprintf buf fmt in
  add "Version Information\n" ;
  add "===================\n\n" ;
  (* Main binary version *)
  let binary =
    match svc.Service.role with
    | "node" -> "octez-node"
    | "baker" -> "octez-baker"
    | "accuser" -> "octez-accuser"
    | "dal-node" | "dal" -> "octez-dal-node"
    | r -> "octez-" ^ r
  in
  (match get_binary_version ~app_bin_dir:svc.Service.app_bin_dir ~binary with
  | Some v -> add "%s: %s\n" binary v
  | None -> add "%s: (not found)\n" binary) ;
  (* Octez client version for reference *)
  (match
     get_binary_version
       ~app_bin_dir:svc.Service.app_bin_dir
       ~binary:"octez-client"
   with
  | Some v -> add "octez-client: %s\n" v
  | None -> ()) ;
  Buffer.contents buf

(** Get dependency and dependent versions *)
let get_related_versions ~svc =
  let buf = Buffer.create 512 in
  let add fmt = Printf.bprintf buf fmt in
  add "\nRelated Services\n" ;
  add "================\n\n" ;
  (* Dependencies *)
  (match svc.Service.depends_on with
  | None -> add "Dependencies: (none)\n"
  | Some dep_inst -> (
      match Service_registry.find ~instance:dep_inst with
      | Ok (Some dep_svc) ->
          let binary =
            match dep_svc.Service.role with
            | "node" -> "octez-node"
            | "dal-node" | "dal" -> "octez-dal-node"
            | r -> "octez-" ^ r
          in
          let version =
            Option.value
              (get_binary_version
                 ~app_bin_dir:dep_svc.Service.app_bin_dir
                 ~binary)
              ~default:"(unknown)"
          in
          add
            "Dependency: %s (%s) - %s: %s\n"
            dep_inst
            dep_svc.Service.role
            binary
            version
      | _ -> add "Dependency: %s (not found)\n" dep_inst)) ;
  (* Dependents *)
  if svc.Service.dependents = [] then add "Dependents: (none)\n"
  else
    List.iter
      (fun dep_inst ->
        match Service_registry.find ~instance:dep_inst with
        | Ok (Some dep_svc) ->
            let binary =
              match dep_svc.Service.role with
              | "baker" -> "octez-baker"
              | "accuser" -> "octez-accuser"
              | "dal-node" | "dal" -> "octez-dal-node"
              | r -> "octez-" ^ r
            in
            let version =
              Option.value
                (get_binary_version
                   ~app_bin_dir:dep_svc.Service.app_bin_dir
                   ~binary)
                ~default:"(unknown)"
            in
            add
              "Dependent: %s (%s) - %s: %s\n"
              dep_inst
              dep_svc.Service.role
              binary
              version
        | _ -> add "Dependent: %s (not found)\n" dep_inst)
      svc.Service.dependents ;
  Buffer.contents buf

(** Get system information *)
let get_system_info () =
  let buf = Buffer.create 512 in
  let add fmt = Printf.bprintf buf fmt in
  add "\nSystem Information\n" ;
  add "==================\n\n" ;
  (* Hostname *)
  (match Common.run_out ["hostname"] with
  | Ok h -> add "Hostname: %s\n" (String.trim h)
  | Error _ -> ()) ;
  (* Kernel version *)
  (match Common.run_out ["uname"; "-r"] with
  | Ok k -> add "Kernel: %s\n" (String.trim k)
  | Error _ -> ()) ;
  (* Date *)
  (match Common.run_out ["date"; "-Iseconds"] with
  | Ok d -> add "Export date: %s\n" (String.trim d)
  | Error _ -> ()) ;
  (* Disk usage for data dir - handled per-instance *)
  Buffer.contents buf

(** Main export function *)
let export_logs ~instance ~svc =
  let role = svc.Service.role in
  let days = 7 in
  (* Create temp directory for export *)
  let timestamp =
    let tm = Unix.time () |> Unix.localtime in
    Printf.sprintf
      "%04d%02d%02d-%02d%02d%02d"
      (1900 + tm.tm_year)
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
  in
  let export_name = Printf.sprintf "%s-logs-%s" instance timestamp in
  let tmp_dir = Filename.get_temp_dir_name () in
  let export_dir = Filename.concat tmp_dir export_name in
  let archive_path = Filename.concat tmp_dir (export_name ^ ".tar.gz") in
  (* Create export directory *)
  let* () =
    try
      Unix.mkdir export_dir 0o755 ;
      Ok ()
    with Unix.Unix_error (e, _, _) ->
      Error
        (`Msg
           (Printf.sprintf
              "Failed to create temp directory: %s"
              (Unix.error_message e)))
  in
  (* Collect daily logs *)
  let daily_logs = collect_daily_logs ~role ~instance ~days in
  let logs_dir = Filename.concat export_dir "daily_logs" in
  if daily_logs <> [] then (
    Unix.mkdir logs_dir 0o755 ;
    List.iter
      (fun src ->
        let dst = Filename.concat logs_dir (Filename.basename src) in
        ignore (Common.copy_file src dst))
      daily_logs) ;
  (* Export journald logs *)
  let journald_file = Filename.concat export_dir "journald.log" in
  let _ =
    export_journald_logs ~role ~instance ~days ~output_file:journald_file
  in
  (* Write instance details *)
  let details_file = Filename.concat export_dir "instance-details.txt" in
  let details_content =
    get_instance_details ~svc ^ get_version_info ~svc
    ^ get_related_versions ~svc ^ get_system_info ()
  in
  let* () =
    try
      let oc = open_out details_file in
      output_string oc details_content ;
      close_out oc ;
      Ok ()
    with Sys_error msg ->
      Error (`Msg (Printf.sprintf "Failed to write details: %s" msg))
  in
  (* Copy env file *)
  let env_src =
    Filename.concat
      (Filename.concat (Common.env_instances_base_dir ()) instance)
      "node.env"
  in
  if Sys.file_exists env_src then
    ignore (Common.copy_file env_src (Filename.concat export_dir "node.env")) ;
  (* Copy service registry entry *)
  let registry_src =
    Filename.concat (Service_registry.services_dir ()) (instance ^ ".json")
  in
  if Sys.file_exists registry_src then
    ignore
      (Common.copy_file
         registry_src
         (Filename.concat export_dir "service.json")) ;
  (* Create tar.gz archive *)
  let tar_cmd =
    Printf.sprintf
      "tar -czf %s -C %s %s"
      (Filename.quote archive_path)
      (Filename.quote tmp_dir)
      (Filename.quote export_name)
  in
  let* () =
    match Sys.command tar_cmd with
    | 0 -> Ok ()
    | code -> Error (`Msg (Printf.sprintf "tar failed with code %d" code))
  in
  (* Clean up temp directory *)
  let _ = Common.remove_tree export_dir in
  Ok archive_path
