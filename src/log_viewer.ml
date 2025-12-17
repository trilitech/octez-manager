(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type log_source = Journald | DailyLogs

let unit_name ~role ~instance = Printf.sprintf "octez-%s@%s" role instance

let get_daily_log_file ~role ~instance =
  (* Read the instance's env file to find the correct base directory *)
  let env =
    match Node_env.read ~inst:instance with Ok pairs -> pairs | Error _ -> []
  in
  let lookup key =
    match List.assoc_opt key env with
    | Some v when String.trim v <> "" -> Some (String.trim v)
    | _ -> None
  in
  (* Find the logs directory based on role *)
  let logs_dir =
    match Service_registry.find ~instance with
    | Error _ | Ok None -> None
    | Ok (Some svc) -> (
        match role with
        | "node" ->
            (* Node: <data_dir>/daily_logs/ *)
            Some (Filename.concat svc.Service.data_dir "daily_logs")
        | "baker" ->
            (* Baker: <base_dir>/logs/octez-baker/ *)
            let base =
              Option.value (lookup "OCTEZ_BAKER_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Some (Filename.concat (Filename.concat base "logs") "octez-baker")
        | "accuser" ->
            (* Accuser: <base_dir>/logs/octez-accuser/ *)
            let base =
              Option.value (lookup "OCTEZ_CLIENT_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Some (Filename.concat (Filename.concat base "logs") "octez-accuser")
        | "dal-node" ->
            (* DAL node: <data_dir>/daily_logs/ *)
            let base =
              Option.value (lookup "OCTEZ_DAL_DATA_DIR")
                ~default:svc.Service.data_dir
            in
            Some (Filename.concat base "daily_logs")
        | "signer" ->
            (* Signer: <base_dir>/logs/octez-signer/ *)
            let base =
              Option.value (lookup "OCTEZ_SIGNER_BASE_DIR")
                ~default:svc.Service.data_dir
            in
            Some (Filename.concat (Filename.concat base "logs") "octez-signer")
        | _ -> Some (Filename.concat svc.Service.data_dir "daily_logs"))
  in
  match logs_dir with
  | None -> Error (`Msg "Instance not found")
  | Some dir ->
      if not (Sys.file_exists dir) then
        Error (`Msg (Printf.sprintf "Logs directory does not exist: %s" dir))
      else
        let today = Unix.time () |> Unix.localtime in
        let filename =
          Printf.sprintf "daily-%04d%02d%02d.log" (1900 + today.tm_year)
            (today.tm_mon + 1) today.tm_mday
        in
        let log_file = Filename.concat dir filename in
        if Sys.file_exists log_file then Ok log_file
        else
          Error (`Msg (Printf.sprintf "Log file does not exist: %s" log_file))

let read_logs ~role ~instance ~source ~lines =
  match source with
  | Journald -> (
      let user_flag = if Common.is_root () then "" else "--user " in
      let unit = unit_name ~role ~instance in
      let cmd =
        Printf.sprintf
          "journalctl %s-u %s -n %d --no-pager"
          user_flag
          (Filename.quote unit)
          lines
      in
      let ic = Unix.open_process_in cmd in
      let rec read_all acc =
        match input_line ic with
        | line -> read_all (line :: acc)
        | exception End_of_file -> List.rev acc
      in
      let logs = read_all [] in
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> Ok logs
      | _ -> Error (`Msg "Failed to read journald logs"))
  | DailyLogs -> (
      match get_daily_log_file ~role ~instance with
      | Error e -> Error e
      | Ok log_file ->
          let cmd =
            Printf.sprintf "tail -n %d %s" lines (Filename.quote log_file)
          in
          let ic = Unix.open_process_in cmd in
          let rec read_all acc =
            match input_line ic with
            | line -> read_all (line :: acc)
            | exception End_of_file -> List.rev acc
          in
          let logs = read_all [] in
          ( match Unix.close_process_in ic with
          | Unix.WEXITED 0 -> Ok logs
          | _ -> Error (`Msg "Failed to read daily log file") ))

(* Unused - File_pager handles tailing for daily logs *)
(* let tail_logs ~role ~instance ~source = ... *)
