(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type log_source = Journald | DailyLogs

let unit_name ~role ~instance = Printf.sprintf "octez-%s@%s" role instance

let get_daily_log_file ~data_dir =
  let daily_logs_dir = Filename.concat data_dir "daily_logs" in
  if not (Sys.file_exists daily_logs_dir) then
    Error
      (`Msg
         (Printf.sprintf
            "daily_logs directory does not exist: %s"
            daily_logs_dir))
  else
    let today = Unix.time () |> Unix.localtime in
    let filename =
      Printf.sprintf
        "daily-%04d%02d%02d.log"
        (1900 + today.tm_year)
        (today.tm_mon + 1)
        today.tm_mday
    in
    let log_file = Filename.concat daily_logs_dir filename in
    if Sys.file_exists log_file then Ok log_file
    else Error (`Msg (Printf.sprintf "Log file does not exist: %s" log_file))

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
      match Service_registry.find ~instance with
      | Error (`Msg e) -> Error (`Msg e)
      | Ok None -> Error (`Msg "Instance not found in registry")
      | Ok (Some svc) -> (
          match get_daily_log_file ~data_dir:svc.Service.data_dir with
          | Error e -> Error e
          | Ok log_file -> (
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
              match Unix.close_process_in ic with
              | Unix.WEXITED 0 -> Ok logs
              | _ -> Error (`Msg "Failed to read daily log file"))))

(* Unused - File_pager handles tailing for daily logs *)
(* let tail_logs ~role ~instance ~source = ... *)
