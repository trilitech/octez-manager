(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** System metrics collection for service instances.

    Collects CPU, memory, disk usage, and version information for
    services managed by systemd. Designed to be reusable across
    different service types (node, baker, accuser, dal, signer). *)

open Octez_manager_lib

(** Per-process resource statistics *)
type process_stats = {
  pid : int;
  cpu_percent : float;  (** 0-100 per core *)
  memory_rss : int64;  (** bytes *)
  memory_percent : float;  (** 0-100 *)
}

(** Previous CPU sample for delta calculation *)
type cpu_sample = {
  utime : int64;  (** user time in clock ticks *)
  stime : int64;  (** system time in clock ticks *)
  timestamp : float;  (** Unix timestamp *)
}

(** Aggregate metrics for a service instance *)
type t = {
  version : string option;  (** e.g. "21.0" *)
  cpu_percent : float;  (** sum across all processes *)
  memory_rss : int64;  (** total RSS bytes *)
  memory_percent : float;  (** sum across all processes *)
  data_dir_size : int64 option;  (** bytes *)
  pids : int list;  (** tracked PIDs *)
}

let empty =
  {
    version = None;
    cpu_percent = 0.0;
    memory_rss = 0L;
    memory_percent = 0.0;
    data_dir_size = None;
    pids = [];
  }

(** Page size in bytes (standard on Linux) *)
let page_size = 4096

(** Clock ticks per second (standard on Linux) *)
let clock_ticks_per_sec = 100

(** Get the main PID for a systemd service *)
let get_service_main_pid ~role ~instance =
  let unit_name = Printf.sprintf "octez-%s@%s" role instance in
  let cmd =
    if Common.is_root () then
      ["systemctl"; "show"; "--property=MainPID"; unit_name]
    else ["systemctl"; "--user"; "show"; "--property=MainPID"; unit_name]
  in
  match Common.run_out cmd with
  | Ok output -> (
      (* Output is "MainPID=12345" *)
      match String.split_on_char '=' output with
      | [_; pid_str] -> (
          let pid_str = String.trim pid_str in
          match int_of_string_opt pid_str with
          | Some pid when pid > 0 -> Some pid
          | _ -> None)
      | _ -> None)
  | Error _ -> None

(** Get child PIDs of a process *)
let get_child_pids ~parent_pid =
  let path =
    Printf.sprintf "/proc/%d/task/%d/children" parent_pid parent_pid
  in
  try
    let ic = open_in path in
    let line = try input_line ic with End_of_file -> "" in
    close_in ic ;
    String.split_on_char ' ' line
    |> List.filter_map (fun s ->
           let s = String.trim s in
           if s = "" then None else int_of_string_opt s)
  with _ -> []

(** Get all PIDs for a service (main + children, recursively) *)
let get_service_pids ~role ~instance =
  match get_service_main_pid ~role ~instance with
  | None -> []
  | Some main_pid ->
      let rec collect_all pid =
        let children = get_child_pids ~parent_pid:pid in
        pid :: List.concat_map collect_all children
      in
      collect_all main_pid

(** Read /proc/<pid>/stat and parse CPU times *)
let read_proc_stat ~pid =
  let path = Printf.sprintf "/proc/%d/stat" pid in
  try
    let ic = open_in path in
    let line = input_line ic in
    close_in ic ;
    (* Format: pid (comm) state ppid pgrp session tty_nr tpgid flags minflt
       cminflt majflt cmajflt utime stime cutime cstime ...
       Fields are space-separated, but comm can contain spaces and is in parens *)
    let close_paren = String.rindex line ')' in
    let rest = String.sub line (close_paren + 2) (String.length line - close_paren - 2) in
    let fields = String.split_on_char ' ' rest in
    (* After (comm), fields are: state(0) ppid(1) ... utime(11) stime(12) ... rss(21) *)
    let utime = List.nth_opt fields 11 |> Option.map Int64.of_string in
    let stime = List.nth_opt fields 12 |> Option.map Int64.of_string in
    let rss_pages = List.nth_opt fields 21 |> Option.map Int64.of_string in
    match (utime, stime, rss_pages) with
    | Some u, Some s, Some rss ->
        Some (u, s, Int64.mul rss (Int64.of_int page_size))
    | _ -> None
  with _ -> None

(** Read total system memory from /proc/meminfo *)
let total_memory =
  lazy
    (try
       let ic = open_in "/proc/meminfo" in
       let result = ref 0L in
       (try
          while true do
            let line = input_line ic in
            if String.length line > 9 && String.sub line 0 9 = "MemTotal:" then (
              let parts =
                String.split_on_char ' ' line |> List.filter (fun s -> s <> "")
              in
              match parts with
              | _ :: kb_str :: _ -> (
                  match Int64.of_string_opt kb_str with
                  | Some kb ->
                      result := Int64.mul kb 1024L ;
                      raise Exit
                  | None -> ())
              | _ -> ())
          done
        with Exit | End_of_file -> ()) ;
       close_in ic ;
       !result
     with _ -> 0L)

(** Calculate CPU percentage from two samples *)
let calc_cpu_percent ~prev ~curr =
  let delta_time = curr.timestamp -. prev.timestamp in
  if delta_time <= 0.0 then 0.0
  else
    let delta_utime = Int64.sub curr.utime prev.utime in
    let delta_stime = Int64.sub curr.stime prev.stime in
    let delta_ticks = Int64.add delta_utime delta_stime in
    let ticks_per_interval =
      delta_time *. Float.of_int clock_ticks_per_sec
    in
    if ticks_per_interval <= 0.0 then 0.0
    else
      (* CPU percentage (can exceed 100% on multi-core systems) *)
      Int64.to_float delta_ticks /. ticks_per_interval *. 100.0

(** Get process stats with CPU calculated from previous sample *)
let get_process_stats ~pid ~prev_sample =
  match read_proc_stat ~pid with
  | None -> None
  | Some (utime, stime, rss) ->
      let now = Unix.gettimeofday () in
      let curr_sample = {utime; stime; timestamp = now} in
      let cpu_percent =
        match prev_sample with
        | None -> 0.0
        | Some prev -> calc_cpu_percent ~prev ~curr:curr_sample
      in
      let mem_total = Lazy.force total_memory in
      let memory_percent =
        if mem_total > 0L then
          Int64.to_float rss /. Int64.to_float mem_total *. 100.0
        else 0.0
      in
      Some
        ( {pid; cpu_percent; memory_rss = rss; memory_percent},
          curr_sample )

(** Extract version string from binary --version output *)
let parse_version_output output =
  (* Look for "Octez XX.Y" pattern *)
  let lines = String.split_on_char '\n' output in
  let rec find_version = function
    | [] -> None
    | line :: rest -> (
        match Str.search_forward (Str.regexp "Octez \\([0-9]+\\.[0-9]+\\)") line 0 with
        | _ -> Some (Str.matched_group 1 line)
        | exception Not_found -> find_version rest)
  in
  find_version lines

(** Get version string from a binary *)
let get_version ~binary =
  let full_path =
    if Filename.is_relative binary then
      match Common.which binary with Some p -> p | None -> binary
    else binary
  in
  if not (Sys.file_exists full_path) then None
  else
    (* Use shell to redirect stderr and timeout to avoid broken pipe noise *)
    let cmd = Printf.sprintf "timeout 2s %s --version 2>/dev/null" (Common.sh_quote full_path) in
    match Common.run_out ["sh"; "-c"; cmd] with
    | Ok output -> parse_version_output output
    | Error _ -> None

(** Get directory size using du *)
let get_dir_size ~path =
  if not (Sys.file_exists path) then None
  else
    match Common.run_out ["du"; "-sb"; path] with
    | Ok output -> (
        (* Output is "12345\t/path" *)
        match String.split_on_char '\t' output with
        | size_str :: _ -> Int64.of_string_opt (String.trim size_str)
        | _ -> None)
    | Error _ -> None

(** Format bytes as human-readable string *)
let format_bytes bytes =
  let b = Int64.to_float bytes in
  if b >= 1099511627776.0 then Printf.sprintf "%.1fT" (b /. 1099511627776.0)
  else if b >= 1073741824.0 then Printf.sprintf "%.1fG" (b /. 1073741824.0)
  else if b >= 1048576.0 then Printf.sprintf "%.0fM" (b /. 1048576.0)
  else if b >= 1024.0 then Printf.sprintf "%.0fK" (b /. 1024.0)
  else Printf.sprintf "%LdB" bytes
