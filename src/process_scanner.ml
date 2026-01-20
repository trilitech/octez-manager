(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Scan running processes for Octez binaries not managed by systemd *)

type process_info = {
  pid : int;
  cmdline : string;
  binary_path : string option;
  binary_realpath : string option;  (** Resolved from /proc/PID/exe *)
  parent_pid : int option;
  user : string option;
}

(** Read /proc/<pid>/cmdline and parse as null-separated string *)
let read_cmdline pid =
  let path = Printf.sprintf "/proc/%d/cmdline" pid in
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let buffer = Buffer.create 4096 in
        let rec read_loop () =
          try
            Buffer.add_channel buffer ic 4096 ;
            read_loop ()
          with End_of_file -> ()
        in
        read_loop () ;
        let content = Buffer.contents buffer in
        (* cmdline is null-separated, convert to spaces *)
        let cmdline =
          String.map (fun c -> if c = '\000' then ' ' else c) content
        in
        Some (String.trim cmdline))
  with _ -> None

(** Read /proc/<pid>/status to get PPid and Uid *)
let read_status pid =
  let path = Printf.sprintf "/proc/%d/status" pid in
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let rec read_fields ppid uid =
          match input_line ic with
          | line ->
              if String.starts_with ~prefix:"PPid:" line then
                let ppid_str =
                  String.sub line 5 (String.length line - 5) |> String.trim
                in
                read_fields (int_of_string_opt ppid_str) uid
              else if String.starts_with ~prefix:"Uid:" line then
                let parts = String.split_on_char '\t' line in
                let uid =
                  match parts with
                  | _ :: uid_str :: _ -> int_of_string_opt (String.trim uid_str)
                  | _ -> None
                in
                read_fields ppid uid
              else read_fields ppid uid
          | exception End_of_file -> (ppid, uid)
        in
        read_fields None None)
  with _ -> (None, None)

(** Get username from UID *)
let get_username uid =
  try
    let ic =
      Unix.open_process_in (Printf.sprintf "id -un %d 2>/dev/null" uid)
    in
    let username = input_line ic in
    let _ = Unix.close_process_in ic in
    Some (String.trim username)
  with _ -> None

(** List all PIDs in /proc *)
let list_all_pids () =
  try
    Sys.readdir "/proc" |> Array.to_list
    |> List.filter_map (fun entry ->
        match int_of_string_opt entry with
        | Some pid when pid > 0 -> Some pid
        | _ -> None)
  with _ -> []

(** Check if string contains substring *)
let contains_substring str sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) str 0 in
    true
  with Not_found -> false

(** Extract binary path from command line (first token) *)
let extract_binary_path cmdline =
  match String.split_on_char ' ' cmdline with
  | [] -> None
  | first :: _ ->
      let first = String.trim first in
      if first <> "" && not (String.starts_with ~prefix:"-" first) then
        Some first
      else None

(** Resolve absolute path to binary from /proc/<pid>/exe symlink *)
let read_binary_realpath pid =
  let path = Printf.sprintf "/proc/%d/exe" pid in
  try
    (* readlink on /proc/PID/exe gives the actual binary path *)
    let ic =
      Unix.open_process_in (Printf.sprintf "readlink %s 2>/dev/null" path)
    in
    Fun.protect
      ~finally:(fun () -> ignore (Unix.close_process_in ic))
      (fun () ->
        try
          let line = input_line ic in
          Some (String.trim line)
        with End_of_file -> None)
  with _ -> None

(** Check if command line's first token is an Octez binary *)
let is_octez_binary cmdline =
  let octez_binaries =
    [
      "octez-node";
      "octez-baker";
      "octez-accuser";
      "octez-dal-node";
      "octez-signer";
      "octez-client";
      "tezos-node";
      "tezos-baker";
      "tezos-accuser";
    ]
  in
  (* Extract first token (the executable) *)
  match extract_binary_path cmdline with
  | None -> false
  | Some binary_path ->
      (* Check if the binary path ends with one of the Octez binary names *)
      List.exists
        (fun binary ->
          String.ends_with ~suffix:binary binary_path
          || String.ends_with ~suffix:("/" ^ binary) binary_path)
        octez_binaries

(** Scan all processes for Octez binaries *)
let scan_octez_processes () =
  list_all_pids ()
  |> List.filter_map (fun pid ->
      match read_cmdline pid with
      | None -> None
      | Some cmdline when not (is_octez_binary cmdline) -> None
      | Some cmdline ->
          let ppid, uid = read_status pid in
          let user = Option.bind uid get_username in
          let binary_path = extract_binary_path cmdline in
          let binary_realpath = read_binary_realpath pid in
          Some
            {
              pid;
              cmdline;
              binary_path;
              binary_realpath;
              parent_pid = ppid;
              user;
            })

(** Check if PID is managed by a systemd service unit (not just in user.slice) *)
let is_systemd_managed pid =
  (* Check if process has a systemd service cgroup.
     We need to distinguish between:
     - Actual systemd services: /system.slice/octez-node@mainnet.service/...
     - User session processes: /user.slice/user-1000.slice/session-123.scope/...
     
     Only the former should be filtered out (as they're already detected via systemd). *)
  let path = Printf.sprintf "/proc/%d/cgroup" pid in
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let rec check_lines () =
          match input_line ic with
          | line ->
              (* Look for actual systemd SERVICE units in cgroup path.
                 Examples that SHOULD be filtered (systemd-managed):
                   0::/system.slice/octez-node@mainnet.service
                   0::/system.slice/system-octez\x2dnode.slice/octez-node@mainnet.service
                 Examples that should NOT be filtered (standalone):
                   0::/user.slice/user-1000.slice/session-3.scope
                   0::/user.slice/user-1000.slice/user@1000.service/app.slice/vte-spawn-xxx.scope
              *)
              if
                (* Check for octez/tezos service units specifically *)
                contains_substring line "octez-"
                && contains_substring line ".service"
                || contains_substring line "tezos-"
                   && contains_substring line ".service"
              then true
              else check_lines ()
          | exception End_of_file -> false
        in
        check_lines ())
  with _ -> false

(** Get standalone (non-systemd) Octez processes *)
let get_standalone_processes () =
  scan_octez_processes ()
  |> List.filter (fun proc -> not (is_systemd_managed proc.pid))
