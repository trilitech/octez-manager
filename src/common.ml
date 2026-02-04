(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

(* Eio process manager, set at TUI startup for non-blocking process execution *)
type any_proc_mgr = Mgr : _ Eio.Process.mgr -> any_proc_mgr

let proc_mgr_ref : any_proc_mgr option Atomic.t = Atomic.make None

let set_process_mgr mgr = Atomic.set proc_mgr_ref (Some (Mgr mgr))

let get_process_mgr () = Atomic.get proc_mgr_ref

let is_root () = Unix.geteuid () = 0

let home_dir () =
  match Sys.getenv_opt "HOME" with
  | Some h when h <> "" -> h
  | _ -> ( try (Unix.getpwuid (Unix.geteuid ())).Unix.pw_dir with _ -> ".")

let xdg_config_home () =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some d when d <> "" -> d
  | _ -> Filename.concat (home_dir ()) ".config"

let xdg_data_home () =
  match Sys.getenv_opt "XDG_DATA_HOME" with
  | Some d when d <> "" -> d
  | _ -> Filename.concat (home_dir ()) ".local/share"

let xdg_state_home () =
  match Sys.getenv_opt "XDG_STATE_HOME" with
  | Some d when d <> "" -> d
  | _ -> Filename.concat (home_dir ()) ".local/state"

let current_user_group_names () =
  try
    let pw = Unix.getpwuid (Unix.geteuid ()) in
    let gr = Unix.getgrgid pw.Unix.pw_gid in
    (pw.Unix.pw_name, gr.Unix.gr_name)
  with _ -> ("", "")

let env_instances_base_dir () =
  if is_root () then "/etc/octez/instances"
  else Filename.concat (xdg_config_home ()) "octez/instances"

let default_data_dir inst =
  if is_root () then Filename.concat "/var/lib/octez" inst
  else Filename.concat (Filename.concat (xdg_data_home ()) "octez") inst

let default_role_dir role inst =
  let sanitize s =
    let lower = String.lowercase_ascii (String.trim s) in
    let buf = Bytes.of_string lower in
    for i = 0 to Bytes.length buf - 1 do
      let c = Bytes.get buf i in
      let allowed =
        (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '-' || c = '_'
      in
      if not allowed then Bytes.set buf i '-'
    done ;
    Bytes.to_string buf
  in
  let role_part = match sanitize role with "" -> "service" | clean -> clean in
  let inst_lower = String.lowercase_ascii (String.trim inst) in
  (* Check if instance name already starts with the role prefix *)
  let prefix = role_part ^ "-" in
  let suffix =
    if String.starts_with ~prefix inst_lower then
      (* Instance already has the role prefix - use lowercase for consistency *)
      inst_lower
    else
      (* Instance doesn't have prefix - add it, preserving original case *)
      let inst_trimmed = String.trim inst in
      Printf.sprintf "%s-%s" role_part inst_trimmed
  in
  default_data_dir suffix

let default_log_dir ~role:_ ~instance:_ =
  if is_root () then "/var/log/octez"
  else Filename.concat (xdg_state_home ()) "octez/logs"

let which prog =
  let search_paths =
    let path_entries =
      match Sys.getenv_opt "PATH" with
      | Some p when p <> "" -> String.split_on_char ':' p
      | _ -> []
    in
    let fallbacks =
      ["/usr/bin"; "/usr/sbin"; "/usr/local/bin"; "/usr/local/sbin"]
    in
    path_entries @ fallbacks
  in
  let is_executable path =
    try
      let stats = Unix.stat path in
      stats.Unix.st_kind = Unix.S_REG
      &&
      (Unix.access path [Unix.X_OK] ;
       true)
    with Unix.Unix_error _ -> false
  in
  let candidate dir = if dir = "" then prog else Filename.concat dir prog in
  let rec loop = function
    | [] -> None
    | dir :: rest ->
        let path = candidate dir in
        if is_executable path then Some path else loop rest
  in
  loop search_paths

let make_absolute_path path =
  let trimmed = String.trim path in
  if trimmed = "" then Error "Path cannot be empty"
  else if Filename.is_relative trimmed then
    Ok (Filename.concat (Sys.getcwd ()) trimmed)
  else Ok trimmed

let ensure_dir_path ~owner ~group ~mode path =
  let rec mkdir_p p =
    if p = "/" || p = "." then ()
    else (
      mkdir_p (Filename.dirname p) ;
      try Unix.mkdir p mode with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  mkdir_p path ;
  if is_root () then
    try
      let pw = Unix.getpwnam owner in
      let gr = Unix.getgrnam group in
      Unix.chown path pw.Unix.pw_uid gr.Unix.gr_gid ;
      Unix.chmod path mode ;
      Ok ()
    with
    | Not_found ->
        R.error_msgf
          "User '%s' or group '%s' does not exist (required for %s)"
          owner
          group
          path
    | Unix.Unix_error (err, fn, arg) ->
        R.error_msgf
          "Failed to set ownership on %s: %s(%s, %s)"
          path
          (Unix.error_message err)
          fn
          arg
  else (
    (try
       let pw = Unix.getpwnam owner in
       let gr = Unix.getgrnam group in
       Unix.chown path pw.Unix.pw_uid gr.Unix.gr_gid ;
       Unix.chmod path mode
     with _ -> ()) ;
    Ok ())

let write_file ~mode ~owner ~group path contents =
  let dir = Filename.dirname path in
  let* _ = ensure_dir_path ~owner ~group ~mode:0o755 dir in
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  output_string oc contents ;
  close_out oc ;
  (try Unix.chmod tmp mode with _ -> ()) ;
  (try
     let pw = Unix.getpwnam owner in
     let gr = Unix.getgrnam group in
     Unix.chown tmp pw.Unix.pw_uid gr.Unix.gr_gid
   with _ -> ()) ;
  Sys.rename tmp path ;
  Ok ()

let append_debug_log line =
  try
    let oc =
      open_out_gen [Open_append; Open_creat] 0o644 "/tmp/octez_manager_cmds.log"
    in
    output_string oc (line ^ "\n") ;
    close_out oc
  with _ -> ()

let sh_quote s =
  let needs =
    let n = String.length s in
    let rec loop i =
      if i = n then false
      else
        match s.[i] with
        | ' ' | '\t' | '\n' | '"' | '\'' | '$' | '`' | '\\' | '*' | '?' | '['
        | ']' | ';' | '&' | '|' | '<' | '>' | '(' | ')' | '{' | '}' ->
            true
        | _ -> loop (i + 1)
    in
    loop 0
  in
  if not needs then s
  else
    let parts = String.split_on_char '\'' s in
    "'" ^ String.concat "'\"'\"'" parts ^ "'"

let cmd_to_string argv = String.concat " " (List.map sh_quote argv)

(* --- Eio-based process execution (used when process_mgr is set) --- *)

let read_lines_eio flow ~on_line =
  let reader = Eio.Buf_read.of_flow ~max_size:(10 * 1024 * 1024) flow in
  let rec loop () =
    match Eio.Buf_read.line reader with
    | line ->
        on_line line ;
        loop ()
    | exception End_of_file -> ()
  in
  loop ()

(* In TUI mode stdout/stderr are always captured via pipes — there is no
   terminal to inherit — so the [quiet] flag has no effect on the Eio path. *)
let run_eio (Mgr mgr) ~quiet:_ ?on_log argv =
  Eio.Switch.run @@ fun sw ->
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
  let proc = Eio.Process.spawn ~sw mgr ~stdout:stdout_w ~stderr:stderr_w argv in
  Eio.Flow.close stdout_w ;
  Eio.Flow.close stderr_w ;
  let log_lines = ref [] in
  let handle_line line =
    (match on_log with Some f -> f line | None -> ()) ;
    log_lines := line :: !log_lines
  in
  Eio.Fiber.both
    (fun () ->
      read_lines_eio (stdout_r :> _ Eio.Flow.source) ~on_line:handle_line)
    (fun () ->
      read_lines_eio (stderr_r :> _ Eio.Flow.source) ~on_line:handle_line) ;
  match Eio.Process.await proc with
  | `Exited 0 -> Ok ()
  | _ ->
      let msg =
        Printf.sprintf
          "Command failed: %s\nOutput:\n%s"
          (cmd_to_string argv)
          (String.concat "\n" (List.rev !log_lines))
      in
      append_debug_log ("RUN ERROR: " ^ msg) ;
      Error (`Msg msg)

(* Drain stderr in parallel with stdout to prevent the process from blocking
   if the stderr pipe buffer fills up. *)
let run_out_eio (Mgr mgr) argv =
  Eio.Switch.run @@ fun sw ->
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
  let proc = Eio.Process.spawn ~sw mgr ~stdout:stdout_w ~stderr:stderr_w argv in
  Eio.Flow.close stdout_w ;
  Eio.Flow.close stderr_w ;
  let stdout_out = ref "" in
  Eio.Fiber.both
    (fun () ->
      stdout_out :=
        Eio.Buf_read.(of_flow ~max_size:(10 * 1024 * 1024) stdout_r |> take_all))
    (fun () ->
      (* Drain stderr silently *)
      ignore
        Eio.Buf_read.(of_flow ~max_size:(10 * 1024 * 1024) stderr_r |> take_all)) ;
  match Eio.Process.await proc with
  | `Exited 0 -> Ok (String.trim !stdout_out)
  | _ -> Error (`Msg (Printf.sprintf "Command failed: %s" (cmd_to_string argv)))

let run_out_silent_eio (Mgr mgr) argv =
  Eio.Switch.run @@ fun sw ->
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
  let proc = Eio.Process.spawn ~sw mgr ~stdout:stdout_w ~stderr:stderr_w argv in
  Eio.Flow.close stdout_w ;
  Eio.Flow.close stderr_w ;
  let stdout_out = ref "" in
  let stderr_out = ref "" in
  Eio.Fiber.both
    (fun () ->
      stdout_out :=
        Eio.Buf_read.(of_flow ~max_size:(10 * 1024 * 1024) stdout_r |> take_all))
    (fun () ->
      stderr_out :=
        Eio.Buf_read.(of_flow ~max_size:(10 * 1024 * 1024) stderr_r |> take_all)) ;
  match Eio.Process.await proc with
  | `Exited 0 -> Ok (String.trim !stdout_out)
  | _ ->
      let stdout_lines = String.trim !stdout_out in
      let stderr_lines = String.trim !stderr_out in
      let msg =
        Printf.sprintf
          "Command failed: %s\nStdout:\n%s\nStderr:\n%s"
          (cmd_to_string argv)
          stdout_lines
          stderr_lines
      in
      append_debug_log ("RUN_OUT_SILENT ERROR: " ^ msg) ;
      Error (`Msg msg)

(* Streaming run via Eio that handles \r and \n as line delimiters *)
let run_streaming_eio (Mgr mgr) ~on_log argv =
  Eio.Switch.run @@ fun sw ->
  let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
  let proc = Eio.Process.spawn ~sw mgr ~stdout:stdout_w ~stderr:stderr_w argv in
  Eio.Flow.close stdout_w ;
  Eio.Flow.close stderr_w ;
  let log_lines = ref [] in
  let read_streaming flow =
    let buf = Buffer.create 256 in
    let reader = Eio.Buf_read.of_flow ~max_size:(10 * 1024 * 1024) flow in
    let rec loop () =
      match Eio.Buf_read.any_char reader with
      | c ->
          if c = '\n' || c = '\r' then (
            let line = Buffer.contents buf in
            Buffer.clear buf ;
            if String.length line > 0 then (
              on_log (line ^ "\n") ;
              log_lines := line :: !log_lines) ;
            loop ())
          else (
            Buffer.add_char buf c ;
            loop ())
      | exception End_of_file ->
          let remaining = Buffer.contents buf in
          if remaining <> "" then (
            on_log (remaining ^ "\n") ;
            log_lines := remaining :: !log_lines)
    in
    loop ()
  in
  Eio.Fiber.both
    (fun () -> read_streaming (stdout_r :> _ Eio.Flow.source))
    (fun () -> read_streaming (stderr_r :> _ Eio.Flow.source)) ;
  match Eio.Process.await proc with
  | `Exited 0 -> Ok ()
  | _ ->
      let msg =
        Printf.sprintf
          "Command failed: %s\nOutput:\n%s"
          (cmd_to_string argv)
          (String.concat "\n" (List.rev !log_lines))
      in
      append_debug_log ("RUN_STREAMING ERROR: " ^ msg) ;
      Error (`Msg msg)

let download_file_with_progress_eio (Mgr mgr) ~url ~dest_path ~on_progress =
  let cmd =
    [
      "curl";
      "-fSL";
      "--connect-timeout";
      "30";
      "--speed-limit";
      "102400";
      "--speed-time";
      "60";
      "--progress-meter";
      url;
      "-o";
      dest_path;
    ]
  in
  let parse_size_str s =
    try
      let len = String.length s in
      if len = 0 then None
      else
        let suffix = s.[len - 1] in
        let multiplier, num_str =
          match suffix with
          | 'k' | 'K' -> (1024L, String.sub s 0 (len - 1))
          | 'm' | 'M' -> (Int64.mul 1024L 1024L, String.sub s 0 (len - 1))
          | 'g' | 'G' ->
              (Int64.mul (Int64.mul 1024L 1024L) 1024L, String.sub s 0 (len - 1))
          | '0' .. '9' -> (1L, s)
          | _ -> (1L, s)
        in
        match float_of_string_opt num_str with
        | Some f -> Some (Int64.of_float (f *. Int64.to_float multiplier))
        | None -> None
    with _ -> None
  in
  Eio.Switch.run @@ fun sw ->
  let stderr_r, stderr_w = Eio.Process.pipe ~sw mgr in
  let proc = Eio.Process.spawn ~sw mgr ~stderr:stderr_w cmd in
  Eio.Flow.close stderr_w ;
  let buffer = Buffer.create 128 in
  let reader = Eio.Buf_read.of_flow ~max_size:(10 * 1024 * 1024) stderr_r in
  let rec loop () =
    match Eio.Buf_read.any_char reader with
    | c ->
        if c = '\r' || c = '\n' then (
          let line = Buffer.contents buffer in
          Buffer.clear buffer ;
          (try
             let trimmed = String.trim line in
             if String.length trimmed > 0 then
               let tokens = String.split_on_char ' ' trimmed in
               let non_empty =
                 List.filter (fun s -> String.trim s <> "") tokens
               in
               match non_empty with
               | _ :: total_str :: _ :: received_str :: _ -> (
                   match
                     (parse_size_str total_str, parse_size_str received_str)
                   with
                   | Some total_bytes, Some received_bytes ->
                       let total_int =
                         Int64.to_int total_bytes |> max 0 |> min max_int
                       in
                       let received_int =
                         Int64.to_int received_bytes |> max 0 |> min max_int
                       in
                       on_progress received_int (Some total_int)
                   | _ -> ())
               | _ -> ()
           with _ -> ()) ;
          loop ())
        else (
          Buffer.add_char buffer c ;
          loop ())
    | exception End_of_file -> ()
  in
  loop () ;
  match Eio.Process.await proc with
  | `Exited 0 -> Ok ()
  | _ -> R.error_msgf "curl download failed for %s" url

(* --- Blocking process execution (original Unix-based implementations) --- *)

let run_blocking ~quiet ?on_log argv =
  let cmd_str = cmd_to_string argv in
  if quiet || on_log <> None then (
    let ic, oc, ec = Unix.open_process_full cmd_str (Unix.environment ()) in
    close_out oc ;
    let log_lines = ref [] in
    try
      let rec loop () =
        try
          let line = input_line ic in
          (match on_log with Some f -> f line | None -> ()) ;
          log_lines := line :: !log_lines ;
          loop ()
        with End_of_file -> ()
      in
      loop () ;
      let rec loop_err () =
        try
          let line = input_line ec in
          (match on_log with Some f -> f line | None -> ()) ;
          log_lines := line :: !log_lines ;
          loop_err ()
        with End_of_file -> ()
      in
      loop_err () ;
      match Unix.close_process_full (ic, oc, ec) with
      | Unix.WEXITED 0 -> Ok ()
      | _status ->
          let msg =
            Printf.sprintf
              "Command failed: %s\nOutput:\n%s"
              cmd_str
              (String.concat "\n" (List.rev !log_lines))
          in
          append_debug_log ("RUN ERROR: " ^ msg) ;
          Error (`Msg msg)
    with e ->
      ignore (Unix.close_process_full (ic, oc, ec)) ;
      Error (`Msg (Printexc.to_string e)))
  else
    let cmd = Bos.Cmd.of_list argv in
    match Bos.OS.Cmd.run cmd with
    | Ok () -> Ok ()
    | Error (`Msg m) -> Error (`Msg m)

let run ?(quiet = false) ?on_log argv =
  append_debug_log ("RUN " ^ (if quiet then "[Q] " else "") ^ cmd_to_string argv) ;
  match (Sys.getenv_opt "OCTEZ_MANAGER_TEST_MODE", argv) with
  | Some ("1" | "true"), "systemctl" :: _rest ->
      append_debug_log "TEST_MODE: systemctl command intercepted" ;
      Ok ()
  | _ -> (
      match Atomic.get proc_mgr_ref with
      | Some mgr -> run_eio mgr ~quiet ?on_log argv
      | None -> run_blocking ~quiet ?on_log argv)

let run_silent = run ~quiet:true

(* Streaming run that reads stdout and stderr concurrently using Unix.select.
   This ensures output is captured as it's produced, not blocked waiting for
   one stream to complete before reading the other. Handles both \n and \r
   as line delimiters to capture progress updates that use carriage returns. *)
let run_streaming_blocking ~on_log argv =
  let cmd_str = cmd_to_string argv in
  let ic, oc, ec = Unix.open_process_full cmd_str (Unix.environment ()) in
  close_out oc ;
  let ic_fd = Unix.descr_of_in_channel ic in
  let ec_fd = Unix.descr_of_in_channel ec in
  let log_lines = ref [] in
  let ic_buf = Buffer.create 256 in
  let ec_buf = Buffer.create 256 in
  let ic_open = ref true in
  let ec_open = ref true in
  let find_line_end s pos =
    let len = String.length s in
    let rec loop i =
      if i >= len then None
      else if s.[i] = '\n' || s.[i] = '\r' then Some i
      else loop (i + 1)
    in
    loop pos
  in
  let read_available fd buf =
    let tmp = Bytes.create 1024 in
    let n = Unix.read fd tmp 0 1024 in
    if n = 0 then `Eof
    else (
      Buffer.add_subbytes buf tmp 0 n ;
      let content = Buffer.contents buf in
      let rec extract_lines pos =
        match find_line_end content pos with
        | Some end_pos ->
            let line = String.sub content pos (end_pos - pos) in
            if String.length line > 0 then (
              on_log (line ^ "\n") ;
              log_lines := line :: !log_lines) ;
            extract_lines (end_pos + 1)
        | None ->
            Buffer.clear buf ;
            if pos < String.length content then
              Buffer.add_substring buf content pos (String.length content - pos)
      in
      extract_lines 0 ;
      `Ok)
  in
  while !ic_open || !ec_open do
    let read_fds =
      (if !ic_open then [ic_fd] else []) @ if !ec_open then [ec_fd] else []
    in
    if read_fds <> [] then
      let ready, _, _ = Unix.select read_fds [] [] 0.1 in
      List.iter
        (fun fd ->
          let buf = if fd = ic_fd then ic_buf else ec_buf in
          let is_open = if fd = ic_fd then ic_open else ec_open in
          match read_available fd buf with
          | `Eof -> is_open := false
          | `Ok -> ())
        ready
  done ;
  let flush_buf buf =
    let remaining = Buffer.contents buf in
    if remaining <> "" then (
      on_log (remaining ^ "\n") ;
      log_lines := remaining :: !log_lines)
  in
  flush_buf ic_buf ;
  flush_buf ec_buf ;
  match Unix.close_process_full (ic, oc, ec) with
  | Unix.WEXITED 0 -> Ok ()
  | _status ->
      let msg =
        Printf.sprintf
          "Command failed: %s\nOutput:\n%s"
          cmd_str
          (String.concat "\n" (List.rev !log_lines))
      in
      append_debug_log ("RUN_STREAMING ERROR: " ^ msg) ;
      Error (`Msg msg)

let run_streaming ~on_log argv =
  append_debug_log ("RUN_STREAMING " ^ cmd_to_string argv) ;
  match Atomic.get proc_mgr_ref with
  | Some mgr -> run_streaming_eio mgr ~on_log argv
  | None -> run_streaming_blocking ~on_log argv

let run_verbose = run ~quiet:false

let run_out argv =
  append_debug_log ("RUN_OUT " ^ cmd_to_string argv) ;
  match Atomic.get proc_mgr_ref with
  | Some mgr -> run_out_eio mgr argv
  | None -> (
      let cmd = Bos.Cmd.of_list argv in
      match Bos.OS.Cmd.(run_out cmd |> out_string ~trim:true) with
      | Ok (out, _) -> Ok out
      | Error (`Msg m) -> Error (`Msg m))

let run_out_silent_blocking argv =
  let cmd_str = cmd_to_string argv in
  let ic, oc, ec = Unix.open_process_full cmd_str (Unix.environment ()) in
  close_out oc ;
  let stdout_lines = ref [] in
  let stderr_lines = ref [] in
  (try
     while true do
       stdout_lines := input_line ic :: !stdout_lines
     done
   with End_of_file -> ()) ;
  (try
     while true do
       stderr_lines := input_line ec :: !stderr_lines
     done
   with End_of_file -> ()) ;
  match Unix.close_process_full (ic, oc, ec) with
  | Unix.WEXITED 0 -> Ok (String.concat "\n" (List.rev !stdout_lines))
  | _status ->
      let msg =
        Printf.sprintf
          "Command failed: %s\nStdout:\n%s\nStderr:\n%s"
          cmd_str
          (String.concat "\n" (List.rev !stdout_lines))
          (String.concat "\n" (List.rev !stderr_lines))
      in
      append_debug_log ("RUN_OUT_SILENT ERROR: " ^ msg) ;
      Error (`Msg msg)

let run_out_silent argv =
  append_debug_log ("RUN_OUT_SILENT " ^ cmd_to_string argv) ;
  match Atomic.get proc_mgr_ref with
  | Some mgr -> run_out_silent_eio mgr argv
  | None -> run_out_silent_blocking argv

let run_as ?(quiet = false) ?on_log ~user argv =
  let trimmed = String.trim user in
  let current_user, _ = current_user_group_names () in
  if trimmed = "" || (not (is_root ())) || String.equal trimmed current_user
  then run ~quiet ?on_log argv
  else
    let command = cmd_to_string argv in
    run ~quiet ?on_log ["su"; "-s"; "/bin/sh"; "-c"; command; trimmed]

let ensure_tree_owner ~owner ~group path =
  if not (is_root ()) then Ok ()
  else if not (Sys.file_exists path) then Ok ()
  else
    match run ["chown"; "-R"; Printf.sprintf "%s:%s" owner group; path] with
    | Ok () -> Ok ()
    | Error (`Msg e) ->
        R.error_msgf "Failed to set ownership recursively on %s: %s" path e

let download_file ?(quiet = false) ~url ~dest_path () =
  append_debug_log (Printf.sprintf "DOWNLOAD %s -> %s" url dest_path) ;
  (* Connection timeout 30s, speed limit 100KB/s for at least 60s before abort *)
  run
    ~quiet
    [
      "curl";
      "-fSL";
      "--connect-timeout";
      "30";
      "--speed-limit";
      "102400";
      "--speed-time";
      "60";
      url;
      "-o";
      dest_path;
    ]

(* Track active download process for cleanup on exit.
   Note: Only one download runs at a time (snapshot download is sequential),
   so a single reference is sufficient. If concurrent downloads are needed
   in the future, this would need to be a list or set. *)
let active_download :
    (in_channel * out_channel * in_channel * string) option ref =
  ref None

let active_download_lock = Mutex.create ()

(** Kill any active download process. Call this on app exit. *)
let kill_active_download () =
  Mutex.protect active_download_lock (fun () ->
      match !active_download with
      | None -> ()
      | Some (ic, oc, ec, dest_path) -> (
          active_download := None ;
          (* Close channels to kill the curl process *)
          (try close_in_noerr ic with _ -> ()) ;
          (try close_out_noerr oc with _ -> ()) ;
          (try close_in_noerr ec with _ -> ()) ;
          (* Clean up partial download *)
          try if Sys.file_exists dest_path then Sys.remove dest_path
          with _ -> ()))

(* Streaming download progress using curl progress meter. We parse byte counts from
   stderr lines; when parsing fails we still complete without progress ticks.
   
   Curl --progress-meter format:
     % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                    Dload  Upload   Total   Spent    Left  Speed
    20 100M   20 20.0M    0     0  1024k      0  0:01:40  0:00:20  0:01:20 1024k
   
   We extract:
   - Column 2: Total size in bytes (may have K/M/G suffix)
   - Column 4: Bytes received (may have K/M/G suffix)
*)
let download_file_with_progress_blocking ~url ~dest_path ~on_progress =
  let cmd =
    [
      "curl";
      "-fSL";
      "--connect-timeout";
      "30";
      "--speed-limit";
      "102400";
      "--speed-time";
      "60";
      "--progress-meter";
      url;
      "-o";
      dest_path;
    ]
  in
  let parse_size_str s =
    try
      let len = String.length s in
      if len = 0 then None
      else
        let suffix = s.[len - 1] in
        let multiplier, num_str =
          match suffix with
          | 'k' | 'K' -> (1024L, String.sub s 0 (len - 1))
          | 'm' | 'M' -> (Int64.mul 1024L 1024L, String.sub s 0 (len - 1))
          | 'g' | 'G' ->
              (Int64.mul (Int64.mul 1024L 1024L) 1024L, String.sub s 0 (len - 1))
          | '0' .. '9' -> (1L, s)
          | _ -> (1L, s)
        in
        match float_of_string_opt num_str with
        | Some f -> Some (Int64.of_float (f *. Int64.to_float multiplier))
        | None -> None
    with _ -> None
  in
  let ic, oc, ec =
    Unix.open_process_full (cmd_to_string cmd) (Unix.environment ())
  in
  Mutex.protect active_download_lock (fun () ->
      active_download := Some (ic, oc, ec, dest_path)) ;
  close_out oc ;
  let buffer = Buffer.create 128 in
  let input_char_opt ch = try Some (input_char ch) with End_of_file -> None in
  let rec loop () =
    match input_char_opt ec with
    | None -> ()
    | Some c ->
        if c = '\r' || c = '\n' then (
          let line = Buffer.contents buffer in
          Buffer.clear buffer ;
          (try
             let trimmed = String.trim line in
             if String.length trimmed > 0 then
               let tokens = String.split_on_char ' ' trimmed in
               let non_empty =
                 List.filter (fun s -> String.trim s <> "") tokens
               in
               match non_empty with
               | _ :: total_str :: _ :: received_str :: _ -> (
                   match
                     (parse_size_str total_str, parse_size_str received_str)
                   with
                   | Some total_bytes, Some received_bytes ->
                       let total_int =
                         Int64.to_int total_bytes |> max 0 |> min max_int
                       in
                       let received_int =
                         Int64.to_int received_bytes |> max 0 |> min max_int
                       in
                       on_progress received_int (Some total_int)
                   | _ -> ())
               | _ -> ()
           with _ -> ()) ;
          loop ())
        else (
          Buffer.add_char buffer c ;
          loop ())
  in
  loop () ;
  Mutex.protect active_download_lock (fun () -> active_download := None) ;
  close_in_noerr ic ;
  close_in_noerr ec ;
  match Unix.close_process_full (ic, oc, ec) with
  | Unix.WEXITED 0 -> Ok ()
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      R.error_msgf "curl download failed for %s" url

let download_file_with_progress ~url ~dest_path ~on_progress =
  append_debug_log (Printf.sprintf "DOWNLOAD_PROGRESS %s -> %s" url dest_path) ;
  match Atomic.get proc_mgr_ref with
  | Some mgr -> download_file_with_progress_eio mgr ~url ~dest_path ~on_progress
  | None -> download_file_with_progress_blocking ~url ~dest_path ~on_progress

let remove_path path =
  if Sys.file_exists path then try Sys.remove path with Sys_error _ -> ()

let rec remove_tree path =
  if not (Sys.file_exists path) then Ok ()
  else
    let remove_dir dir =
      let entries = Sys.readdir dir in
      let* () =
        Array.fold_left
          (fun acc entry ->
            match acc with
            | Error _ as e -> e
            | Ok () ->
                if entry = "." || entry = ".." then Ok ()
                else remove_tree (Filename.concat dir entry))
          (Ok ())
          entries
      in
      try
        Unix.rmdir dir ;
        Ok ()
      with Unix.Unix_error (err, fn, _arg) ->
        R.error_msgf
          "Failed to remove directory %s: %s (%s)"
          dir
          (Unix.error_message err)
          fn
    in
    try
      match (Unix.lstat path).Unix.st_kind with
      | Unix.S_DIR -> remove_dir path
      | _ -> (
          try
            Unix.unlink path ;
            Ok ()
          with Unix.Unix_error (err, fn, _arg) ->
            R.error_msgf
              "Failed to remove %s: %s (%s)"
              path
              (Unix.error_message err)
              fn)
    with Unix.Unix_error (err, fn, _arg) ->
      R.error_msgf
        "Failed to inspect %s: %s (%s)"
        path
        (Unix.error_message err)
        fn

let copy_file src dst =
  let buffer = Bytes.create 65_536 in
  let result =
    try
      let src_stats = Unix.stat src in
      let ic = open_in_bin src in
      let oc = open_out_bin dst in
      Fun.protect
        ~finally:(fun () ->
          close_in_noerr ic ;
          close_out_noerr oc)
        (fun () ->
          let rec loop () =
            let read = input ic buffer 0 (Bytes.length buffer) in
            if read = 0 then Ok ()
            else (
              output oc buffer 0 read ;
              loop ())
          in
          let* () = loop () in
          (try Unix.chmod dst src_stats.Unix.st_perm with _ -> ()) ;
          (if is_root () then
             try Unix.chown dst src_stats.Unix.st_uid src_stats.Unix.st_gid
             with _ -> ()) ;
          Ok ())
    with
    | Sys_error msg -> Error (`Msg msg)
    | Unix.Unix_error (err, fn, arg) ->
        Error
          (`Msg
             (Printf.sprintf
                "Failed to copy %s to %s: %s (%s %s)"
                src
                dst
                (Unix.error_message err)
                fn
                arg))
  in
  match result with
  | Ok () -> Ok ()
  | Error _ as e ->
      remove_path dst ;
      e

let port_in_use_override : (int -> bool) option ref = ref None

let set_port_in_use_override f = port_in_use_override := Some f

let clear_port_in_use_override () = port_in_use_override := None

let is_port_in_use (port : int) : bool =
  match !port_in_use_override with
  | Some f -> f port
  | None -> (
      (* Check using ss (if available) or lsof *)
      let has_ss =
        match Bos.OS.Cmd.exists (Bos.Cmd.v "ss") with
        | Ok exists -> exists
        | Error _ -> false
      in
      if has_ss then
        match run_out ["ss"; "-ltnH"; Printf.sprintf "sport = :%d" port] with
        | Ok out -> String.trim out <> ""
        | Error _ -> false
      else
        match
          run_out ["lsof"; "-nP"; "-iTCP:" ^ string_of_int port; "-sTCP:LISTEN"]
        with
        | Ok out -> String.trim out <> ""
        | Error _ -> false)

let get_remote_file_size url =
  (* Use curl -I (HEAD request) to get Content-Length *)
  match run_out ["curl"; "-sfI"; "--connect-timeout"; "10"; url] with
  | Error _ -> None
  | Ok output ->
      let lines = String.split_on_char '\n' output in
      let rec find_content_length = function
        | [] -> None
        | line :: rest ->
            let lower = String.lowercase_ascii line in
            if
              String.length lower > 16
              && String.sub lower 0 16 = "content-length: "
            then
              let value =
                String.trim (String.sub line 16 (String.length line - 16))
              in
              match Int64.of_string_opt value with
              | Some size -> Some size
              | None -> find_content_length rest
            else find_content_length rest
      in
      find_content_length lines

let get_available_space dir =
  (* Check if path exists before calling df to avoid stderr noise *)
  if not (Sys.file_exists dir) then None
  else
    (* Use df to get available space in bytes *)
    match run_out ["df"; "-B1"; "--output=avail"; dir] with
    | Error _ -> None
    | Ok output -> (
        let lines = String.split_on_char '\n' output in
        (* Skip header line, get second line *)
        match lines with
        | _ :: value_line :: _ -> Int64.of_string_opt (String.trim value_line)
        | _ -> None)

let get_filesystem_id path =
  (* Use stat to get the filesystem (device) ID for a path *)
  try
    let stats = Unix.stat path in
    Some stats.Unix.st_dev
  with Unix.Unix_error _ -> None

let same_filesystem path1 path2 =
  (* Check if two paths are on the same filesystem *)
  match (get_filesystem_id path1, get_filesystem_id path2) with
  | Some id1, Some id2 -> Some (id1 = id2)
  | _ -> None

(** Map Octez exit codes to human-readable descriptions.
    See https://octez.tezos.com/docs/user/exits.html *)
let octez_exit_code_description code =
  match code with
  | 0 -> "success"
  | 126 -> "unhandled exception (bug)"
  | 127 -> "terminated by signal"
  | 128 -> "error during shutdown"
  | 254 -> "unhandled exception with shutdown error"
  | 255 -> "forcefully terminated"
  | n when n >= 1 && n <= 125 -> "configuration or startup error"
  | n when n >= 129 && n <= 253 -> "error with shutdown failure"
  | _ -> Printf.sprintf "exit code %d" code

(** {1 Editor Integration} *)

(** Get the user's preferred editor from environment variables.
    Tries $VISUAL, $EDITOR, then falls back to sensible-editor or vi. *)
let get_editor () =
  match Sys.getenv_opt "VISUAL" with
  | Some e when e <> "" -> e
  | Some _ | None -> (
      match Sys.getenv_opt "EDITOR" with
      | Some e when e <> "" -> e
      | Some _ | None -> (
          match which "sensible-editor" with
          | Some path -> path
          | None -> (
              match which "vi" with Some path -> path | None -> "/usr/bin/vi")))

(** Open a file in the user's preferred editor.
    Blocks until the editor exits.
    
    @param file_path Path to the file to edit
    @return Ok () if editor exited successfully, Error otherwise *)
let open_in_editor file_path =
  let editor = get_editor () in
  (* Use Unix.create_process directly for interactive editor *)
  let pid =
    Unix.create_process
      editor
      [|editor; file_path|]
      Unix.stdin
      Unix.stdout
      Unix.stderr
  in
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> Ok ()
  | _, Unix.WEXITED code ->
      Error (`Msg (Printf.sprintf "Editor exited with code %d" code))
  | _, Unix.WSIGNALED signal ->
      Error (`Msg (Printf.sprintf "Editor terminated by signal %d" signal))
  | _, Unix.WSTOPPED signal ->
      Error (`Msg (Printf.sprintf "Editor stopped by signal %d" signal))
