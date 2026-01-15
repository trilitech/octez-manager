(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

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
        | ' ' | '\t' | '\n' | '"' | '\'' | '$' | '`' | '\\' -> true
        | _ -> loop (i + 1)
    in
    loop 0
  in
  if not needs then s
  else
    let parts = String.split_on_char '\'' s in
    "'" ^ String.concat "'\"'\"'" parts ^ "'"

let cmd_to_string argv = String.concat " " (List.map sh_quote argv)

let run ?(quiet = false) ?on_log argv =
  append_debug_log ("RUN " ^ (if quiet then "[Q] " else "") ^ cmd_to_string argv) ;
  let cmd_str = cmd_to_string argv in
  if quiet || on_log <> None then (
    (* Capture output to avoid polluting TUI, or to feed on_log *)
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
      (* Also read stderr *)
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
    (* Stream command output to stdout/stderr (CLI-friendly) *)
    let cmd = Bos.Cmd.of_list argv in
    match Bos.OS.Cmd.run cmd with
    | Ok () -> Ok ()
    | Error (`Msg m) -> Error (`Msg m)

let run_silent = run ~quiet:true

(* Streaming run that reads stdout and stderr concurrently using Unix.select.
   This ensures output is captured as it's produced, not blocked waiting for
   one stream to complete before reading the other. Handles both \n and \r
   as line delimiters to capture progress updates that use carriage returns. *)
let run_streaming ~on_log argv =
  append_debug_log ("RUN_STREAMING " ^ cmd_to_string argv) ;
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
  (* Find first occurrence of \n or \r *)
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
      (* Extract complete lines (delimited by \n or \r) *)
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
            (* Keep remaining partial line in buffer *)
            Buffer.clear buf ;
            if pos < String.length content then
              Buffer.add_substring buf content pos (String.length content - pos)
      in
      extract_lines 0 ;
      `Ok)
  in
  (* Main loop using select *)
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
  (* Flush any remaining partial lines *)
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

let run_verbose = run ~quiet:false

let run_out argv =
  append_debug_log ("RUN_OUT " ^ cmd_to_string argv) ;
  let cmd = Bos.Cmd.of_list argv in
  match Bos.OS.Cmd.(run_out cmd |> out_string ~trim:true) with
  | Ok (out, _) -> Ok out
  | Error (`Msg m) -> Error (`Msg m)

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

(* Streaming download progress using curl progress meter. We parse percent from
   stderr lines; when parsing fails we still complete without progress ticks. *)
let download_file_with_progress ~url ~dest_path ~on_progress =
  append_debug_log (Printf.sprintf "DOWNLOAD_PROGRESS %s -> %s" url dest_path) ;
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
          (* curl progress lines often start with percent. *)
          (try
             let trimmed = String.trim line in
             if String.length trimmed > 0 then
               let tokens = String.split_on_char ' ' trimmed in
               match tokens |> List.filter (fun s -> String.trim s <> "") with
               | pct_str :: _ ->
                   let pct =
                     int_of_string_opt pct_str |> Option.value ~default:0
                   in
                   let pct = max 0 (min 100 pct) in
                   on_progress pct (Some 100)
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

let is_port_in_use (port : int) : bool =
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
    | Error _ -> false

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
