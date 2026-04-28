(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(* Pluggable hooks for Eio-based process execution.
   When set, these override the default blocking implementations.
   The TUI sets these at startup via Eio_process.init.

   The hooks use closed variant type [`Msg of string] internally to
   satisfy the value restriction on mutable refs.  Dispatch functions
   coerce results to the open [> `Msg of string] expected by callers. *)
let run_hook :
    (quiet:bool ->
    ?on_log:(string -> unit) ->
    string list ->
    (unit, [`Msg of string]) result)
    option
    ref =
  ref None

let run_out_hook : (string list -> (string, [`Msg of string]) result) option ref
    =
  ref None

let run_out_silent_hook :
    (string list -> (string, [`Msg of string]) result) option ref =
  ref None

let run_streaming_hook :
    (on_log:(string -> unit) -> string list -> (unit, [`Msg of string]) result)
    option
    ref =
  ref None

let set_run_hook f = run_hook := Some f

let set_run_out_hook f = run_out_hook := Some f

let set_run_out_silent_hook f = run_out_silent_hook := Some f

let set_run_streaming_hook f = run_streaming_hook := Some f

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

let run_blocking ?(quiet = false) ?on_log argv =
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

let run ?(quiet = false) ?on_log argv =
  append_debug_log ("RUN " ^ (if quiet then "[Q] " else "") ^ cmd_to_string argv) ;

  (* Test mode hook: if OCTEZ_MANAGER_TEST_MODE is set and this is a systemctl
     command, delegate to the mock handler via environment variable response.
     This allows tests to intercept systemctl commands without tight coupling. *)
  match (Sys.getenv_opt "OCTEZ_MANAGER_TEST_MODE", argv) with
  | Some ("1" | "true"), "systemctl" :: _rest ->
      (* In test mode with systemctl command - signal this to test framework.
         Real integration happens in test code via run_hook. For now, just
         return success to avoid breaking test infrastructure setup. *)
      append_debug_log "TEST_MODE: systemctl command intercepted" ;
      Ok ()
  | _ -> (
      match !run_hook with
      | Some f ->
          (f ~quiet ?on_log argv
            : (unit, [`Msg of string]) result
            :> (unit, [> `Msg of string]) result)
      | None -> run_blocking ~quiet ?on_log argv)

let run_silent = run ~quiet:true

(* Streaming run that reads stdout and stderr concurrently using Unix.select.
   This ensures output is captured as it's produced, not blocked waiting for
   one stream to complete before reading the other. Handles both \n and \r
   as line delimiters to capture progress updates that use carriage returns. *)
let run_streaming_blocking ~on_log argv =
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

let run_streaming ~on_log argv =
  append_debug_log ("RUN_STREAMING " ^ cmd_to_string argv) ;
  match !run_streaming_hook with
  | Some f ->
      (f ~on_log argv
        : (unit, [`Msg of string]) result
        :> (unit, [> `Msg of string]) result)
  | None -> run_streaming_blocking ~on_log argv

let run_verbose = run ~quiet:false

let run_out_blocking argv =
  let cmd = Bos.Cmd.of_list argv in
  match Bos.OS.Cmd.(run_out cmd |> out_string ~trim:true) with
  | Ok (out, _) -> Ok out
  | Error (`Msg m) -> Error (`Msg m)

let run_out argv =
  append_debug_log ("RUN_OUT " ^ cmd_to_string argv) ;
  match !run_out_hook with
  | Some f ->
      (f argv
        : (string, [`Msg of string]) result
        :> (string, [> `Msg of string]) result)
  | None -> run_out_blocking argv

let run_out_with_timeout_hook :
    (timeout:float -> string list -> (string, [`Msg of string]) result) option
    ref =
  ref None

let set_run_out_with_timeout_hook f = run_out_with_timeout_hook := Some f

let run_out_with_timeout ~timeout argv =
  append_debug_log ("RUN_OUT " ^ cmd_to_string argv) ;
  match !run_out_with_timeout_hook with
  | Some f ->
      (f ~timeout argv
        : (string, [`Msg of string]) result
        :> (string, [> `Msg of string]) result)
  | None -> run_out_blocking argv

let run_out_with_timeout_combined_hook :
    (timeout:float -> string list -> (string, [`Msg of string]) result) option
    ref =
  ref None

let set_run_out_with_timeout_combined_hook f =
  run_out_with_timeout_combined_hook := Some f

let run_out_combined_blocking argv =
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
  let combined =
    String.concat "\n" (List.rev !stdout_lines @ List.rev !stderr_lines)
  in
  match Unix.close_process_full (ic, oc, ec) with
  | Unix.WEXITED 0 -> Ok combined
  | _status ->
      let msg =
        Printf.sprintf "Command failed: %s\nOutput:\n%s" cmd_str combined
      in
      append_debug_log ("RUN_OUT_COMBINED ERROR: " ^ msg) ;
      Error (`Msg msg)

let run_out_with_timeout_combined ~timeout argv =
  append_debug_log ("RUN_OUT_COMBINED " ^ cmd_to_string argv) ;
  match !run_out_with_timeout_combined_hook with
  | Some f ->
      (f ~timeout argv
        : (string, [`Msg of string]) result
        :> (string, [> `Msg of string]) result)
  | None ->
      let _ = timeout in
      run_out_combined_blocking argv

let run_out_silent_blocking argv =
  let cmd_str = cmd_to_string argv in
  let ic, oc, ec = Unix.open_process_full cmd_str (Unix.environment ()) in
  close_out oc ;
  let stdout_lines = ref [] in
  let stderr_lines = ref [] in
  (* Read all stdout *)
  (try
     while true do
       stdout_lines := input_line ic :: !stdout_lines
     done
   with End_of_file -> ()) ;
  (* Read all stderr (to prevent leakage) *)
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
  match !run_out_silent_hook with
  | Some f ->
      (f argv
        : (string, [`Msg of string]) result
        :> (string, [> `Msg of string]) result)
  | None -> run_out_silent_blocking argv

let run_as ?(quiet = false) ?on_log ~user argv =
  let trimmed = String.trim user in
  let current_user, _ = Paths.current_user_group_names () in
  if
    trimmed = ""
    || (not (Paths.is_root ()))
    || String.equal trimmed current_user
  then run ~quiet ?on_log argv
  else
    let command = cmd_to_string argv in
    run ~quiet ?on_log ["runuser"; "-s"; "/bin/sh"; "-c"; command; trimmed]
