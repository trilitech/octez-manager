(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

(* Existential wrapper for the Eio process manager *)
type any_proc_mgr = Mgr : _ Eio.Process.mgr -> any_proc_mgr

(* Stored process manager, set by init *)
let process_mgr_ref : any_proc_mgr option Atomic.t = Atomic.make None

let get_process_mgr () = Atomic.get process_mgr_ref

(** Read lines from an Eio flow, calling [on_line] for each line read. *)
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

(** Run a command via Eio, capturing stdout and stderr.

    In TUI mode stdout/stderr are always captured via pipes -- there is no
    terminal to inherit -- so the [quiet] flag has no effect on the Eio path. *)
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
          (Cmd_runner.cmd_to_string argv)
          (String.concat "\n" (List.rev !log_lines))
      in
      Cmd_runner.append_debug_log ("RUN ERROR: " ^ msg) ;
      Error (`Msg msg)

(** Run a command via Eio and return its stdout as a trimmed string.

    Drains stderr in parallel with stdout to prevent the process from blocking
    if the stderr pipe buffer fills up. *)
let run_out_eio (Mgr mgr) argv =
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
      let combined =
        String.trim !stdout_out ^ "\n" ^ String.trim !stderr_out |> String.trim
      in
      let msg =
        Printf.sprintf
          "Command failed: %s\nOutput:\n%s"
          (Cmd_runner.cmd_to_string argv)
          combined
      in
      Cmd_runner.append_debug_log ("RUN_OUT ERROR: " ^ msg) ;
      Error (`Msg msg)

(** Run a command via Eio and return stdout, including stderr in error messages. *)
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
          (Cmd_runner.cmd_to_string argv)
          stdout_lines
          stderr_lines
      in
      Cmd_runner.append_debug_log ("RUN_OUT_SILENT ERROR: " ^ msg) ;
      Error (`Msg msg)

(** Run a command via Eio with streaming output, handling [\\r] and [\\n] as
    line delimiters for progress-style output. *)
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
          (Cmd_runner.cmd_to_string argv)
          (String.concat "\n" (List.rev !log_lines))
      in
      Cmd_runner.append_debug_log ("RUN_STREAMING ERROR: " ^ msg) ;
      Error (`Msg msg)

(** Download a file via curl using Eio, parsing progress from stderr. *)
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

(* --- Initialization --- *)

let init proc_mgr =
  let mgr = Mgr proc_mgr in
  Atomic.set process_mgr_ref (Some mgr) ;
  Cmd_runner.set_run_hook (run_eio mgr) ;
  Cmd_runner.set_run_out_hook (run_out_eio mgr) ;
  Cmd_runner.set_run_out_silent_hook (run_out_silent_eio mgr) ;
  Cmd_runner.set_run_streaming_hook (run_streaming_eio mgr) ;
  Download.set_download_with_progress_hook (download_file_with_progress_eio mgr) ;
  Binary_downloader.set_yield_hook (fun () -> Eio_unix.sleep 0.01)
