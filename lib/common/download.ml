(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

(* Pluggable hook for Eio-based download with progress.
   Set by the TUI at startup via Eio_process.init.
   Uses closed variant type internally to satisfy the value restriction
   on mutable refs.  Dispatch coerces to open variant for callers. *)
let download_with_progress_hook :
    (url:string ->
    dest_path:string ->
    on_progress:(int -> int option -> unit) ->
    (unit, [`Msg of string]) result)
    option
    ref =
  ref None

let set_download_with_progress_hook f = download_with_progress_hook := Some f

let download_file ?(quiet = false) ~url ~dest_path () =
  Cmd_runner.append_debug_log (Printf.sprintf "DOWNLOAD %s -> %s" url dest_path) ;
  (* Connection timeout 30s, speed limit 100KB/s for at least 60s before abort *)
  Cmd_runner.run
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
  Cmd_runner.append_debug_log
    (Printf.sprintf "DOWNLOAD_PROGRESS %s -> %s" url dest_path) ;
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
  (* Parse curl size format: "123", "1.5k", "20.0M", etc. *)
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
        (* Handle decimal numbers *)
        match float_of_string_opt num_str with
        | Some f -> Some (Int64.of_float (f *. Int64.to_float multiplier))
        | None -> None
    with _ -> None
  in
  let ic, oc, ec =
    Unix.open_process_full (Cmd_runner.cmd_to_string cmd) (Unix.environment ())
  in
  Mutex.protect active_download_lock (fun () ->
      active_download := Some (ic, oc, ec, dest_path)) ;
  close_out oc ;
  let buffer = Buffer.create 128 in
  let input_char_opt ch = try Some (input_char ch) with End_of_file -> None in
  let parse_failure_count = ref 0 in
  let max_logged_failures = 3 in
  let rec loop () =
    match input_char_opt ec with
    | None -> ()
    | Some c ->
        if c = '\r' || c = '\n' then (
          let line = Buffer.contents buffer in
          Buffer.clear buffer ;
          (* Parse curl progress lines:
             Format: "pct_total total pct_recv bytes_recv pct_xfer bytes_xfer ..."
             Example: "20 100M 20 20.0M 0 0 ..."
             We extract tokens[1] (total) and tokens[3] (bytes_recv) *)
          (try
             let trimmed = String.trim line in
             if String.length trimmed > 0 then
               let tokens = String.split_on_char ' ' trimmed in
               let non_empty =
                 List.filter (fun s -> String.trim s <> "") tokens
               in
               match non_empty with
               | _ :: total_str :: _ :: received_str :: _ -> (
                   (* Try to parse as byte counts *)
                   match
                     (parse_size_str total_str, parse_size_str received_str)
                   with
                   | Some total_bytes, Some received_bytes ->
                       (* Convert int64 to int (should be safe for reasonable file sizes) *)
                       let total_int =
                         Int64.to_int total_bytes |> max 0 |> min max_int
                       in
                       let received_int =
                         Int64.to_int received_bytes |> max 0 |> min max_int
                       in
                       on_progress received_int (Some total_int)
                   | _ ->
                       if !parse_failure_count < max_logged_failures then (
                         incr parse_failure_count ;
                         Cmd_runner.append_debug_log
                           (Printf.sprintf
                              "DOWNLOAD_PROGRESS parse failure (size): %s"
                              trimmed)))
               | _ ->
                   if !parse_failure_count < max_logged_failures then (
                     incr parse_failure_count ;
                     Cmd_runner.append_debug_log
                       (Printf.sprintf
                          "DOWNLOAD_PROGRESS parse failure (format): %s"
                          trimmed))
           with _ ->
             if !parse_failure_count < max_logged_failures then (
               incr parse_failure_count ;
               Cmd_runner.append_debug_log
                 (Printf.sprintf
                    "DOWNLOAD_PROGRESS parse exception: %s"
                    (String.trim line)))) ;
          loop ())
        else (
          Buffer.add_char buffer c ;
          loop ())
  in
  loop () ;
  Mutex.protect active_download_lock (fun () -> active_download := None) ;
  (* NOTE: We use Printf.sprintf + Error (`Msg ...) instead of R.error_msgf
     because this function may be called from parallel domains, and
     Format.kasprintf (used by R.error_msgf) is not domain-safe in OCaml 5. *)
  match Unix.close_process_full (ic, oc, ec) with
  | Unix.WEXITED 0 -> Ok ()
  | Unix.WEXITED 22 ->
      Error
        (`Msg
           (Printf.sprintf
              "curl download failed for %s (HTTP error - file not found)"
              url))
  | Unix.WEXITED code ->
      Error
        (`Msg
           (Printf.sprintf
              "curl download failed for %s (exit code %d)"
              url
              code))
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      Error (`Msg (Printf.sprintf "curl download failed for %s" url))

let download_file_with_progress ~url ~dest_path ~on_progress =
  Cmd_runner.append_debug_log
    (Printf.sprintf "DOWNLOAD_PROGRESS %s -> %s" url dest_path) ;
  match !download_with_progress_hook with
  | Some f ->
      (f ~url ~dest_path ~on_progress
        : (unit, [`Msg of string]) result
        :> (unit, [> `Msg of string]) result)
  | None -> download_file_with_progress_blocking ~url ~dest_path ~on_progress

let get_remote_file_size url =
  (* Use curl -I (HEAD request) to get Content-Length *)
  match Cmd_runner.run_out ["curl"; "-sfI"; "--connect-timeout"; "10"; url] with
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

let compute_sha256 filepath =
  match Cmd_runner.run_out ["sha256sum"; filepath] with
  | Ok output -> (
      match String.split_on_char ' ' output with
      | hash :: _ -> Ok (String.trim hash)
      | _ -> R.error_msg "Unexpected sha256sum output")
  | Error _ as e -> e
