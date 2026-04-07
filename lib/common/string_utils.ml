(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let interruptible_sleep stop_flag seconds =
  let deadline = Unix.gettimeofday () +. seconds in
  while (not (Atomic.get stop_flag)) && Unix.gettimeofday () < deadline do
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining > 0.0 then
      ignore (Unix.select [] [] [] (Float.min 0.5 remaining))
  done

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
          match Paths.which "sensible-editor" with
          | Some path -> path
          | None -> (
              match Paths.which "vi" with
              | Some path -> path
              | None -> "/usr/bin/vi")))

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

(** {1 String Utilities} *)

(** Check whether [needle] is a substring of [haystack]. *)
let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec loop idx =
    if idx + nlen > hlen then false
    else if String.sub haystack idx nlen = needle then true
    else loop (idx + 1)
  in
  if nlen = 0 then true else loop 0

(** {1 Timestamp Utilities} *)

(** Format the current local time as ["YYYY-MM-DD HH:MM:SS"]. *)
let now () =
  let tm = Unix.time () |> Unix.localtime in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

(** Strip non-printable and non-ASCII bytes, keeping only printable ASCII
    (0x20-0x7E). Browsers inject U+00A0, U+200B, U+FEFF and similar when
    copying from web UIs. *)
let strip_non_ascii s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      let code = Char.code c in
      if code >= 0x20 && code <= 0x7E then Buffer.add_char buf c)
    s ;
  Buffer.contents buf

(** {1 Size Formatting} *)

(** Format a byte count as a human-readable string using integer
    division (truncating).  Produces e.g. ["3 GB"], ["450 MB"],
    ["12 KB"], ["800 bytes"]. *)
let format_size bytes =
  let kb = Int64.div bytes 1024L in
  let mb = Int64.div kb 1024L in
  let gb = Int64.div mb 1024L in
  if gb > 0L then Printf.sprintf "%Ld GB" gb
  else if mb > 0L then Printf.sprintf "%Ld MB" mb
  else if kb > 0L then Printf.sprintf "%Ld KB" kb
  else Printf.sprintf "%Ld bytes" bytes

(** Format a byte count as a compact human-readable string with float
    precision.  Produces e.g. ["1.2G"], ["450M"], ["12K"], ["800B"].
    Handles values up to terabytes. *)
let format_bytes bytes =
  let b = Int64.to_float bytes in
  if b >= 1099511627776.0 then Printf.sprintf "%.1fT" (b /. 1099511627776.0)
  else if b >= 1073741824.0 then Printf.sprintf "%.1fG" (b /. 1073741824.0)
  else if b >= 1048576.0 then Printf.sprintf "%.0fM" (b /. 1048576.0)
  else if b >= 1024.0 then Printf.sprintf "%.0fK" (b /. 1024.0)
  else Printf.sprintf "%LdB" bytes

(** Remove surrounding quotes from a string.
    Handles backslash-escaped double quotes, regular double quotes,
    and single quotes.
    Returns the string unchanged if it is not quoted. *)
let unquote s =
  let len = String.length s in
  (* Handle backslash-escaped double quotes in shell *)
  if
    len >= 4
    && s.[0] = '\\'
    && s.[1] = '"'
    && s.[len - 2] = '\\'
    && s.[len - 1] = '"'
  then String.sub s 2 (len - 4)
  else if
    (* Handle regular quotes *)
    len >= 2
    && ((s.[0] = '"' && s.[len - 1] = '"')
       || (s.[0] = '\'' && s.[len - 1] = '\''))
  then String.sub s 1 (len - 2)
  else s

(** Format a byte count as a human-readable string with float precision
    and spaced units.  Produces e.g. ["1.5 GB"], ["100 MB"], ["512 bytes"].
    Best for disk-space messages shown to the user. *)
let format_size_float bytes =
  let b = Int64.to_float bytes in
  let gb = b /. (1024. *. 1024. *. 1024.) in
  if gb >= 1.0 then Printf.sprintf "%.1f GB" gb
  else
    let mb = b /. (1024. *. 1024.) in
    if mb >= 1.0 then
      let mb_rounded = Float.round mb in
      if mb_rounded >= 1024.0 then Printf.sprintf "%.1f GB" gb
      else Printf.sprintf "%.0f MB" mb
    else Printf.sprintf "%Ld bytes" bytes
