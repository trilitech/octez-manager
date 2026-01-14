(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Printf

let truthy value =
  let normalized = String.lowercase_ascii (String.trim value) in
  match normalized with "" | "0" | "false" | "off" | "no" -> false | _ -> true

let default_capture_dir () =
  match Sys.getenv_opt "MIAOU_DEBUG_CAPTURE_DIR" with
  | Some dir when String.trim dir <> "" -> dir
  | _ -> ( try Sys.getcwd () with _ -> Filename.get_temp_dir_name ())

let timestamp_suffix () =
  let tm = Unix.localtime (Unix.time ()) in
  sprintf
    "%04d%02d%02d-%02d%02d%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let rec ensure_dir path =
  if path = "" || path = "." then ()
  else if Sys.file_exists path then ()
  else
    let parent = Filename.dirname path in
    if parent = path then ()
    else (
      ensure_dir parent ;
      try Unix.mkdir path 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
      | _ -> ())

type writer = {oc : out_channel}

type writer_state = Uninitialized | Active of writer | Disabled

let keystroke_writer : writer_state ref = ref Uninitialized

let frame_writer : writer_state ref = ref Uninitialized

let writer_mutex = Mutex.create ()

let close_writer_opt slot =
  Mutex.lock writer_mutex ;
  (match !slot with
  | Active w ->
      (try close_out w.oc with _ -> ()) ;
      slot := Uninitialized
  | _ -> ()) ;
  Mutex.unlock writer_mutex

let () =
  at_exit (fun () ->
      close_writer_opt keystroke_writer ;
      close_writer_opt frame_writer)

let open_writer kind path =
  try
    let dir = Filename.dirname path in
    if dir <> path && dir <> "" then ensure_dir dir ;
    let oc =
      open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o644 path
    in
    fprintf stderr "[miaou][capture] %s -> %s\n%!" kind path ;
    Some {oc}
  with exn ->
    fprintf
      stderr
      "[miaou][capture] failed to open %s (%s): %s\n%!"
      kind
      path
      (Printexc.to_string exn) ;
    None

let resolve_path kind path_env =
  match Sys.getenv_opt path_env with
  | Some path when String.trim path <> "" -> path
  | _ ->
      let dir = default_capture_dir () in
      Filename.concat
        dir
        (sprintf "miaou_tui_%s_%s.jsonl" kind (timestamp_suffix ()))

let writer_enabled flag_env path_env =
  match (Sys.getenv_opt flag_env, Sys.getenv_opt path_env) with
  | None, None -> false
  | Some flag, _ -> truthy flag
  | None, Some path -> String.trim path <> ""

let create_writer ~kind ~flag_env ~path_env =
  if writer_enabled flag_env path_env then
    let path = resolve_path kind path_env in
    open_writer kind path
  else None

let ensure_writer slot create =
  match !slot with
  | Active w -> Some w
  | Disabled -> None
  | Uninitialized -> (
      Mutex.lock writer_mutex ;
      match !slot with
      | Active w ->
          Mutex.unlock writer_mutex ;
          Some w
      | Disabled ->
          Mutex.unlock writer_mutex ;
          None
      | Uninitialized -> (
          match create () with
          | Some w ->
              slot := Active w ;
              Mutex.unlock writer_mutex ;
              Some w
          | None ->
              slot := Disabled ;
              Mutex.unlock writer_mutex ;
              None))

let record_keystroke key =
  match
    ensure_writer keystroke_writer (fun () ->
        create_writer
          ~kind:"keystrokes"
          ~flag_env:"MIAOU_DEBUG_KEYSTROKE_CAPTURE"
          ~path_env:"MIAOU_DEBUG_KEYSTROKE_CAPTURE_PATH")
  with
  | None -> ()
  | Some w -> (
      try
        fprintf
          w.oc
          "{\"timestamp\": %.6f, \"key\": %S}\n"
          (Unix.gettimeofday ())
          key ;
        flush w.oc
      with _ -> ())

let record_frame ~rows ~cols frame =
  match
    ensure_writer frame_writer (fun () ->
        create_writer
          ~kind:"frames"
          ~flag_env:"MIAOU_DEBUG_FRAME_CAPTURE"
          ~path_env:"MIAOU_DEBUG_FRAME_CAPTURE_PATH")
  with
  | None -> ()
  | Some w -> (
      try
        fprintf
          w.oc
          "{\"timestamp\": %.6f, \"size\": {\"rows\": %d, \"cols\": %d}, \
           \"frame\": %S}\n"
          (Unix.gettimeofday ())
          rows
          cols
          frame ;
        flush w.oc
      with _ -> ())

let reset_for_tests () =
  close_writer_opt keystroke_writer ;
  close_writer_opt frame_writer
