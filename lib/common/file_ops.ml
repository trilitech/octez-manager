(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

let rec mkdir_p path =
  if path = "/" || path = "." then ()
  else if Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path) ;
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let ensure_dir_path ~owner ~group ~mode path =
  mkdir_p path ;
  if Paths.is_root () then
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
    (* Best-effort chown — may fail if caller is not root; chmod is separate
       so it always runs even when chown is not permitted. *)
    (try
       let pw = Unix.getpwnam owner in
       let gr = Unix.getgrnam group in
       Unix.chown path pw.Unix.pw_uid gr.Unix.gr_gid
     with _ -> ()) ;
    (try Unix.chmod path mode with _ -> ()) ;
    Ok ())

let write_file ~mode ~owner ~group path contents =
  let dir = Filename.dirname path in
  let* _ = ensure_dir_path ~owner ~group ~mode:0o755 dir in
  (* Use a PID-unique temp file to avoid rename races when multiple
     processes write to the same path concurrently. *)
  let tmp = Printf.sprintf "%s.%d.tmp" path (Unix.getpid ()) in
  let oc = open_out_bin tmp in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents) ;
  (try Unix.chmod tmp mode with _ -> ()) ;
  (try
     let pw = Unix.getpwnam owner in
     let gr = Unix.getgrnam group in
     Unix.chown tmp pw.Unix.pw_uid gr.Unix.gr_gid
   with _ -> ()) ;
  try
    Sys.rename tmp path ;
    Ok ()
  with Sys_error msg ->
    (try Sys.remove tmp with Sys_error _ -> ()) ;
    R.error_msgf "write_file: failed to rename %s -> %s: %s" tmp path msg

let with_file_lock lock_path f =
  let dir = Filename.dirname lock_path in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()) ;
  let fd = Unix.openfile lock_path [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ()) ;
      Unix.close fd)
    (fun () ->
      Unix.lockf fd Unix.F_LOCK 0 ;
      f ())

let ensure_tree_owner ~owner ~group path =
  if not (Paths.is_root ()) then Ok ()
  else if not (Sys.file_exists path) then Ok ()
  else
    match
      Cmd_runner.run ["chown"; "-R"; Printf.sprintf "%s:%s" owner group; path]
    with
    | Ok () -> Ok ()
    | Error (`Msg e) ->
        R.error_msgf "Failed to set ownership recursively on %s: %s" path e

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
          (if Paths.is_root () then
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

let get_available_space dir =
  (* Check if path exists before calling df to avoid stderr noise *)
  if not (Sys.file_exists dir) then None
  else
    (* Use df to get available space in bytes *)
    match Cmd_runner.run_out ["df"; "-B1"; "--output=avail"; dir] with
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

let get_dir_size path =
  if not (Sys.file_exists path) then None
  else
    match
      Cmd_runner.run_out
        ["sh"; "-c"; "du -sb " ^ Filename.quote path ^ " 2>/dev/null"]
    with
    | Ok output -> (
        match String.split_on_char '\t' output with
        | size_str :: _ -> Int64.of_string_opt (String.trim size_str)
        | _ -> None)
    | Error _ -> None
