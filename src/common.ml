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
  let suffix = Printf.sprintf "%s-%s" role_part inst in
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

let run argv =
  append_debug_log ("RUN " ^ cmd_to_string argv) ;
  let cmd = Bos.Cmd.of_list argv in
  match Bos.OS.Cmd.run cmd with
  | Ok () -> Ok ()
  | Error (`Msg m) -> Error (`Msg m)

let run_out argv =
  append_debug_log ("RUN_OUT " ^ cmd_to_string argv) ;
  let cmd = Bos.Cmd.of_list argv in
  match Bos.OS.Cmd.(run_out cmd |> out_string ~trim:true) with
  | Ok (out, _) -> Ok out
  | Error (`Msg m) -> Error (`Msg m)

let run_as ~user argv =
  let trimmed = String.trim user in
  let current_user, _ = current_user_group_names () in
  if trimmed = "" || (not (is_root ())) || String.equal trimmed current_user
  then run argv
  else
    let command = cmd_to_string argv in
    run ["su"; "-s"; "/bin/sh"; "-c"; command; trimmed]

let ensure_tree_owner ~owner ~group path =
  if not (is_root ()) then Ok ()
  else if not (Sys.file_exists path) then Ok ()
  else
    match run ["chown"; "-R"; Printf.sprintf "%s:%s" owner group; path] with
    | Ok () -> Ok ()
    | Error (`Msg e) ->
        R.error_msgf "Failed to set ownership recursively on %s: %s" path e

let download_file ~url ~dest_path =
  append_debug_log (Printf.sprintf "DOWNLOAD %s -> %s" url dest_path) ;
  run ["curl"; "-fSL"; url; "-o"; dest_path]

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
