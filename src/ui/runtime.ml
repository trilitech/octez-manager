open Rresult
open Octez_manager_lib

let rec ensure_dir path =
  let dir = Filename.dirname path in
  if path = dir || path = "." || path = "/" then ()
  else (
    ensure_dir dir ;
    if not (Sys.file_exists path) then
      try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let read_file path =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        Ok (really_input_string ic len))
  with Sys_error msg -> Error msg

let write_file path contents =
  try
    ensure_dir (Filename.dirname path) ;
    let oc = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc contents ;
        Ok ())
  with Sys_error msg -> Error msg

let mkdir path =
  try
    if Sys.file_exists path then Ok ()
    else (
      ensure_dir (Filename.dirname path) ;
      Unix.mkdir path 0o755 ;
      Ok ())
  with Unix.Unix_error (err, _, _) -> Error (Unix.error_message err)

let list_dir path =
  try Sys.readdir path |> Array.to_list |> List.sort String.compare |> Result.ok
  with Sys_error msg -> Error msg

let probe_writable path =
  try
    if Sys.file_exists path then (
      let st = Unix.stat path in
      match st.Unix.st_kind with
      | Unix.S_DIR ->
          Unix.access path [Unix.W_OK; Unix.X_OK] ;
          Ok true
      | _ ->
          let oc = open_out_gen [Open_wronly; Open_append] 0 path in
          close_out oc ;
          Ok true)
    else (
      ensure_dir (Filename.dirname path) ;
      let tmp =
        Filename.concat (Filename.dirname path) (Filename.basename path ^ ".tmp")
      in
      let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o600 tmp in
      close_out oc ;
      Sys.remove tmp ;
      Ok true)
  with
  | Sys_error msg -> Error msg
  | Unix.Unix_error (err, _, path) ->
      Error (Printf.sprintf "%s: %s" path (Unix.error_message err))

let read_all chan =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buf (input_line chan) ;
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ()) ;
  Buffer.contents buf

let run_command argv cwd =
  match argv with
  | [] -> Error "empty command"
  | _ -> (
      let base = Common.cmd_to_string argv in
      let command =
        match cwd with
        | None -> base
        | Some dir -> Printf.sprintf "cd %s && %s" (Common.sh_quote dir) base
      in
      let ic, oc, ec = Unix.open_process_full command (Unix.environment ()) in
      close_out_noerr oc ;
      let stdout = read_all ic in
      let stderr = read_all ec in
      match Unix.close_process_full (ic, oc, ec) with
      | Unix.WEXITED code ->
          Ok {Miaou_interfaces.System.exit_code = code; stdout; stderr}
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          Ok {Miaou_interfaces.System.exit_code = 128 + signal; stdout; stderr})

let get_current_user_info () =
  try
    let pw = Unix.getpwuid (Unix.geteuid ()) in
    Ok (pw.Unix.pw_name, pw.Unix.pw_dir)
  with Unix.Unix_error (err, _, _) -> Error (Unix.error_message err)

let get_disk_usage path =
  match run_command ["du"; "-sb"; path] None with
  | Ok {Miaou_interfaces.System.exit_code = 0; stdout; _} -> (
      match String.split_on_char '\t' stdout with
      | size :: _ -> (
          try Ok (Int64.of_string (String.trim size)) with _ -> Ok 0L)
      | _ -> Ok 0L)
  | Ok res ->
      Error
        (Printf.sprintf
           "du failed with exit code %d (%s)"
           res.Miaou_interfaces.System.exit_code
           res.stderr)
  | Error msg -> Error msg

let system_impl () =
  let module System_if = Miaou_interfaces.System in
  System_if.
    {
      file_exists = Sys.file_exists;
      is_directory =
        (fun path ->
          try (Unix.stat path).Unix.st_kind = Unix.S_DIR with _ -> false);
      read_file = (fun path -> read_file path);
      write_file = (fun path contents -> write_file path contents);
      mkdir = (fun path -> mkdir path);
      run_command = (fun ~argv ~cwd -> run_command argv cwd);
      get_current_user_info = (fun () -> get_current_user_info ());
      get_disk_usage = (fun ~path -> get_disk_usage path);
      list_dir = (fun path -> list_dir path);
      probe_writable = (fun ~path -> probe_writable path);
      get_env_var = Sys.getenv_opt;
    }

let log_enabled = ref false

let logfile = ref None

let close_logfile () =
  match !logfile with
  | None -> ()
  | Some oc ->
      close_out_noerr oc ;
      logfile := None

let set_logfile path_opt =
  close_logfile () ;
  match path_opt with
  | None -> Ok ()
  | Some path -> (
      try
        ensure_dir (Filename.dirname path) ;
        let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 path in
        logfile := Some oc ;
        Ok ()
      with Sys_error msg -> Error msg)

let log_line level msg =
  if !log_enabled then (
    let module Logger = Miaou_interfaces.Logger_capability in
    let prefix =
      match level with
      | Logger.Debug -> "DBG"
      | Logger.Info -> "INF"
      | Logger.Warning -> "WRN"
      | Logger.Error -> "ERR"
    in
    let line = Printf.sprintf "[%s] %s" prefix msg in
    prerr_endline line ;
    match !logfile with
    | None -> ()
    | Some oc ->
        output_string oc (line ^ "\n") ;
        flush oc)

let register_logger ~log ~logfile_path =
  let module Logger = Miaou_interfaces.Logger_capability in
  let impl =
    Logger.
      {
        logf = log_line;
        set_enabled = (fun flag -> log_enabled := flag);
        set_logfile;
      }
  in
  Logger.set impl ;
  log_enabled := log ;
  ignore (set_logfile logfile_path)

let register_palette () =
  let module Palette = Miaou_interfaces.Palette in
  let module W = Miaou_widgets_display.Widgets in
  let id x = x in
  let gradient _ ~total_visible:_ ~start_pos:_ s = s in
  (* 256-color codes matching octez_setup palette *)
  let c_tezos_blue = 27 in
  let c_tezos_blue_dark = 24 in
  let c_white = 15 in
  let c_steel = 245 in
  let palette : Palette.t =
    {
      fg_primary = W.fg c_tezos_blue;
      fg_secondary = id;
      fg_muted = id;
      bg_primary = id;
      fg_stealth = id;
      bg_stealth = id;
      fg_slate = id;
      bg_slate = id;
      fg_steel = id;
      bg_steel = id;
      fg_white = W.fg c_white;
      bg_white = id;
      purple_gradient = id;
      purple_gradient_at = gradient;
      purple_gradient_line = (fun _ s -> s);
      fg_success = W.fg c_tezos_blue;
      fg_error = id;
      selection_bg = W.bg c_tezos_blue_dark;
      selection_fg = W.fg c_white;
      fixed_region_bg = W.bg c_steel;
      header_bg = W.bg c_steel;
    }
  in
  Palette.set palette

let register_system () = Miaou_interfaces.System.set (system_impl ())

let initialized = ref false

let initialize ?(log = false) ?logfile () =
  if not !initialized then (
    register_system () ;
    register_palette () ;
    Rpc_scheduler.start () ;
    Metrics.maybe_start_from_env () ;
    initialized := true) ;
  register_logger ~log ~logfile_path:logfile
