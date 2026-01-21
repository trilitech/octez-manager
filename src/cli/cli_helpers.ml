(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib
module Term = Cmdliner.Term

let cmdliner_error msg = `Error (false, msg)

let resolve_app_bin_dir ?octez_version ?bin_dir_alias app_bin_dir =
  (* Priority: octez_version > bin_dir_alias > app_bin_dir > auto-detect *)
  match (octez_version, bin_dir_alias, app_bin_dir) with
  | Some version, _, _ ->
      (* Use managed version *)
      let version = String.trim version in
      if Binary_registry.managed_version_exists version then
        Ok (Binary_registry.managed_version_path version)
      else
        Error
          (Printf.sprintf
             "Managed version v%s not found. Download it first with:\n\
             \  octez-manager binaries download %s"
             version
             version)
  | None, Some alias, _ -> (
      (* Use linked directory alias *)
      let alias = String.trim alias in
      match Binary_registry.find_linked_dir alias with
      | Ok (Some ld) -> Ok ld.Binary_registry.path
      | Ok None ->
          Error
            (Printf.sprintf
               "Linked directory alias '%s' not found. Create it with:\n\
               \  octez-manager binaries link --alias %s /path/to/binaries"
               alias
               alias)
      | Error (`Msg e) -> Error e)
  | None, None, Some dir when String.trim dir <> "" -> (
      (* Use raw path *)
      match Common.make_absolute_path dir with
      | Ok abs_path -> Ok abs_path
      | Error msg -> Error msg)
  | None, None, _ -> (
      (* Auto-detect from PATH *)
      match Common.which "octez-node" with
      | Some path -> Ok (Filename.dirname path)
      | None ->
          Error
            "Unable to locate octez-node in PATH. Install Octez binaries or \
             use --octez-version, --bin-dir-alias, or --app-bin-dir")

let interactive_tty =
  lazy
    (let fd_isatty fd = try Unix.isatty fd with Unix.Unix_error _ -> false in
     fd_isatty Unix.stdin && fd_isatty Unix.stdout)

let is_interactive () = Lazy.force interactive_tty

let normalize_opt_string = function
  | Some s ->
      let trimmed = String.trim s in
      if String.equal trimmed "" then None else Some trimmed
  | None -> None

let prompt_input ?default question =
  if not (is_interactive ()) then None
  else
    let suffix =
      match default with
      | Some (display, _) when String.trim display <> "" ->
          Printf.sprintf " [%s]" display
      | _ -> ""
    in
    let prompt = Printf.sprintf "%s%s: " question suffix in
    match LNoise.linenoise prompt with
    | exception Sys.Break ->
        prerr_endline "" ;
        exit 130 (* Standard exit code for Ctrl-C *)
    | exception End_of_file -> Option.map snd default
    | None -> Option.map snd default
    | Some line ->
        let trimmed = String.trim line in
        if String.equal trimmed "" then Option.map snd default else Some trimmed

let rec prompt_required_string question =
  match prompt_input question with
  | Some value -> value
  | None ->
      prerr_endline "A value is required." ;
      prompt_required_string question

(** Prompt with filesystem directory completion *)
let prompt_directory question =
  if not (is_interactive ()) then None
  else (
    (* Set up directory completion *)
    LNoise.set_completion_callback (fun line_so_far ln_completions ->
        let dir, prefix =
          if String.ends_with ~suffix:"/" line_so_far then (line_so_far, "")
          else (Filename.dirname line_so_far, Filename.basename line_so_far)
        in
        let dir = if dir = "" then "." else dir in
        try
          let entries = Sys.readdir dir in
          Array.iter
            (fun entry ->
              if
                String.starts_with
                  ~prefix:(String.lowercase_ascii prefix)
                  (String.lowercase_ascii entry)
              then
                let full_path = Filename.concat dir entry in
                if Sys.is_directory full_path then
                  LNoise.add_completion ln_completions (full_path ^ "/"))
            entries
        with _ -> ()) ;
    LNoise.set_hints_callback (fun _ -> None) ;
    match LNoise.linenoise (question ^ ": ") with
    | None -> None
    | Some line ->
        let trimmed = String.trim line in
        if trimmed = "" then None else Some trimmed)

let rec prompt_required_directory question =
  match prompt_directory question with
  | Some value -> value
  | None ->
      prerr_endline "A directory path is required." ;
      prompt_required_directory question

(* Inline copy of prompt_with_completion placed before prompt_history_mode so it
   can be referenced. This mirrors the main prompt_with_completion later in the
   file. *)
let prompt_with_completion_inline question completions =
  if not (is_interactive ()) then None
  else
    let completions_lower =
      List.map (fun c -> (c, String.lowercase_ascii c)) completions
    in
    LNoise.set_completion_callback (fun line_so_far ln_completions ->
        let prefix = String.lowercase_ascii line_so_far in
        List.iter
          (fun (candidate, candidate_lower) ->
            if String.starts_with ~prefix candidate_lower then
              LNoise.add_completion ln_completions candidate)
          completions_lower) ;
    LNoise.set_hints_callback (fun line_so_far ->
        let prefix = String.lowercase_ascii line_so_far in
        match
          List.find_opt
            (fun (_, candidate_lower) ->
              String.starts_with ~prefix candidate_lower)
            completions_lower
        with
        | Some (hint, _) when String.length hint > String.length line_so_far ->
            Some
              ( String.sub
                  hint
                  (String.length line_so_far)
                  (String.length hint - String.length line_so_far),
                LNoise.Yellow,
                false )
        | _ -> None) ;
    let res =
      match LNoise.linenoise (question ^ ": ") with
      | exception Sys.Break ->
          prerr_endline "" ;
          exit 130
      | None -> None
      | Some line ->
          let trimmed = String.trim line in
          if String.equal trimmed "" then None else Some trimmed
    in
    LNoise.set_completion_callback (fun _ _ -> ()) ;
    LNoise.set_hints_callback (fun _ -> None) ;
    res

let prompt_history_mode default =
  if not (is_interactive ()) then default
  else
    let rec loop () =
      let choices = ["rolling"; "full"; "archive"] in
      match prompt_with_completion_inline "History mode" choices with
      | Some "" | None ->
          prerr_endline "Please enter rolling, full or archive." ;
          loop ()
      | Some raw_value -> (
          let value = String.trim raw_value in
          (* First try direct parse *)
          match History_mode.of_string value with
          | Ok hm -> hm
          | Error _ -> (
              (* If value looks like "full:50" try the prefix before ':' *)
              let prefix_opt =
                try Some (String.sub value 0 (String.index value ':'))
                with Not_found -> None
              in
              match prefix_opt with
              | Some p -> (
                  match History_mode.of_string p with
                  | Ok hm -> hm
                  | Error _ ->
                      prerr_endline "Please enter rolling, full or archive." ;
                      loop ())
              | None ->
                  prerr_endline "Please enter rolling, full or archive." ;
                  loop ()))
    in
    loop ()

let prompt_yes_no question ~default =
  if not (is_interactive ()) then default
  else
    let rec loop () =
      let label = if default then "Y/n" else "y/N" in
      let fallback = if default then "yes" else "no" in
      match prompt_input ~default:(label, fallback) question with
      | Some answer -> (
          match String.lowercase_ascii (String.trim answer) with
          | "y" | "yes" | "true" | "1" -> true
          | "n" | "no" | "false" | "0" -> false
          | _ ->
              prerr_endline "Please answer yes or no." ;
              loop ())
      | None -> loop ()
    in
    loop ()

let resolve_tmp_dir_for_snapshot ~snapshot_url ~tmp_dir =
  (* If tmp_dir is already specified, use it *)
  match tmp_dir with
  | Some dir -> Ok (Some dir)
  | None -> (
      (* Check snapshot size vs /tmp space *)
      match Common.get_remote_file_size snapshot_url with
      | None ->
          (* Can't determine size, proceed with default /tmp *)
          Ok None
      | Some size -> (
          let tmp_path = Filename.get_temp_dir_name () in
          match Common.get_available_space tmp_path with
          | None ->
              (* Can't determine space, proceed with default *)
              Ok None
          | Some available ->
              (* Add 10% buffer for safety *)
              let required = Int64.add size (Int64.div size 10L) in
              if available >= required then Ok None
              else if is_interactive () then (
                Printf.printf
                  "Snapshot size is %s but %s only has %s available.\n"
                  (Cli_output.format_bytes size)
                  tmp_path
                  (Cli_output.format_bytes available) ;
                let rec prompt_dir () =
                  let dir =
                    prompt_required_directory
                      "Enter a directory with enough space for the download"
                  in
                  if Sys.file_exists dir && Sys.is_directory dir then (
                    match Common.get_available_space dir with
                    | Some space when space >= required -> Some dir
                    | Some space ->
                        Printf.printf
                          "%s only has %s available, need %s.\n"
                          dir
                          (Cli_output.format_bytes space)
                          (Cli_output.format_bytes required) ;
                        prompt_dir ()
                    | None ->
                        Printf.printf "Cannot determine space in %s.\n" dir ;
                        prompt_dir ())
                  else (
                    Printf.printf "%s is not a valid directory.\n" dir ;
                    prompt_dir ())
                in
                Ok (prompt_dir ()))
              else
                Error
                  (Printf.sprintf
                     "Snapshot size is %s but %s only has %s available. Use \
                      --tmp-dir to specify an alternative download location."
                     (Cli_output.format_bytes size)
                     tmp_path
                     (Cli_output.format_bytes available))))

let check_data_dir_space ~snapshot_url ~data_dir =
  (* Check if data directory has enough space for imported snapshot data *)
  match Common.get_remote_file_size snapshot_url with
  | None -> Ok () (* Can't determine size, proceed *)
  | Some snapshot_size -> (
      (* Storage needs ~1.2x snapshot size (imported data with buffer) *)
      let required = Int64.add snapshot_size (Int64.div snapshot_size 5L) in
      (* Use parent dir if data_dir doesn't exist yet *)
      let check_path =
        if Sys.file_exists data_dir then data_dir else Filename.dirname data_dir
      in
      match Common.get_available_space check_path with
      | None -> Ok () (* Can't determine space, proceed *)
      | Some available ->
          if available >= required then Ok ()
          else
            Error
              (Printf.sprintf
                 "Data directory %s needs %s for node storage but only has %s \
                  available."
                 data_dir
                 (Cli_output.format_bytes required)
                 (Cli_output.format_bytes available)))

(* Prompt with linenoise for autocompletion support *)
let prompt_with_completion question completions =
  if not (is_interactive ()) then None
  else
    (* Pre-compute lowercase versions for efficient matching *)
    let completions_lower =
      List.map (fun c -> (c, String.lowercase_ascii c)) completions
    in
    (* Set up completions *)
    LNoise.set_completion_callback (fun line_so_far ln_completions ->
        let prefix = String.lowercase_ascii line_so_far in
        List.iter
          (fun (candidate, candidate_lower) ->
            if String.starts_with ~prefix candidate_lower then
              LNoise.add_completion ln_completions candidate)
          completions_lower) ;
    (* Set hints *)
    LNoise.set_hints_callback (fun line_so_far ->
        let prefix = String.lowercase_ascii line_so_far in
        match
          List.find_opt
            (fun (_, candidate_lower) ->
              String.starts_with ~prefix candidate_lower)
            completions_lower
        with
        | Some (hint, _) when String.length hint > String.length line_so_far ->
            Some
              ( String.sub
                  hint
                  (String.length line_so_far)
                  (String.length hint - String.length line_so_far),
                LNoise.Yellow,
                false )
        | _ -> None) ;
    (* Read a line, then clear callbacks to avoid leaking completions to later prompts *)
    let res =
      match LNoise.linenoise (question ^ ": ") with
      | exception Sys.Break ->
          prerr_endline "" ;
          exit 130
      | None -> None
      | Some line ->
          let trimmed = String.trim line in
          if String.equal trimmed "" then None else Some trimmed
    in
    (* Clear callbacks *)
    LNoise.set_completion_callback (fun _ _ -> ()) ;
    LNoise.set_hints_callback (fun _ -> None) ;
    res

(* Prompt with linenoise for comma-separated multi-value autocompletion.
   Each value after a comma gets its own completion. *)
let prompt_with_multi_completion question completions =
  if not (is_interactive ()) then None
  else
    (* Pre-compute lowercase versions for efficient matching *)
    let completions_lower =
      List.map (fun c -> (c, String.lowercase_ascii c)) completions
    in
    (* Helper to get prefix (text after last comma) and already-entered values *)
    let split_at_last_comma line =
      match String.rindex_opt line ',' with
      | None -> ("", String.trim line)
      | Some idx ->
          let before = String.sub line 0 (idx + 1) in
          let after =
            String.trim
              (String.sub line (idx + 1) (String.length line - idx - 1))
          in
          (before, after)
    in
    (* Set up completions - complete based on text after last comma *)
    LNoise.set_completion_callback (fun line_so_far ln_completions ->
        let before, current = split_at_last_comma line_so_far in
        let prefix = String.lowercase_ascii current in
        (* Get already-selected values to exclude them *)
        let already_selected =
          String.split_on_char ',' before
          |> List.map (fun s -> String.lowercase_ascii (String.trim s))
          |> List.filter (fun s -> s <> "")
        in
        List.iter
          (fun (candidate, candidate_lower) ->
            if
              String.starts_with ~prefix candidate_lower
              && not (List.mem candidate_lower already_selected)
            then LNoise.add_completion ln_completions (before ^ candidate))
          completions_lower) ;
    (* Set hints - show hint based on text after last comma *)
    LNoise.set_hints_callback (fun line_so_far ->
        let _before, current = split_at_last_comma line_so_far in
        let prefix = String.lowercase_ascii current in
        (* Get already-selected values to exclude them *)
        let already_selected =
          String.split_on_char ',' line_so_far
          |> List.map (fun s -> String.lowercase_ascii (String.trim s))
          |> List.filter (fun s -> s <> "")
        in
        match
          List.find_opt
            (fun (_, candidate_lower) ->
              String.starts_with ~prefix candidate_lower
              && not (List.mem candidate_lower already_selected))
            completions_lower
        with
        | Some (hint, _) when String.length hint > String.length current ->
            Some
              ( String.sub
                  hint
                  (String.length current)
                  (String.length hint - String.length current),
                LNoise.Yellow,
                false )
        | _ -> None) ;
    (* Read a line, then clear callbacks *)
    let res =
      match LNoise.linenoise (question ^ ": ") with
      | exception Sys.Break ->
          prerr_endline "" ;
          exit 130
      | None -> None
      | Some line ->
          let trimmed = String.trim line in
          if String.equal trimmed "" then None else Some trimmed
    in
    (* Clear callbacks *)
    LNoise.set_completion_callback (fun _ _ -> ()) ;
    LNoise.set_hints_callback (fun _ -> None) ;
    res

(** Validate a port address, re-prompting in interactive mode if invalid.
    @param label Label for error messages (e.g., "RPC address")
    @param addr The address to validate
    @param default Default address if none provided (also used as example in error messages)
    @param exclude_instance Instance to exclude from "in use" checks (for edit mode)
    @return Ok addr if valid, Error msg if invalid in non-interactive mode *)
let rec validate_port_addr ~label ~addr ~default ?exclude_instance () =
  let validate a =
    Port_validation.validate_addr ~addr:a ?exclude_instance ~example:default ()
  in
  match validate addr with
  | Ok () -> Ok addr
  | Error err ->
      if is_interactive () then (
        Printf.eprintf "%s: %s\n%!" label (Port_validation.pp_error err) ;
        let new_addr =
          prompt_input ~default:(default, default) label
          |> Option.value ~default
        in
        validate_port_addr ~label ~addr:new_addr ~default ?exclude_instance ())
      else Error (Printf.sprintf "%s: %s" label (Port_validation.pp_error err))

let rec resolve_node_instance_or_endpoint ~node_instance =
  let ( let* ) = Result.bind in
  let* services = Service_registry.list () in
  let node_services =
    List.filter (fun (svc : Service.t) -> String.equal svc.role "node") services
  in
  let default = "127.0.0.1:8732" in
  let choice =
    match node_instance with
    | Some ni -> Some ni
    | None ->
        if not (is_interactive ()) then Some default
        else
          let instance_names =
            List.map (fun (svc : Service.t) -> svc.instance) node_services
          in
          (if node_services = [] then
             prerr_endline
               "No node instances found. You can specify a custom endpoint."
           else
             let instance_map =
               List.map
                 (fun (svc : Service.t) -> (svc.instance, svc.network))
                 node_services
             in
             Format.printf
               "Available node instances: %s@."
               (String.concat
                  ", "
                  (List.map
                     (fun (inst, net) -> Printf.sprintf "%s (%s)" inst net)
                     instance_map))) ;
          prompt_with_completion "Node instance" (default :: instance_names)
  in
  match choice with
  | None ->
      prerr_endline "Enter a node instance or custom endpoint" ;
      resolve_node_instance_or_endpoint ~node_instance
  | Some choice ->
      if
        List.exists
          (fun (svc : Service.t) -> String.equal svc.instance choice)
          node_services
      then Ok (`Instance choice)
      else Ok (`Endpoint choice)

let run_result = function
  | Ok () -> `Ok ()
  | Error (`Msg msg) -> cmdliner_error msg

(* Logging is always via journald - octez binaries handle their own file logging *)
let logging_mode_term = Term.(const Logging_mode.default)

let history_mode_doc =
  "History mode to configure on octez-node (rolling|full|archive)."

let history_mode_choices =
  [
    ("rolling", History_mode.Rolling);
    ("full", History_mode.Full);
    ("archive", History_mode.Archive);
  ]

let history_mode_opt_term =
  Arg.(
    value
    & opt (some (enum history_mode_choices)) None
    & info ["history-mode"] ~doc:history_mode_doc ~docv:"MODE")
