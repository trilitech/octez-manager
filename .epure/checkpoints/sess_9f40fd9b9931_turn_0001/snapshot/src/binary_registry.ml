(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

type bin_source =
  | Managed_octez_version of string
  | Managed_signatory_version of string
  | Managed_octez_index_version of string
      (** Downloaded/managed octez-index version e.g. "0.1.0" *)
  | Registered_alias of string
  | Raw_path of string

type registered_dir = {alias : string; path : string}

(* Bin source operations *)

let bin_source_to_string = function
  | Managed_octez_version v -> Printf.sprintf "v%s (managed)" v
  | Managed_signatory_version v -> Printf.sprintf "signatory-v%s (managed)" v
  | Managed_octez_index_version v ->
      Printf.sprintf "octez-index-v%s (managed)" v
  | Registered_alias a -> Printf.sprintf "%s (registered)" a
  | Raw_path p -> p

let bin_source_to_yojson = function
  | Managed_octez_version v ->
      `Assoc [("type", `String "managed_octez"); ("version", `String v)]
  | Managed_signatory_version v ->
      `Assoc [("type", `String "managed_signatory"); ("version", `String v)]
  | Managed_octez_index_version v ->
      `Assoc [("type", `String "managed_index"); ("version", `String v)]
  | Registered_alias a ->
      `Assoc [("type", `String "registered"); ("alias", `String a)]
  | Raw_path p ->
      (* For backward compatibility, raw paths are stored as plain string
         in the app_bin_dir field - handled in service.ml *)
      `Assoc [("type", `String "path"); ("path", `String p)]

let bin_source_of_yojson json =
  let open Yojson.Safe.Util in
  (* Migration helper: infer Octez vs Signatory from filesystem *)
  let migrate_legacy_managed_version version =
    let xdg_data = Paths.xdg_data_home () in
    let octez_path =
      Filename.concat
        (Filename.concat xdg_data "octez-manager/binaries")
        ("v" ^ version)
    in
    let signatory_path =
      Filename.concat
        (Filename.concat xdg_data "octez-manager/signatory-binaries")
        ("v" ^ version)
    in
    if Sys.file_exists octez_path && Sys.is_directory octez_path then (
      Logs.warn (fun m ->
          m
            "Migrating legacy Managed_version \"%s\" to Managed_octez_version"
            version) ;
      Managed_octez_version version)
    else if Sys.file_exists signatory_path && Sys.is_directory signatory_path
    then (
      Logs.warn (fun m ->
          m
            "Migrating legacy Managed_version \"%s\" to \
             Managed_signatory_version"
            version) ;
      Managed_signatory_version version)
    else (
      (* Default to Octez for backward compatibility *)
      Logs.warn (fun m ->
          m
            "Migrating legacy Managed_version \"%s\" to Managed_octez_version \
             (path not found)"
            version) ;
      Managed_octez_version version)
  in
  try
    match json with
    | `Assoc _ -> (
        match member "type" json with
        | `String "managed_octez" ->
            let version = member "version" json |> to_string in
            Ok (Managed_octez_version version)
        | `String "managed_signatory" ->
            let version = member "version" json |> to_string in
            Ok (Managed_signatory_version version)
        | `String "managed_index" ->
            let version = member "version" json |> to_string in
            Ok (Managed_octez_index_version version)
        | `String "managed" ->
            (* Legacy format - migrate *)
            let version = member "version" json |> to_string in
            Ok (migrate_legacy_managed_version version)
        | `String "registered" ->
            let alias = member "alias" json |> to_string in
            Ok (Registered_alias alias)
        | `String "linked" ->
            (* Backward compatibility: old "linked" type *)
            let alias = member "alias" json |> to_string in
            Ok (Registered_alias alias)
        | `String "path" ->
            let path = member "path" json |> to_string in
            Ok (Raw_path path)
        | _ -> R.error_msg "Invalid bin_source type")
    | `String path ->
        (* Backward compatibility: plain string is treated as raw path *)
        Ok (Raw_path path)
    | _ -> R.error_msg "Invalid bin_source format"
  with Type_error (msg, _) -> R.error_msg msg

let bin_source_of_legacy path = Raw_path path

(* XDG paths *)

let binaries_dir () =
  Filename.concat (Paths.xdg_data_home ()) "octez-manager/binaries"

let managed_version_path version =
  Filename.concat (binaries_dir ()) ("v" ^ version)

let index_binaries_dir () =
  Filename.concat (Paths.xdg_data_home ()) "octez-manager/octez-index-binaries"

let managed_index_path version =
  Filename.concat (index_binaries_dir ()) ("v" ^ version)

let registered_dirs_file () =
  Filename.concat
    (Paths.xdg_data_home ())
    "octez-manager/registered-directories.json"

let registered_dirs_lock_file () =
  Filename.concat
    (Paths.xdg_data_home ())
    "octez-manager/registered-directories.json.lock"

(* Registered directories JSON operations *)

let registered_dir_to_yojson ld =
  `Assoc [("alias", `String ld.alias); ("path", `String ld.path)]

let registered_dir_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let alias = member "alias" json |> to_string in
    let path = member "path" json |> to_string in
    Ok {alias; path}
  with Type_error (msg, _) -> R.error_msg msg

let registered_dirs_to_yojson dirs =
  `List (List.map registered_dir_to_yojson dirs)

let registered_dirs_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let dirs = to_list json in
    let rec parse acc = function
      | [] -> Ok (List.rev acc)
      | h :: t -> (
          match registered_dir_of_yojson h with
          | Ok ld -> parse (ld :: acc) t
          | Error _ as e -> e)
    in
    parse [] dirs
  with Type_error (msg, _) -> R.error_msg msg

(* Registered directories file operations *)

let ensure_parent_dir path =
  let dir = Filename.dirname path in
  let owner, group = Paths.current_user_group_names () in
  File_ops.ensure_dir_path ~owner ~group ~mode:0o755 dir

let load_registered_dirs () =
  let path = registered_dirs_file () in
  if Sys.file_exists path then
    try
      let json = Yojson.Safe.from_file path in
      registered_dirs_of_yojson json
    with exn ->
      R.error_msgf
        "Failed to load registered directories: %s"
        (Printexc.to_string exn)
  else Ok []

let save_registered_dirs dirs =
  let path = registered_dirs_file () in
  let* () = ensure_parent_dir path in
  try
    let json = registered_dirs_to_yojson dirs in
    Yojson.Safe.to_file path json ;
    Ok ()
  with exn ->
    R.error_msgf
      "Failed to save registered directories: %s"
      (Printexc.to_string exn)

let find_registered_dir alias =
  let* dirs = load_registered_dirs () in
  Ok (List.find_opt (fun ld -> ld.alias = alias) dirs)

let add_registered_dir ~alias ~path =
  if not (Sys.file_exists path) then R.error_msgf "Path does not exist: %s" path
  else if not (Sys.is_directory path) then
    R.error_msgf "Path is not a directory: %s" path
  else
    File_ops.with_file_lock (registered_dirs_lock_file ()) (fun () ->
        let* dirs = load_registered_dirs () in
        if List.exists (fun ld -> ld.alias = alias) dirs then
          R.error_msgf "Alias '%s' already exists" alias
        else
          let dirs = {alias; path} :: dirs in
          save_registered_dirs dirs)

let remove_registered_dir alias =
  File_ops.with_file_lock (registered_dirs_lock_file ()) (fun () ->
      let* dirs = load_registered_dirs () in
      if not (List.exists (fun ld -> ld.alias = alias) dirs) then
        R.error_msgf "Alias '%s' not found" alias
      else
        let dirs = List.filter (fun ld -> ld.alias <> alias) dirs in
        save_registered_dirs dirs)

let rename_registered_dir ~old_alias ~new_alias =
  File_ops.with_file_lock (registered_dirs_lock_file ()) (fun () ->
      let* dirs = load_registered_dirs () in
      if not (List.exists (fun ld -> ld.alias = old_alias) dirs) then
        R.error_msgf "Alias '%s' not found" old_alias
      else if List.exists (fun ld -> ld.alias = new_alias) dirs then
        R.error_msgf "Alias '%s' already exists" new_alias
      else
        let dirs =
          List.map
            (fun ld ->
              if ld.alias = old_alias then {ld with alias = new_alias} else ld)
            dirs
        in
        save_registered_dirs dirs)

(* Managed versions *)

(* Check if a version installation is complete *)
let is_complete_installation version =
  let dest_dir = managed_version_path version in
  if not (Sys.file_exists dest_dir && Sys.is_directory dest_dir) then false
  else
    (* Check for metadata file *)
    let metadata_file = Filename.concat dest_dir ".metadata.json" in
    if not (Sys.file_exists metadata_file) then false
    else
      (* Check that all expected binaries exist *)
      let binaries =
        ["octez-node"; "octez-client"; "octez-baker"; "octez-dal-node"]
      in
      List.for_all
        (fun binary ->
          let path = Filename.concat dest_dir binary in
          Sys.file_exists path)
        binaries

let compare_versions = Version_utils.compare_versions

let list_managed_versions () =
  let dir = binaries_dir () in
  if Sys.file_exists dir && Sys.is_directory dir then
    try
      let entries = Sys.readdir dir |> Array.to_list in
      let versions =
        entries
        |> List.filter (fun e ->
            String.length e > 1
            && e.[0] = 'v'
            && Sys.is_directory (Filename.concat dir e))
        |> List.map (fun e -> String.sub e 1 (String.length e - 1))
        |> List.filter is_complete_installation
        (* Filter out incomplete installations *)
        |> List.sort (fun a b -> compare_versions b a)
        (* newest first *)
      in
      Ok versions
    with exn ->
      R.error_msgf
        "Failed to list managed versions: %s"
        (Printexc.to_string exn)
  else Ok []

let managed_version_exists version =
  let path = managed_version_path version in
  Sys.file_exists path && Sys.is_directory path

let list_managed_index_versions () =
  let dir = index_binaries_dir () in
  if Sys.file_exists dir && Sys.is_directory dir then
    try
      let entries = Sys.readdir dir |> Array.to_list in
      let versions =
        entries
        |> List.filter (fun e ->
            String.length e > 1
            && e.[0] = 'v'
            && Sys.is_directory (Filename.concat dir e))
        |> List.map (fun e -> String.sub e 1 (String.length e - 1))
        |> List.sort (fun a b -> compare_versions b a)
      in
      Ok versions
    with exn ->
      R.error_msgf
        "Failed to list managed index versions: %s"
        (Printexc.to_string exn)
  else Ok []

(* Path resolution *)

let resolve_bin_source = function
  | Managed_octez_version version ->
      let path = managed_version_path version in
      if Sys.file_exists path && Sys.is_directory path then Ok path
      else R.error_msgf "Managed Octez version v%s is not installed" version
  | Managed_signatory_version version ->
      let path = Signatory_downloader.signatory_version_path version in
      if Sys.file_exists path && Sys.is_directory path then Ok path
      else R.error_msgf "Managed Signatory version v%s is not installed" version
  | Managed_octez_index_version version ->
      let path = managed_index_path version in
      if Sys.file_exists path && Sys.is_directory path then Ok path
      else
        R.error_msgf "Managed octez-index version v%s is not installed" version
  | Registered_alias alias -> (
      match find_registered_dir alias with
      | Ok (Some ld) ->
          if Sys.file_exists ld.path && Sys.is_directory ld.path then Ok ld.path
          else
            R.error_msgf
              "Registered directory '%s' path does not exist: %s"
              alias
              ld.path
      | Ok None -> R.error_msgf "Registered alias '%s' not found" alias
      | Error _ as e -> e)
  | Raw_path path ->
      if Sys.file_exists path && Sys.is_directory path then Ok path
      else R.error_msgf "Binary path does not exist: %s" path

(** Expose internal functions for testing *)
module For_tests = struct
  let bin_source_to_string = bin_source_to_string

  let bin_source_to_yojson = bin_source_to_yojson

  let bin_source_of_yojson = bin_source_of_yojson

  let bin_source_of_legacy = bin_source_of_legacy

  let registered_dir_to_yojson = registered_dir_to_yojson

  let registered_dir_of_yojson = registered_dir_of_yojson

  let registered_dirs_to_yojson = registered_dirs_to_yojson

  let registered_dirs_of_yojson = registered_dirs_of_yojson

  let compare_versions = compare_versions
end
