(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

type bin_source =
  | Managed_version of string
  | Linked_alias of string
  | Raw_path of string

type linked_dir = {alias : string; path : string}

(* Bin source operations *)

let bin_source_to_string = function
  | Managed_version v -> Printf.sprintf "v%s (managed)" v
  | Linked_alias a -> Printf.sprintf "%s (linked)" a
  | Raw_path p -> p

let bin_source_to_yojson = function
  | Managed_version v ->
      `Assoc [("type", `String "managed"); ("version", `String v)]
  | Linked_alias a -> `Assoc [("type", `String "linked"); ("alias", `String a)]
  | Raw_path p ->
      (* For backward compatibility, raw paths are stored as plain string
         in the app_bin_dir field - handled in service.ml *)
      `Assoc [("type", `String "path"); ("path", `String p)]

let bin_source_of_yojson json =
  let open Yojson.Safe.Util in
  try
    match json with
    | `Assoc _ -> (
        match member "type" json with
        | `String "managed" ->
            let version = member "version" json |> to_string in
            Ok (Managed_version version)
        | `String "linked" ->
            let alias = member "alias" json |> to_string in
            Ok (Linked_alias alias)
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
  Filename.concat (Common.xdg_data_home ()) "octez-manager/binaries"

let managed_version_path version =
  Filename.concat (binaries_dir ()) ("v" ^ version)

let linked_dirs_file () =
  Filename.concat
    (Common.xdg_data_home ())
    "octez-manager/linked-directories.json"

(* Linked directories JSON operations *)

let linked_dir_to_yojson ld =
  `Assoc [("alias", `String ld.alias); ("path", `String ld.path)]

let linked_dir_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let alias = member "alias" json |> to_string in
    let path = member "path" json |> to_string in
    Ok {alias; path}
  with Type_error (msg, _) -> R.error_msg msg

let linked_dirs_to_yojson dirs = `List (List.map linked_dir_to_yojson dirs)

let linked_dirs_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let dirs = to_list json in
    let rec parse acc = function
      | [] -> Ok (List.rev acc)
      | h :: t -> (
          match linked_dir_of_yojson h with
          | Ok ld -> parse (ld :: acc) t
          | Error _ as e -> e)
    in
    parse [] dirs
  with Type_error (msg, _) -> R.error_msg msg

(* Linked directories file operations *)

let ensure_parent_dir path =
  let dir = Filename.dirname path in
  let owner, group = Common.current_user_group_names () in
  Common.ensure_dir_path ~owner ~group ~mode:0o755 dir

let load_linked_dirs () =
  let path = linked_dirs_file () in
  if Sys.file_exists path then
    try
      let json = Yojson.Safe.from_file path in
      linked_dirs_of_yojson json
    with exn ->
      R.error_msgf
        "Failed to load linked directories: %s"
        (Printexc.to_string exn)
  else Ok []

let save_linked_dirs dirs =
  let path = linked_dirs_file () in
  let* () = ensure_parent_dir path in
  try
    let json = linked_dirs_to_yojson dirs in
    Yojson.Safe.to_file path json ;
    Ok ()
  with exn ->
    R.error_msgf
      "Failed to save linked directories: %s"
      (Printexc.to_string exn)

let find_linked_dir alias =
  let* dirs = load_linked_dirs () in
  Ok (List.find_opt (fun ld -> ld.alias = alias) dirs)

let add_linked_dir ~alias ~path =
  if not (Sys.file_exists path) then R.error_msgf "Path does not exist: %s" path
  else if not (Sys.is_directory path) then
    R.error_msgf "Path is not a directory: %s" path
  else
    let* dirs = load_linked_dirs () in
    if List.exists (fun ld -> ld.alias = alias) dirs then
      R.error_msgf "Alias '%s' already exists" alias
    else
      let dirs = {alias; path} :: dirs in
      save_linked_dirs dirs

let remove_linked_dir alias =
  let* dirs = load_linked_dirs () in
  if not (List.exists (fun ld -> ld.alias = alias) dirs) then
    R.error_msgf "Alias '%s' not found" alias
  else
    let dirs = List.filter (fun ld -> ld.alias <> alias) dirs in
    save_linked_dirs dirs

let rename_linked_dir ~old_alias ~new_alias =
  let* dirs = load_linked_dirs () in
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
    save_linked_dirs dirs

(* Managed versions *)

(* Compare version strings numerically (e.g., "24.0" > "9.0") *)
let compare_versions a b =
  let parse_version v =
    String.split_on_char '.' v
    |> List.map (fun s -> try int_of_string s with _ -> 0)
  in
  let rec cmp l1 l2 =
    match (l1, l2) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1 :: t1, h2 :: t2 ->
        let c = compare h1 h2 in
        if c <> 0 then c else cmp t1 t2
  in
  cmp (parse_version a) (parse_version b)

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

(* Path resolution *)

let resolve_bin_source = function
  | Managed_version version ->
      let path = managed_version_path version in
      if Sys.file_exists path && Sys.is_directory path then Ok path
      else R.error_msgf "Managed version v%s is not installed" version
  | Linked_alias alias -> (
      match find_linked_dir alias with
      | Ok (Some ld) ->
          if Sys.file_exists ld.path && Sys.is_directory ld.path then Ok ld.path
          else
            R.error_msgf
              "Linked directory '%s' path does not exist: %s"
              alias
              ld.path
      | Ok None -> R.error_msgf "Linked alias '%s' not found" alias
      | Error _ as e -> e)
  | Raw_path path ->
      if Sys.file_exists path && Sys.is_directory path then Ok path
      else R.error_msgf "Binary path does not exist: %s" path
