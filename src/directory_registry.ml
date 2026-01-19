(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

type dir_type = Node_data_dir | Client_base_dir | App_bin_dir

type directory_entry = {
  path : string;
  dir_type : dir_type;
  created_at : string;
  last_used_at : string;
  linked_services : string list;
}

(* Maximum entries per directory type to avoid unbounded growth *)
let max_entries_per_type = 10

(* JSON serialization *)

let dir_type_to_yojson = function
  | Node_data_dir -> `String "node_data_dir"
  | Client_base_dir -> `String "client_base_dir"
  | App_bin_dir -> `String "app_bin_dir"

let dir_type_of_yojson json =
  let open Yojson.Safe.Util in
  try
    match to_string json with
    | "node_data_dir" -> Ok Node_data_dir
    | "client_base_dir" -> Ok Client_base_dir
    | "app_bin_dir" -> Ok App_bin_dir
    | other -> Error (Printf.sprintf "Unknown dir_type: %s" other)
  with Type_error (msg, _) | Undefined (msg, _) -> Error msg

let directory_entry_to_yojson entry =
  `Assoc
    [
      ("path", `String entry.path);
      ("dir_type", dir_type_to_yojson entry.dir_type);
      ("created_at", `String entry.created_at);
      ("last_used_at", `String entry.last_used_at);
      ( "linked_services",
        `List (List.map (fun s -> `String s) entry.linked_services) );
    ]

let directory_entry_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let path = json |> member "path" |> to_string in
    let* dir_type =
      json |> member "dir_type" |> dir_type_of_yojson
      |> Result.map_error (fun msg -> `Msg msg)
    in
    let created_at = json |> member "created_at" |> to_string in
    (* Backwards compatibility: default last_used_at to created_at if missing *)
    let last_used_at =
      match json |> member "last_used_at" with
      | `Null -> created_at
      | j -> to_string j
    in
    let linked_services =
      json |> member "linked_services" |> to_list |> List.map to_string
    in
    Ok {path; dir_type; created_at; last_used_at; linked_services}
  with
  | Type_error (msg, _) -> Error (`Msg msg)
  | Undefined (msg, _) -> Error (`Msg msg)

(* File paths *)

let registry_root () =
  if Common.is_root () then "/etc/octez_manager"
  else Filename.concat (Common.xdg_config_home ()) "octez-manager"

let directories_file () = Filename.concat (registry_root ()) "directories.json"

let old_base_dirs_file () = Filename.concat (registry_root ()) "base_dirs.json"

let migrated_marker_file () =
  Filename.concat (registry_root ()) ".directories_migrated"

(* Timestamp *)

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

(* I/O operations *)

let write_all entries =
  let owner, group =
    if Common.is_root () then ("root", "root")
    else Common.current_user_group_names ()
  in
  let json =
    `List (List.map directory_entry_to_yojson entries)
    |> Yojson.Safe.pretty_to_string
  in
  let* () =
    Common.ensure_dir_path ~owner ~group ~mode:0o755 (registry_root ())
  in
  Common.write_file ~mode:0o644 ~owner ~group (directories_file ()) json

let read_all_internal () =
  let path = directories_file () in
  if not (Sys.file_exists path) then Ok []
  else
    try
      let json = Yojson.Safe.from_file path in
      match json with
      | `List entries ->
          let results = List.map directory_entry_of_yojson entries in
          (* Collect errors or return the list *)
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | Ok entry :: rest -> collect (entry :: acc) rest
            | Error msg :: _ -> Error msg
          in
          collect [] results
      | _ -> Error (`Msg "Invalid directories.json format")
    with
    | Sys_error msg -> Error (`Msg msg)
    | Yojson.Json_error msg -> Error (`Msg msg)

(* Migration from old base_dirs.json *)

let migrate_from_base_dir_registry () =
  let old_file = old_base_dirs_file () in
  let migrated_marker = migrated_marker_file () in

  (* Skip if already migrated *)
  if Sys.file_exists migrated_marker then Ok ()
  else if not (Sys.file_exists old_file) then Ok ()
  else
    try
      (* Read old format *)
      let json = Yojson.Safe.from_file old_file in
      match json with
      | `List entries ->
          (* Convert old entries to new format *)
          let open Yojson.Safe.Util in
          let old_entry_of_yojson json =
            try
              let path = json |> member "path" |> to_string in
              let created_at = json |> member "created_at" |> to_string in
              let linked_services =
                json |> member "linked_services" |> to_list
                |> List.map to_string
              in
              Ok
                {
                  path;
                  dir_type = Client_base_dir;
                  (* All old entries were client base dirs *)
                  created_at;
                  last_used_at = created_at;
                  linked_services;
                }
            with
            | Type_error (msg, _) -> Error (`Msg msg)
            | Undefined (msg, _) -> Error (`Msg msg)
          in

          let results = List.map old_entry_of_yojson entries in
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | Ok entry :: rest -> collect (entry :: acc) rest
            | Error msg :: _ -> Error msg
          in
          let* converted_entries = collect [] results in

          (* Write to new format *)
          let* () = write_all converted_entries in

          (* Rename old file to .migrated *)
          let* () =
            try
              Unix.rename old_file (old_file ^ ".migrated") ;
              Ok ()
            with Unix.Unix_error (e, _, _) ->
              Error
                (`Msg
                   (Printf.sprintf
                      "Failed to rename old file: %s"
                      (Unix.error_message e)))
          in

          (* Create migration marker *)
          let owner, group =
            if Common.is_root () then ("root", "root")
            else Common.current_user_group_names ()
          in
          Common.write_file ~mode:0o644 ~owner ~group migrated_marker (now ())
      | _ -> Error (`Msg "Invalid base_dirs.json format during migration")
    with
    | Sys_error msg -> Error (`Msg ("Migration error: " ^ msg))
    | Yojson.Json_error msg -> Error (`Msg ("Migration JSON error: " ^ msg))

(* Helper: sort entries by last_used_at descending (most recent first) *)
let sort_by_last_used entries =
  List.sort (fun a b -> String.compare b.last_used_at a.last_used_at) entries

(* Helper: limit entries per type to max_entries_per_type, keeping most recent *)
let limit_entries_per_type entries =
  let all_types = [Node_data_dir; Client_base_dir; App_bin_dir] in
  let limited =
    List.concat_map
      (fun dt ->
        entries |> List.filter (fun e -> e.dir_type = dt) |> sort_by_last_used
        |> fun lst ->
        let rec take n acc = function
          | [] -> List.rev acc
          | _ when n <= 0 -> List.rev acc
          | x :: xs -> take (n - 1) (x :: acc) xs
        in
        take max_entries_per_type [] lst)
      all_types
  in
  sort_by_last_used limited

(* Public API *)

let read_all () =
  (* Attempt migration first *)
  let _ = migrate_from_base_dir_registry () in
  let* entries = read_all_internal () in
  (* Return sorted by most recently used *)
  Ok (sort_by_last_used entries)

let add ~path ~dir_type ~linked_services =
  let* existing = read_all () in
  let timestamp = now () in
  (* Check if path already exists *)
  let existing_entry = List.find_opt (fun e -> e.path = path) existing in
  let filtered = List.filter (fun e -> e.path <> path) existing in
  let new_entry =
    match existing_entry with
    | Some e ->
        (* Update existing: keep created_at, update last_used_at *)
        {e with last_used_at = timestamp; linked_services; dir_type}
    | None ->
        (* New entry *)
        {
          path;
          dir_type;
          created_at = timestamp;
          last_used_at = timestamp;
          linked_services;
        }
  in
  (* Add new entry at front, then limit per type *)
  let all_entries = new_entry :: filtered in
  write_all (limit_entries_per_type all_entries)

let find_by_path path =
  let* all = read_all () in
  Ok (List.find_opt (fun e -> e.path = path) all)

let list ?dir_type () =
  let* all = read_all () in
  match dir_type with
  | None -> Ok all
  | Some dt -> Ok (List.filter (fun e -> e.dir_type = dt) all)

let remove path =
  let* existing = read_all () in
  let filtered = List.filter (fun e -> e.path <> path) existing in
  write_all filtered

let update_linked_services ~path ~linked_services =
  let* existing = read_all () in
  match List.find_opt (fun e -> e.path = path) existing with
  | None -> Ok () (* Path not found, nothing to update *)
  | Some entry ->
      let updated = {entry with linked_services; last_used_at = now ()} in
      let filtered = List.filter (fun e -> e.path <> path) existing in
      write_all (limit_entries_per_type (updated :: filtered))

let clear_all () = write_all []
