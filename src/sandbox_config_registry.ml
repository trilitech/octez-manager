(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

let sandbox_configs_dir () =
  Filename.concat (Paths.registry_root ()) "sandbox-configs"

let is_safe_name name =
  String.length name > 0
  && String.length name <= 64
  && String.to_seq name
     |> Seq.for_all (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '-' || c = '_')

let config_path name =
  if not (is_safe_name name) then
    invalid_arg (Printf.sprintf "Sandbox_config_registry: invalid name %S" name) ;
  Filename.concat (sandbox_configs_dir ()) (name ^ ".json")

let write cfg =
  let owner, group_name =
    if Paths.is_root () then ("root", "root")
    else Paths.current_user_group_names ()
  in
  let json = Sandbox_config.to_yojson cfg |> Yojson.Safe.pretty_to_string in
  let* () =
    File_ops.ensure_dir_path
      ~owner
      ~group:group_name
      ~mode:0o755
      (sandbox_configs_dir ())
  in
  File_ops.write_file
    ~mode:0o644
    ~owner
    ~group:group_name
    (config_path cfg.group_name)
    json

let read_one path =
  try
    let json = Yojson.Safe.from_file path in
    Sandbox_config.of_yojson json
  with
  | Sys_error msg -> Error (`Msg msg)
  | Yojson.Json_error msg -> Error (`Msg msg)

let find ~name =
  let path = config_path name in
  if not (Sys.file_exists path) then Ok None
  else match read_one path with Ok cfg -> Ok (Some cfg) | Error _ as e -> e

let remove ~name =
  let path = config_path name in
  if Sys.file_exists path then
    try
      Sys.remove path ;
      Ok ()
    with Sys_error msg -> Error (`Msg msg)
  else Ok ()

(* Try to build a Sandbox_config from legacy fields in the group JSON file.
   Pre-refactor group files included "network" and "bin_source" fields that
   are no longer part of Group.t.  We read the raw JSON here so that
   existing sandbox groups continue to work after the first upgrade. *)
let migrate_from_group ~name =
  let groups_dir = Filename.concat (Paths.registry_root ()) "groups" in
  let group_path = Filename.concat groups_dir (name ^ ".json") in
  if not (Sys.file_exists group_path) then Ok None
  else
    try
      let json = Yojson.Safe.from_file group_path in
      let open Yojson.Safe.Util in
      let network_json = json |> member "network" in
      let bin_source_json = json |> member "bin_source" in
      (match (network_json, bin_source_json) with
      | `Null, _ | _, `Null -> Ok None
      | network_j, bs_j -> (
          let network = network_j |> to_string in
          match Binary_registry.bin_source_of_yojson bs_j with
          | Error _ -> Ok None
          | Ok bin_source ->
              let cfg = Sandbox_config.make ~group_name:name ~network ~bin_source () in
              (* Persist so future calls don't need to migrate again *)
              let* () = write cfg in
              Ok (Some cfg)))
    with
    | Sys_error _ | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ ->
        Ok None

let find_or_migrate ~name =
  let* found = find ~name in
  match found with Some _ as r -> Ok r | None -> migrate_from_group ~name
