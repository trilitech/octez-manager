(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

let groups_dir () = Filename.concat (Paths.registry_root ()) "groups"

let is_safe_name name =
  String.length name > 0
  && String.length name <= 64
  && String.to_seq name
     |> Seq.for_all (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '-' || c = '_')

let group_path name =
  if not (is_safe_name name) then
    invalid_arg (Printf.sprintf "Group_registry: invalid group name %S" name) ;
  Filename.concat (groups_dir ()) (name ^ ".json")

let write group =
  let owner, group_name =
    if Paths.is_root () then ("root", "root")
    else Paths.current_user_group_names ()
  in
  let json = Group.to_yojson group |> Yojson.Safe.pretty_to_string in
  let* () =
    File_ops.ensure_dir_path
      ~owner
      ~group:group_name
      ~mode:0o755
      (groups_dir ())
  in
  File_ops.write_file
    ~mode:0o644
    ~owner
    ~group:group_name
    (group_path group.name)
    json

let read_one path =
  try
    let json = Yojson.Safe.from_file path in
    Group.of_yojson json
  with
  | Sys_error msg -> Error (`Msg msg)
  | Yojson.Json_error msg -> Error (`Msg msg)

let list () =
  let dir = groups_dir () in
  if not (Sys.file_exists dir) then Ok []
  else
    let files = Sys.readdir dir |> Array.to_list in
    let groups =
      files
      |> List.filter (fun f -> Filename.check_suffix f ".json")
      |> List.filter_map (fun f ->
          let path = Filename.concat dir f in
          match read_one path with
          | Ok grp -> Some grp
          | Error (`Msg msg) ->
              Printf.eprintf "Warning: skipping %s: %s\n%!" path msg ;
              None)
    in
    Ok groups

let find ~name =
  let path = group_path name in
  if not (Sys.file_exists path) then Ok None
  else match read_one path with Ok grp -> Ok (Some grp) | Error _ as e -> e

let remove ~name =
  let path = group_path name in
  if Sys.file_exists path then
    try
      Sys.remove path ;
      Ok ()
    with Sys_error msg -> Error (`Msg msg)
  else Ok ()
