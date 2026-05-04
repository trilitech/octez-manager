(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

let registry_root = Paths.registry_root

let services_dir () = Filename.concat (registry_root ()) "services"

let service_path instance =
  Filename.concat (services_dir ()) (instance ^ ".json")

let write service =
  let owner, group =
    if Paths.is_root () then ("root", "root")
    else Paths.current_user_group_names ()
  in
  let json = Service.to_yojson service |> Yojson.Safe.pretty_to_string in
  let* () =
    File_ops.ensure_dir_path ~owner ~group ~mode:0o755 (services_dir ())
  in
  File_ops.write_file
    ~mode:0o644
    ~owner
    ~group
    (service_path service.instance)
    json

let read_one path =
  try
    let json = Yojson.Safe.from_file path in
    Service.of_yojson json
  with
  | Sys_error msg -> Error (`Msg msg)
  | Yojson.Json_error msg -> Error (`Msg msg)

let list () =
  let dir = services_dir () in
  if not (Sys.file_exists dir) then Ok []
  else
    let files = Sys.readdir dir |> Array.to_list in
    let services =
      files
      |> List.filter (fun f -> Filename.check_suffix f ".json")
      |> List.filter_map (fun f ->
          let path = Filename.concat dir f in
          match read_one path with
          | Ok svc -> Some svc
          | Error (`Msg msg) ->
              (* File was deleted between readdir and read_one, or is
                 temporarily corrupt during a concurrent write — skip it
                 with a warning rather than failing the entire listing. *)
              Printf.eprintf
                "Warning: skipping %s: %s\n%!"
                (Filename.concat dir f)
                msg ;
              None)
    in
    Ok services

let find ~instance =
  let path = service_path instance in
  if not (Sys.file_exists path) then Ok None
  else match read_one path with Ok svc -> Ok (Some svc) | Error _ as e -> e

let remove ~instance =
  let path = service_path instance in
  if Sys.file_exists path then
    try
      Sys.remove path ;
      Ok ()
    with Sys_error msg -> Error (`Msg msg)
  else Ok ()

let count_instances_using bin_source =
  match list () with
  | Error _ -> 0
  | Ok services ->
      List.filter (fun svc -> Service.get_bin_source svc = bin_source) services
      |> List.length

let get_instances_using bin_source =
  match list () with
  | Error _ -> []
  | Ok services ->
      List.filter_map
        (fun svc ->
          if Service.get_bin_source svc = bin_source then
            Some svc.Service.instance
          else None)
        services
