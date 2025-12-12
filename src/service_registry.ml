open Rresult

let ( let* ) = Result.bind

let registry_root () =
  if Common.is_root () then "/etc/octez_manager"
  else Filename.concat (Common.xdg_config_home ()) "octez-manager"

let services_dir () = Filename.concat (registry_root ()) "services"

let service_path instance role =
  Filename.concat (services_dir ()) (instance ^ "-" ^ role ^ ".json")

let write service =
  let owner, group =
    if Common.is_root () then ("root", "root")
    else Common.current_user_group_names ()
  in
  let json = Service.to_yojson service |> Yojson.Safe.pretty_to_string in
  let* () =
    Common.ensure_dir_path ~owner ~group ~mode:0o755 (services_dir ())
  in
  Common.write_file
    ~mode:0o644
    ~owner
    ~group
    (service_path service.instance service.role)
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
      |> List.map (fun f -> read_one (Filename.concat dir f))
    in
    List.fold_left
      (fun acc res ->
        match acc with
        | Error _ as e -> e
        | Ok lst -> (
            match res with Error _ as e -> e | Ok svc -> Ok (svc :: lst)))
      (Ok [])
      services

let find ~instance ~role =
  let path = service_path instance role in
  if not (Sys.file_exists path) then Ok None
  else match read_one path with Ok svc -> Ok (Some svc) | Error _ as e -> e

let remove ~instance ~role =
  let path = service_path instance role in
  if Sys.file_exists path then
    try
      Sys.remove path ;
      Ok ()
    with Sys_error msg -> Error (`Msg msg)
  else Ok ()
