(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

type t = {
  app_bin_dir : string option;
  default_history_mode : History_mode.t option;
  default_logging_mode : Logging_mode.t option;
}

let default =
  {app_bin_dir = None; default_history_mode = None; default_logging_mode = None}

let settings_path () =
  Filename.concat (Common.xdg_config_home ()) "octez-manager/settings.json"

let to_yojson t =
  `Assoc
    [
      ( "app_bin_dir",
        match t.app_bin_dir with Some s -> `String s | None -> `Null );
      ( "default_history_mode",
        match t.default_history_mode with
        | Some h -> `String (History_mode.to_string h)
        | None -> `Null );
      ( "default_logging_mode",
        match t.default_logging_mode with
        | Some l -> Logging_mode.to_yojson l
        | None -> `Null );
    ]

let of_yojson json =
  let open Yojson.Safe.Util in
  try
    let app_bin_dir =
      match member "app_bin_dir" json with
      | `String s -> Some s
      | `Null -> None
      | _ -> None
    in
    let default_history_mode =
      match member "default_history_mode" json with
      | `String s -> Result.to_option (History_mode.of_string s)
      | `Null -> None
      | _ -> None
    in
    let default_logging_mode =
      match member "default_logging_mode" json with
      | `Null -> None
      | json -> Result.to_option (Logging_mode.of_yojson json)
    in
    Ok {app_bin_dir; default_history_mode; default_logging_mode}
  with Type_error (msg, _) -> R.error_msg msg

module For_tests = struct
  let default = default

  let to_yojson = to_yojson

  let of_yojson = of_yojson
end

let load () =
  let path = settings_path () in
  if Sys.file_exists path then
    try
      let json = Yojson.Safe.from_file path in
      of_yojson json
    with exn ->
      R.error_msgf "Failed to load settings: %s" (Printexc.to_string exn)
  else Ok default

let save t =
  let path = settings_path () in
  let dir = Filename.dirname path in
  let* () =
    let owner, group = Common.current_user_group_names () in
    Common.ensure_dir_path ~owner ~group ~mode:0o755 dir
  in
  try
    let json = to_yojson t in
    Yojson.Safe.to_file path json ;
    Ok ()
  with exn ->
    R.error_msgf "Failed to save settings: %s" (Printexc.to_string exn)
