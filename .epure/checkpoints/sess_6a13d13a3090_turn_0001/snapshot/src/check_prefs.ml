(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

type prefs = {check_enabled : bool; dismissed_versions : string list}

let default = {check_enabled = true; dismissed_versions = []}

let load ~file ?extra_of_json () =
  if not (Sys.file_exists file) then Ok (default, None)
  else
    try
      let json = Yojson.Safe.from_file file in
      let open Yojson.Safe.Util in
      let check_enabled =
        json |> member "check_enabled" |> to_bool_option
        |> Option.value ~default:true
      in
      let dismissed_versions =
        json |> member "dismissed_versions" |> to_list |> List.map to_string
      in
      let extra =
        match extra_of_json with
        | Some f -> ( try Some (f json) with _ -> None)
        | None -> None
      in
      Ok ({check_enabled; dismissed_versions}, extra)
    with e ->
      Cmd_runner.append_debug_log
        (Printf.sprintf
           "Failed to load prefs from %s: %s"
           file
           (Printexc.to_string e)) ;
      Ok (default, None)

let save ~file ?(extra_to_json = []) prefs =
  let dir = Filename.dirname file in
  let owner, group = Paths.current_user_group_names () in
  let* () = File_ops.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  try
    let json =
      `Assoc
        ([
           ("check_enabled", `Bool prefs.check_enabled);
           ( "dismissed_versions",
             `List (List.map (fun v -> `String v) prefs.dismissed_versions) );
         ]
        @ extra_to_json)
    in
    Yojson.Safe.to_file file json ;
    Ok ()
  with e ->
    R.error_msgf "Failed to save prefs to %s: %s" file (Printexc.to_string e)

let is_check_enabled ~file =
  match load ~file () with
  | Ok ({check_enabled; _}, _) -> check_enabled
  | Error _ -> true

let set_check_enabled ~file enabled =
  match load ~file () with
  | Error _ as e -> e
  | Ok (prefs, _) -> save ~file {prefs with check_enabled = enabled}

let dismiss_version ~file version =
  match load ~file () with
  | Error _ as e -> e
  | Ok (prefs, _) ->
      if List.mem version prefs.dismissed_versions then Ok ()
      else
        let dismissed_versions = version :: prefs.dismissed_versions in
        save ~file {prefs with dismissed_versions}

let is_version_dismissed ~file version =
  match load ~file () with
  | Ok ({dismissed_versions; _}, _) -> List.mem version dismissed_versions
  | Error _ -> false
