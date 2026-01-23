(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

type check_result =
  | UpdateAvailable of {
      latest_version : string;
      current_version : string option;
      should_notify : bool;
    }
  | UpToDate of string option
  | CheckDisabled
  | CheckFailed of string

(** Settings file for version checker preferences *)
let prefs_file () =
  let config_dir = Common.xdg_config_home () in
  Filename.concat config_dir "version-check.json"

type prefs = {check_enabled : bool; dismissed_versions : string list}

let default_prefs = {check_enabled = true; dismissed_versions = []}

let load_prefs () =
  let file = prefs_file () in
  if not (Sys.file_exists file) then Ok default_prefs
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
      Ok {check_enabled; dismissed_versions}
    with e ->
      Common.append_debug_log
        (Printf.sprintf
           "Failed to load version check prefs: %s"
           (Printexc.to_string e)) ;
      Ok default_prefs

let save_prefs prefs =
  let file = prefs_file () in
  let dir = Filename.dirname file in
  let owner, group = Common.current_user_group_names () in
  let* () = Common.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  try
    let json =
      `Assoc
        [
          ("check_enabled", `Bool prefs.check_enabled);
          ( "dismissed_versions",
            `List (List.map (fun v -> `String v) prefs.dismissed_versions) );
        ]
    in
    Yojson.Safe.to_file file json ;
    Ok ()
  with e ->
    Error
      (`Msg (Printf.sprintf "Failed to save prefs: %s" (Printexc.to_string e)))

let is_check_enabled () =
  match load_prefs () with Ok prefs -> prefs.check_enabled | Error _ -> true

let set_check_enabled enabled =
  let* prefs = load_prefs () in
  save_prefs {prefs with check_enabled = enabled}

let dismiss_version version =
  let* prefs = load_prefs () in
  if List.mem version prefs.dismissed_versions then Ok ()
  else
    let dismissed_versions = version :: prefs.dismissed_versions in
    save_prefs {prefs with dismissed_versions}

(** Parse version string into components for comparison
    e.g., "24.1" -> [24; 1], "24.0-rc1" -> [24; 0] *)
let parse_version s =
  (* Strip leading 'v' if present *)
  let s =
    if String.length s > 0 && s.[0] = 'v' then
      String.sub s 1 (String.length s - 1)
    else s
  in
  let base =
    if String.contains s '-' then List.hd (String.split_on_char '-' s) else s
  in
  try
    String.split_on_char '.' base
    |> List.map (fun part -> int_of_string (String.trim part))
  with _ -> []

let is_rc version = String.contains version '-'

let extract_rc_number version =
  if not (is_rc version) then None
  else
    try
      let parts = String.split_on_char '-' version in
      match parts with
      | [_; rc_part] ->
          let rc_str = String.sub rc_part 2 (String.length rc_part - 2) in
          Some (int_of_string rc_str)
      | _ -> None
    with _ -> None

let compare_versions v1 v2 =
  let parts1 = parse_version v1 in
  let parts2 = parse_version v2 in
  let rec compare_parts l1 l2 =
    match (l1, l2) with
    | [], [] -> 0
    | [], 0 :: t2 -> compare_parts [] t2 (* Treat missing as 0 *)
    | 0 :: t1, [] -> compare_parts t1 [] (* Treat missing as 0 *)
    | [], _ -> -1
    | _, [] -> 1
    | h1 :: t1, h2 :: t2 ->
        if h1 < h2 then -1 else if h1 > h2 then 1 else compare_parts t1 t2
  in
  let base_cmp = compare_parts parts1 parts2 in
  if base_cmp <> 0 then base_cmp
  else
    (* Same base version, check RC status *)
    match (is_rc v1, is_rc v2) with
    | false, false -> 0 (* Both release *)
    | true, false -> -1 (* v1 is RC, v2 is release *)
    | false, true -> 1 (* v1 is release, v2 is RC *)
    | true, true -> (
        (* Both RC, compare RC numbers *)
        match (extract_rc_number v1, extract_rc_number v2) with
        | Some rc1, Some rc2 -> compare rc1 rc2
        | Some _, None -> 1
        | None, Some _ -> -1
        | None, None -> 0)

let get_current_version () =
  match Binary_registry.list_managed_versions () with
  | Error _ -> None
  | Ok [] -> None
  | Ok (first :: rest) ->
      (* Find highest version *)
      Some
        (List.fold_left
           (fun acc v -> if compare_versions v acc > 0 then v else acc)
           first
           rest)

let check_for_updates ?(force = false) () =
  let _ = force in
  (* force parameter kept for API compatibility but no longer used *)
  (* Check if enabled *)
  match load_prefs () with
  | Error _ -> CheckFailed "Failed to load preferences"
  | Ok prefs when not prefs.check_enabled -> CheckDisabled
  | Ok prefs -> (
      (* Get latest remote version *)
      match Binary_downloader.fetch_versions ~include_rc:false () with
      | Error (`Msg e) -> CheckFailed e
      | Ok [] -> CheckFailed "No versions available"
      | Ok (first :: rest) ->
          (* Get latest version (should already be sorted, but ensure) *)
          let latest =
            List.fold_left
              (fun acc (vi : Binary_downloader.version_info) ->
                if compare_versions vi.version acc > 0 then vi.version else acc)
              first.version
              rest
          in
          let current = get_current_version () in
          (* Check if update is needed *)
          let needs_update =
            match current with
            | None -> true (* No version installed *)
            | Some cur -> compare_versions latest cur > 0
          in
          if not needs_update then UpToDate current
          else
            (* Check if user dismissed this version *)
            let should_notify =
              not (List.mem latest prefs.dismissed_versions)
            in
            UpdateAvailable
              {
                latest_version = latest;
                current_version = current;
                should_notify;
              })

(** Exported for tests *)
module For_tests = struct
  let parse_version = parse_version
end
