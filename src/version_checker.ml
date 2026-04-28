(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

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
  let config_dir = Paths.xdg_config_home () in
  Filename.concat config_dir "version-check.json"

let is_check_enabled () = Check_prefs.is_check_enabled ~file:(prefs_file ())

let set_check_enabled enabled =
  Check_prefs.set_check_enabled ~file:(prefs_file ()) enabled

let dismiss_version version =
  Check_prefs.dismiss_version ~file:(prefs_file ()) version

(** Parse version string into components for comparison
    e.g., "24.1" -> [24; 1], "24.0-rc1" -> [24; 0] *)
let parse_version = Version_utils.parse_version

let compare_versions = Version_utils.compare_versions

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
  (* Load prefs once to avoid redundant disk I/O *)
  let prefs =
    match Check_prefs.load ~file:(prefs_file ()) () with
    | Ok (p, _) -> p
    | Error _ -> Check_prefs.default
  in
  if not prefs.check_enabled then CheckDisabled
  else
    match
      Binary_downloader.fetch_versions ~include_rc:(Prerelease_flag.get ()) ()
    with
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
          let should_notify = not (List.mem latest prefs.dismissed_versions) in
          UpdateAvailable
            {latest_version = latest; current_version = current; should_notify}

(** Exported for tests *)
module For_tests = struct
  let parse_version = parse_version
end
