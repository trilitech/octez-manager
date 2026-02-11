(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

(** Current version - kept in sync with main.ml *)
let current_version = "0.3.0"

(** GitHub repository info *)
let github_owner = "trilitech"

let github_repo = "octez-manager"

let releases_api_url =
  Printf.sprintf
    "https://api.github.com/repos/%s/%s/releases/latest"
    github_owner
    github_repo

let release_download_url ~version =
  Printf.sprintf
    "https://github.com/%s/%s/releases/download/v%s/octez-manager-v%s-linux-x86_64"
    github_owner
    github_repo
    version
    version

let checksums_url ~version =
  Printf.sprintf
    "https://github.com/%s/%s/releases/download/v%s/sha256sums.txt"
    github_owner
    github_repo
    version

let release_notes_url ~version =
  Printf.sprintf
    "https://github.com/%s/%s/releases/tag/v%s"
    github_owner
    github_repo
    version

(** {1 Installation method detection} *)

type install_method = Deb_package | Binary_install | Manual_install

let detect_install_method () =
  (* Check if installed via dpkg *)
  match Cmd_runner.run_out_silent ["dpkg"; "-s"; "octez-manager"] with
  | Ok _ -> Deb_package
  | Error _ ->
      (* Check if binary is in standard install location *)
      let exe_path =
        try Sys.executable_name with _ -> "/usr/local/bin/octez-manager"
      in
      if
        String.length exe_path >= 15
        && String.sub exe_path 0 15 = "/usr/local/bin/"
      then Binary_install
      else Manual_install

(** {1 Version comparison} *)

let parse_version = Version_utils.parse_version

let is_rc = Version_utils.is_rc

let extract_rc_number = Version_utils.extract_rc_number

let compare_versions = Version_utils.compare_versions

let is_major_update ~current ~latest =
  let current_parts = parse_version current in
  let latest_parts = parse_version latest in
  match (current_parts, latest_parts) with
  | c_major :: _, l_major :: _ -> l_major > c_major
  | _ -> false

(** {1 Preferences}

    Preferences are stored in the shared {!Check_prefs} format. *)

let prefs_file () =
  Filename.concat (Paths.xdg_config_home ()) "octez-manager/self-update.json"

let is_check_enabled () = Check_prefs.is_check_enabled ~file:(prefs_file ())

let set_check_enabled enabled =
  Check_prefs.set_check_enabled ~file:(prefs_file ()) enabled

let dismiss_version version =
  Check_prefs.dismiss_version ~file:(prefs_file ()) version

let is_version_dismissed version =
  Check_prefs.is_version_dismissed ~file:(prefs_file ()) version

(** {1 Version checking} *)

type update_info = {
  latest_version : string;
  current_version : string;
  is_major_update : bool;
  download_url : string;
  checksums_url : string;
  release_notes_url : string;
}

type check_result =
  | Update_available of update_info
  | Up_to_date
  | Check_disabled
  | Check_failed of string

(** Cache for version check results *)
let check_cache : (check_result * float) option ref = ref None

let cache_ttl = 600.0 (* 10 minutes *)

let fetch_latest_version () =
  match
    Cmd_runner.run_out_silent
      [
        "curl";
        "-fsSL";
        "--max-time";
        "10";
        "--connect-timeout";
        "5";
        "-H";
        "Accept: application/vnd.github.v3+json";
        releases_api_url;
      ]
  with
  | Ok body when String.trim body <> "" -> (
      try
        let json = Yojson.Safe.from_string body in
        let open Yojson.Safe.Util in
        let tag_name = json |> member "tag_name" |> to_string in
        (* Strip leading 'v' if present *)
        let version =
          if String.length tag_name > 0 && tag_name.[0] = 'v' then
            String.sub tag_name 1 (String.length tag_name - 1)
          else tag_name
        in
        Ok version
      with e ->
        R.error_msgf "Failed to parse release info: %s" (Printexc.to_string e))
  | Ok _ -> R.error_msg "Empty response from GitHub API"
  | Error (`Msg e) -> R.error_msgf "Failed to fetch release info: %s" e

let check_for_updates ?(force = false) () =
  (* Check if enabled *)
  if not (is_check_enabled ()) then Check_disabled
  else
    (* Check cache *)
    let now = Unix.gettimeofday () in
    match !check_cache with
    | Some (result, cached_at) when (not force) && now -. cached_at < cache_ttl
      ->
        result
    | _ -> (
        match fetch_latest_version () with
        | Error (`Msg e) ->
            let result = Check_failed e in
            check_cache := Some (result, now) ;
            result
        | Ok latest ->
            let result =
              if compare_versions latest current_version > 0 then
                Update_available
                  {
                    latest_version = latest;
                    current_version;
                    is_major_update =
                      is_major_update ~current:current_version ~latest;
                    download_url = release_download_url ~version:latest;
                    checksums_url = checksums_url ~version:latest;
                    release_notes_url = release_notes_url ~version:latest;
                  }
              else Up_to_date
            in
            check_cache := Some (result, now) ;
            result)

(** {1 Update operations} *)

type upgrade_result =
  | Upgrade_success of {new_version : string; needs_restart : bool}
  | Upgrade_needs_elevation of string
  | Upgrade_failed of string

let fetch_checksum ~version =
  let url = checksums_url ~version in
  match
    Cmd_runner.run_out_silent
      ["curl"; "-fsSL"; "--max-time"; "10"; "--connect-timeout"; "5"; url]
  with
  | Ok body when String.trim body <> "" ->
      (* Parse sha256sums.txt format: "hash  filename" *)
      let lines = String.split_on_char '\n' body in
      let binary_name =
        Printf.sprintf "octez-manager-v%s-linux-x86_64" version
      in
      let rec find_checksum = function
        | [] -> R.error_msgf "Checksum not found for %s" binary_name
        | line :: rest -> (
            match
              String.split_on_char ' ' line |> List.filter (fun s -> s <> "")
            with
            | hash :: filename :: _ when filename = binary_name -> Ok hash
            | _ -> find_checksum rest)
      in
      find_checksum lines
  | Ok _ -> R.error_msg "Empty checksums file"
  | Error (`Msg e) -> R.error_msgf "Failed to fetch checksums: %s" e

let verify_checksum ~filepath ~expected =
  match Download.compute_sha256 filepath with
  | Ok actual ->
      if String.equal actual expected then Ok ()
      else R.error_msgf "Checksum mismatch: expected %s, got %s" expected actual
  | Error (`Msg e) -> R.error_msgf "Failed to compute checksum: %s" e

let download_binary ~version ~dest ?on_progress () =
  let url = release_download_url ~version in
  match on_progress with
  | Some callback ->
      let on_progress current total =
        let downloaded = Int64.of_int current in
        let total_opt = Option.map Int64.of_int total in
        callback ~downloaded ~total:total_opt
      in
      Download.download_file_with_progress ~url ~dest_path:dest ~on_progress
  | None ->
      Cmd_runner.run_silent
        [
          "curl";
          "-fsSL";
          "--max-time";
          "300";
          "--connect-timeout";
          "10";
          "-o";
          dest;
          url;
        ]

let try_replace_binary ~src ~dest =
  try
    (* Try direct move *)
    Sys.rename src dest ;
    Ok `Direct
  with Sys_error _ -> (
    (* Try with pkexec *)
    match Cmd_runner.run_silent ["pkexec"; "mv"; src; dest] with
    | Ok () -> Ok `Pkexec
    | Error _ -> (
        (* Try with sudo *)
        match Cmd_runner.run_silent ["sudo"; "mv"; src; dest] with
        | Ok () -> Ok `Sudo
        | Error _ -> Error `Permission_denied))

let perform_upgrade ~version ?on_progress () =
  let exe_path =
    try Sys.executable_name with _ -> "/usr/local/bin/octez-manager"
  in
  let tmp_path = Printf.sprintf "/tmp/octez-manager-v%s" version in
  let backup_path = exe_path ^ ".bak" in

  (* Download new binary *)
  match download_binary ~version ~dest:tmp_path ?on_progress () with
  | Error (`Msg e) -> Upgrade_failed (Printf.sprintf "Download failed: %s" e)
  | Ok () -> (
      (* Fetch and verify checksum *)
      match fetch_checksum ~version with
      | Error (`Msg e) ->
          (try Sys.remove tmp_path with _ -> ()) ;
          Upgrade_failed (Printf.sprintf "Checksum fetch failed: %s" e)
      | Ok expected_checksum -> (
          match
            verify_checksum ~filepath:tmp_path ~expected:expected_checksum
          with
          | Error (`Msg e) ->
              (try Sys.remove tmp_path with _ -> ()) ;
              Upgrade_failed
                (Printf.sprintf "Checksum verification failed: %s" e)
          | Ok () -> (
              (* Make executable *)
              (try Unix.chmod tmp_path 0o755 with _ -> ()) ;

              (* Backup current binary *)
              (try Sys.rename exe_path backup_path with _ -> ()) ;

              (* Try to replace binary *)
              match try_replace_binary ~src:tmp_path ~dest:exe_path with
              | Ok `Direct ->
                  Upgrade_success {new_version = version; needs_restart = false}
              | Ok (`Pkexec | `Sudo) ->
                  Upgrade_success {new_version = version; needs_restart = true}
              | Error `Permission_denied ->
                  (* Restore backup if it exists *)
                  (try Sys.rename backup_path exe_path with _ -> ()) ;
                  (try Sys.remove tmp_path with _ -> ()) ;
                  Upgrade_needs_elevation
                    (Printf.sprintf "sudo mv %s %s" tmp_path exe_path))))

module For_tests = struct
  let parse_version = parse_version

  let is_rc = is_rc

  let extract_rc_number = extract_rc_number
end

let exec_restart () =
  let exe_path =
    try Sys.executable_name with _ -> "/usr/local/bin/octez-manager"
  in
  (* Get original command line arguments *)
  let args = Array.to_list Sys.argv in
  try Unix.execv exe_path (Array.of_list args)
  with Unix.Unix_error (err, _, _) ->
    Cmd_runner.append_debug_log
      (Printf.sprintf "exec_restart failed: %s" (Unix.error_message err))
