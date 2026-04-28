(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** octez-index binary downloader from GitLab releases.

    Releases live at https://gitlab.com/tezos/octez-index. Each release
    provides a single pre-built binary per architecture as a direct asset
    (no tarball). No checksums file is published, so verification is skipped. *)

open Rresult

let ( let* ) = Result.bind

(** Constants *)

let gitlab_api_url =
  "https://gitlab.com/api/v4/projects/tezos%2Foctez-index/releases"

let gitlab_artifacts_base =
  "https://gitlab.com/tezos/octez-index/-/jobs/artifacts"

(** Types *)

type arch = X86_64 | Arm64

type version_info = {
  version : string;
  release_date : string option;
  is_prerelease : bool;
}

type progress_callback = downloaded:int64 -> total:int64 option -> unit

type checksum_status = Verified | Skipped | Failed of string

type download_result = {
  version : string;
  installed_path : string;
  checksum_status : checksum_status;
}

(** Architecture detection *)

let detect_arch () =
  match Cmd_runner.run_out ["uname"; "-m"] with
  | Ok output -> (
      let machine = String.trim output in
      match machine with
      | "x86_64" | "amd64" -> Ok X86_64
      | "aarch64" | "arm64" -> Ok Arm64
      | _ ->
          R.error_msgf
            "Unsupported architecture: %s (octez-index supports x86_64 or \
             arm64)"
            machine)
  | Error _ as e -> e

let arch_to_string = function X86_64 -> "amd64" | Arm64 -> "arm64"

(** URL construction *)

let binary_url ~version ~arch =
  Printf.sprintf
    "%s/v%s/raw/octez-index?job=build-release-binary-%s"
    gitlab_artifacts_base
    version
    (arch_to_string arch)

(** Version fetching from GitLab API *)

let is_prerelease_tag tag =
  let lower = String.lowercase_ascii tag in
  let contains sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) lower 0 in
      true
    with Not_found -> false
  in
  contains "rc" || contains "beta" || contains "alpha" || contains "dev"

let parse_release_json json =
  try
    let open Yojson.Safe.Util in
    let releases = to_list json in
    let parse_release r =
      let tag_name = member "tag_name" r |> to_string in
      let version =
        if String.length tag_name > 0 && tag_name.[0] = 'v' then
          String.sub tag_name 1 (String.length tag_name - 1)
        else tag_name
      in
      let released_at = member "released_at" r |> to_string_option in
      let release_date =
        Option.map
          (fun iso_str ->
            if String.length iso_str >= 10 then String.sub iso_str 0 10
            else iso_str)
          released_at
      in
      let is_prerelease = is_prerelease_tag tag_name in
      {version; release_date; is_prerelease}
    in
    Ok (List.map parse_release releases)
  with exn ->
    R.error_msgf
      "Failed to parse GitLab releases JSON: %s"
      (Printexc.to_string exn)

let fetch_releases_json () =
  match
    Cmd_runner.run_out_silent
      [
        "curl";
        "-fsL";
        "--max-time";
        "10";
        "--connect-timeout";
        "5";
        gitlab_api_url;
      ]
  with
  | Ok body when String.trim body <> "" -> Ok body
  | Ok _ -> R.error_msg "Empty response from GitLab API"
  | Error _ as e -> e

let filter_versions ~include_prerelease versions =
  if include_prerelease then versions
  else List.filter (fun v -> not v.is_prerelease) versions

let fetch_versions ?(include_prerelease = false) () =
  let* json_str = fetch_releases_json () in
  let json = Yojson.Safe.from_string json_str in
  let* versions = parse_release_json json in
  Ok (filter_versions ~include_prerelease versions)

(** Installation path management *)

let octez_index_binaries_dir () =
  Filename.concat (Paths.xdg_data_home ()) "octez-manager/octez-index-binaries"

let octez_index_version_path version =
  Filename.concat (octez_index_binaries_dir ()) ("v" ^ version)

let is_complete_installation version =
  let dest_dir = octez_index_version_path version in
  if not (Sys.file_exists dest_dir && Sys.is_directory dest_dir) then false
  else
    let binary = Filename.concat dest_dir "octez-index" in
    let metadata = Filename.concat dest_dir ".metadata.json" in
    Sys.file_exists binary && Sys.file_exists metadata

let list_managed_versions () =
  let dir = octez_index_binaries_dir () in
  if Sys.file_exists dir && Sys.is_directory dir then
    try
      let entries = Sys.readdir dir |> Array.to_list in
      let versions =
        entries
        |> List.filter (fun e ->
            String.length e > 1
            && e.[0] = 'v'
            && Sys.is_directory (Filename.concat dir e))
        |> List.map (fun e -> String.sub e 1 (String.length e - 1))
        |> List.filter is_complete_installation
        |> List.sort (fun a b -> Version_utils.compare_versions b a)
      in
      Ok versions
    with exn ->
      R.error_msgf
        "Failed to list octez-index versions: %s"
        (Printexc.to_string exn)
  else Ok []

(** Temporary directory for atomic installation *)

let temp_version_dir version =
  let pid = Unix.getpid () in
  Filename.concat
    (octez_index_binaries_dir ())
    (Printf.sprintf ".tmp.v%s.%d" version pid)

let cleanup_stale_temp_dirs ?(max_age_seconds = 3600) () =
  let dir = octez_index_binaries_dir () in
  if Sys.file_exists dir && Sys.is_directory dir then
    try
      let now = Unix.time () in
      let entries = Sys.readdir dir |> Array.to_list in
      let temp_dirs =
        entries
        |> List.filter (fun e ->
            String.length e > 5
            && String.sub e 0 5 = ".tmp."
            && Sys.is_directory (Filename.concat dir e))
      in
      List.iter
        (fun temp_dir ->
          let full_path = Filename.concat dir temp_dir in
          try
            let stat = Unix.stat full_path in
            let age = now -. stat.Unix.st_mtime in
            if age > float_of_int max_age_seconds then
              ignore (Cmd_runner.run_out ["rm"; "-rf"; full_path])
          with _ -> ())
        temp_dirs
    with _ -> ()

(** ISO8601 timestamp *)

let iso8601_now () =
  let open Unix in
  let tm = gmtime (time ()) in
  Printf.sprintf
    "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

(** Main download function *)

let download_version ~version ?progress () =
  let* arch = detect_arch () in
  let final_dir = octez_index_version_path version in
  let temp_dir = temp_version_dir version in

  let binaries_dir = octez_index_binaries_dir () in
  let owner, group = Paths.current_user_group_names () in
  let* () = File_ops.ensure_dir_path ~owner ~group ~mode:0o755 binaries_dir in

  let* () =
    if Sys.file_exists final_dir then
      if is_complete_installation version then
        R.error_msgf "octez-index v%s is already installed" version
      else (
        (try ignore (Cmd_runner.run_out ["rm"; "-rf"; final_dir]) with _ -> ()) ;
        Ok ())
    else Ok ()
  in

  cleanup_stale_temp_dirs () ;

  (try
     if Sys.file_exists temp_dir then
       ignore (Cmd_runner.run_out ["rm"; "-rf"; temp_dir])
   with _ -> ()) ;

  let* () = File_ops.ensure_dir_path ~owner ~group ~mode:0o755 temp_dir in

  (* Download binary directly *)
  let url = binary_url ~version ~arch in
  let binary_dest = Filename.concat temp_dir "octez-index" in
  let* () =
    match progress with
    | Some callback ->
        let on_progress current total =
          let downloaded = Int64.of_int current in
          let total_opt = Option.map Int64.of_int total in
          callback ~downloaded ~total:total_opt
        in
        Download.download_file_with_progress
          ~url
          ~dest_path:binary_dest
          ~on_progress
    | None ->
        Cmd_runner.run
          [
            "curl";
            "-fsSL";
            "--max-time";
            "300";
            "--connect-timeout";
            "10";
            "-o";
            binary_dest;
            url;
          ]
  in

  let* () = Cmd_runner.run ["chmod"; "+x"; binary_dest] in

  (* No checksums file published — skip verification *)
  let checksum_status = Skipped in

  let metadata =
    `Assoc
      [
        ("version", `String version);
        ("architecture", `String (arch_to_string arch));
        ("download_date", `String (iso8601_now ()));
        ("checksum_status", `String "skipped");
      ]
  in
  let metadata_file = Filename.concat temp_dir ".metadata.json" in
  (try Yojson.Safe.to_file metadata_file metadata with _ -> ()) ;

  let* () =
    match Cmd_runner.run ["mv"; temp_dir; final_dir] with
    | Ok () -> Ok ()
    | Error _ as e ->
        (try ignore (Cmd_runner.run_out ["rm"; "-rf"; temp_dir]) with _ -> ()) ;
        e
  in

  Ok {version; installed_path = final_dir; checksum_status}

(** Remove version *)

let remove_version version =
  let dest_dir = octez_index_version_path version in
  if not (Sys.file_exists dest_dir) then
    R.error_msgf "octez-index v%s is not installed" version
  else
    match Cmd_runner.run ["rm"; "-rf"; dest_dir] with
    | Ok () -> Ok ()
    | Error _ as e -> e

(** Calculate directory size *)

let calculate_directory_size path =
  if not (Sys.file_exists path) then Ok 0L
  else
    try
      match Cmd_runner.run_out ["du"; "-sb"; path] with
      | Ok out -> (
          try
            let size_str = List.hd (String.split_on_char '\t' out) in
            Ok (Int64.of_string (String.trim size_str))
          with _ -> R.error_msgf "Failed to parse du output: %s" out)
      | Error _ as e -> e
    with e ->
      R.error_msgf
        "Failed to calculate directory size: %s"
        (Printexc.to_string e)

let format_size_bytes bytes =
  let open Int64 in
  if bytes < 1024L then Printf.sprintf "%Ld B" bytes
  else if bytes < mul 1024L 1024L then
    Printf.sprintf "%.1f KB" (to_float bytes /. 1024.0)
  else if bytes < mul (mul 1024L 1024L) 1024L then
    Printf.sprintf "%.1f MB" (to_float bytes /. 1024.0 /. 1024.0)
  else Printf.sprintf "%.1f GB" (to_float bytes /. 1024.0 /. 1024.0 /. 1024.0)

let get_version_size version =
  let path = octez_index_version_path version in
  match calculate_directory_size path with
  | Ok bytes -> Ok (bytes, format_size_bytes bytes)
  | Error _ as e -> e

(** For tests *)

module For_tests = struct
  let parse_release_json = parse_release_json

  let detect_arch = detect_arch

  let arch_to_string = arch_to_string

  let binary_url = binary_url

  let filter_versions = filter_versions
end
