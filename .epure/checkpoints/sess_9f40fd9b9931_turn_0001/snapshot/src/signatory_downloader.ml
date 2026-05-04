(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Signatory binary downloader from GitHub releases.
    
    This module handles downloading Signatory binaries from the ECAD Labs
    GitHub repository. Unlike Octez binaries (which use a custom distribution
    site), Signatory follows standard GitHub releases with tarballs. *)

open Rresult

let ( let* ) = Result.bind

(** Constants *)

let github_api_url = "https://api.github.com/repos/ecadlabs/signatory/releases"

let github_releases_base = "https://github.com/ecadlabs/signatory/releases"

(** Types *)

type arch = X86_64 | Arm64

type version_info = {
  version : string;  (** Version string without 'v' prefix (e.g., "1.3.1") *)
  release_date : string option;  (** ISO date format *)
  is_prerelease : bool;  (** RC, beta, alpha releases *)
}

type progress_callback = downloaded:int64 -> total:int64 option -> unit

type checksum_status = Verified | Skipped | Failed of string

type download_result = {
  version : string;
  installed_path : string;
  checksum_status : checksum_status;
}

(** Architecture detection - reuse from Binary_downloader *)

let detect_arch () =
  match Cmd_runner.run_out ["uname"; "-m"] with
  | Ok output -> (
      let machine = String.trim output in
      match machine with
      | "x86_64" | "amd64" -> Ok X86_64
      | "aarch64" | "arm64" -> Ok Arm64
      | _ ->
          R.error_msgf
            "Unsupported architecture: %s (Signatory supports x86_64 or arm64)"
            machine)
  | Error _ as e -> e

let arch_to_string = function X86_64 -> "amd64" | Arm64 -> "arm64"

(** URL construction *)

let tarball_url ~version ~arch =
  let os = "linux" in
  let arch_str = arch_to_string arch in
  Printf.sprintf
    "%s/download/v%s/signatory_%s_%s_%s.tar.gz"
    github_releases_base
    version
    version
    os
    arch_str

let checksums_url ~version =
  Printf.sprintf "%s/download/v%s/checksums.txt" github_releases_base version

(** Version fetching from GitHub API *)

let parse_release_json json =
  try
    let open Yojson.Safe.Util in
    let releases = to_list json in
    let parse_release r =
      let tag_name = member "tag_name" r |> to_string in
      (* Remove 'v' prefix if present *)
      let version =
        if String.length tag_name > 0 && tag_name.[0] = 'v' then
          String.sub tag_name 1 (String.length tag_name - 1)
        else tag_name
      in
      let published_at = member "published_at" r |> to_string_option in
      let release_date =
        Option.map
          (fun iso_str ->
            (* Keep first 10 chars: "2026-01-20" from "2026-01-20T20:10:16Z" *)
            if String.length iso_str >= 10 then String.sub iso_str 0 10
            else iso_str)
          published_at
      in
      let is_prerelease = member "prerelease" r |> to_bool in
      {version; release_date; is_prerelease}
    in
    Ok (List.map parse_release releases)
  with exn ->
    R.error_msgf
      "Failed to parse GitHub releases JSON: %s"
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
        "-H";
        "Accept: application/vnd.github+json";
        github_api_url;
      ]
  with
  | Ok body when String.trim body <> "" -> Ok body
  | Ok _ -> R.error_msg "Empty response from GitHub API"
  | Error _ as e -> e

let fetch_versions ?(include_prerelease = false) () =
  let* json_str = fetch_releases_json () in
  let json = Yojson.Safe.from_string json_str in
  let* versions = parse_release_json json in
  let filtered =
    if include_prerelease then versions
    else List.filter (fun v -> not v.is_prerelease) versions
  in
  Ok filtered

(** Checksum verification *)

let fetch_checksums ~version =
  let url = checksums_url ~version in
  match
    Cmd_runner.run_out_silent
      ["curl"; "-fsL"; "--max-time"; "10"; "--connect-timeout"; "5"; url]
  with
  | Ok body when String.trim body <> "" ->
      (* Parse checksums.txt format: "hash  filename" *)
      let lines = String.split_on_char '\n' body in
      let parse_line line =
        match String.split_on_char ' ' line |> List.filter (( <> ) "") with
        | hash :: filename :: _ -> Some (filename, hash)
        | _ -> None
      in
      Ok (List.filter_map parse_line lines)
  | Ok _ -> R.error_msg "Empty checksums file"
  | Error _ as e -> e

let verify_checksum ~filepath ~expected_hash =
  let* actual_hash = Download.compute_sha256 filepath in
  if String.equal actual_hash expected_hash then Ok ()
  else
    R.error_msgf
      "Checksum mismatch for %s: expected %s, got %s"
      filepath
      expected_hash
      actual_hash

(** Download utilities *)

let download_file_curl ~url ~dest ?progress () =
  match progress with
  | Some callback ->
      let on_progress current total =
        let downloaded = Int64.of_int current in
        let total_opt = Option.map Int64.of_int total in
        callback ~downloaded ~total:total_opt
      in
      Download.download_file_with_progress ~url ~dest_path:dest ~on_progress
  | None ->
      let cmd =
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
      in
      Cmd_runner.run cmd

(** Extract tarball *)

let extract_tarball ~tarball ~dest_dir =
  (* Signatory tarballs contain a single 'signatory' binary at the root *)
  let* () =
    Cmd_runner.run ["tar"; "-xzf"; tarball; "-C"; dest_dir; "signatory"]
  in
  Ok (Filename.concat dest_dir "signatory")

(** Installation path management *)

let signatory_binaries_dir () =
  Filename.concat (Paths.xdg_data_home ()) "octez-manager/signatory-binaries"

let signatory_version_path version =
  Filename.concat (signatory_binaries_dir ()) ("v" ^ version)

let is_complete_installation version =
  let dest_dir = signatory_version_path version in
  if not (Sys.file_exists dest_dir && Sys.is_directory dest_dir) then false
  else
    (* Check for signatory binary and metadata *)
    let binary = Filename.concat dest_dir "signatory" in
    let metadata = Filename.concat dest_dir ".metadata.json" in
    Sys.file_exists binary && Sys.file_exists metadata

let list_managed_versions () =
  let dir = signatory_binaries_dir () in
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
        "Failed to list Signatory versions: %s"
        (Printexc.to_string exn)
  else Ok []

(** Temporary directory for atomic installation *)

let temp_version_dir version =
  let pid = Unix.getpid () in
  Filename.concat
    (signatory_binaries_dir ())
    (Printf.sprintf ".tmp.v%s.%d" version pid)

let cleanup_stale_temp_dirs ?(max_age_seconds = 3600) () =
  let dir = signatory_binaries_dir () in
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

let download_version ~version ?(verify_checksums = true) ?progress () =
  let* arch = detect_arch () in
  let final_dir = signatory_version_path version in
  let temp_dir = temp_version_dir version in

  (* Ensure parent directory exists *)
  let binaries_dir = signatory_binaries_dir () in
  let owner, group = Paths.current_user_group_names () in
  let* () = File_ops.ensure_dir_path ~owner ~group ~mode:0o755 binaries_dir in

  (* Check if already installed *)
  let* () =
    if Sys.file_exists final_dir then
      if is_complete_installation version then
        R.error_msgf "Signatory v%s is already installed" version
      else (
        (* Incomplete installation - remove it *)
        (try ignore (Cmd_runner.run_out ["rm"; "-rf"; final_dir]) with _ -> ()) ;
        Ok ())
    else Ok ()
  in

  (* Clean up stale temp directories *)
  cleanup_stale_temp_dirs () ;

  (* Remove temp directory if it exists *)
  (try
     if Sys.file_exists temp_dir then
       ignore (Cmd_runner.run_out ["rm"; "-rf"; temp_dir])
   with _ -> ()) ;

  (* Create temp directory *)
  let* () = File_ops.ensure_dir_path ~owner ~group ~mode:0o755 temp_dir in

  (* Download tarball *)
  let url = tarball_url ~version ~arch in
  let tarball = Filename.concat temp_dir "signatory.tar.gz" in
  let* () = download_file_curl ~url ~dest:tarball ?progress () in

  (* Extract binary *)
  let* binary_path = extract_tarball ~tarball ~dest_dir:temp_dir in

  (* Make executable *)
  let* () = Cmd_runner.run ["chmod"; "+x"; binary_path] in

  (* Remove tarball *)
  (try Sys.remove tarball with _ -> ()) ;

  (* Verify checksum if requested *)
  let checksum_status =
    if verify_checksums then
      match fetch_checksums ~version with
      | Ok checksums -> (
          let tarball_name =
            Printf.sprintf
              "signatory_%s_linux_%s.tar.gz"
              version
              (arch_to_string arch)
          in
          match List.assoc_opt tarball_name checksums with
          | Some expected_hash -> (
              (* Re-download tarball temporarily for verification *)
              let verify_tarball = Filename.concat temp_dir "verify.tar.gz" in
              match download_file_curl ~url ~dest:verify_tarball () with
              | Ok () -> (
                  match
                    verify_checksum ~filepath:verify_tarball ~expected_hash
                  with
                  | Ok () ->
                      (try Sys.remove verify_tarball with _ -> ()) ;
                      Verified
                  | Error (`Msg reason) ->
                      (try Sys.remove verify_tarball with _ -> ()) ;
                      Failed reason)
              | Error (`Msg reason) -> Failed reason)
          | None -> Skipped)
      | Error (`Msg reason) -> Failed reason
    else Skipped
  in

  (* Save metadata *)
  let metadata =
    `Assoc
      [
        ("version", `String version);
        ("architecture", `String (arch_to_string arch));
        ("download_date", `String (iso8601_now ()));
        ( "checksum_status",
          `String
            (match checksum_status with
            | Verified -> "verified"
            | Skipped -> "skipped"
            | Failed _ -> "failed") );
      ]
  in
  let metadata_file = Filename.concat temp_dir ".metadata.json" in
  (try Yojson.Safe.to_file metadata_file metadata with _ -> ()) ;

  (* Atomic rename *)
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
  let dest_dir = signatory_version_path version in
  if not (Sys.file_exists dest_dir) then
    R.error_msgf "Signatory v%s is not installed" version
  else
    match Cmd_runner.run ["rm"; "-rf"; dest_dir] with
    | Ok () -> Ok ()
    | Error _ as e -> e

(** Calculate directory size *)

let calculate_directory_size path =
  if not (Sys.file_exists path) then Ok 0L
  else
    try
      let output = Cmd_runner.run_out ["du"; "-sb"; path] in
      match output with
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
  let path = signatory_version_path version in
  match calculate_directory_size path with
  | Ok bytes -> Ok (bytes, format_size_bytes bytes)
  | Error _ as e -> e

(** For tests *)

module For_tests = struct
  let parse_release_json = parse_release_json

  let detect_arch = detect_arch

  let arch_to_string = arch_to_string

  let tarball_url = tarball_url

  let checksums_url = checksums_url
end
