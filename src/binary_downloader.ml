(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

(** Utilities *)

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

(** Constants *)
let base_url = "https://octez.tezos.com/releases"

let versions_url = base_url ^ "/versions.json"

let cache_ttl_seconds = 3600

(* 1 hour *)

(** Types *)

type arch = X86_64 | Arm64

type version_info = {
  version : string;
  release_date : string option;
  is_rc : bool;
}

type progress_callback = downloaded:int64 -> total:int64 option -> unit

type checksum_status = Verified | Skipped | Failed of string

type download_result = {
  version : string;
  installed_path : string;
  binaries : string list;
  checksum_status : checksum_status;
}

(** Architecture detection *)

let detect_arch () =
  match Common.run_out ["uname"; "-m"] with
  | Ok output -> (
      let machine = String.trim output in
      match machine with
      | "x86_64" | "amd64" -> Ok X86_64
      | "aarch64" | "arm64" -> Ok Arm64
      | _ ->
          R.error_msgf
            "Unsupported architecture: %s (expected x86_64 or arm64)"
            machine)
  | Error _ as e -> e

let arch_to_string = function X86_64 -> "x86_64" | Arm64 -> "arm64"

(** URL construction *)

let binary_url ~version ~arch ~binary =
  Printf.sprintf
    "%s/octez-v%s/binaries/%s/%s"
    base_url
    version
    (arch_to_string arch)
    binary

let checksums_url ~version ~arch =
  Printf.sprintf
    "%s/octez-v%s/binaries/%s/sha256sums.txt"
    base_url
    version
    (arch_to_string arch)

(** Version fetching and caching *)

let versions_cache_file () =
  Filename.concat (Common.xdg_data_home ()) "octez-manager/versions-cache.json"

let cache_timestamp_file () =
  Filename.concat
    (Common.xdg_data_home ())
    "octez-manager/versions-cache.timestamp"

let is_cache_fresh () =
  let ts_file = cache_timestamp_file () in
  if Sys.file_exists ts_file then
    try
      let ic = open_in ts_file in
      let line = input_line ic in
      close_in ic ;
      let cache_time = float_of_string line in
      let now = Unix.time () in
      now -. cache_time < float_of_int cache_ttl_seconds
    with _ -> false
  else false

let save_cache_timestamp () =
  let ts_file = cache_timestamp_file () in
  let dir = Filename.dirname ts_file in
  let owner, group = Common.current_user_group_names () in
  let* () = Common.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  try
    let oc = open_out ts_file in
    output_string oc (string_of_float (Unix.time ())) ;
    close_out oc ;
    Ok ()
  with exn ->
    R.error_msgf "Failed to save cache timestamp: %s" (Printexc.to_string exn)

let parse_version_json json =
  try
    let open Yojson.Safe.Util in
    let versions_array =
      match json with
      | `List lst -> lst
      | `Assoc _ ->
          (* Try to find a versions array inside *)
          member "versions" json |> to_list
      | _ -> failwith "Expected JSON array or object"
    in
    let parse_version v =
      (* Parse major.minor format with optional rc *)
      let major = member "major" v |> to_int in
      let minor = member "minor" v |> to_int in
      let rc = member "rc" v |> to_int_option in
      let version =
        match rc with
        | Some rc_num -> Printf.sprintf "%d.%d-rc%d" major minor rc_num
        | None -> Printf.sprintf "%d.%d" major minor
      in
      let release_date =
        member "pubDate" v |> to_int_option
        |> Option.map (fun ts ->
            if ts > 0 then
              (* Convert Unix timestamp to ISO date *)
              let open Unix in
              let tm = gmtime (float_of_int ts) in
              Printf.sprintf
                "%04d-%02d-%02d"
                (tm.tm_year + 1900)
                (tm.tm_mon + 1)
                tm.tm_mday
            else "")
        |> fun opt -> match opt with Some s when s <> "" -> Some s | _ -> None
      in
      let is_rc = rc <> None in
      {version; release_date; is_rc}
    in
    Ok (List.map parse_version versions_array)
  with exn ->
    R.error_msgf "Failed to parse versions.json: %s" (Printexc.to_string exn)

let fetch_versions_json () =
  match
    Common.run_out_silent
      [
        "curl";
        "-fsL";
        "--max-time";
        "10";
        "--connect-timeout";
        "5";
        versions_url;
      ]
  with
  | Ok body when String.trim body <> "" -> Ok body
  | Ok _ -> R.error_msg "Empty versions.json response"
  | Error _ as e -> e

let save_versions_cache json_str =
  let cache_file = versions_cache_file () in
  let dir = Filename.dirname cache_file in
  let owner, group = Common.current_user_group_names () in
  let* () = Common.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  try
    let oc = open_out cache_file in
    output_string oc json_str ;
    close_out oc ;
    save_cache_timestamp ()
  with exn ->
    R.error_msgf "Failed to save versions cache: %s" (Printexc.to_string exn)

let load_versions_cache () =
  let cache_file = versions_cache_file () in
  if Sys.file_exists cache_file then
    try
      let ic = open_in cache_file in
      let json_str = really_input_string ic (in_channel_length ic) in
      close_in ic ;
      Ok json_str
    with exn ->
      R.error_msgf "Failed to load versions cache: %s" (Printexc.to_string exn)
  else R.error_msg "Cache file does not exist"

let clear_cache () =
  let cache_file = versions_cache_file () in
  let ts_file = cache_timestamp_file () in
  (try if Sys.file_exists cache_file then Sys.remove cache_file with _ -> ()) ;
  try if Sys.file_exists ts_file then Sys.remove ts_file with _ -> ()

let fetch_versions ?(include_rc = false) () =
  let* json_str = fetch_versions_json () in
  let* () = save_versions_cache json_str in
  let json = Yojson.Safe.from_string json_str in
  let* versions = parse_version_json json in
  let filtered =
    if include_rc then versions else List.filter (fun v -> not v.is_rc) versions
  in
  Ok filtered

let get_versions_cached ?(include_rc = false) () =
  if is_cache_fresh () then
    match load_versions_cache () with
    | Ok json_str -> (
        let json = Yojson.Safe.from_string json_str in
        match parse_version_json json with
        | Ok versions ->
            let filtered =
              if include_rc then versions
              else List.filter (fun v -> not v.is_rc) versions
            in
            Ok filtered
        | Error _ ->
            (* Cache parse failed, fetch fresh *)
            fetch_versions ~include_rc ())
    | Error _ ->
        (* Cache load failed, fetch fresh *)
        fetch_versions ~include_rc ()
  else fetch_versions ~include_rc ()

(** Binary names *)

let binaries_for_version _version =
  (* For now, return the list of binaries we always download.
     In the future, we could fetch protocol-specific baker/accuser variants
     from the release metadata. *)
  Ok
    [
      "octez-node";
      "octez-client";
      "octez-dal-node";
      (* Baker and accuser variants will be added based on available protocols *)
    ]

(** Disk space utilities *)

let check_disk_space () =
  let dir = Binary_registry.binaries_dir () in
  (* Ensure directory exists before running df *)
  let owner, group = Common.current_user_group_names () in
  let* () = Common.ensure_dir_path ~owner ~group ~mode:0o755 dir in
  match Common.run_out ["df"; "-B1"; dir] with
  | Ok output -> (
      (* Parse df output: lines like:
         Filesystem           1B-blocks        Used   Available Use% Mounted on
         /dev/sda1         1000000000000 50000000000 95000000000  35% / *)
      let lines = String.split_on_char '\n' output in
      match lines with
      | _ :: data_line :: _ -> (
          let parts =
            String.split_on_char ' ' data_line
            |> List.filter (fun s -> String.trim s <> "")
          in
          match parts with
          | _ :: _ :: _ :: available :: _ -> (
              try Ok (Int64.of_string available)
              with _ -> R.error_msg "Failed to parse df output")
          | _ -> R.error_msg "Unexpected df output format")
      | _ -> R.error_msg "No df output")
  | Error _ as e -> e

let estimate_download_size _version =
  (* Rough estimate: node ~100MB, client ~50MB, dal-node ~50MB = ~200MB
     In the future, we could get this from release metadata *)
  Ok 200_000_000L

(** Checksum verification *)

let fetch_checksums ~version ~arch =
  let url = checksums_url ~version ~arch in
  match
    Common.run_out_silent
      ["curl"; "-fsL"; "--max-time"; "10"; "--connect-timeout"; "5"; url]
  with
  | Ok body when String.trim body <> "" ->
      (* Parse sha256sums.txt format: "hash  filename" *)
      let lines = String.split_on_char '\n' body in
      let parse_line line =
        match String.split_on_char ' ' line |> List.filter (( <> ) "") with
        | hash :: filename :: _ -> Some (filename, hash)
        | _ -> None
      in
      Ok (List.filter_map parse_line lines)
  | Ok _ -> R.error_msg "Empty checksums file"
  | Error _ as e -> e

let compute_sha256 filepath =
  match Common.run_out ["sha256sum"; filepath] with
  | Ok output -> (
      match String.split_on_char ' ' output with
      | hash :: _ -> Ok (String.trim hash)
      | _ -> R.error_msg "Unexpected sha256sum output")
  | Error _ as e -> e

let verify_checksum ~filepath ~expected_hash =
  let* actual_hash = compute_sha256 filepath in
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
      (* Use progress-aware download *)
      let on_progress current total =
        let downloaded = Int64.of_int current in
        let total_opt = Option.map Int64.of_int total in
        callback ~downloaded ~total:total_opt
      in
      Common.download_file_with_progress ~url ~dest_path:dest ~on_progress
  | None ->
      (* Fallback to simple curl without progress *)
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
      Common.run cmd

let download_binary ~version ~arch ~binary ~dest_dir ?progress () =
  let url = binary_url ~version ~arch ~binary in
  let dest = Filename.concat dest_dir binary in
  let* () = download_file_curl ~url ~dest ?progress () in
  (* Make executable *)
  let* () = Common.run ["chmod"; "+x"; dest] in
  Ok dest

(** Main download function *)

let download_version ~version ?(verify_checksums = true) ?progress () =
  let* arch = detect_arch () in
  let dest_dir = Binary_registry.managed_version_path version in

  (* Ensure parent binaries directory exists *)
  let binaries_dir = Binary_registry.binaries_dir () in
  let owner, group = Common.current_user_group_names () in
  let* () = Common.ensure_dir_path ~owner ~group ~mode:0o755 binaries_dir in

  (* Check if already exists *)
  if Sys.file_exists dest_dir then
    R.error_msgf "Version v%s is already installed" version
  else
    (* Check disk space *)
    let* available = check_disk_space () in
    let* required = estimate_download_size version in
    if available < required then
      R.error_msgf
        "Insufficient disk space: need %Ld bytes, have %Ld bytes"
        required
        available
    else
      (* Create version directory *)
      let owner, group = Common.current_user_group_names () in
      let* () = Common.ensure_dir_path ~owner ~group ~mode:0o755 dest_dir in

      (* Get list of binaries to download *)
      let* binary_list = binaries_for_version version in

      (* Download all binaries *)
      let rec download_all acc = function
        | [] -> Ok (List.rev acc)
        | binary :: rest -> (
            match
              download_binary ~version ~arch ~binary ~dest_dir ?progress ()
            with
            | Ok _path ->
                let _ = progress in
                (* TODO: Update progress *)
                download_all (binary :: acc) rest
            | Error _ as e ->
                (* Cleanup on failure *)
                (try Common.run_out ["rm"; "-rf"; dest_dir] |> ignore
                 with _ -> ()) ;
                e)
      in

      let* downloaded_binaries = download_all [] binary_list in

      (* Verify checksums if requested *)
      let checksum_status =
        if verify_checksums then
          match fetch_checksums ~version ~arch with
          | Ok checksums -> (
              let verify_all () =
                let rec check = function
                  | [] -> Ok ()
                  | binary :: rest -> (
                      let filepath = Filename.concat dest_dir binary in
                      match List.assoc_opt binary checksums with
                      | Some expected_hash -> (
                          match verify_checksum ~filepath ~expected_hash with
                          | Ok () -> check rest
                          | Error _ as e -> e)
                      | None ->
                          (* Binary not in checksums file - skip *)
                          check rest)
                in
                check downloaded_binaries
              in
              match verify_all () with
              | Ok () -> Verified
              | Error (`Msg reason) -> Failed reason)
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
      let metadata_file = Filename.concat dest_dir ".metadata.json" in
      (try Yojson.Safe.to_file metadata_file metadata with _ -> ()) ;

      Ok
        {
          version;
          installed_path = dest_dir;
          binaries = downloaded_binaries;
          checksum_status;
        }

(** Remove version *)

let remove_version version =
  let dest_dir = Binary_registry.managed_version_path version in
  if not (Sys.file_exists dest_dir) then
    R.error_msgf "Version v%s is not installed" version
  else
    match Common.run ["rm"; "-rf"; dest_dir] with
    | Ok () -> Ok ()
    | Error _ as e -> e

(** For tests *)

module For_tests = struct
  let parse_version_json = parse_version_json
end
