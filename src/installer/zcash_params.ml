(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult

let ( let* ) = Result.bind

(** Parameter file metadata *)
type param_file = {
  name : string;  (** Base filename *)
  sha256 : string;  (** Expected SHA256 checksum (hex) *)
}

(** Required Zcash Sapling parameters with their SHA256 checksums.
    Octez nodes only need the Sapling parameters, not Sprout. *)
let required_params =
  [
    {
      name = "sapling-spend.params";
      sha256 =
        "8e48ffd23abb3a5fd9c5589204f32d9c31285a04b78096ba40a79b75677efc13";
    };
    {
      name = "sapling-output.params";
      sha256 =
        "2f0ebbcbb9bb0bcffe95a397e7eba89c29eb4dde6191c339db88570e3f3fb0e4";
    };
  ]

let download_base_url = "https://download.z.cash/downloads/"

(** Get the home directory for a given user *)
let get_user_home username =
  try
    let pw = Unix.getpwnam username in
    Ok pw.Unix.pw_dir
  with Not_found ->
    R.error_msgf "User '%s' does not exist on this system" username

(** Get all standard search paths for zcash params relative to a home directory *)
let get_search_paths ~home_dir =
  [
    Filename.concat home_dir ".local/share/zcash-params";
    Filename.concat home_dir ".zcash-params";
    Filename.concat home_dir "_opam/share/zcash-params";
    "/usr/local/share/zcash-params";
    "/usr/share/zcash-params";
  ]

(** Check if a file exists and matches the expected SHA256 checksum *)
let verify_file_checksum path expected_sha256 =
  if not (Sys.file_exists path) then false
  else
    match Download.compute_sha256 path with
    | Ok actual_sha256 -> String.equal actual_sha256 expected_sha256
    | Error _ -> false

(** Check if all required params exist in a directory with correct checksums *)
let verify_params_in_dir dir =
  if not (Sys.file_exists dir) then false
  else
    List.for_all
      (fun param ->
        let path = Filename.concat dir param.name in
        verify_file_checksum path param.sha256)
      required_params

let find_existing_params ~home_dir =
  try
    let search_paths = get_search_paths ~home_dir in
    let found =
      List.find_opt (fun dir -> verify_params_in_dir dir) search_paths
    in
    Ok found
  with Sys_error msg -> R.error_msg msg

(** Download and verify a single parameter file *)
let download_and_verify_param ~quiet ?on_log ~dest_dir param =
  let log msg = match on_log with Some f -> f msg | None -> () in
  log (Printf.sprintf "Downloading %s...\n" param.name) ;
  let url = download_base_url ^ param.name in
  let dest_path = Filename.concat dest_dir param.name in
  (* Download the file *)
  let* () = Download.download_file ~quiet ~url ~dest_path () in
  (* Verify checksum *)
  log (Printf.sprintf "  Verifying checksum...\n") ;
  let* actual_sha256 = Download.compute_sha256 dest_path in
  if String.equal actual_sha256 param.sha256 then (
    log (Printf.sprintf "  ✓ %s verified\n" param.name) ;
    Ok ())
  else (
    (* Delete invalid file *)
    (try Sys.remove dest_path with Sys_error _ -> ()) ;
    R.error_msgf
      "Checksum mismatch for %s: expected %s, got %s"
      param.name
      param.sha256
      actual_sha256)

(** Download all parameter files to the destination directory *)
let download_params ~quiet ?on_log ~dest_dir () =
  let log msg = match on_log with Some f -> f msg | None -> () in
  (* Ensure destination directory exists *)
  let* () =
    try
      if not (Sys.file_exists dest_dir) then
        (* Create parent directory if needed *)
        let parent_dir = Filename.dirname dest_dir in
        if not (Sys.file_exists parent_dir) then
          R.error_msgf
            "Parent directory %s does not exist. Please ensure the service \
             user's home directory is set up correctly."
            parent_dir
        else (
          Unix.mkdir dest_dir 0o755 ;
          Ok ())
      else Ok ()
    with
    | Unix.Unix_error (err, _, _) ->
        R.error_msgf
          "Failed to create directory %s: %s"
          dest_dir
          (Unix.error_message err)
    | Sys_error msg -> R.error_msg msg
  in
  log
    (Printf.sprintf
       "Downloading Zcash parameters to %s (this may take a few minutes)...\n"
       dest_dir) ;
  (* Download each parameter file *)
  List.fold_left
    (fun acc param ->
      let* () = acc in
      download_and_verify_param ~quiet ?on_log ~dest_dir param)
    (Ok ())
    required_params

let ensure_params ?(quiet = false) ?on_log ~service_user () =
  let log msg = match on_log with Some f -> f msg | None -> () in
  (* Get service user's home directory *)
  let* home_dir = get_user_home service_user in
  (* Check if params already exist *)
  let* existing = find_existing_params ~home_dir in
  match existing with
  | Some path ->
      log (Printf.sprintf "Zcash parameters already present in %s\n" path) ;
      Ok ()
  | None ->
      (* Download to ~/.zcash-params *)
      let dest_dir = Filename.concat home_dir ".zcash-params" in
      download_params ~quiet ?on_log ~dest_dir ()

module Internal_for_tests = struct
  type nonrec param_file = param_file = {name : string; sha256 : string}

  let required_params = required_params

  let get_user_home = get_user_home

  let get_search_paths = get_search_paths

  let verify_params_in_dir = verify_params_in_dir

  let download_base_url = download_base_url
end
