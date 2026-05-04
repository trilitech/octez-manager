(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Signatory binary downloader from GitHub releases. *)

(** {1 Types} *)

type arch = X86_64 | Arm64  (** Supported architectures *)

(** Information about a Signatory release *)
type version_info = {
  version : string;  (** Version string without 'v' prefix *)
  release_date : string option;  (** ISO date format *)
  is_prerelease : bool;  (** RC, beta, alpha releases *)
}

(** Progress callback during download *)
type progress_callback = downloaded:int64 -> total:int64 option -> unit

type checksum_status =
  | Verified  (** Checksum verified successfully *)
  | Skipped  (** Checksum verification skipped *)
  | Failed of string  (** Checksum verification failed with reason *)

(** Result of a successful download *)
type download_result = {
  version : string;
  installed_path : string;
  checksum_status : checksum_status;
}

(** {1 Version Management} *)

(** Fetch available Signatory versions from GitHub releases.
    
    @param include_prerelease If true, include RC/beta/alpha releases
    @return List of available versions, newest first *)
val fetch_versions :
  ?include_prerelease:bool ->
  unit ->
  (version_info list, [> `Msg of string]) result

(** List locally installed Signatory versions.
    
    @return List of installed versions (without 'v' prefix), newest first *)
val list_managed_versions : unit -> (string list, [> `Msg of string]) result

(** {1 Download} *)

(** Download and install a Signatory version.
    
    Downloads the tarball from GitHub releases, verifies checksums if enabled,
    and installs to [~/.local/share/octez-manager/signatory-binaries/vX.Y.Z/].
    
    The installation is atomic - uses a temporary directory during download
    and renames on success. Incomplete installations are cleaned up automatically.
    
    @param version Version to download (e.g., "1.3.1")
    @param verify_checksums Enable checksum verification (default: true)
    @param progress Optional progress callback
    @return Download result with installed path and checksum status *)
val download_version :
  version:string ->
  ?verify_checksums:bool ->
  ?progress:progress_callback ->
  unit ->
  (download_result, [> `Msg of string]) result

(** Remove an installed Signatory version.
    
    @param version Version to remove (e.g., "1.3.1")
    @return Ok () on success *)
val remove_version : string -> (unit, [> `Msg of string]) result

(** {1 Utilities} *)

(** Base directory for Signatory binaries:
    [~/.local/share/octez-manager/signatory-binaries/] *)
val signatory_binaries_dir : unit -> string

(** Path to specific version directory.
    
    @param version Version string (e.g., "1.3.1")
    @return Path like [~/.local/share/octez-manager/signatory-binaries/v1.3.1] *)
val signatory_version_path : string -> string

(** Check if a version installation is complete (binary + metadata exist).
    
    @param version Version to check
    @return true if installation is complete *)
val is_complete_installation : string -> bool

(** Clean up stale temporary download directories.
    
    @param max_age_seconds Maximum age in seconds (default: 3600) *)
val cleanup_stale_temp_dirs : ?max_age_seconds:int -> unit -> unit

(** Get the disk size of an installed version.
    
    @param version Version to measure
    @return (bytes, formatted_string) *)
val get_version_size : string -> (int64 * string, [> `Msg of string]) result

(** Format bytes into human-readable string (B, KB, MB, GB) *)
val format_size_bytes : int64 -> string

(** {1 For Testing} *)

module For_tests : sig
  val parse_release_json :
    Yojson.Safe.t -> (version_info list, [> `Msg of string]) result

  (** Architecture detection and conversion *)
  val detect_arch : unit -> (arch, [> `Msg of string]) result

  val arch_to_string : arch -> string

  (** URL construction *)
  val tarball_url : version:string -> arch:arch -> string

  val checksums_url : version:string -> string
end
