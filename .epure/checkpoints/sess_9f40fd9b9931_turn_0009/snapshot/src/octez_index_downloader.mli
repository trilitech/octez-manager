(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** octez-index binary downloader from GitLab releases.

    Downloads octez-index binaries from the official GitLab repository at
    https://gitlab.com/tezos/octez-index. Unlike the standard Octez suite,
    octez-index ships as a single binary asset attached directly to each
    GitLab release (not a tarball). *)

(** {1 Types} *)

type arch = X86_64 | Arm64  (** Supported architectures *)

(** Information about a release *)
type version_info = {
  version : string;  (** Version string without 'v' prefix e.g. "0.1.0" *)
  release_date : string option;  (** ISO date format e.g. "2026-03-18" *)
  is_prerelease : bool;  (** RC, beta, alpha, dev releases *)
}

(** Progress callback during download *)
type progress_callback = downloaded:int64 -> total:int64 option -> unit

type checksum_status =
  | Verified  (** Checksum verified successfully *)
  | Skipped  (** Checksum verification skipped or unavailable *)
  | Failed of string  (** Checksum verification failed *)

(** Result of a successful download *)
type download_result = {
  version : string;
  installed_path : string;
  checksum_status : checksum_status;
}

(** {1 Version Management} *)

(** Fetch available octez-index versions from GitLab releases.

    @param include_prerelease If true, include RC/beta/alpha/dev releases
    @return List of available versions, newest first *)
val fetch_versions :
  ?include_prerelease:bool ->
  unit ->
  (version_info list, [> `Msg of string]) result

(** List locally installed octez-index versions.

    @return List of installed versions (without 'v' prefix), newest first *)
val list_managed_versions : unit -> (string list, [> `Msg of string]) result

(** {1 Download} *)

(** Download and install an octez-index version.

    Downloads the binary directly from GitLab release assets and installs to
    [~/.local/share/octez-manager/octez-index-binaries/vX.Y.Z/].

    The installation is atomic — uses a temporary directory during download
    and renames on success.

    @param version Version to download (e.g., "0.1.0")
    @param progress Optional progress callback
    @return Download result with installed path *)
val download_version :
  version:string ->
  ?progress:progress_callback ->
  unit ->
  (download_result, [> `Msg of string]) result

(** Remove an installed octez-index version.

    @param version Version to remove (e.g., "0.1.0")
    @return Ok () on success *)
val remove_version : string -> (unit, [> `Msg of string]) result

(** {1 Utilities} *)

(** Base directory for octez-index binaries:
    [~/.local/share/octez-manager/octez-index-binaries/] *)
val octez_index_binaries_dir : unit -> string

(** Path to specific version directory.

    @param version Version string (e.g., "0.1.0")
    @return Path like [~/.local/share/octez-manager/octez-index-binaries/v0.1.0] *)
val octez_index_version_path : string -> string

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

  val detect_arch : unit -> (arch, [> `Msg of string]) result

  val arch_to_string : arch -> string

  val binary_url : version:string -> arch:arch -> string
end
