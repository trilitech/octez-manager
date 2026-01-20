(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Download engine for Octez binaries from official distribution *)

(** System architecture *)
type arch = X86_64 | Arm64

(** Version information from versions.json *)
type version_info = {
  version : string;  (** Version string (e.g., "24.0") *)
  release_date : string option;  (** Release date if available *)
  is_rc : bool;  (** Whether this is a release candidate *)
}

(** Download progress callback
    @param downloaded Number of bytes downloaded so far
    @param total Total size in bytes (None if unknown) *)
type progress_callback = downloaded:int64 -> total:int64 option -> unit

(** Checksum verification status *)
type checksum_status =
  | Verified  (** Checksum matched *)
  | Skipped  (** User skipped verification *)
  | Failed of string  (** Verification failed with reason *)

(** Download result *)
type download_result = {
  version : string;
  installed_path : string;
  binaries : string list;  (** List of downloaded binary names *)
  checksum_status : checksum_status;
}

(** {2 Architecture detection} *)

(** Detect current system architecture *)
val detect_arch : unit -> (arch, Rresult.R.msg) result

(** Convert architecture to string for URL construction *)
val arch_to_string : arch -> string

(** {2 Version fetching} *)

(** Fetch available versions from remote
    @param include_rc Whether to include release candidates *)
val fetch_versions :
  ?include_rc:bool -> unit -> (version_info list, Rresult.R.msg) result

(** Get cached versions list, or fetch if cache is stale/missing
    @param include_rc Whether to include release candidates *)
val get_versions_cached :
  ?include_rc:bool -> unit -> (version_info list, Rresult.R.msg) result

(** Clear versions cache (forces re-fetch on next request) *)
val clear_cache : unit -> unit

(** {2 Binary download} *)

(** List of binaries to download for a version *)
val binaries_for_version : string -> (string list, Rresult.R.msg) result

(** Download a version to the managed binaries directory
    @param version Version to download (e.g., "24.0")
    @param verify_checksums Whether to verify checksums (can be cancelled by user)
    @param progress Optional progress callback
    @return Download result with installed path and status *)
val download_version :
  version:string ->
  ?verify_checksums:bool ->
  ?progress:progress_callback ->
  unit ->
  (download_result, Rresult.R.msg) result

(** Remove a downloaded version
    @param version Version to remove (e.g., "24.0") *)
val remove_version : string -> (unit, Rresult.R.msg) result

(** {2 Disk space} *)

(** Estimate download size for a version in bytes *)
val estimate_download_size : string -> (int64, Rresult.R.msg) result

(** Check available disk space at managed binaries directory in bytes *)
val check_disk_space : unit -> (int64, Rresult.R.msg) result

(** {2 URL construction} *)

(** Construct URL for a binary
    @param version Version string (e.g., "24.0")
    @param arch Architecture
    @param binary Binary name (e.g., "octez-node") *)
val binary_url : version:string -> arch:arch -> binary:string -> string

(** Construct URL for checksums file
    @param version Version string (e.g., "24.0")
    @param arch Architecture *)
val checksums_url : version:string -> arch:arch -> string

(** {2 For tests} *)

module For_tests : sig
  (** Parse versions.json content *)
  val parse_version_json :
    Yojson.Safe.t -> (version_info list, Rresult.R.msg) result
end
