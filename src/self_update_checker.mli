(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Self-update checker for octez-manager.

    Checks for new versions of octez-manager and provides functionality
    to download and install updates. Supports different installation methods
    (deb package, binary install, manual) with appropriate behavior for each. *)

(** {1 Version} *)

(** Current version of octez-manager *)
val current_version : string

(** {1 Installation method detection} *)

(** How octez-manager was installed *)
type install_method =
  | Deb_package  (** Installed via apt/dpkg - suggest apt upgrade *)
  | Binary_install  (** Installed via install script - can auto-update *)
  | Manual_install  (** Manual/source build - show release notes only *)

(** Detect how octez-manager was installed *)
val detect_install_method : unit -> install_method

(** {1 Version checking} *)

(** Result of checking for updates *)
type update_info = {
  latest_version : string;
  current_version : string;
  is_major_update : bool;  (** true if major version number differs *)
  download_url : string;
  checksums_url : string;
  release_notes_url : string;
}

(** Result of update check *)
type check_result =
  | Update_available of update_info
  | Up_to_date
  | Check_disabled
  | Check_failed of string

(** Check for updates.
    @param force Bypass cache and force fresh check from GitHub *)
val check_for_updates : ?force:bool -> unit -> check_result

(** {1 Update preferences} *)

(** Check if update checking is enabled *)
val is_check_enabled : unit -> bool

(** Enable or disable update checking *)
val set_check_enabled : bool -> (unit, Rresult.R.msg) result

(** Mark a version as dismissed (don't notify about it again) *)
val dismiss_version : string -> (unit, Rresult.R.msg) result

(** Check if a version has been dismissed *)
val is_version_dismissed : string -> bool

(** {1 Update operations} *)

(** Result of an upgrade attempt *)
type upgrade_result =
  | Upgrade_success of {
      new_version : string;
      needs_restart : bool;  (** true if process should be restarted *)
    }
  | Upgrade_needs_elevation of string  (** Command to run with sudo *)
  | Upgrade_failed of string  (** Error message *)

(** Download and install an update (for binary installs only).
    @param version Version to install
    @param on_progress Optional callback for download progress *)
val perform_upgrade :
  version:string ->
  ?on_progress:(downloaded:int64 -> total:int64 option -> unit) ->
  unit ->
  upgrade_result

(** Try to restart the process with the new binary.
    Returns only if exec fails. *)
val exec_restart : unit -> unit

(** {1 Version comparison} *)

(** Compare two version strings.
    @return -1 if v1 < v2, 0 if equal, 1 if v1 > v2 *)
val compare_versions : string -> string -> int

(** Check if update is a major version bump *)
val is_major_update : current:string -> latest:string -> bool
