(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Version update checker for notifying users of new Octez releases *)

(** Version check result *)
type check_result =
  | UpdateAvailable of {
      latest_version : string;
      current_version : string option;
      should_notify : bool;  (** false if user dismissed this version *)
    }
  | UpToDate of string option  (** Optional current version *)
  | CheckDisabled
  | CheckFailed of string  (** Error message *)

(** Check for version updates
    - Fetches latest available version from remote
    - Compares with highest installed managed version
    - Respects user preferences (disabled checks, dismissed versions)
    @param force Bypass cache and force fresh check *)
val check_for_updates : ?force:bool -> unit -> check_result

(** Compare two version strings (e.g., "24.0" vs "24.1")
    @return -1 if v1 < v2, 0 if equal, 1 if v1 > v2 *)
val compare_versions : string -> string -> int

(** Mark a version as dismissed (don't notify about it again) *)
val dismiss_version : string -> (unit, Rresult.R.msg) result

(** Get the highest installed managed version, if any *)
val get_current_version : unit -> string option

(** Enable or disable version checking *)
val set_check_enabled : bool -> (unit, Rresult.R.msg) result

(** Check if version checking is enabled *)
val is_check_enabled : unit -> bool

(** Exported for tests *)
module For_tests : sig
  val parse_version : string -> int list
end
