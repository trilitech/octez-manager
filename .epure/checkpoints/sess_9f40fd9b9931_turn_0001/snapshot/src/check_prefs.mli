(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shared preferences for version-check subsystems.

    Both {!Self_update_checker} (octez-manager self-update) and
    {!Version_checker} (Octez binary updates) persist a small JSON
    preferences file with:
    - [check_enabled] — whether periodic checking is on
    - [dismissed_versions] — versions the user chose to ignore

    This module captures the shared logic so each checker only needs to
    provide its filename and any extra fields. *)

(** Core preferences shared by all checkers. *)
type prefs = {check_enabled : bool; dismissed_versions : string list}

(** Default preferences (checking on, no dismissed versions). *)
val default : prefs

(** {1 Persistence}

    All functions take [~file] so each subsystem can use its own path. *)

(** Load preferences from [file].
    Returns {!default} if the file does not exist or cannot be parsed.

    @param extra_of_json  Optional function to extract extra fields from the
    JSON object.  Called with the root JSON value.  If omitted or if it
    raises, the extra value is [None]. *)
val load :
  file:string ->
  ?extra_of_json:(Yojson.Safe.t -> 'a) ->
  unit ->
  (prefs * 'a option, Rresult.R.msg) result

(** Save preferences to [file].

    @param extra_to_json  Optional list of [(key, json_value)] pairs to
    merge into the JSON object alongside [check_enabled] and
    [dismissed_versions]. *)
val save :
  file:string ->
  ?extra_to_json:(string * Yojson.Safe.t) list ->
  prefs ->
  (unit, Rresult.R.msg) result

(** {1 Convenience wrappers} *)

(** Check if update checking is enabled for the given prefs file. *)
val is_check_enabled : file:string -> bool

(** Enable or disable update checking. *)
val set_check_enabled : file:string -> bool -> (unit, Rresult.R.msg) result

(** Mark a version as dismissed (won't notify about it again). *)
val dismiss_version : file:string -> string -> (unit, Rresult.R.msg) result

(** Check if a specific version has been dismissed. *)
val is_version_dismissed : file:string -> string -> bool
