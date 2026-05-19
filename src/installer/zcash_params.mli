(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Zcash Sapling parameter management for Octez nodes.

    Octez nodes require Zcash Sapling parameters to operate. This module
    handles detection and automatic download of these parameters if they
    are not already present on the system. *)

(** {1 Parameter Detection} *)

(** Check if all required Zcash parameters exist in any of the standard
    search paths relative to the given home directory.

    Standard search paths (in order):
    - [~/.local/share/zcash-params]
    - [~/.zcash-params]
    - [~/_opam/share/zcash-params]
    - [/usr/local/share/zcash-params]
    - [/usr/share/zcash-params]

    @param home_dir The home directory of the service user
    @return [Ok (Some path)] if all parameters exist with correct checksums,
            [Ok None] if parameters are missing or invalid,
            [Error] on filesystem errors *)
val find_existing_params :
  home_dir:string -> (string option, [> `Msg of string]) result

(** {1 Parameter Download} *)

(** Ensure Zcash parameters are available for the given service user.

    If parameters are already present with correct checksums, this is a no-op.
    Otherwise, downloads the parameters to [~/.zcash-params/] in the service
    user's home directory.

    The two required parameter files are:
    - [sapling-spend.params] (47MB)
    - [sapling-output.params] (3.5MB)

    Each file is downloaded directly from the Zcash download server and verified
    against its SHA256 checksum.

    @param quiet Suppress progress output
    @param on_log Optional callback for progress messages
    @param service_user The user account that will run the node
    @return [Ok ()] if parameters are available (already present or successfully downloaded),
            [Error] if download or verification fails *)
val ensure_params :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  service_user:string ->
  unit ->
  (unit, [> `Msg of string]) result

(**/**)

(** Internal functions exposed for testing *)
module Internal_for_tests : sig
  (** Parameter file metadata *)
  type param_file = {
    name : string;  (** Base filename (e.g., "sapling-spend.params") *)
    sha256 : string;  (** Expected SHA256 checksum (hex) *)
  }

  (** List of required parameter files with their checksums *)
  val required_params : param_file list

  (** Get the home directory for a given user *)
  val get_user_home : string -> (string, [> `Msg of string]) result

  (** Get all standard search paths for zcash params relative to a home directory *)
  val get_search_paths : home_dir:string -> string list

  (** Check if all required params exist in a directory with correct checksums *)
  val verify_params_in_dir : string -> bool

  (** Download URL base for Zcash parameters *)
  val download_base_url : string
end

(**/**)
