(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Binary source types - how an instance references its Octez binaries *)
type bin_source =
  | Managed_octez_version of string
      (** Downloaded/managed Octez version e.g. "24.0" *)
  | Managed_signatory_version of string
      (** Downloaded/managed Signatory version e.g. "1.3.1" *)
  | Registered_alias of string
      (** Registered directory alias e.g. "dev-build" *)
  | Raw_path of string  (** Raw filesystem path e.g. "/usr/local/bin" *)

(** Registered directory entry *)
type registered_dir = {alias : string; path : string}

(** {2 Bin source operations} *)

(** Human-readable representation *)
val bin_source_to_string : bin_source -> string

(** Serialize to JSON (for service config) *)
val bin_source_to_yojson : bin_source -> Yojson.Safe.t

(** Deserialize from JSON with backward compatibility for legacy app_bin_dir *)
val bin_source_of_yojson : Yojson.Safe.t -> (bin_source, Rresult.R.msg) result

(** Convert legacy app_bin_dir string to bin_source (always Raw_path) *)
val bin_source_of_legacy : string -> bin_source

(** {2 Path resolution} *)

(** XDG data directory for managed binaries: $XDG_DATA_HOME/octez-manager/binaries/ *)
val binaries_dir : unit -> string

(** Path to a managed version directory: binaries_dir/v{version}/ *)
val managed_version_path : string -> string

(** Resolve a bin_source to an actual filesystem path *)
val resolve_bin_source : bin_source -> (string, Rresult.R.msg) result

(** {2 Registered directories management} *)

(** Path to registered-directories.json *)
val registered_dirs_file : unit -> string

(** Load all registered directories *)
val load_registered_dirs : unit -> (registered_dir list, Rresult.R.msg) result

(** Save registered directories *)
val save_registered_dirs : registered_dir list -> (unit, Rresult.R.msg) result

(** Find a registered directory by alias *)
val find_registered_dir :
  string -> (registered_dir option, Rresult.R.msg) result

(** Add a new registered directory. Fails if alias already exists. *)
val add_registered_dir :
  alias:string -> path:string -> (unit, Rresult.R.msg) result

(** Remove a registered directory by alias *)
val remove_registered_dir : string -> (unit, Rresult.R.msg) result

(** Rename a registered directory alias *)
val rename_registered_dir :
  old_alias:string -> new_alias:string -> (unit, Rresult.R.msg) result

(** {2 Managed versions} *)

(** Compare version strings numerically (e.g., "24.0" > "9.0").
    Returns: negative if a < b, 0 if a = b, positive if a > b *)
val compare_versions : string -> string -> int

(** List all installed managed versions *)
val list_managed_versions : unit -> (string list, Rresult.R.msg) result

(** Check if a managed version is installed *)
val managed_version_exists : string -> bool

(** Check if a version installation is complete (has all binaries and metadata)
    @param version Version to check (e.g., "24.0") *)
val is_complete_installation : string -> bool

(** {2 Testing interface} *)

module For_tests : sig
  val bin_source_to_string : bin_source -> string

  val bin_source_to_yojson : bin_source -> Yojson.Safe.t

  val bin_source_of_yojson : Yojson.Safe.t -> (bin_source, Rresult.R.msg) result

  val bin_source_of_legacy : string -> bin_source

  val registered_dir_to_yojson : registered_dir -> Yojson.Safe.t

  val registered_dir_of_yojson :
    Yojson.Safe.t -> (registered_dir, Rresult.R.msg) result

  val registered_dirs_to_yojson : registered_dir list -> Yojson.Safe.t

  val registered_dirs_of_yojson :
    Yojson.Safe.t -> (registered_dir list, Rresult.R.msg) result

  val compare_versions : string -> string -> int
end
