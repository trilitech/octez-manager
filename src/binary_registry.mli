(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Binary source types - how an instance references its Octez binaries *)
type bin_source =
  | Managed_version of string  (** Downloaded/managed version e.g. "24.0" *)
  | Linked_alias of string  (** Linked directory alias e.g. "dev-build" *)
  | Raw_path of string  (** Raw filesystem path e.g. "/usr/local/bin" *)

(** Linked directory entry *)
type linked_dir = {alias : string; path : string}

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

(** {2 Linked directories management} *)

(** Path to linked-directories.json *)
val linked_dirs_file : unit -> string

(** Load all linked directories *)
val load_linked_dirs : unit -> (linked_dir list, Rresult.R.msg) result

(** Save linked directories *)
val save_linked_dirs : linked_dir list -> (unit, Rresult.R.msg) result

(** Find a linked directory by alias *)
val find_linked_dir : string -> (linked_dir option, Rresult.R.msg) result

(** Add a new linked directory. Fails if alias already exists. *)
val add_linked_dir : alias:string -> path:string -> (unit, Rresult.R.msg) result

(** Remove a linked directory by alias *)
val remove_linked_dir : string -> (unit, Rresult.R.msg) result

(** Rename a linked directory alias *)
val rename_linked_dir :
  old_alias:string -> new_alias:string -> (unit, Rresult.R.msg) result

(** {2 Managed versions} *)

(** Compare version strings numerically (e.g., "24.0" > "9.0").
    Returns: negative if a < b, 0 if a = b, positive if a > b *)
val compare_versions : string -> string -> int

(** List all installed managed versions *)
val list_managed_versions : unit -> (string list, Rresult.R.msg) result

(** Check if a managed version is installed *)
val managed_version_exists : string -> bool
