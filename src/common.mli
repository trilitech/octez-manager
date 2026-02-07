(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

val is_root : unit -> bool

val home_dir : unit -> string

val xdg_config_home : unit -> string

val xdg_data_home : unit -> string

val xdg_state_home : unit -> string

val current_user_group_names : unit -> string * string

val env_instances_base_dir : unit -> string

(** Root directory for octez-manager registries (services, directories).
    Returns ["/etc/octez_manager"] when running as root,
    otherwise ["$XDG_CONFIG_HOME/octez-manager"]. *)
val registry_root : unit -> string

val default_data_dir : string -> string

val default_role_dir : string -> string -> string

val default_log_dir : role:string -> instance:string -> string

val which : string -> string option

val make_absolute_path : string -> (string, string) result

val ensure_dir_path :
  owner:string ->
  group:string ->
  mode:int ->
  string ->
  (unit, Rresult.R.msg) result

val ensure_tree_owner :
  owner:string -> group:string -> string -> (unit, Rresult.R.msg) result

val write_file :
  mode:int ->
  owner:string ->
  group:string ->
  string ->
  string ->
  (unit, Rresult.R.msg) result

val append_debug_log : string -> unit

(** Execute [f] while holding an exclusive file lock on [lock_path].
    The lock is released when [f] returns or raises. Uses [Unix.lockf]
    with [F_LOCK] (blocking). The lock file is created if it doesn't exist. *)
val with_file_lock : string -> (unit -> 'a) -> 'a

(** Sleep for up to [seconds], checking [stop_flag] every 0.5s.
    Returns early when [stop_flag] becomes [true], allowing background
    domains to shut down promptly instead of blocking for the full duration. *)
val interruptible_sleep : bool Atomic.t -> float -> unit

val run :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  string list ->
  (unit, [> `Msg of string]) result

val run_silent :
  ?on_log:(string -> unit) -> string list -> (unit, [> `Msg of string]) result

val run_verbose :
  ?on_log:(string -> unit) -> string list -> (unit, [> `Msg of string]) result

(** Run command with streaming output. Reads stdout and stderr concurrently
    using Unix.select, ensuring output is captured as produced. Use this for
    long-running commands where real-time output is needed. *)
val run_streaming :
  on_log:(string -> unit) -> string list -> (unit, [> `Msg of string]) result

val run_out : string list -> (string, [> `Msg of string]) result

(** Like run_out but captures stderr to prevent it from leaking to the terminal.
    Returns only stdout. Useful for HTTP operations in the TUI where curl errors
    would corrupt the display. *)
val run_out_silent : string list -> (string, [> `Msg of string]) result

val run_as :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  user:string ->
  string list ->
  (unit, [> `Msg of string]) result

val download_file :
  ?quiet:bool ->
  url:string ->
  dest_path:string ->
  unit ->
  (unit, [> `Msg of string]) result

(** Download a file with progress reporting.
    
    The [on_progress] callback receives:
    - First parameter: bytes downloaded so far
    - Second parameter: total file size in bytes (if known)
    
    Both values are reported as [int] (byte counts). *)
val download_file_with_progress :
  url:string ->
  dest_path:string ->
  on_progress:(int -> int option -> unit) ->
  (unit, [> `Msg of string]) result

(** Kill any active download process and clean up partial file. Call on exit. *)
val kill_active_download : unit -> unit

val sh_quote : string -> string

val cmd_to_string : string list -> string

val remove_path : string -> unit

val remove_tree : string -> (unit, [> `Msg of string]) result

val copy_file : string -> string -> (unit, [> `Msg of string]) result

val get_remote_file_size : string -> int64 option

val get_available_space : string -> int64 option

(** Get the filesystem device ID for a path. *)
val get_filesystem_id : string -> int option

(** Check if two paths are on the same filesystem.
    Returns [Some true] if same, [Some false] if different, [None] if unknown. *)
val same_filesystem : string -> string -> bool option

(** Map Octez exit codes to human-readable descriptions. *)
val octez_exit_code_description : int -> string

(** {1 Editor Integration} *)

(** Get the user's preferred editor from environment variables.
    Tries $VISUAL, $EDITOR, then falls back to sensible-editor or vi.
    
    @return Editor command/path *)
val get_editor : unit -> string

(** Open a file in the user's preferred editor.
    Blocks until the editor exits.
    
    @param file_path Path to the file to edit
    @return Ok () if editor exited successfully, Error otherwise *)
val open_in_editor : string -> (unit, Rresult.R.msg) result

(** {1 String Utilities} *)

(** Check whether [needle] is a substring of [haystack]. *)
val string_contains : needle:string -> string -> bool

(** {1 Timestamp Utilities} *)

(** Format the current local time as ["YYYY-MM-DD HH:MM:SS"]. *)
val now : unit -> string

(** {1 Checksum Utilities} *)

(** Compute the SHA-256 hash of a file.

    @return [Ok hash] with the hex-encoded hash, or [Error] on failure. *)
val compute_sha256 : string -> (string, [> `Msg of string]) result

(** {1 Directory Utilities} *)

(** Get the size of a directory in bytes using [du -sb].
    Returns [None] if the path does not exist or the command fails. *)
val get_dir_size : string -> int64 option

(** {1 Size Formatting} *)

(** Format a byte count as a human-readable string using integer
    division (truncating).  Produces e.g. ["3 GB"], ["450 MB"],
    ["12 KB"], ["800 bytes"]. *)
val format_size : int64 -> string

(** Format a byte count as a compact human-readable string with float
    precision.  Produces e.g. ["1.2G"], ["450M"], ["12K"], ["800B"].
    Handles values up to terabytes. *)
val format_bytes : int64 -> string

(** Remove surrounding quotes from a string.
    Handles backslash-escaped double quotes, regular double quotes,
    and single quotes.
    Returns the string unchanged if it is not quoted. *)
val unquote : string -> string

(** Format a byte count as a human-readable string with float precision
    and spaced units.  Produces e.g. ["1.5 GB"], ["100 MB"], ["512 bytes"].
    Best for disk-space messages shown to the user. *)
val format_size_float : int64 -> string
