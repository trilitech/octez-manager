(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** File and directory manipulation. *)

(** Recursively create directories along [path] (like [mkdir -p]).
    Uses mode [0o755]. No-op if the path already exists. *)
val mkdir_p : string -> unit

(** Create a directory tree, setting ownership and permissions.
    When running as root, sets the owner/group/mode on the final
    directory; otherwise ownership is set on a best-effort basis. *)
val ensure_dir_path :
  owner:string ->
  group:string ->
  mode:int ->
  string ->
  (unit, [> `Msg of string]) result

(** Atomically write a file via a temp file + rename.
    Creates parent directories as needed. *)
val write_file :
  mode:int ->
  owner:string ->
  group:string ->
  string ->
  string ->
  (unit, [> `Msg of string]) result

(** Execute [f] while holding an exclusive file lock on [lock_path].
    The lock is released when [f] returns or raises.  Uses [Unix.lockf]
    with [F_LOCK] (blocking).  The lock file is created if absent. *)
val with_file_lock : string -> (unit -> 'a) -> 'a

(** Recursively [chown -R owner:group path].  No-op when not root
    or when [path] does not exist. *)
val ensure_tree_owner :
  owner:string -> group:string -> string -> (unit, [> `Msg of string]) result

(** Remove a single file.  No-op if the file does not exist. *)
val remove_path : string -> unit

(** Recursively remove a directory tree. *)
val remove_tree : string -> (unit, [> `Msg of string]) result

(** Copy a file, preserving permissions and ownership (when root). *)
val copy_file : string -> string -> (unit, [> `Msg of string]) result

(** Available disk space on the filesystem containing [dir], in bytes. *)
val get_available_space : string -> int64 option

(** Filesystem device ID for [path]. *)
val get_filesystem_id : string -> int option

(** Check if two paths are on the same filesystem.
    Returns [Some true] if same, [Some false] if different,
    [None] if unknown. *)
val same_filesystem : string -> string -> bool option

(** Size of a directory in bytes using [du -sb].
    Returns [None] if the path does not exist or the command fails. *)
val get_dir_size : string -> int64 option
