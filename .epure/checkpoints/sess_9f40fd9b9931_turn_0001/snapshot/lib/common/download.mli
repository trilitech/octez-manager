(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** HTTP downloads and checksums.

    The [download_file_with_progress] function supports a pluggable
    hook for non-blocking Eio-based execution in TUI mode. *)

(** {1 Hook registration} *)

(** Override [download_file_with_progress] with a non-blocking implementation. *)
val set_download_with_progress_hook :
  (url:string ->
  dest_path:string ->
  on_progress:(int -> int option -> unit) ->
  (unit, [`Msg of string]) result) ->
  unit

(** {1 Downloads} *)

(** Download a file using curl.
    @param quiet suppress output (default [false])
    @param url source URL
    @param dest_path local destination path *)
val download_file :
  ?quiet:bool ->
  url:string ->
  dest_path:string ->
  unit ->
  (unit, [> `Msg of string]) result

(** Download a file with progress reporting.

    The [on_progress] callback receives:
    - first parameter: bytes downloaded so far
    - second parameter: total file size in bytes (if known) *)
val download_file_with_progress :
  url:string ->
  dest_path:string ->
  on_progress:(int -> int option -> unit) ->
  (unit, [> `Msg of string]) result

(** Kill any active download process and clean up partial file.
    Call on app exit. *)
val kill_active_download : unit -> unit

(** Get the size of a remote file via HTTP HEAD / Content-Length. *)
val get_remote_file_size : string -> int64 option

(** Compute the SHA-256 hash of a file.
    @return [Ok hash] with the hex-encoded hash, or [Error] on failure. *)
val compute_sha256 : string -> (string, [> `Msg of string]) result
