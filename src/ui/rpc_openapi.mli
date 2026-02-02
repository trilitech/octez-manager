(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC OpenAPI spec management.

    Handles downloading, caching, and providing OpenAPI specs for RPC Browser. *)

(** {1 Paths} *)

(** Directory where OpenAPI specs are stored. *)
val openapi_dir : unit -> string

(** Path to main OpenAPI spec file. *)
val openapi_path : unit -> string

(** {1 Status} *)

(** Download status. *)
type status =
  | NotDownloaded  (** Specs not yet downloaded *)
  | Downloading  (** Download in progress *)
  | Ready  (** Specs available *)
  | Error of string  (** Download failed *)

(** Get current download status. *)
val get_status : unit -> status

(** {1 Download} *)

(** Check if OpenAPI files need to be downloaded.
    @return true if files are missing *)
val needs_download : unit -> bool

(** Download OpenAPI specs in background.
    Shows progress via Job_manager.
    @param on_complete Called when download finishes *)
val download_async : on_complete:(status -> unit) -> unit

(** Download OpenAPI specs synchronously.
    @return Ok () on success, Error msg on failure *)
val download_sync : unit -> (unit, string) result

(** {1 Spec Access} *)

(** Read OpenAPI spec from cache.
    @return JSON string if available *)
val read_spec : unit -> string option
