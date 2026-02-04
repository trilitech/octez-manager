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

(** {1 Navigation} *)

(** Check if OpenAPI data is available for navigation. *)
val is_available : unit -> bool

(** Get navigation entries for a path from OpenAPI.
    Returns list of alternating [name; kind; name; kind; ...] where kind is
    "__SUB__", "__GET__", or "__DYN__".
    @param segs Path segments to navigate to *)
val entries_for : segs:string list -> string list

(** Clear cached trie and entries (useful when OpenAPI is re-downloaded). *)
val clear_cache : unit -> unit

module For_tests : sig
  type endpoint = {template : string; placeholders : string list}

  type node

  val parse_openapi_json : string -> endpoint list

  val extract_placeholders : string -> string list

  val extract_placeholder_name : string -> string option

  val build_trie : endpoint list -> node

  val traverse : node -> string list -> node option

  val with_prefix : string -> endpoint list -> endpoint list

  val node_has_get : node -> bool
end
