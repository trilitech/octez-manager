(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC /describe endpoint parser.

    Used for navigating Octez node RPC endpoints by fetching the /describe
    endpoint which returns schema information about available sub-endpoints. *)

open Octez_manager_lib

(** Source of endpoint information. *)
type describe_source = [`Describe | `None]

(** Entry kind for display. *)
type entry_kind =
  | Sub  (** Static subdirectory *)
  | Get  (** GET endpoint *)
  | Dyn of string  (** Dynamic segment with argument name *)

(** A parsed entry from /describe. *)
type entry = {
  name : string;  (** Display name *)
  kind : entry_kind;  (** Entry kind *)
}

(** {1 Fetching Entries} *)

(** Fetch entries at a given path.

    Tries both prefix form (/describe/path) and suffix form (path/describe).
    Returns empty list with [`None] if all methods fail.

    @param service The service to query
    @param segs Path segments (e.g., ["chains"; "main"])
    @return (entries, source) *)
val fetch_entries :
  Service.t -> segs:string list -> entry list * describe_source

(** {1 Description Text} *)

(** Get description text for an endpoint.

    @param service The service to query
    @param segs Path segments
    @return Description text if available *)
val fetch_description : Service.t -> segs:string list -> string option

(** {1 Cache Management} *)

(** Clear cached describe results. *)
val clear_cache : unit -> unit

(** {1 Low-level Parsing} *)

(** Build candidate /describe URLs to try.
    Returns both prefix and suffix forms for non-root paths. *)
val candidate_paths : string list -> string list

(** Parse /describe JSON response into entries.
    Exposed for testing. *)
val parse_describe_json : Yojson.Safe.t -> entry list

(** Parse description text from /describe JSON response. *)
val parse_description : Yojson.Safe.t -> string option
