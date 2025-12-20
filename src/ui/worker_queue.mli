(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** A single-threaded worker queue with request deduplication.

    Requests are identified by a string key. If a request with the same key
    is already pending, the new request is dropped. *)

type 'a t

(** Create a new worker queue.
    @param name Identifier for this queue (used in stats) *)
val create : name:string -> unit -> 'a t

(** Submit a request. Returns true if queued, false if deduplicated.
    @param key Unique identifier for deduplication
    @param work The function to execute
    @param on_complete Callback with result when done *)
val submit :
  'a t -> key:string -> work:(unit -> 'a) -> on_complete:('a -> unit) -> bool

(** Submit a unit-returning request without callback. *)
val submit_unit : unit t -> key:string -> work:(unit -> unit) -> unit

(** Start the worker domain. *)
val start : 'a t -> unit

(** Stop the worker. *)
val stop : 'a t -> unit

(** Per-key deduplication count *)
type key_dedup = {key : string; count : int}

(** Statistics for a worker queue *)
type stats = {
  name : string;
  requests_total : int;
  requests_deduped : int;
  deduped_by_key : key_dedup list;
      (** Per-key dedup counts, sorted by count desc *)
  p50_ms : float;
  p90_ms : float;
  p95_ms : float;
  p99_ms : float;
}

(** Get current statistics for this queue. *)
val get_stats : 'a t -> stats
