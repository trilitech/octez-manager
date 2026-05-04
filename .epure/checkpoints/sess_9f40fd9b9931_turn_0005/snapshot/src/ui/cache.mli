(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Centralized cache management for form validators and UI data.

    All caches are registered globally and can be invalidated together. *)

(** {1 Cache Registry} *)

(** Invalidate all registered caches. Call from diagnostics page. *)
val invalidate_all : unit -> unit

(** Sub-entry for keyed caches showing per-key status *)
type sub_entry = {key : string; age : float; expired : bool}

(** Register a custom cache with the global registry.
    Use this for caches that don't use [Cache.create] but still need
    to be invalidated by [invalidate_all]. *)
val register :
  name:string ->
  invalidate:(unit -> unit) ->
  get_age:(unit -> float option) ->
  get_ttl:(unit -> float) ->
  get_sub_entries:(unit -> sub_entry list) ->
  unit

(** Get cache statistics: (name, hits, misses, age_secs, ttl_secs, expired, sub_entries) for each cache.
    age_secs is None if cache is empty. sub_entries is empty for non-keyed caches. *)
val get_stats :
  unit ->
  (string * int * int * float option * float * bool * sub_entry list) list

(** Reset hit/miss counters for all caches. *)
val reset_stats : unit -> unit

(** {1 Generic TTL Cache}

    For caching expensive computations with automatic expiration. *)

type 'a t

(** Create a cache with the given TTL (in seconds) and fetch function.
    The cache is automatically registered for global invalidation. *)
val create : name:string -> ttl:float -> (unit -> 'a) -> 'a t

(** Get cached value, refreshing if expired. *)
val get : 'a t -> 'a

(** Get cached value without triggering refresh, even if expired.
    Returns None only if nothing has ever been cached.
    Use for validators during typing to avoid blocking syscalls. *)
val get_cached : 'a t -> 'a option

(** Manually invalidate this cache. *)
val invalidate : 'a t -> unit

(** {1 Result Cache}

    For caching operations that may fail. Only successful results are cached. *)

type 'a result_cache

(** Create a result cache. Failed fetches are not cached. *)
val create_result :
  name:string -> ttl:float -> (unit -> ('a, string) result) -> 'a result_cache

(** Get cached value or fetch. Errors are not cached. *)
val get_result : 'a result_cache -> ('a, string) result

(** Manually invalidate this cache. *)
val invalidate_result : 'a result_cache -> unit

(** {1 Keyed Cache}

    For caching per-key lookups (e.g., user validation by username). *)

type ('k, 'v) keyed_cache

(** Create a keyed cache. All entries expire together when TTL passes. *)
val create_keyed :
  name:string -> ttl:float -> ('k -> 'v) -> ('k, 'v) keyed_cache

(** Get cached value for key, computing if not present or expired. *)
val get_keyed : ('k, 'v) keyed_cache -> 'k -> 'v

(** Manually invalidate all entries in this cache. *)
val invalidate_keyed : ('k, 'v) keyed_cache -> unit

(** {1 Thread-safe Keyed Cache with Per-Key TTL}

    For caches that need mutex protection and per-key expiration.
    Each entry has its own timestamp and expires independently. *)

type ('k, 'v) safe_keyed_cache

(** Create a thread-safe string-keyed cache with per-key TTL.
    Each entry expires independently after [ttl] seconds.
    This is the common case - use [create_safe_keyed_custom] for non-string keys. *)
val create_safe_keyed :
  name:string -> ttl:float -> unit -> (string, 'v) safe_keyed_cache

(** Create a thread-safe keyed cache with custom key type.
    [key_to_string] converts keys to strings for diagnostics display. *)
val create_safe_keyed_custom :
  key_to_string:('k -> string) ->
  name:string ->
  ttl:float ->
  unit ->
  ('k, 'v) safe_keyed_cache

(** Get value for key, fetching if expired or missing. Thread-safe. *)
val get_safe_keyed : ('k, 'v) safe_keyed_cache -> 'k -> fetch:(unit -> 'v) -> 'v

(** Set value for key, updating timestamp. Thread-safe. *)
val set_safe_keyed : ('k, 'v) safe_keyed_cache -> 'k -> 'v -> unit

(** Get cached value without fetching. Returns None if missing. Thread-safe. *)
val get_safe_keyed_cached : ('k, 'v) safe_keyed_cache -> 'k -> 'v option

(** Remove a specific key from cache. Thread-safe. *)
val remove_safe_keyed : ('k, 'v) safe_keyed_cache -> 'k -> unit

(** Invalidate all entries. Thread-safe. *)
val invalidate_safe_keyed : ('k, 'v) safe_keyed_cache -> unit
