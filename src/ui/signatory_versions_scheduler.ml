(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** In-memory cache for available Signatory versions
    
    This scheduler periodically fetches the list of available versions
    from the remote repository and caches them in RAM. This avoids
    repeated network calls during TUI rendering while ensuring the
    data is kept reasonably fresh. *)

open Octez_manager_lib

(** Cache storage *)
let cache : Signatory_downloader.version_info list option ref = ref None

let lock = Mutex.create ()

let shutdown_requested = Atomic.make false

(** Get cached versions (returns None if not yet fetched) *)
let get_cached () =
  Mutex.lock lock ;
  try
    let v = !cache in
    Mutex.unlock lock ;
    v
  with e ->
    Mutex.unlock lock ;
    raise e

(** Fetch and update cache *)
let refresh () =
  match
    Signatory_downloader.fetch_versions
      ~include_prerelease:(Prerelease_flag.get ())
      ()
  with
  | Ok versions -> (
      Mutex.lock lock ;
      try
        cache := Some versions ;
        Mutex.unlock lock
      with e ->
        Mutex.unlock lock ;
        raise e)
  | Error _ ->
      (* Keep existing cache on error *)
      ()

(** Start background scheduler *)
let start () =
  (* Initial fetch *)
  refresh () ;
  (* Submit background refresh fiber to domain pool *)
  Domain_pool.submit (fun () ->
      while not (Atomic.get shutdown_requested) do
        Eio_unix.sleep 300.0 ;
        (* 5 minutes *)
        refresh ()
      done)

(** Shutdown scheduler *)
let shutdown () = Atomic.set shutdown_requested true

(** Clear cache (for testing) *)
let clear () =
  Mutex.lock lock ;
  try
    cache := None ;
    Mutex.unlock lock
  with e ->
    Mutex.unlock lock ;
    raise e
