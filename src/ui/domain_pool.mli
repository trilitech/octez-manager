(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Fixed-size Eio domain pool for background I/O.

    Work is submitted via {!submit} and executed as fibers on pooled domains.
    Each domain hosts its own Eio event loop, so fibers that call
    [Eio.Process], [Eio_unix.sleep], etc. yield cooperatively without
    blocking other fibers on the same domain.

    Typical usage:
    {[
      let pool = Domain_pool.create ~sw ~domain_mgr ~num_domains:4 in
      Domain_pool.set pool;     (* store in module-level ref *)
      Domain_pool.submit (fun () -> ...) (* from any thread *)
    ]} *)

type t

(** Create a domain pool.

    @param sw Parent switch — pool domains are forked as daemon fibers.
    @param domain_mgr Eio domain manager from the environment.
    @param num_domains Number of domains (OS threads) in the pool. *)
val create :
  sw:Eio.Switch.t -> domain_mgr:_ Eio.Domain_manager.t -> num_domains:int -> t

(** Submit a task to the pool. The task runs as a fiber on one of the
    pooled domains. Safe to call from any thread/domain. *)
val submit : (unit -> unit) -> unit

(** Store the global pool reference. Call once at startup. *)
val set : t -> unit

(** Shutdown the pool. Signals all worker domains to stop. *)
val shutdown : unit -> unit
