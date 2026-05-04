(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Cached status metrics for an octez-index instance. *)

type t = {head_level : int option; synced : bool option; last_check : float}

let table : (string, t) Hashtbl.t = Hashtbl.create 17

let lock = Mutex.create ()

let get ~instance =
  Mutex.protect lock (fun () -> Hashtbl.find_opt table instance)

let set ~instance v =
  Mutex.protect lock (fun () -> Hashtbl.replace table instance v)

let clear () = Mutex.protect lock (fun () -> Hashtbl.clear table)
