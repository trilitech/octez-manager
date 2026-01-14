(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  last_notify : float Atomic.t; (* Timestamp of last notification *)
  debounce_s : float; (* Minimum time between refreshes *)
}

let create ?(debounce_s = 0.08) () = {last_notify = Atomic.make 0.0; debounce_s}

let notify t = Atomic.set t.last_notify (Unix.gettimeofday ())

let should_refresh t =
  let last = Atomic.get t.last_notify in
  if last = 0.0 then false (* No pending notification *)
  else
    let now = Unix.gettimeofday () in
    now -. last >= t.debounce_s

let mark_refreshed t = Atomic.set t.last_notify 0.0

let get_debounce t = t.debounce_s
