(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(* Background runner that delegates to Domain_pool. Producers enqueue tasks
   from any thread; tasks run as fibers on pooled Eio domains. *)

let queue_depth = Atomic.make 0

let default_enqueue fn =
  let enqueued_at = Unix.gettimeofday () in
  let depth_after_enqueue = Atomic.fetch_and_add queue_depth 1 + 1 in
  Metrics.record_bg_enqueue ~queued_depth:depth_after_enqueue ;
  Domain_pool.submit (fun () ->
      let depth_after_take = Atomic.fetch_and_add queue_depth (-1) - 1 in
      let wait_ms = max 0. (Unix.gettimeofday () -. enqueued_at) *. 1000. in
      Metrics.record_bg_dequeue ~queued_depth:depth_after_take ~wait_ms ;
      try fn ()
      with exn ->
        Context.toast_error
          (Printf.sprintf "Background task failed: %s" (Printexc.to_string exn)))

let enqueue_ref : ((unit -> unit) -> unit) Atomic.t =
  Atomic.make default_enqueue

let enqueue fn = (Atomic.get enqueue_ref) fn

let shutdown () = ()

let submit_blocking ?on_complete f =
  let task () =
    Fun.protect ~finally:(fun () -> Option.iter (fun g -> g ()) on_complete) f
  in
  enqueue task

module For_tests = struct
  let with_synchronous_runner f =
    let original = Atomic.get enqueue_ref in
    Atomic.set enqueue_ref (fun fn -> fn ()) ;
    Fun.protect ~finally:(fun () -> Atomic.set enqueue_ref original) f
end
