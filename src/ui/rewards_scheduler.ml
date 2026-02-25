(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
open Octez_manager_rewards

let poll_interval = 60.0

let shutdown_requested = Atomic.make false

(* Cycle data cache: keyed by (baker_pkh, cycle) *)
let cycle_cache : (string * int, Rewards.cycle_rewards) Hashtbl.t =
  Hashtbl.create 64

let cycle_lock = Mutex.create ()

(* Recent cycles cache: keyed by baker_pkh *)
let recent_cache : (string, Rewards.cycle_rewards list) Hashtbl.t =
  Hashtbl.create 8

let recent_lock = Mutex.create ()

(* Current cycle *)
let current_cycle_ref : int option Atomic.t = Atomic.make None

(* Cache accessors — safe for view functions *)

let get_cycle_data ~baker ~cycle =
  Mutex.protect cycle_lock (fun () ->
      Hashtbl.find_opt cycle_cache (baker, cycle))

let get_recent_cycles ~baker =
  Mutex.protect recent_lock (fun () ->
      Hashtbl.find_opt recent_cache baker |> Option.value ~default:[])

let get_current_cycle () = Atomic.get current_cycle_ref

let get_payout_status ~instance ~cycle =
  if Payout_report.cycle_is_paid ~instance ~cycle then Rewards.Paid
  else Rewards.Unpaid

(* Polling logic *)

let poll_baker ~instance =
  (* Read config to get baker_pkh and tzkt_url *)
  match Payout_config.load ~instance with
  | Error _ -> ()
  | Ok config -> (
      let baker = config.baker_pkh in
      let tzkt_url = config.tzkt_url in
      (* Fetch recent cycles *)
      (match Cycle_data.fetch_recent_cycles ~tzkt_url ~baker ~limit:10 with
      | Error _ -> ()
      | Ok cycles ->
          Mutex.protect recent_lock (fun () ->
              Hashtbl.replace recent_cache baker cycles) ;
          (* Also populate individual cycle cache *)
          Mutex.protect cycle_lock (fun () ->
              List.iter
                (fun (cr : Rewards.cycle_rewards) ->
                  Hashtbl.replace cycle_cache (baker, cr.cycle) cr)
                cycles)) ;
      (* Fetch current cycle *)
      match Cycle_data.fetch_current_cycle ~tzkt_url with
      | Error _ -> ()
      | Ok c -> Atomic.set current_cycle_ref (Some c))

let refresh_baker ~instance = poll_baker ~instance

let tick () =
  let bakers =
    Data.load_service_states ()
    |> List.filter (fun (st : Data.Service_state.t) ->
        st.service.Service.role = "baker")
  in
  List.iter
    (fun (st : Data.Service_state.t) ->
      poll_baker ~instance:st.service.Service.instance)
    bakers

let started = ref false

let last_poll = ref 0.0

let start () =
  if not !started then (
    started := true ;
    Domain_pool.submit (fun () ->
        Eio_unix.sleep 3.0 ;
        while not (Atomic.get shutdown_requested) do
          let now = Unix.gettimeofday () in
          if now -. !last_poll >= poll_interval then (
            last_poll := now ;
            Metrics.record_scheduler_tick ~scheduler:"rewards" tick) ;
          Eio_unix.sleep 5.0
        done))

let shutdown () = Atomic.set shutdown_requested true

let clear () =
  Mutex.protect cycle_lock (fun () -> Hashtbl.clear cycle_cache) ;
  Mutex.protect recent_lock (fun () -> Hashtbl.clear recent_cache) ;
  Atomic.set current_cycle_ref None
