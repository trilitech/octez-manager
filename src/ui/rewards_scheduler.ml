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

(* ── Continual mode state ────────────────────────────── *)

(* Track the last cycle we saw per instance (for cycle transition detection) *)
let continual_last_cycle : (string, int) Hashtbl.t = Hashtbl.create 4

(* The time at which we should trigger payouts (random delay) *)
let continual_delay_until : (string, float) Hashtbl.t = Hashtbl.create 4

let continual_lock = Mutex.create ()

(** Sync continual mode from config (enable if config says so). *)
let sync_continual_from_config ~instance =
  match Payout_config.load ~instance with
  | Ok c when c.continual_enabled -> Payout_continual.enable ~instance
  | Ok _ -> ()
  | Error _ -> ()

(** Check for cycle transition and trigger continual payouts when due. *)
let check_continual ~instance ~(svc : Data.Service_state.t) =
  sync_continual_from_config ~instance ;
  if not (Payout_continual.is_active ~instance) then ()
  else
    match (Atomic.get current_cycle_ref, Payout_config.load ~instance) with
    | Some current_cycle, Ok config ->
        let prev =
          Mutex.protect continual_lock (fun () ->
              Hashtbl.find_opt continual_last_cycle instance)
        in
        (* Detect cycle transition: new cycle appeared *)
        (match prev with
        | Some p when p >= current_cycle -> ()
        | _ ->
            Mutex.protect continual_lock (fun () ->
                Hashtbl.replace continual_last_cycle instance current_cycle) ;
            (* Schedule payout with random delay *)
            let range = config.max_delay_blocks - config.min_delay_blocks in
            let delay_blocks =
              config.min_delay_blocks
              + if range > 0 then Random.int (range + 1) else 0
            in
            (* ~15 seconds per block on Tezos *)
            let delay_seconds = Float.of_int delay_blocks *. 15.0 in
            let trigger_at = Unix.gettimeofday () +. delay_seconds in
            Mutex.protect continual_lock (fun () ->
                Hashtbl.replace continual_delay_until instance trigger_at)) ;
        (* Check if the delay has expired *)
        let ready =
          Mutex.protect continual_lock (fun () ->
              match Hashtbl.find_opt continual_delay_until instance with
              | Some t when Unix.gettimeofday () >= t ->
                  Hashtbl.remove continual_delay_until instance ;
                  true
              | _ -> false)
        in
        if ready then
          let service = svc.service in
          let octez_client_bin =
            Filename.concat service.Service.app_bin_dir "octez-client"
          in
          let endpoint =
            Delegate_scheduler.get_baker_node_endpoint ~instance
            |> Option.value
                 ~default:
                   ("http://" ^ Rpc_addr.to_string service.Service.rpc_addr)
          in
          let ctx : Payout_executor.context =
            {
              octez_client_bin;
              endpoint;
              base_dir = None;
              password_file = None;
              payout_key_alias = config.payout_key_alias;
              instance;
            }
          in
          let results =
            Payout_continual.pay_due_cycles
              ~ctx
              ~baker:config.baker_pkh
              ~network:service.network
              ~current_cycle
              ~interval:config.continual_interval
              ~offset:config.continual_offset
          in
          List.iter
            (fun (cycle, result) ->
              match result with
              | Ok () ->
                  Context.toast_info
                    (Printf.sprintf
                       "Continual: cycle %d paid for %s"
                       cycle
                       instance)
              | Error msg ->
                  Context.toast_warn
                    (Printf.sprintf
                       "Continual: cycle %d failed for %s: %s"
                       cycle
                       instance
                       msg))
            results
    | _ -> ()

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
        String.equal st.service.Service.role "baker")
  in
  List.iter
    (fun (st : Data.Service_state.t) ->
      let instance = st.service.Service.instance in
      poll_baker ~instance ;
      check_continual ~instance ~svc:st)
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
  Atomic.set current_cycle_ref None ;
  Mutex.protect continual_lock (fun () ->
      Hashtbl.clear continual_last_cycle ;
      Hashtbl.clear continual_delay_until)
