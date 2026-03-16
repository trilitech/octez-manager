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

(* Current cycle per instance *)
let current_cycle_cache : (string, int) Hashtbl.t = Hashtbl.create 4

let current_cycle_lock = Mutex.create ()

(* Cache accessors — safe for view functions *)

let get_cycle_data ~baker ~cycle =
  Mutex.protect cycle_lock (fun () ->
      Hashtbl.find_opt cycle_cache (baker, cycle))

let get_recent_cycles ~baker =
  Mutex.protect recent_lock (fun () ->
      Hashtbl.find_opt recent_cache baker |> Option.value ~default:[])

let get_current_cycle ~instance =
  Mutex.protect current_cycle_lock (fun () ->
      Hashtbl.find_opt current_cycle_cache instance)

(* Payout status cache: keyed by (instance, cycle) *)
let payout_status_cache : (string * int, Rewards.payout_status) Hashtbl.t =
  Hashtbl.create 64

let payout_status_lock = Mutex.create ()

(* Track payouts currently being executed *)
let in_progress_payouts : (string * int, unit) Hashtbl.t = Hashtbl.create 4

let in_progress_lock = Mutex.create ()

(* Payout summary cache: keyed by (instance, cycle) *)
let summary_cache : (string * int, Rewards.cycle_summary) Hashtbl.t =
  Hashtbl.create 64

let summary_lock = Mutex.create ()

let get_payout_summary ~instance ~cycle =
  Mutex.protect summary_lock (fun () ->
      Hashtbl.find_opt summary_cache (instance, cycle))

let get_payout_status ~instance ~cycle =
  let in_prog =
    Mutex.protect in_progress_lock (fun () ->
        Hashtbl.mem in_progress_payouts (instance, cycle))
  in
  if in_prog then Rewards.In_progress
  else
    Mutex.protect payout_status_lock (fun () ->
        Hashtbl.find_opt payout_status_cache (instance, cycle)
        |> Option.value ~default:Rewards.Unpaid)

let refresh_payout_status ~instance ~cycle =
  match Payout_report.read_summary_json ~instance ~cycle with
  | Ok summary ->
      Mutex.protect summary_lock (fun () ->
          Hashtbl.replace summary_cache (instance, cycle) summary) ;
      let status =
        if summary.paid_delegators < summary.delegators then Rewards.Partial
        else Rewards.Paid
      in
      Mutex.protect payout_status_lock (fun () ->
          Hashtbl.replace payout_status_cache (instance, cycle) status)
  | Error _ ->
      if Payout_report.cycle_is_paid ~instance ~cycle then
        Mutex.protect payout_status_lock (fun () ->
            Hashtbl.replace payout_status_cache (instance, cycle) Rewards.Paid)

let mark_in_progress ~instance ~cycle =
  Mutex.protect in_progress_lock (fun () ->
      Hashtbl.replace in_progress_payouts (instance, cycle) ())

let clear_in_progress ~instance ~cycle =
  Mutex.protect in_progress_lock (fun () ->
      Hashtbl.remove in_progress_payouts (instance, cycle))

(* Auto-detected baker address per instance *)
let baker_instance_cache : (string, string) Hashtbl.t = Hashtbl.create 4

let baker_instance_lock = Mutex.create ()

let get_baker_for_instance ~instance =
  Mutex.protect baker_instance_lock (fun () ->
      Hashtbl.find_opt baker_instance_cache instance)

(* Network per instance (for test bakers that lack a service registry entry) *)
let network_cache : (string, string) Hashtbl.t = Hashtbl.create 4

let network_lock = Mutex.create ()

let get_network_for_instance ~instance =
  Mutex.protect network_lock (fun () -> Hashtbl.find_opt network_cache instance)

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
    match (get_current_cycle ~instance, Payout_config.load ~instance) with
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
                 ~default:(Rpc_addr.to_endpoint service.Service.rpc_addr)
          in
          let base_dir =
            match Node_env.read ~inst:instance with
            | Ok pairs -> (
                match List.assoc_opt "OCTEZ_CLIENT_BASE_DIR" pairs with
                | Some d -> Some d
                | None -> List.assoc_opt "OCTEZ_BAKER_BASE_DIR" pairs)
            | Error _ -> None
          in
          let ctx : Payout_executor.context =
            {
              octez_client_bin;
              endpoint;
              base_dir;
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

(** Try fetching recent cycles for [baker]. Returns [Some cycles] on success
    with at least one cycle, [None] otherwise. *)
let try_fetch_baker ~network ~preferred_base ~baker =
  match
    Cycle_data.fetch_recent_cycles ~network ~preferred_base ~baker ~limit:10
  with
  | Ok (_ :: _ as cycles) -> Some cycles
  | Ok [] | Error _ -> None

(** Cache cycles in both [recent_cache] and [cycle_cache]. *)
let cache_cycles ~baker cycles =
  Mutex.protect recent_lock (fun () ->
      Hashtbl.replace recent_cache baker cycles) ;
  Mutex.protect cycle_lock (fun () ->
      List.iter
        (fun (cr : Rewards.cycle_rewards) ->
          Hashtbl.replace cycle_cache (baker, cr.cycle) cr)
        cycles)

let poll_baker ~instance ~network =
  Mutex.protect network_lock (fun () ->
      Hashtbl.replace network_cache instance network) ;
  let delegates = Delegate_scheduler.get_baker_delegates ~instance in
  let config_opt =
    match Payout_config.load ~instance with Ok c -> Some c | Error _ -> None
  in
  let preferred_base =
    Option.map (fun c -> Payout_config.effective_tzkt_url ~network c) config_opt
  in
  (* Try the configured baker first, then fall back to each delegate,
     then the cached baker (for test bakers from OM_TEST_BAKER). *)
  let configured_baker =
    match config_opt with
    | Some c -> Some c.baker_pkh
    | None -> get_baker_for_instance ~instance
  in
  let result =
    (* 1. Try the configured baker (if any) *)
    let from_config =
      match configured_baker with
      | Some baker -> (
          match try_fetch_baker ~network ~preferred_base ~baker with
          | Some cycles -> Some (baker, cycles)
          | None -> None)
      | None -> None
    in
    match from_config with
    | Some _ -> from_config
    | None ->
        (* 2. Try each delegate key to find the registered baker *)
        let candidates =
          match configured_baker with
          | Some cb -> List.filter (fun d -> not (String.equal d cb)) delegates
          | None -> delegates
        in
        List.find_map
          (fun baker ->
            match try_fetch_baker ~network ~preferred_base ~baker with
            | Some cycles -> Some (baker, cycles)
            | None -> None)
          candidates
  in
  (match result with
  | Some (baker, cycles) ->
      cache_cycles ~baker cycles ;
      (* Backfill delegator details for cycles that lack them.
         The list endpoint does not return the delegators array,
         so we fetch each cycle individually via the split endpoint. *)
      let to_backfill =
        List.filteri
          (fun i (cr : Rewards.cycle_rewards) ->
            i < 3 && cr.num_delegators > 0 && cr.delegators = [])
          cycles
      in
      List.iter
        (fun (cr : Rewards.cycle_rewards) ->
          match
            Cycle_data.fetch_cycle
              ~network
              ~preferred_base
              ~baker
              ~cycle:cr.cycle
          with
          | Ok full_cr ->
              Mutex.protect cycle_lock (fun () ->
                  Hashtbl.replace cycle_cache (baker, cr.cycle) full_cr)
          | Error _ -> ())
        to_backfill ;
      (* Cache the detected baker for the page to read *)
      Mutex.protect baker_instance_lock (fun () ->
          Hashtbl.replace baker_instance_cache instance baker) ;
      (* If no config exists or baker_pkh was wrong, save the correct one *)
      let need_save =
        match config_opt with
        | None -> true
        | Some c -> not (String.equal c.baker_pkh baker)
      in
      if need_save then begin
        let default_tzkt_url = Indexer.tzkt_base_url ~network in
        let config =
          match config_opt with
          | Some c -> {c with baker_pkh = baker}
          | None ->
              {
                (Payout_config.default ~network ~baker_pkh:baker ()) with
                tzkt_url = default_tzkt_url;
                explorer_url =
                  (if String.equal network "mainnet" then "https://tzkt.io"
                   else Printf.sprintf "https://%s.tzkt.io" network);
              }
        in
        ignore (Payout_config.save ~instance config)
      end
  | None -> (
      (* No delegate returned data — cache the first delegate as placeholder *)
      match delegates with
      | pkh :: _ ->
          Mutex.protect baker_instance_lock (fun () ->
              Hashtbl.replace baker_instance_cache instance pkh)
      | [] -> ())) ;
  (* Refresh payout status for each cached cycle *)
  (match result with
  | Some (_, cycles) ->
      List.iter
        (fun (cr : Rewards.cycle_rewards) ->
          refresh_payout_status ~instance ~cycle:cr.cycle)
        cycles
  | None -> ()) ;
  (* Fetch current cycle *)
  match Cycle_data.fetch_current_cycle ~network ~preferred_base with
  | Error _ -> ()
  | Ok c ->
      Mutex.protect current_cycle_lock (fun () ->
          Hashtbl.replace current_cycle_cache instance c)

let ensure_cycle_detail ~instance ~baker ~cycle =
  let needs_fetch =
    match get_cycle_data ~baker ~cycle with
    | None -> true
    | Some cr -> cr.delegators = [] && cr.num_delegators > 0
  in
  if needs_fetch then begin
    let network =
      Mutex.protect network_lock (fun () ->
          Hashtbl.find_opt network_cache instance)
      |> Option.value ~default:"mainnet"
    in
    let preferred_base =
      match Payout_config.load ~instance with
      | Ok c -> Some (Payout_config.effective_tzkt_url ~network c)
      | Error _ -> None
    in
    ignore
      (Domain_pool.submit (fun () ->
           match
             Cycle_data.fetch_cycle ~network ~preferred_base ~baker ~cycle
           with
           | Ok full_cr ->
               Mutex.protect cycle_lock (fun () ->
                   Hashtbl.replace cycle_cache (baker, cycle) full_cr)
           | Error _ -> ()))
  end

let refresh_baker ~instance =
  let network =
    match Service_registry.find ~instance with
    | Ok (Some svc) -> svc.Service.network
    | _ -> "mainnet"
  in
  poll_baker ~instance ~network

(** Parse OM_TEST_BAKER env var: "network/pkh" or "network/pkh,network/pkh,..." *)
let parse_test_bakers () =
  match Sys.getenv_opt "OM_TEST_BAKER" with
  | None | Some "" -> []
  | Some s ->
      String.split_on_char ',' s
      |> List.filter_map (fun entry ->
          let entry = String.trim entry in
          match String.index_opt entry '/' with
          | None -> None
          | Some i ->
              let network = String.sub entry 0 i in
              let pkh =
                String.sub entry (i + 1) (String.length entry - i - 1)
              in
              if String.length network > 0 && String.length pkh > 0 then
                let instance = Printf.sprintf "test-%s" network in
                Some (instance, network, pkh)
              else None)

let tick () =
  let bakers =
    Data.load_service_states ()
    |> List.filter (fun (st : Data.Service_state.t) ->
        String.equal st.service.Service.role "baker")
  in
  List.iter
    (fun (st : Data.Service_state.t) ->
      let instance = st.service.Service.instance in
      let network = st.service.Service.network in
      poll_baker ~instance ~network ;
      check_continual ~instance ~svc:st)
    bakers ;
  (* Also poll any test bakers from OM_TEST_BAKER env var *)
  List.iter
    (fun (instance, network, pkh) ->
      Mutex.protect baker_instance_lock (fun () ->
          Hashtbl.replace baker_instance_cache instance pkh) ;
      poll_baker ~instance ~network)
    (parse_test_bakers ())

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
  Mutex.protect current_cycle_lock (fun () -> Hashtbl.clear current_cycle_cache) ;
  Mutex.protect payout_status_lock (fun () -> Hashtbl.clear payout_status_cache) ;
  Mutex.protect in_progress_lock (fun () -> Hashtbl.clear in_progress_payouts) ;
  Mutex.protect summary_lock (fun () -> Hashtbl.clear summary_cache) ;
  Mutex.protect baker_instance_lock (fun () ->
      Hashtbl.clear baker_instance_cache) ;
  Mutex.protect network_lock (fun () -> Hashtbl.clear network_cache) ;
  Mutex.protect continual_lock (fun () ->
      Hashtbl.clear continual_last_cycle ;
      Hashtbl.clear continual_delay_until)
