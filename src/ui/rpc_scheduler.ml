(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module Rpc_m = Rpc_metrics
module Rpc = Rpc_client

let now_ref = ref Unix.gettimeofday

let shutdown_requested = Atomic.make false

(* Polling cadence *)
let boot_pending_interval = 6.0

let boot_ok_interval = 10.0

let last_boot_at : (string, float) Hashtbl.t = Hashtbl.create 17

let last_boot_state : (string, bool option) Hashtbl.t = Hashtbl.create 17

let head_monitors : (string, Rpc_client.monitor_handle) Hashtbl.t =
  Hashtbl.create 17

(** Worker queue for processing RPC requests one at a time with dedup *)
let worker : unit Worker_queue.t = Worker_queue.create ~name:"rpc" ()

let poll_boot (svc : Service.t) now =
  let boot = Rpc.rpc_is_bootstrapped svc in
  let prev =
    Hashtbl.find_opt last_boot_state svc.Service.instance |> Option.join
  in
  (* Toast on bootstrap state change *)
  (match (prev, boot) with
  | Some false, Some true ->
      Context.toast_success
        (Printf.sprintf "%s is now synced" svc.Service.instance)
  | Some true, Some false ->
      Context.toast_warn (Printf.sprintf "%s lost sync" svc.Service.instance)
  | None, Some true ->
      Context.toast_success (Printf.sprintf "%s is synced" svc.Service.instance)
  | _ -> ()) ;
  Hashtbl.replace last_boot_state svc.Service.instance boot ;
  let existing = Rpc_m.get ~instance:svc.Service.instance in
  let node_version =
    match existing with
    | Some m when Option.is_some m.Rpc_metrics.node_version ->
        m.Rpc_metrics.node_version
    | _ -> Rpc.node_version svc
  in
  (* Fetch fresh head/proto/chain during sync when head monitor isn't streaming *)
  let head_level =
    match existing with
    | Some m when Option.is_some m.Rpc_metrics.head_level ->
        m.Rpc_metrics.head_level
    | _ -> Rpc.rpc_head_header svc
  in
  let proto =
    match existing with
    | Some m when Option.is_some m.Rpc_metrics.proto -> m.Rpc_metrics.proto
    | _ -> Rpc.rpc_protocol svc
  in
  let last_error = Rpc.rpc_last_error svc in
  let chain_id =
    match existing with
    | Some m when Option.is_some m.Rpc_metrics.chain_id ->
        m.Rpc_metrics.chain_id
    | _ -> Rpc.rpc_chain_id svc
  in
  let last_block_time =
    match existing with
    | Some m when Option.is_some m.Rpc_metrics.last_block_time ->
        m.Rpc_metrics.last_block_time
    | _ -> None
  in
  Rpc_m.set
    ~instance:svc.Service.instance
    {
      Rpc_metrics.chain_id;
      head_level;
      bootstrapped = boot;
      last_rpc_refresh = Some now;
      node_version;
      data_size = None;
      proto;
      last_error;
      last_block_time;
    } ;
  Context.mark_instances_dirty ()

let start_head_monitor (svc : Service.t) =
  let instance = svc.Service.instance in
  (* Remove stale monitor if it's no longer alive *)
  (match Hashtbl.find_opt head_monitors instance with
  | Some h when not (h.Rpc_client.alive ()) ->
      Hashtbl.remove head_monitors instance
  | _ -> ()) ;
  if Hashtbl.mem head_monitors instance then ()
  else
    let on_disconnect () = Hashtbl.remove head_monitors instance in
    let handle =
      Rpc.start_head_monitor
        svc
        ~on_disconnect
        ~on_head:(fun ~level ~proto ~chain_id ->
          let now = Unix.gettimeofday () in
          let boot = Rpc.rpc_is_bootstrapped svc in
          Hashtbl.replace last_boot_state instance boot ;
          let existing = Rpc_m.get ~instance in
          let node_version =
            match existing with
            | Some m when Option.is_some m.Rpc_metrics.node_version ->
                m.Rpc_metrics.node_version
            | _ -> Rpc.node_version svc
          in
          let chain_id =
            match chain_id with
            | Some _ as v -> v
            | None -> (
                match
                  Option.bind existing (fun m -> m.Rpc_metrics.chain_id)
                with
                | Some _ as v -> v
                | None -> Rpc.rpc_chain_id svc)
          in
          let proto =
            match proto with
            | Some _ as v -> v
            | None -> (
                match Option.bind existing (fun m -> m.Rpc_metrics.proto) with
                | Some _ as v -> v
                | None -> Rpc.rpc_protocol svc)
          in
          let last_error = Rpc.rpc_last_error svc in
          Rpc_m.set
            ~instance
            {
              Rpc_metrics.chain_id;
              head_level = level;
              bootstrapped = boot;
              last_rpc_refresh = Some now;
              node_version;
              data_size = None;
              proto;
              last_error;
              last_block_time = Some now;
            } ;
          Context.mark_instances_dirty ())
    in
    Hashtbl.replace head_monitors instance handle

let stop_head_monitor instance =
  match Hashtbl.find_opt head_monitors instance with
  | None -> ()
  | Some h ->
      h.stop () ;
      Hashtbl.remove head_monitors instance

let poll_interval instance =
  match Hashtbl.find_opt last_boot_state instance |> Option.join with
  | Some true -> boot_ok_interval
  | _ -> boot_pending_interval

let is_due_for_poll now (svc : Service.t) =
  match Hashtbl.find_opt last_boot_at svc.Service.instance with
  | None -> true
  | Some last -> now -. last >= poll_interval svc.Service.instance

let poll_boot_ref = ref poll_boot

(** Submit a poll request to the worker queue *)
let submit_poll (svc : Service.t) =
  let key = Printf.sprintf "rpc-poll:%s" svc.Service.instance in
  let now = !now_ref () in
  Worker_queue.submit_unit worker ~key ~work:(fun () ->
      try
        !poll_boot_ref svc now ;
        Hashtbl.replace last_boot_at svc.Service.instance (!now_ref ())
      with _ -> ())

(** Poll an external node for head/bootstrap status via direct RPC calls *)
let poll_external_node (ext : External_service.t) now =
  let instance = ext.suggested_instance_name in
  match ext.config.rpc_addr.value with
  | None -> ()
  | Some rpc_addr ->
      let endpoint =
        if String.starts_with ~prefix:"http" rpc_addr then rpc_addr
        else "http://" ^ rpc_addr
      in
      (* Fetch head level and bootstrap status using curl *)
      let head_level =
        try
          let url = endpoint ^ "/chains/main/blocks/head/header" in
          match
            Common.run_out
              ["curl"; "-sfm"; "2"; "--connect-timeout"; "0.8"; url]
          with
          | Ok json_str -> (
              try
                let json = Yojson.Safe.from_string json_str in
                match Yojson.Safe.Util.member "level" json with
                | `Int level -> Some level
                | _ -> None
              with _ -> None)
          | Error _ -> None
        with _ -> None
      in
      let bootstrapped =
        try
          let url = endpoint ^ "/monitor/bootstrapped" in
          match
            Common.run_out
              ["curl"; "-sfm"; "2"; "--connect-timeout"; "0.8"; url]
          with
          | Ok json_str -> (
              try
                let json = Yojson.Safe.from_string json_str in
                match Yojson.Safe.Util.member "bootstrapped" json with
                | `Bool b -> Some b
                | _ -> None
              with _ -> None)
          | Error _ -> None
        with _ -> None
      in
      (* Update metrics - only update last_block_time if head changed *)
      let existing = Rpc_m.get ~instance in
      let previous_head =
        match existing with Some m -> m.Rpc_metrics.head_level | None -> None
      in
      let last_block_time =
        match (previous_head, head_level) with
        | Some old_level, Some new_level when old_level <> new_level ->
            Some now (* Head changed, update time *)
        | None, Some _ -> Some now (* First head detected *)
        | _ -> (
            (* Head unchanged or no head - preserve previous time *)
            match existing with
            | Some m -> m.Rpc_metrics.last_block_time
            | None -> None)
      in
      Rpc_m.set
        ~instance
        {
          Rpc_metrics.chain_id = None;
          node_version = None;
          head_level;
          proto = None;
          last_error = None;
          bootstrapped;
          last_rpc_refresh = Some now;
          data_size = None;
          last_block_time;
        }

(** Submit external node poll *)
let submit_poll_external (ext : External_service.t) now =
  let key =
    Printf.sprintf
      "rpc-poll-ext:%s"
      ext.External_service.suggested_instance_name
  in
  Worker_queue.submit_unit worker ~key ~work:(fun () ->
      try poll_external_node ext now with _ -> ())

let tick () =
  let nodes =
    match
      Miaou_interfaces.Capability.get
        Manager_interfaces.Service_manager_capability.key
    with
    | Some cap ->
        let module SM =
          (val (cap : Manager_interfaces.Service_manager_capability.t))
        in
        (match SM.list () with Ok l -> l | Error _ -> [])
        |> List.filter (fun (svc : Service.t) -> String.equal svc.role "node")
    | None ->
        Data.load_service_states ()
        |> List.map (fun st -> st.Data.Service_state.service)
        |> List.filter (fun (svc : Service.t) -> String.equal svc.role "node")
  in
  (* Also get external nodes *)
  let external_nodes =
    External_services_scheduler.get ()
    |> List.filter (fun (ext : External_service.t) ->
        match ext.config.role.value with
        | Some External_service.Node -> true
        | _ -> false)
  in
  let now = !now_ref () in
  (* Start head monitors for all nodes *)
  List.iter (fun (svc : Service.t) -> start_head_monitor svc) nodes ;
  (* Submit poll requests for nodes that are due *)
  List.iter (fun svc -> if is_due_for_poll now svc then submit_poll svc) nodes ;
  (* Poll external nodes too *)
  List.iter (fun ext -> submit_poll_external ext now) external_nodes

let started = ref false

let start () =
  if not !started then (
    started := true ;
    (* Start the worker that processes RPC requests *)
    Worker_queue.start worker ;
    (* Spawn scheduler domain that submits poll requests *)
    ignore
      (Domain.spawn (fun () ->
           (* Brief delay to let UI initialize *)
           Unix.sleepf 0.2 ;
           (* Submit poll requests periodically *)
           while not (Atomic.get shutdown_requested) do
             Metrics.record_scheduler_tick ~scheduler:"rpc" tick ;
             Unix.sleepf 1.0
           done)))

(** Stop all active head monitors. Call this on app exit to kill curl processes. *)
let stop_all_monitors () =
  Hashtbl.iter (fun _ handle -> handle.Rpc_client.stop ()) head_monitors ;
  Hashtbl.clear head_monitors

let shutdown () =
  Atomic.set shutdown_requested true ;
  stop_all_monitors () ;
  Worker_queue.stop worker

(** Get worker queue stats *)
let get_worker_stats () = Worker_queue.get_stats worker

module For_tests = struct
  let reset_state () =
    stop_all_monitors () ;
    Hashtbl.reset last_boot_at ;
    Hashtbl.reset last_boot_state ;
    now_ref := Unix.gettimeofday ;
    poll_boot_ref := poll_boot

  let with_now now f =
    let original = !now_ref in
    now_ref := now ;
    Fun.protect ~finally:(fun () -> now_ref := original) f

  let with_poll_boot stub f =
    let original = !poll_boot_ref in
    poll_boot_ref := stub ;
    Fun.protect ~finally:(fun () -> poll_boot_ref := original) f
end
