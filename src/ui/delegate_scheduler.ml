(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for polling delegate data.

    Polls delegate participation data from node RPC every minute
    and tracks status changes (forbidden, deactivated).

    Baker config (delegates, node endpoint) is cached to avoid file I/O
    during rendering. *)

open Octez_manager_lib

(** Poll interval in seconds *)
let poll_interval = 60.0

(** {1 Baker config cache} *)

type baker_config = {
  delegates : string list;
  node_endpoint : string option;
  has_dal : bool;
}

let config_cache : (string, baker_config) Hashtbl.t = Hashtbl.create 17

let config_lock = Mutex.create ()

(** Read baker config from disk (internal, does file I/O) *)
let read_baker_config_from_disk ~instance =
  match Node_env.read ~inst:instance with
  | Error _ -> {delegates = []; node_endpoint = None; has_dal = false}
  | Ok pairs ->
      let delegates =
        match List.assoc_opt "OCTEZ_BAKER_DELEGATES_CSV" pairs with
        | None -> []
        | Some csv ->
            String.split_on_char ',' csv
            |> List.map String.trim
            |> List.filter (fun s -> s <> "")
      in
      let node_endpoint =
        match List.assoc_opt "OCTEZ_NODE_ENDPOINT" pairs with
        | None -> None
        | Some ep ->
            let ep = String.trim ep in
            if ep = "" then None else Some ep
      in
      let has_dal =
        match List.assoc_opt "OCTEZ_DAL_CONFIG" pairs with
        | None -> false
        | Some cfg ->
            let cfg = String.trim (String.lowercase_ascii cfg) in
            cfg <> "" && cfg <> "disabled" && cfg <> "none"
      in
      {delegates; node_endpoint; has_dal}

(** Refresh config cache for an instance (called by scheduler) *)
let refresh_config ~instance =
  let config = read_baker_config_from_disk ~instance in
  Mutex.protect config_lock (fun () ->
      Hashtbl.replace config_cache instance config)

(** Get cached baker config (never blocks on I/O) *)
let get_config ~instance =
  Mutex.protect config_lock (fun () ->
      Hashtbl.find_opt config_cache instance
      |> Option.value
           ~default:{delegates = []; node_endpoint = None; has_dal = false})

(** Get delegates for a baker (from cache, never blocks) *)
let get_baker_delegates ~instance = (get_config ~instance).delegates

(** Get node endpoint for a baker (from cache, never blocks) *)
let get_baker_node_endpoint ~instance = (get_config ~instance).node_endpoint

(** Check if baker has DAL enabled (from cache, never blocks) *)
let baker_has_dal ~instance = (get_config ~instance).has_dal

(** {1 Forbidden status tracking} *)

(** Track previous forbidden status for change detection *)
let forbidden_status : (string, bool) Hashtbl.t = Hashtbl.create 17

let forbidden_lock = Mutex.create ()

let with_forbidden_lock f =
  Mutex.lock forbidden_lock ;
  Fun.protect ~finally:(fun () -> Mutex.unlock forbidden_lock) f

(** {1 Polling} *)

(** Poll delegates for a baker instance *)
let poll_baker ~instance =
  (* Refresh config from disk first *)
  refresh_config ~instance ;
  (* Refresh highwatermarks *)
  Baker_highwatermarks.refresh ~instance ;
  (* Now poll delegate data via RPC *)
  match get_baker_node_endpoint ~instance with
  | None -> ()
  | Some node_endpoint ->
      let delegates = get_baker_delegates ~instance in
      List.iter
        (fun pkh ->
          match Delegate_data.fetch ~node_endpoint ~pkh with
          | None -> ()
          | Some data ->
              (* Check for forbidden status change *)
              let prev_forbidden =
                with_forbidden_lock (fun () ->
                    Hashtbl.find_opt forbidden_status pkh)
              in
              (match prev_forbidden with
              | Some false when data.is_forbidden ->
                  (* Changed from not forbidden to forbidden - show error toast *)
                  Context.toast_error
                    (Printf.sprintf "ALERT: %s is now FORBIDDEN!" pkh)
              | None when data.is_forbidden ->
                  (* First check and already forbidden *)
                  Context.toast_error
                    (Printf.sprintf "ALERT: %s is FORBIDDEN!" pkh)
              | _ -> ()) ;
              with_forbidden_lock (fun () ->
                  Hashtbl.replace forbidden_status pkh data.is_forbidden) ;
              (* Store in cache *)
              Delegate_data.set data)
        delegates

(** Poll all bakers *)
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
    (* Refresh baker configs and highwatermarks immediately so UI has data
       before first full poll (which is delayed 2 seconds for UI init).
       Wrapped in try-with since capability may not be registered in tests. *)
    (try
       let bakers =
         Data.load_service_states ()
         |> List.filter (fun (st : Data.Service_state.t) ->
             st.service.Service.role = "baker")
       in
       List.iter
         (fun (st : Data.Service_state.t) ->
           let instance = st.service.Service.instance in
           refresh_config ~instance ;
           Baker_highwatermarks.refresh ~instance)
         bakers
     with exn ->
       prerr_endline
         (Printf.sprintf
            "delegate_scheduler: startup refresh failed: %s"
            (Printexc.to_string exn))) ;
    (* Spawn dedicated domain for delegate polling - no Eio needed for simple I/O *)
    ignore
      (Domain.spawn (fun () ->
           (* Delay to let UI initialize first *)
           Unix.sleepf 2.0 ;
           (* Simple polling loop *)
           while true do
             let now = Unix.gettimeofday () in
             if now -. !last_poll >= poll_interval then (
               last_poll := now ;
               Metrics.record_scheduler_tick ~scheduler:"delegate" tick) ;
             Unix.sleepf 5.0
           done)))

(** Get delegate data for display *)
let get_delegate_data ~pkh = Delegate_data.get ~pkh

(** Get all delegates for a baker instance *)
let get_baker_delegate_data ~instance =
  let delegates = get_baker_delegates ~instance in
  List.filter_map (fun pkh -> Delegate_data.get ~pkh) delegates

(** Invalidate and refresh cached config for an instance (call after editing baker).
    This immediately reloads the config from disk so the UI shows fresh data. *)
let invalidate_config ~instance = refresh_config ~instance

(** Clear all state *)
let clear () =
  Delegate_data.clear () ;
  with_forbidden_lock (fun () -> Hashtbl.clear forbidden_status) ;
  Mutex.protect config_lock (fun () -> Hashtbl.clear config_cache)
