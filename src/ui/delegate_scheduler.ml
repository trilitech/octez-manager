(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for polling delegate data.

    Polls delegate participation data from node RPC every minute
    and tracks status changes (forbidden, deactivated). *)

open Octez_manager_lib
module Bg = Background_runner

(** Poll interval in seconds *)
let poll_interval = 60.0

(** Track previous forbidden status for change detection *)
let forbidden_status : (string, bool) Hashtbl.t = Hashtbl.create 17

let forbidden_lock = Mutex.create ()

let with_forbidden_lock f =
  Mutex.lock forbidden_lock ;
  Fun.protect ~finally:(fun () -> Mutex.unlock forbidden_lock) f

(** Get delegates for a baker from env file *)
let get_baker_delegates ~instance =
  match Node_env.read ~inst:instance with
  | Error _ -> []
  | Ok pairs -> (
      match List.assoc_opt "OCTEZ_BAKER_DELEGATES_CSV" pairs with
      | None -> []
      | Some csv ->
          String.split_on_char ',' csv
          |> List.map String.trim
          |> List.filter (fun s -> s <> ""))

(** Get node endpoint for a baker from env file *)
let get_baker_node_endpoint ~instance =
  match Node_env.read ~inst:instance with
  | Error _ -> None
  | Ok pairs -> (
      match List.assoc_opt "OCTEZ_NODE_ENDPOINT" pairs with
      | None -> None
      | Some ep ->
          let ep = String.trim ep in
          if ep = "" then None else Some ep)

(** Poll delegates for a baker instance *)
let poll_baker ~instance =
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
    (* Spawn dedicated domain for delegate polling - no Eio needed for simple I/O *)
    ignore (Domain.spawn (fun () ->
        (* Delay to let UI initialize first *)
        Unix.sleepf 2.0 ;
        (* Simple polling loop *)
        while true do
          let now = Unix.gettimeofday () in
          if now -. !last_poll >= poll_interval then (
            last_poll := now ;
            tick ()) ;
          Unix.sleepf 5.0
        done)))

(** Get delegate data for display *)
let get_delegate_data ~pkh = Delegate_data.get ~pkh

(** Get all delegates for a baker instance *)
let get_baker_delegate_data ~instance =
  let delegates = get_baker_delegates ~instance in
  List.filter_map (fun pkh -> Delegate_data.get ~pkh) delegates

(** Clear all state *)
let clear () =
  Delegate_data.clear () ;
  with_forbidden_lock (fun () -> Hashtbl.clear forbidden_status)
