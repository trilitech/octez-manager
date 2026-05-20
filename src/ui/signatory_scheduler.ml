(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for polling Signatory health and metrics.

    This scheduler periodically checks Signatory instances to populate
    the {!Signatory_metrics} cache. It reads configuration files and
    will eventually poll health endpoints. *)

open Octez_manager_lib

let refresh_interval = 5.0

(** Unix timestamp of last refresh per instance *)
let last_refresh : (string, float) Hashtbl.t = Hashtbl.create 17

let last_refresh_lock = Mutex.create ()

let shutdown_requested = Atomic.make false

(** Worker queue for processing signatory checks *)
let worker : unit Worker_queue.t = Worker_queue.create ~name:"signatory" ()

(** Poll Signatory HTTP endpoint to check if it's responding *)
let check_http_health address =
  try
    (* Try to reach the /authorized_keys endpoint with a short timeout *)
    let url = "http://" ^ address ^ "/authorized_keys" in
    match
      Cmd_runner.run_out
        ["curl"; "-sf"; "--connect-timeout"; "2"; "--max-time"; "3"; url]
    with
    | Ok _ -> Some true
    | Error _ -> Some false
  with _ -> None

(** Parse Prometheus metrics from Signatory metrics endpoint *)
let parse_prometheus_metrics text =
  try
    let lines = String.split_on_char '\n' text in
    let total_requests = ref None in
    let successful_requests = ref None in
    let failed_requests = ref None in
    List.iter
      (fun line ->
        let trimmed = String.trim line in
        (* Skip comments and empty lines *)
        if
          String.length trimmed > 0
          && not (String.starts_with ~prefix:"#" trimmed)
        then
          (* Look for request counter metrics *)
          (* Example: signatory_requests_total 42 *)
          (* Example: signatory_requests{status="success"} 40 *)
          (* Example: signatory_requests{status="error"} 2 *)
          if String.starts_with ~prefix:"signatory_requests" trimmed then
            try
              (* Extract the value (last token on the line) *)
              match String.split_on_char ' ' trimmed |> List.rev with
              | value_str :: _ ->
                  let value = int_of_string (String.trim value_str) in
                  if String.contains trimmed '"' then
                    (* Has labels - check for success/error *)
                    if
                      String.contains trimmed 's'
                      && String.contains trimmed 'u'
                      && String.contains trimmed 'c'
                    then successful_requests := Some value
                    else if
                      String.contains trimmed 'e'
                      && String.contains trimmed 'r'
                      && String.contains trimmed 'r'
                    then failed_requests := Some value
                    else ()
                  else
                    (* No labels - total count *)
                    total_requests := Some value
              | [] -> ()
            with _ -> ())
      lines ;
    (!total_requests, !successful_requests, !failed_requests)
  with _ -> (None, None, None)

(** Fetch metrics from Signatory metrics endpoint *)
let fetch_metrics metrics_address =
  try
    let url = "http://" ^ metrics_address ^ "/metrics" in
    match
      Cmd_runner.run_out
        ["curl"; "-sf"; "--connect-timeout"; "2"; "--max-time"; "3"; url]
    with
    | Ok text -> Some (parse_prometheus_metrics text)
    | Error _ -> None
  with _ -> None

(** Determine health status from systemd state and HTTP check *)
let determine_health ~systemd_active ~http_responding =
  match (systemd_active, http_responding) with
  | Ok true, Some true -> Signatory_metrics.Up
  | Ok true, Some false ->
      Signatory_metrics.Degraded (* Service running but not responding *)
  | Ok true, None ->
      Signatory_metrics.Up (* Service running, HTTP check uncertain *)
  | Ok false, _ -> Signatory_metrics.Down
  | Error _, Some true ->
      Signatory_metrics.Up (* Service status unknown but responding *)
  | Error _, Some false -> Signatory_metrics.Down
  | Error _, None -> Signatory_metrics.Unknown

(** Read signatory configuration and populate metrics *)
let refresh_instance (svc : Service.t) =
  let instance = svc.Service.instance in
  try
    (* Read full config including address, metrics_address, backend, and keys *)
    let config_result = Signatory.read_config instance in
    let authorized_keys, address, metrics_address, backend =
      match config_result with
      | Ok cfg ->
          ( cfg.Signatory.authorized_keys,
            cfg.address,
            cfg.metrics_address,
            cfg.backend )
      | Error _ -> ([], None, None, None)
    in
    (* Check systemd service state *)
    let systemd_active = Systemd.is_active ~role:"signatory" ~instance in
    (* Check HTTP health if we have an address *)
    let http_responding =
      match address with Some addr -> check_http_health addr | None -> None
    in
    (* Fetch Prometheus metrics if we have a metrics address *)
    let total_requests, successful_requests, failed_requests =
      match metrics_address with
      | Some addr -> (
          match fetch_metrics addr with
          | Some (t, s, f) -> (t, s, f)
          | None -> (None, None, None))
      | None -> (None, None, None)
    in
    (* Determine overall health status *)
    let health = determine_health ~systemd_active ~http_responding in
    (* Update metrics cache *)
    Signatory_metrics.set
      ~instance
      {
        Signatory_metrics.health;
        last_check = Some (Unix.gettimeofday ());
        authorized_keys;
        address;
        metrics_address;
        version = None;
        backend;
        total_requests;
        successful_requests;
        failed_requests;
        last_error = None;
      } ;
    Context.mark_instances_dirty ()
  with _ -> ()

(** Submit a refresh request to the worker queue *)
let submit_refresh (svc : Service.t) =
  let key = Printf.sprintf "signatory-refresh:%s" svc.Service.instance in
  Worker_queue.submit_unit worker ~key ~work:(fun () ->
      try
        refresh_instance svc ;
        Mutex.protect last_refresh_lock (fun () ->
            Hashtbl.replace
              last_refresh
              svc.Service.instance
              (Unix.gettimeofday ()))
      with _ -> ())

(** Check if an instance is due for refresh *)
let is_due_for_refresh now instance =
  match
    Mutex.protect last_refresh_lock (fun () ->
        Hashtbl.find_opt last_refresh instance)
  with
  | None -> true
  | Some last -> now -. last >= refresh_interval

(** Start the scheduler in a background domain *)
let start () =
  Worker_queue.start worker ;
  Domain_pool.submit (fun () ->
      (* Simple polling loop *)
      while not (Atomic.get shutdown_requested) do
        try
          let now = Unix.gettimeofday () in
          (* Get all signatory instances from registry *)
          let all_services =
            match Service_registry.list () with
            | Ok svcs -> svcs
            | Error _ -> []
          in
          let signatory_services =
            List.filter
              (fun (svc : Service.t) -> svc.Service.role = "signatory")
              all_services
          in
          (* Submit refresh for instances that are due *)
          List.iter
            (fun svc ->
              if is_due_for_refresh now svc.Service.instance then
                submit_refresh svc)
            signatory_services ;
          Eio_unix.sleep 1.0
        with _ -> Eio_unix.sleep 1.0
      done)

(** Request scheduler shutdown *)
let stop () = Atomic.set shutdown_requested true
