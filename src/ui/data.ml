(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Octez_manager_lib
open Manager_interfaces
module Bg = Background_runner

module Service_state = struct
  type status = Running | Stopped | Unknown of string

  type t = {
    service : Service.t;
    enabled : bool option;
    active : bool option;
    status : status;
    status_text : string option;
  }

  let status_label {status; _} =
    match status with
    | Running -> "running"
    | Stopped -> "stopped"
    | Unknown msg -> Printf.sprintf "unknown (%s)" msg
end

module Summary = struct
  type t = {total : int; running : int; stopped : int; unknown : int}
end

let cache : Service_state.t list Atomic.t = Atomic.make []

let last_refresh = Atomic.make 0.0

let refresh_inflight = Atomic.make false

let cache_ttl_secs = 5.0

let set_cache states =
  Atomic.set cache states ;
  Atomic.set last_refresh (Unix.gettimeofday ())

let parse_enabled_response resp =
  let trimmed = String.lowercase_ascii (String.trim resp) in
  match trimmed with
  | "enabled" | "static" -> Some true
  | "disabled" -> Some false
  | _ -> None

(** Classify using detailed systemd state, detecting failed services *)
let classify_unit_state result =
  match result with
  | Ok Systemd.{active_state; result = failure_reason; exit_status; _} -> (
      match active_state with
      | "active" -> (Some true, Service_state.Running)
      | "failed" -> (
          (* Check if this is a normal stop (signal termination) vs actual failure *)
          match exit_status with
          | Some 127 | Some 255 ->
              (* 127 = terminated by signal (normal stop)
                 255 = forcefully terminated (SIGKILL)
                 These are normal stops, not failures *)
              (Some false, Service_state.Stopped)
          | Some code when code <> 0 ->
              (* Actual failure - use Octez-specific exit code descriptions *)
              let msg = Common.octez_exit_code_description code in
              (Some false, Service_state.Unknown msg)
          | _ -> (
              match failure_reason with
              | Some reason -> (Some false, Service_state.Unknown reason)
              | None -> (Some false, Service_state.Stopped)))
      | "inactive" | "deactivating" -> (Some false, Service_state.Stopped)
      | _ -> (None, Service_state.Stopped))
  | Error (`Msg msg) -> (None, Service_state.Unknown msg)

let fetch_status ?(detail = false) service =
  let role = service.Service.role in
  let instance = service.Service.instance in
  (* Use detailed state to detect failed services *)
  let unit_state = Systemd.get_unit_state ~role ~instance in
  let active, status = classify_unit_state unit_state in
  let enabled =
    match Systemd.is_enabled ~role ~instance with
    | Ok resp -> parse_enabled_response resp
    | Error _ -> None
  in
  let status_text =
    if detail then
      match Systemd.status ~role ~instance with
      | Ok text when String.trim text <> "" -> Some text
      | Ok _ -> None
      | Error (`Msg msg) -> Some msg
    else None
  in
  let is_active =
    match status with Service_state.Running -> true | _ -> false
  in
  Metrics.record_service_status ~service:instance ~is_active ;
  Service_state.{service; enabled; active; status; status_text}

let fetch_statuses ?detail services =
  let safe_fetch ?detail svc =
    try fetch_status ?detail svc
    with exn ->
      Service_state.
        {
          service = svc;
          enabled = None;
          active = None;
          status = Unknown (Printexc.to_string exn);
          status_text = None;
        }
  in
  List.map (safe_fetch ?detail) services

let refresh_cache ?detail () =
  let module SM =
    (val Miaou_interfaces.Capability.require Service_manager_capability.key)
  in
  match SM.list () with
  | Ok services ->
      let states = fetch_statuses ?detail services in
      let now = Unix.gettimeofday () in
      List.iter
        (fun st ->
          let svc = st.Service_state.service in
          if String.equal svc.Service.role "node" then
            match Rpc_metrics.get ~instance:svc.instance with
            | None ->
                Rpc_metrics.set
                  ~instance:svc.instance
                  {
                    Rpc_metrics.chain_id = None;
                    head_level = None;
                    bootstrapped = None;
                    last_rpc_refresh = Some now;
                    node_version = None;
                    data_size = None;
                    proto = None;
                    last_error = None;
                    last_block_time = None;
                  }
            | Some _ -> ())
        states ;
      set_cache states ;
      states
  | Error (`Msg msg) ->
      prerr_endline (Printf.sprintf "Failed to read registry: %s" msg) ;
      set_cache [] ;
      []

let schedule_refresh ?detail () =
  if Atomic.compare_and_set refresh_inflight false true then
    Bg.submit_blocking (fun () ->
        Fun.protect
          ~finally:(fun () -> Atomic.set refresh_inflight false)
          (fun () -> ignore (refresh_cache ?detail ())))

let load_service_states ?detail () =
  match detail with
  | Some true ->
      (* Force fresh fetch with detailed status text (for diagnostics) *)
      refresh_cache ~detail:true ()
  | Some false | None ->
      (* Use cache, refresh in background if stale.
         When detail=false, we skip fetching status_text but still use cache. *)
      let cached = Atomic.get cache in
      let now = Unix.gettimeofday () in
      let age = now -. Atomic.get last_refresh in
      if cached = [] && not (Atomic.get refresh_inflight) then
        refresh_cache ?detail ()
      else (
        if age > cache_ttl_secs then schedule_refresh ?detail () ;
        cached)

let summarize states =
  let open Service_state in
  List.fold_left
    (fun (acc : Summary.t) state ->
      let running =
        if state.status = Running then acc.running + 1 else acc.running
      in
      let stopped =
        if state.status = Stopped then acc.stopped + 1 else acc.stopped
      in
      let unknown =
        match state.status with
        | Unknown _ -> acc.unknown + 1
        | _ -> acc.unknown
      in
      Summary.{total = acc.total + 1; running; stopped; unknown})
    Summary.{total = 0; running = 0; stopped = 0; unknown = 0}
    states

let split_lines text = String.split_on_char '\n' text

let diagnostics_lines states =
  let header = ["Diagnostics"; "-----------"] in
  let per_service state =
    let svc = state.Service_state.service in
    let head =
      Printf.sprintf
        "- %s (%s, %s) → %s%s"
        svc.Service.instance
        svc.Service.role
        svc.Service.network
        (Service_state.status_label state)
        (match state.enabled with
        | Some true -> " [enabled]"
        | Some false -> " [disabled]"
        | None -> "")
    in
    let details =
      match state.status_text with
      | None -> []
      | Some text -> List.map (fun line -> "    " ^ line) (split_lines text)
    in
    head :: details
  in
  header @ List.concat_map per_service states

let activity_lines states =
  let per_service state =
    let svc = state.Service_state.service in
    Printf.sprintf
      "%s  •  %s (%s) history=%s data=%s"
      svc.Service.created_at
      svc.Service.instance
      svc.Service.network
      (History_mode.to_string svc.Service.history_mode)
      svc.Service.data_dir
  in
  "Recent activity" :: "----------------" :: List.map per_service states

let formatted_timestamp ts =
  let tm = Unix.localtime ts in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let rec take n acc = function
  | [] -> List.rev acc
  | _ when n = 0 -> List.rev acc
  | x :: xs -> take (n - 1) (x :: acc) xs

let spotlight_lines states ~limit =
  let sorted =
    List.sort
      (fun a b ->
        String.compare
          a.Service_state.service.instance
          b.Service_state.service.instance)
      states
  in
  let focus = if limit <= 0 then sorted else take limit [] sorted in
  List.map
    (fun state ->
      let svc = state.Service_state.service in
      let marker =
        match state.status with
        | Service_state.Running -> "●"
        | Service_state.Stopped -> "○"
        | Service_state.Unknown _ -> "?"
      in
      Printf.sprintf "%s %s (%s)" marker svc.Service.instance svc.Service.role)
    focus

module For_tests = struct
  let parse_enabled_response = parse_enabled_response

  let classify_unit_state = classify_unit_state
end
