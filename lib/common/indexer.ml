(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Centralised TzKT routing with local-indexer support.
    This is the ONLY module that may embed "api.tzkt.io" URL literals. *)

(* Per-network state: registered local endpoints + round-robin counter. *)
type network_state = {mutable endpoints : string list; mutable rr_idx : int}

let network_states : (string, network_state) Hashtbl.t = Hashtbl.create 4

let state_lock = Mutex.create ()

let debug_mode = Atomic.make false

(* ── HTTP backend (overridable for tests) ──────────────────────────────── *)

let default_http_fn ~url ~timeout =
  let max_time = string_of_int (Float.to_int (Float.round timeout)) in
  Cmd_runner.run_out_silent ["curl"; "-fsSL"; "--max-time"; max_time; url]

let http_fn_ref :
    (url:string -> timeout:float -> (string, [`Msg of string]) result) ref =
  ref default_http_fn

(* ── Warn logger (overridable for tests) ──────────────────────────────── *)

let log_warn_fn : (string -> unit) ref = ref (fun _msg -> ())

(* ── Internal helpers ─────────────────────────────────────────────────── *)

(** Deduplicate [lst] while preserving order. *)
let stable_dedup lst =
  let seen = Hashtbl.create 4 in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then false
      else (
        Hashtbl.replace seen x () ;
        true))
    lst

(** Perform one HTTP GET; treats empty body as failure. *)
let do_fetch ~url ~timeout =
  match !http_fn_ref ~url ~timeout with
  | Error _ as e -> e
  | Ok "" -> Error (`Msg "empty response body")
  | Ok body -> Ok body

(** Pick one local endpoint via round-robin; [None] if none are registered. *)
let pick_local_rr ~network =
  Mutex.protect state_lock (fun () ->
      match Hashtbl.find_opt network_states network with
      | None -> None
      | Some ns -> (
          match ns.endpoints with
          | [] -> None
          | eps ->
              let n = List.length eps in
              let idx = ns.rr_idx mod n in
              ns.rr_idx <- ns.rr_idx + 1 ;
              Some (List.nth eps idx)))

(* ── Public API ───────────────────────────────────────────────────────── *)

let tzkt_base_url ~network =
  if String.equal network "mainnet" then "https://api.tzkt.io"
  else Printf.sprintf "https://api.%s.tzkt.io" network

let register_local ~network ~base_url =
  Mutex.protect state_lock (fun () ->
      match Hashtbl.find_opt network_states network with
      | Some ns -> ns.endpoints <- ns.endpoints @ [base_url]
      | None ->
          Hashtbl.replace
            network_states
            network
            {endpoints = [base_url]; rr_idx = 0})

let clear_local ~network =
  Mutex.protect state_lock (fun () ->
      match Hashtbl.find_opt network_states network with
      | Some ns ->
          ns.endpoints <- [] ;
          ns.rr_idx <- 0
      | None -> ())

let set_debug_mode b = Atomic.set debug_mode b

let fetch ~network ?preferred_base ?(timeout = 15.0) path =
  let local_ep = pick_local_rr ~network in
  let tzkt = tzkt_base_url ~network in
  let sources =
    stable_dedup
      ((match preferred_base with Some b -> [b] | None -> [])
      @ (match local_ep with Some ep -> [ep] | None -> [])
      @ [tzkt])
  in
  let last_error = ref (`Msg (Printf.sprintf "no sources for %s" path)) in
  let result =
    List.find_map
      (fun src ->
        let url = src ^ path in
        match do_fetch ~url ~timeout with
        | Ok body ->
            (* In debug mode, compare with TzKT if we used a different source. *)
            if Atomic.get debug_mode && not (String.equal src tzkt) then begin
              let tzkt_url = tzkt ^ path in
              match do_fetch ~url:tzkt_url ~timeout with
              | Ok tzkt_body when not (String.equal body tzkt_body) ->
                  !log_warn_fn (Printf.sprintf "indexer divergence on %s" path)
              | _ -> ()
            end ;
            Some body
        | Error e ->
            last_error := e ;
            None)
      sources
  in
  match result with Some body -> Ok body | None -> Error !last_error

let query_all ~network path =
  let local_eps =
    Mutex.protect state_lock (fun () ->
        match Hashtbl.find_opt network_states network with
        | None -> []
        | Some ns -> ns.endpoints)
  in
  let tzkt = tzkt_base_url ~network in
  (* Build (label, base_url) pairs, deduplicated by base URL. *)
  let all_labeled =
    List.mapi (fun i ep -> (Printf.sprintf "local[%d](%s)" i ep, ep)) local_eps
    @ [(Printf.sprintf "tzkt(%s)" tzkt, tzkt)]
  in
  let seen = Hashtbl.create 4 in
  let deduped =
    List.filter
      (fun (_, base) ->
        if Hashtbl.mem seen base then false
        else (
          Hashtbl.replace seen base () ;
          true))
      all_labeled
  in
  List.map
    (fun (label, base) ->
      let url = base ^ path in
      let result =
        match do_fetch ~url ~timeout:15.0 with
        | Ok body -> Ok body
        | Error (`Msg m) -> Error m
      in
      (label, result))
    deduped

(* ── Internal_for_tests ──────────────────────────────────────────────── *)

module Internal_for_tests = struct
  let set_http_fn f = http_fn_ref := f

  let reset_http_fn () = http_fn_ref := default_http_fn

  let get_local_endpoints ~network =
    Mutex.protect state_lock (fun () ->
        match Hashtbl.find_opt network_states network with
        | None -> []
        | Some ns -> ns.endpoints)

  let set_log_fn f = log_warn_fn := f

  let reset_log_fn () = log_warn_fn := fun _msg -> ()
end
