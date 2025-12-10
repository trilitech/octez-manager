(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Rresult

type sample_ring = {data : float array; mutable next : int; mutable count : int}

let make_ring capacity = {data = Array.make capacity 0.; next = 0; count = 0}

let ring_add ring value =
  ring.data.(ring.next) <- value ;
  ring.next <- (ring.next + 1) mod Array.length ring.data ;
  ring.count <- min (ring.count + 1) (Array.length ring.data)

let ring_values ring =
  let len = ring.count in
  if len = 0 then [||]
  else
    let arr = Array.make len 0. in
    let start =
      (ring.next + Array.length ring.data - ring.count) mod ring.count
    in
    for i = 0 to len - 1 do
      let idx = (start + i) mod Array.length ring.data in
      arr.(i) <- ring.data.(idx)
    done ;
    arr

let percentile values p =
  let len = Array.length values in
  if len = 0 then None
  else (
    Array.sort Float.compare values ;
    let rank =
      let r = float_of_int (len - 1) *. p in
      int_of_float (ceil r)
    in
    Some values.(max 0 (min (len - 1) rank)))

type histogram = {
  ring : sample_ring;
  mutable count : int;
  mutable sum : float;
  mutable min_v : float;
  mutable max_v : float;
}

let make_histogram ?(capacity = 128) () =
  {
    ring = make_ring capacity;
    count = 0;
    sum = 0.;
    min_v = Float.infinity;
    max_v = Float.neg_infinity;
  }

let hist_add h value =
  h.count <- h.count + 1 ;
  h.sum <- h.sum +. value ;
  h.min_v <- Float.min h.min_v value ;
  h.max_v <- Float.max h.max_v value ;
  ring_add h.ring value

type snapshot = {
  count : int;
  sum : float;
  min_v : float;
  max_v : float;
  p50 : float option;
  p90 : float option;
  p99 : float option;
}

let hist_snapshot h =
  let values = ring_values h.ring in
  {
    count = h.count;
    sum = h.sum;
    min_v = h.min_v;
    max_v = h.max_v;
    p50 = percentile (Array.copy values) 0.50;
    p90 = percentile (Array.copy values) 0.90;
    p99 = percentile (Array.copy values) 0.99;
  }

module Page_map = Map.Make (String)
module Service_map = Map.Make (String)

type service_status = Active | Inactive

type state = {
  mutable enabled : bool;
  mutable render_hist : histogram Page_map.t;
  key_to_render : histogram;
  bg_wait : histogram;
  mutable bg_queue_depth : int;
  mutable bg_queue_max : int;
  mutable service_statuses : service_status Service_map.t;
  lock : Mutex.t;
  mutable last_input_ts : float option;
  mutable server_addr : string option;
  mutable server_port : int option;
}

let state =
  {
    enabled = false;
    render_hist = Page_map.empty;
    key_to_render = make_histogram ();
    bg_wait = make_histogram ();
    bg_queue_depth = 0;
    bg_queue_max = 0;
    service_statuses = Service_map.empty;
    lock = Mutex.create ();
    last_input_ts = None;
    server_addr = None;
    server_port = None;
  }

let is_enabled () = state.enabled

let get_server_info () =
  match (state.server_addr, state.server_port) with
  | Some addr, Some port -> Some (addr, port)
  | _ -> None

let get_bg_queue_depth () = state.bg_queue_depth

let get_bg_queue_max () = state.bg_queue_max

let get_service_statuses () =
  Mutex.protect state.lock (fun () ->
      Service_map.bindings state.service_statuses
      |> List.map (fun (name, status) ->
             let is_active = match status with Active -> true | Inactive -> false in
             (name, is_active)))

let record_render_duration_ms ~page duration_ms =
  if not (is_enabled ()) then ()
  else
    let hist =
      Mutex.protect state.lock (fun () ->
          let existing =
            match Page_map.find_opt page state.render_hist with
            | Some h -> h
            | None ->
                let h = make_histogram () in
                state.render_hist <- Page_map.add page h state.render_hist ;
                h
          in
          hist_add existing duration_ms ;
          existing)
    in
    ignore hist

let mark_input_event () =
  if is_enabled () then
    Mutex.protect state.lock (fun () ->
        state.last_input_ts <- Some (Unix.gettimeofday ()))

let consume_key_to_render now =
  Mutex.protect state.lock (fun () ->
      match state.last_input_ts with
      | None -> None
      | Some ts ->
          state.last_input_ts <- None ;
          Some (now -. ts))

let record_render ~page (render : unit -> 'a) : 'a =
  if not (is_enabled ()) then render ()
  else
    let start = Unix.gettimeofday () in
    Fun.protect
      ~finally:(fun () ->
        let finish = Unix.gettimeofday () in
        let duration_ms = (finish -. start) *. 1000. in
        record_render_duration_ms ~page duration_ms ;
        match consume_key_to_render finish with
        | Some lag -> hist_add state.key_to_render (lag *. 1000.)
        | None -> ())
      render

let record_bg_enqueue ~queued_depth =
  if is_enabled () then
    Mutex.protect state.lock (fun () ->
        state.bg_queue_depth <- queued_depth ;
        state.bg_queue_max <- max state.bg_queue_max queued_depth)

let record_bg_dequeue ~queued_depth ~wait_ms =
  if is_enabled () then (
    Mutex.protect state.lock (fun () -> state.bg_queue_depth <- queued_depth) ;
    if wait_ms >= 0. then hist_add state.bg_wait wait_ms)

let record_service_status ~service ~is_active =
  if is_enabled () then
    Mutex.protect state.lock (fun () ->
        let status = if is_active then Active else Inactive in
        state.service_statuses <- Service_map.add service status state.service_statuses)

let metrics_text () =
  let lines = Buffer.create 1024 in
  let add fmt = Printf.bprintf lines "%s\n" fmt in
  let render_snapshots =
    Page_map.bindings state.render_hist
    |> List.map (fun (page, hist) -> (page, hist_snapshot hist))
  in
  List.iter
    (fun (page, snap) ->
      let labels q = Printf.sprintf "{page=\"%s\",quantile=\"%s\"}" page q in
      let emit_quantile q_label v_opt =
        match v_opt with
        | None -> ()
        | Some v ->
            add
              (Printf.sprintf
                 "octez_manager_ui_render_duration_ms%s %.3f"
                 (labels q_label)
                 v)
      in
      emit_quantile "0.50" snap.p50 ;
      emit_quantile "0.90" snap.p90 ;
      emit_quantile "0.99" snap.p99 ;
      add
        (Printf.sprintf
           "octez_manager_ui_render_duration_ms_sum{page=\"%s\"} %.3f"
           page
           snap.sum) ;
      add
        (Printf.sprintf
           "octez_manager_ui_render_duration_ms_count{page=\"%s\"} %d"
           page
           snap.count) ;
      if snap.min_v < Float.infinity then
        add
          (Printf.sprintf
             "octez_manager_ui_render_duration_ms_min{page=\"%s\"} %.3f"
             page
             snap.min_v) ;
      if snap.max_v > Float.neg_infinity then
        add
          (Printf.sprintf
             "octez_manager_ui_render_duration_ms_max{page=\"%s\"} %.3f"
             page
             snap.max_v))
    render_snapshots ;
  let key_snap = hist_snapshot state.key_to_render in
  let emit_quantile q v_opt =
    match v_opt with
    | None -> ()
    | Some v ->
        add
          (Printf.sprintf
             "octez_manager_ui_key_to_render_ms{quantile=\"%s\"} %.3f"
             q
             v)
  in
  emit_quantile "0.50" key_snap.p50 ;
  emit_quantile "0.90" key_snap.p90 ;
  emit_quantile "0.99" key_snap.p99 ;
  add
    (Printf.sprintf "octez_manager_ui_key_to_render_ms_count %d" key_snap.count) ;
  add (Printf.sprintf "octez_manager_ui_key_to_render_ms_sum %.3f" key_snap.sum) ;
  let bg_snap = hist_snapshot state.bg_wait in
  let emit_bg_quantile q v_opt =
    match v_opt with
    | None -> ()
    | Some v ->
        add
          (Printf.sprintf
             "octez_manager_bg_wait_ms{quantile=\"%s\"} %.3f"
             q
             v)
  in
  emit_bg_quantile "0.50" bg_snap.p50 ;
  emit_bg_quantile "0.90" bg_snap.p90 ;
  emit_bg_quantile "0.99" bg_snap.p99 ;
  add (Printf.sprintf "octez_manager_bg_wait_ms_count %d" bg_snap.count) ;
  add (Printf.sprintf "octez_manager_bg_wait_ms_sum %.3f" bg_snap.sum) ;
  add (Printf.sprintf "octez_manager_bg_queue_depth %d" state.bg_queue_depth) ;
  add (Printf.sprintf "octez_manager_bg_queue_depth_max %d" state.bg_queue_max) ;
  Service_map.iter
    (fun service status ->
      let value = match status with Active -> 1 | Inactive -> 0 in
      add
        (Printf.sprintf
           "octez_manager_service_status{service=\"%s\",state=\"%s\"} %d"
           service
           (match status with Active -> "active" | Inactive -> "inactive")
           value))
    state.service_statuses ;
  Buffer.contents lines

let serve_forever ~addr ~port =
  let sockaddr =
    match
      Unix.getaddrinfo addr (string_of_int port) [Unix.AI_FAMILY Unix.PF_INET]
    with
    | {Unix.ai_addr; _} :: _ -> ai_addr
    | [] -> raise_notrace Not_found
  in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true ;
  Unix.bind sock sockaddr ;
  Unix.listen sock 16 ;
  let response_body () = Mutex.protect state.lock (fun () -> metrics_text ()) in
  let rec loop () =
    let fd, _ = Unix.accept sock in
    let body = response_body () in
    let resp =
      Printf.sprintf
        "HTTP/1.1 200 OK\r\n\
         Content-Type: text/plain; charset=utf-8\r\n\
         Content-Length: %d\r\n\
         Connection: close\r\n\
         \r\n\
         %s"
        (String.length body)
        body
    in
    let _ = Unix.write_substring fd resp 0 (String.length resp) in
    Unix.close fd ;
    loop ()
  in
  loop ()

let parse_addr raw =
  match String.split_on_char ':' raw with
  | [host; port_str] -> (
      match int_of_string_opt (String.trim port_str) with
      | Some port when port > 0 && port < 65536 -> Ok (host, port)
      | _ -> Error (`Msg "Invalid port"))
  | [port_str] -> (
      match int_of_string_opt (String.trim port_str) with
      | Some port when port > 0 && port < 65536 -> Ok ("127.0.0.1", port)
      | _ -> Error (`Msg "Invalid port"))
  | _ -> Error (`Msg "Expected [host:]port")

let start_server ~addr ~port =
  if state.enabled then ()
  else (
    state.enabled <- true ;
    state.server_addr <- Some addr ;
    state.server_port <- Some port ;
    ignore
      (Domain.spawn (fun () ->
           try serve_forever ~addr ~port
           with exn ->
             state.enabled <- false ;
             state.server_addr <- None ;
             state.server_port <- None ;
             prerr_endline
               (Printf.sprintf
                  "metrics server stopped: %s"
                  (Printexc.to_string exn)))))

let maybe_start_from_env () =
  match Sys.getenv_opt "OCTEZ_MANAGER_METRICS_ADDR" with
  | None | Some "" -> ()
  | Some raw -> (
      match parse_addr raw with
      | Ok (addr, port) -> start_server ~addr ~port
      | Error (`Msg msg) ->
          prerr_endline
            (Printf.sprintf "metrics server disabled: %s (%s)" msg raw))
