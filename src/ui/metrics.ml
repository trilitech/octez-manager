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

(** Snapshot of all metrics at a point in time *)
type metrics_snapshot = {
  timestamp : float;
  bg_queue_depth : int;
  bg_queue_max : int;
  services_active : int;
  services_total : int;
  render_p50 : float option;
  render_p90 : float option;
  render_p99 : float option;
  key_to_render_p50 : float option;
  key_to_render_p90 : float option;
  bg_wait_p50 : float option;
  bg_wait_p90 : float option;
}

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
  (* Historical recording *)
  mutable recording_enabled : bool;
  mutable recording_duration : int;
  mutable snapshots : metrics_snapshot array;
  mutable snapshots_next : int;
  mutable snapshots_count : int;
}

let state =
  let empty_snapshot = {
    timestamp = 0.;
    bg_queue_depth = 0;
    bg_queue_max = 0;
    services_active = 0;
    services_total = 0;
    render_p50 = None;
    render_p90 = None;
    render_p99 = None;
    key_to_render_p50 = None;
    key_to_render_p90 = None;
    bg_wait_p50 = None;
    bg_wait_p90 = None;
  } in
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
    recording_enabled = false;
    recording_duration = 60;
    snapshots = Array.make 60 empty_snapshot;
    snapshots_next = 0;
    snapshots_count = 0;
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
  (* Always track render metrics in the UI, regardless of server state *)
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
  Mutex.protect state.lock (fun () ->
      state.bg_queue_depth <- queued_depth ;
      state.bg_queue_max <- max state.bg_queue_max queued_depth)

let record_bg_dequeue ~queued_depth ~wait_ms =
  Mutex.protect state.lock (fun () -> state.bg_queue_depth <- queued_depth) ;
  if wait_ms >= 0. then hist_add state.bg_wait wait_ms

let record_service_status ~service ~is_active =
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

(** Recording / Historical Snapshots *)

let take_snapshot () =
  Mutex.protect state.lock (fun () ->
      (* Compute percentiles from histograms *)
      let all_renders =
        Page_map.fold
          (fun _page hist acc -> Array.append acc (ring_values hist.ring))
          state.render_hist
          [||]
      in
      let render_p50 = percentile all_renders 0.5 in
      let render_p90 = percentile all_renders 0.9 in
      let render_p99 = percentile all_renders 0.99 in
      
      let key_vals = ring_values state.key_to_render.ring in
      let key_to_render_p50 = percentile key_vals 0.5 in
      let key_to_render_p90 = percentile key_vals 0.9 in
      
      let bg_vals = ring_values state.bg_wait.ring in
      let bg_wait_p50 = percentile bg_vals 0.5 in
      let bg_wait_p90 = percentile bg_vals 0.9 in
      
      let services_active =
        Service_map.fold
          (fun _name status acc ->
            match status with Active -> acc + 1 | Inactive -> acc)
          state.service_statuses
          0
      in
      let services_total = Service_map.cardinal state.service_statuses in
      
      {
        timestamp = Unix.gettimeofday ();
        bg_queue_depth = state.bg_queue_depth;
        bg_queue_max = state.bg_queue_max;
        services_active;
        services_total;
        render_p50;
        render_p90;
        render_p99;
        key_to_render_p50;
        key_to_render_p90;
        bg_wait_p50;
        bg_wait_p90;
      })

let add_snapshot snapshot =
  Mutex.protect state.lock (fun () ->
      state.snapshots.(state.snapshots_next) <- snapshot ;
      state.snapshots_next <- (state.snapshots_next + 1) mod Array.length state.snapshots ;
      state.snapshots_count <- min (state.snapshots_count + 1) (Array.length state.snapshots))

let get_snapshots () =
  Mutex.protect state.lock (fun () ->
      if state.snapshots_count = 0 then []
      else
        let result = ref [] in
        let start_idx =
          (state.snapshots_next + Array.length state.snapshots - state.snapshots_count)
          mod Array.length state.snapshots
        in
        for i = 0 to state.snapshots_count - 1 do
          let idx = (start_idx + i) mod Array.length state.snapshots in
          result := state.snapshots.(idx) :: !result
        done ;
        List.rev !result)

let set_recording_duration samples =
  Mutex.protect state.lock (fun () ->
      if samples <> state.recording_duration then (
        let old_size = Array.length state.snapshots in
        let old_count = state.snapshots_count in
        
        (* Preserve data: copy as many real snapshots as will fit *)
        let copy_count = min old_count samples in
        
        if copy_count = 0 then
          (* No existing data to preserve, just resize *)
          let empty_snapshot = {
            timestamp = 0.;
            bg_queue_depth = 0;
            bg_queue_max = 0;
            services_active = 0;
            services_total = 0;
            render_p50 = None;
            render_p90 = None;
            render_p99 = None;
            key_to_render_p50 = None;
            key_to_render_p90 = None;
            bg_wait_p50 = None;
            bg_wait_p90 = None;
          } in
          state.snapshots <- Array.make samples empty_snapshot ;
          state.snapshots_next <- 0 ;
          state.snapshots_count <- 0 ;
          state.recording_duration <- samples
        else (
          (* Create new buffer and copy the most recent snapshots *)
          let empty_snapshot = {
            timestamp = 0.;
            bg_queue_depth = 0;
            bg_queue_max = 0;
            services_active = 0;
            services_total = 0;
            render_p50 = None;
            render_p90 = None;
            render_p99 = None;
            key_to_render_p50 = None;
            key_to_render_p90 = None;
            bg_wait_p50 = None;
            bg_wait_p90 = None;
          } in
          let new_snapshots = Array.make samples empty_snapshot in
          
          (* Copy the most recent copy_count snapshots to the start of new buffer *)
          for i = 0 to copy_count - 1 do
            let src_idx = (state.snapshots_next + old_size - copy_count + i) mod old_size in
            new_snapshots.(i) <- state.snapshots.(src_idx)
          done ;
          
          state.snapshots <- new_snapshots ;
          state.snapshots_next <- copy_count mod samples ;
          state.snapshots_count <- copy_count ;
          state.recording_duration <- samples)))

let clear_snapshots () =
  Mutex.protect state.lock (fun () ->
      state.snapshots_next <- 0 ;
      state.snapshots_count <- 0)

let recording_loop () =
  while state.recording_enabled do
    let snapshot = take_snapshot () in
    add_snapshot snapshot ;
    Unix.sleepf 5.0
  done

let start_recording () =
  if not state.recording_enabled then (
    state.recording_enabled <- true ;
    clear_snapshots () ;
    ignore (Domain.spawn recording_loop))

let stop_recording () =
  state.recording_enabled <- false

let is_recording () = state.recording_enabled

let get_recording_duration () = state.recording_duration
