(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-laws.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Background metrics recorder that samples system metrics at regular intervals *)

type duration = OneMin | FiveMin | FifteenMin

type sample = {
  timestamp : float;
  bg_queue_depth : int;
  services_active : int;
  services_total : int;
}

type state = {
  mutable enabled : bool;
  mutable duration : duration;
  mutable samples : sample array;
  mutable next_idx : int;
  mutable count : int;
  lock : Mutex.t;
}

let capacity_for_duration = function
  | OneMin -> 12      (* 1 min / 5s = 12 samples *)
  | FiveMin -> 60     (* 5 min / 5s = 60 samples *)
  | FifteenMin -> 180 (* 15 min / 5s = 180 samples *)

let duration_to_string = function
  | OneMin -> "1 minute"
  | FiveMin -> "5 minutes"
  | FifteenMin -> "15 minutes"

let duration_to_short_string = function
  | OneMin -> "1m"
  | FiveMin -> "5m"
  | FifteenMin -> "15m"

let all_durations = [OneMin; FiveMin; FifteenMin]

let state =
  {
    enabled = false;
    duration = FiveMin;
    samples = Array.make 60 {timestamp = 0.; bg_queue_depth = 0; services_active = 0; services_total = 0};
    next_idx = 0;
    count = 0;
    lock = Mutex.create ();
  }

let is_enabled () = state.enabled

let get_duration () = state.duration

let set_duration dur =
  Mutex.protect state.lock (fun () ->
      if dur <> state.duration then (
        let new_capacity = capacity_for_duration dur in
        let new_samples = Array.make new_capacity {timestamp = 0.; bg_queue_depth = 0; services_active = 0; services_total = 0} in
        (* Copy existing samples to new array *)
        let copy_count = min state.count new_capacity in
        for i = 0 to copy_count - 1 do
          let src_idx = (state.next_idx + state.count - copy_count + i) mod Array.length state.samples in
          new_samples.(i) <- state.samples.(src_idx)
        done ;
        state.samples <- new_samples ;
        state.next_idx <- copy_count mod new_capacity ;
        state.count <- copy_count ;
        state.duration <- dur))

let add_sample sample =
  Mutex.protect state.lock (fun () ->
      state.samples.(state.next_idx) <- sample ;
      state.next_idx <- (state.next_idx + 1) mod Array.length state.samples ;
      state.count <- min (state.count + 1) (Array.length state.samples))

let get_samples () =
  Mutex.protect state.lock (fun () ->
      if state.count = 0 then []
      else
        let result = ref [] in
        let start_idx = (state.next_idx + Array.length state.samples - state.count) mod Array.length state.samples in
        for i = 0 to state.count - 1 do
          let idx = (start_idx + i) mod Array.length state.samples in
          result := state.samples.(idx) :: !result
        done ;
        List.rev !result)

let clear () =
  Mutex.protect state.lock (fun () ->
      state.next_idx <- 0 ;
      state.count <- 0)

let sample_now () =
  let bg_depth = Metrics.get_bg_queue_depth () in
  let services = Data.load_service_states () in
  let services_total = List.length services in
  let services_active =
    List.filter
      (fun (st : Data.Service_state.t) ->
        match st.status with Running -> true | _ -> false)
      services
    |> List.length
  in
  {
    timestamp = Unix.gettimeofday ();
    bg_queue_depth = bg_depth;
    services_active;
    services_total;
  }

let recording_loop () =
  while state.enabled do
    let sample = sample_now () in
    add_sample sample ;
    Unix.sleepf 5.0
  done

let start () =
  if not state.enabled then (
    state.enabled <- true ;
    clear () ;
    ignore (Domain.spawn recording_loop))

let stop () =
  state.enabled <- false

type stats = {
  max_bg_queue : int;
  avg_bg_queue : float;
  current_services_active : int;
  current_services_total : int;
}

let get_stats () =
  let samples = get_samples () in
  if samples = [] then None
  else
    let bg_depths = List.map (fun s -> float_of_int s.bg_queue_depth) samples in
    let max_bg = List.fold_left max 0.0 bg_depths in
    let avg_bg =
      List.fold_left (+.) 0.0 bg_depths /. float_of_int (List.length bg_depths)
    in
    let last = List.hd (List.rev samples) in
    Some {
      max_bg_queue = int_of_float max_bg;
      avg_bg_queue = avg_bg;
      current_services_active = last.services_active;
      current_services_total = last.services_total;
    }
