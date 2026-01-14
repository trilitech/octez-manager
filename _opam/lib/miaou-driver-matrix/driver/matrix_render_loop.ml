(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-69"]

type t = {
  config : Matrix_config.t;
  buffer : Matrix_buffer.t;
  writer : Matrix_ansi_writer.t;
  terminal : Matrix_terminal.t;
  shutdown_flag : bool Atomic.t;
  mutable render_domain : unit Domain.t option;
  mutable last_frame_time : float;
  mutable frame_count : int;
  mutable loop_count : int;
  mutable fps_start_time : float;
  current_fps : float Atomic.t; (* Actual renders per second *)
  loop_fps : float Atomic.t; (* Loop iterations per second (cap) *)
}

let do_render t =
  (* Reset terminal style and writer state to ensure consistency *)
  Matrix_terminal.write t.terminal "\027[0m" ;
  Matrix_ansi_writer.reset t.writer ;

  (* Compute diff atomically - holds lock during read to prevent torn reads.
     This also swaps the buffers while holding the lock. *)
  let changes = Matrix_diff.compute_atomic t.buffer in

  (* Clear dirty flag after atomic read+swap *)
  Matrix_buffer.clear_dirty t.buffer ;

  (* Generate ANSI output *)
  let ansi = Matrix_ansi_writer.render t.writer changes in

  (* Write to terminal *)
  if String.length ansi > 0 then Matrix_terminal.write t.terminal ansi ;

  (* Update frame timing *)
  t.last_frame_time <- Unix.gettimeofday () ;
  t.frame_count <- t.frame_count + 1

(* Render domain main loop *)
let render_loop_fn t =
  let frame_time_s = t.config.frame_time_ms /. 1000.0 in
  while not (Atomic.get t.shutdown_flag) do
    let frame_start = Unix.gettimeofday () in

    (* Count loop iteration *)
    t.loop_count <- t.loop_count + 1 ;

    (* Only render if buffer is dirty *)
    if Matrix_buffer.is_dirty t.buffer then do_render t ;

    (* Update loop FPS calculation every second *)
    let now = Unix.gettimeofday () in
    let elapsed_since_start = now -. t.fps_start_time in
    if elapsed_since_start >= 1.0 then begin
      Atomic.set t.loop_fps (float_of_int t.loop_count /. elapsed_since_start) ;
      Atomic.set
        t.current_fps
        (float_of_int t.frame_count /. elapsed_since_start) ;
      t.loop_count <- 0 ;
      t.frame_count <- 0 ;
      t.fps_start_time <- now
    end ;

    (* Sleep to maintain frame rate *)
    let elapsed = Unix.gettimeofday () -. frame_start in
    let sleep_time = frame_time_s -. elapsed in
    if sleep_time > 0.0 then Thread.delay sleep_time
  done

let create ~config ~buffer ~writer ~terminal =
  let now = Unix.gettimeofday () in
  {
    config;
    buffer;
    writer;
    terminal;
    shutdown_flag = Atomic.make false;
    render_domain = None;
    last_frame_time = now;
    frame_count = 0;
    loop_count = 0;
    fps_start_time = now;
    current_fps = Atomic.make 0.0;
    loop_fps = Atomic.make 0.0;
  }

(* Start the render domain *)
let start t =
  if Option.is_none t.render_domain then begin
    let domain = Domain.spawn (fun () -> render_loop_fn t) in
    t.render_domain <- Some domain
  end

(* Legacy API for compatibility - now just marks dirty *)
let request_frame _t = ()

let frame_pending t = Matrix_buffer.is_dirty t.buffer

(* Synchronous render - for use before domain starts or after shutdown *)
let render_if_needed t =
  if Atomic.get t.shutdown_flag then false
  else if not (Matrix_buffer.is_dirty t.buffer) then false
  else begin
    do_render t ;
    true
  end

let force_render t = if not (Atomic.get t.shutdown_flag) then do_render t

let shutdown t =
  Atomic.set t.shutdown_flag true ;
  match t.render_domain with
  | Some domain ->
      Domain.join domain ;
      t.render_domain <- None
  | None -> ()

let current_fps t = Atomic.get t.current_fps

let loop_fps t = Atomic.get t.loop_fps

let is_running t = Option.is_some t.render_domain
