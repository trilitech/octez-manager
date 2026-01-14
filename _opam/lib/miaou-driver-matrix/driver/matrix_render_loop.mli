(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Render loop for the Matrix driver.

    Manages frame rendering in a dedicated OCaml 5 Domain for true
    parallelism. The render domain runs at configurable FPS, computing
    diffs between front/back buffers and emitting minimal ANSI sequences.

    Architecture:
    - Main domain: Input handling, state updates, writes to back buffer
    - Render domain: Reads buffers, computes diff, writes to terminal

    The buffer uses mutex synchronization for thread safety.
*)

(** Render loop state. *)
type t

(** Create a new render loop (does not start the domain yet).
    @param config Configuration (FPS cap, debug mode)
    @param buffer Double buffer for diff computation
    @param writer ANSI writer for output generation
    @param terminal Terminal for output *)
val create :
  config:Matrix_config.t ->
  buffer:Matrix_buffer.t ->
  writer:Matrix_ansi_writer.t ->
  terminal:Matrix_terminal.t ->
  t

(** Start the render domain. Call once after setup. *)
val start : t -> unit

(** Request a frame to be rendered (legacy API, now no-op).
    The render domain automatically renders when buffer is dirty. *)
val request_frame : t -> unit

(** Render immediately if buffer is dirty.
    For use before domain starts or after shutdown.
    Returns true if a frame was rendered. *)
val render_if_needed : t -> bool

(** Force immediate render regardless of dirty state. *)
val force_render : t -> unit

(** Shutdown the render loop and join the domain. *)
val shutdown : t -> unit

(** Get current achieved FPS - actual renders per second (for diagnostics). Thread-safe. *)
val current_fps : t -> float

(** Get loop FPS - how fast the render loop is checking (the cap). Thread-safe. *)
val loop_fps : t -> float

(** Check if buffer needs render. *)
val frame_pending : t -> bool

(** Check if render domain is running. *)
val is_running : t -> bool
