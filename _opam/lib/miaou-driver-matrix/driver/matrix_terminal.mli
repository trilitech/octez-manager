(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Terminal setup and management for the Matrix driver.

    Handles raw mode, size detection, mouse tracking, and signal handling.
    Adapted from term_terminal_setup.ml and term_size_detection.ml for
    the matrix driver without lambda-term dependency.
*)

type t

(** Setup terminal and return handle. Must be called before any other ops. *)
val setup : unit -> t

(** Cleanup terminal state. Safe to call multiple times. *)
val cleanup : t -> unit

(** Enter raw mode (disable line buffering and echo). *)
val enter_raw : t -> unit

(** Leave raw mode (restore original terminal state). *)
val leave_raw : t -> unit

(** Get terminal size as (rows, cols). Uses cached value if available. *)
val size : t -> int * int

(** Invalidate size cache. Called on SIGWINCH. *)
val invalidate_size_cache : t -> unit

(** Enable mouse tracking (button events + SGR extended). *)
val enable_mouse : t -> unit

(** Disable mouse tracking. *)
val disable_mouse : t -> unit

(** Get the file descriptor for reading input. *)
val fd : t -> Unix.file_descr

(** Write string to terminal output. *)
val write : t -> string -> unit

(** Install signal handlers (SIGINT, SIGTERM, etc.).
    Returns atomic flag that will be set when exit signal received.
    The cleanup function will be called in the signal handler. *)
val install_signals : t -> (unit -> unit) -> bool Atomic.t

(** Check if resize is pending (set by SIGWINCH). *)
val resize_pending : t -> bool

(** Clear the resize pending flag. *)
val clear_resize_pending : t -> unit

(** Set screen content to dump on exit for debugging.
    The content will be printed after exiting alternate screen mode. *)
val set_exit_screen_dump : t -> string -> unit
