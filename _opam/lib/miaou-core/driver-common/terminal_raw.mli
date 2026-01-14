(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Raw terminal operations shared between drivers.

    Provides low-level terminal control: raw mode, mouse tracking, size
    detection, signal handling, and cleanup. Based on lambda-term driver's
    tested implementation.

    Usage:
    {[
      let term = Terminal_raw.setup () in
      at_exit (fun () -> Terminal_raw.cleanup term);
      Terminal_raw.enter_raw term;
      Terminal_raw.enable_mouse term;
      (* ... main loop ... *)
      Terminal_raw.cleanup term
    ]}
*)

(** Terminal state handle. *)
type t

(** Setup terminal for TUI use. Opens /dev/tty for reliable output.
    @raise Failure if stdin is not a terminal. *)
val setup : unit -> t

(** Get the input file descriptor (stdin). *)
val fd : t -> Unix.file_descr

(** Enter raw mode: disable line buffering and echo. *)
val enter_raw : t -> unit

(** Leave raw mode: restore original terminal settings. *)
val leave_raw : t -> unit

(** Enable SGR mouse tracking (1000h + 1006h). *)
val enable_mouse : t -> unit

(** Disable mouse tracking. Idempotent, safe to call multiple times.
    Uses multiple methods (tty, stdout, stderr) to ensure delivery. *)
val disable_mouse : t -> unit

(** Full cleanup: clear screen, show cursor, restore settings, disable mouse.
    Safe to call multiple times (idempotent for terminal restore). *)
val cleanup : t -> unit

(** Write string to terminal via /dev/tty with stdout fallback. *)
val write : t -> string -> unit

(** Detect terminal size. Uses cache; call {!invalidate_size_cache} on resize.
    Falls back to environment variables or (24, 80) if detection fails.
    @return (rows, cols) *)
val size : t -> int * int

(** Invalidate the size cache. Call this on SIGWINCH. *)
val invalidate_size_cache : t -> unit

(** Install signal handlers for resize and exit.
    @param on_resize Called on SIGWINCH (after cache invalidation)
    @param on_exit Called on SIGINT/SIGTERM/SIGHUP/SIGQUIT before exit
    @return Atomic flag that becomes true when exit signal received *)
val install_signals :
  t -> on_resize:(unit -> unit) -> on_exit:(unit -> unit) -> bool Atomic.t

(** Check if a resize signal was received since last clear. *)
val resize_pending : t -> bool

(** Clear the resize pending flag. *)
val clear_resize_pending : t -> unit

(** Set screen content to dump on exit for debugging.
    The content will be printed to stdout after exiting alternate screen mode,
    preserving the TUI output in terminal scrollback. *)
val set_exit_screen_dump : t -> string -> unit
