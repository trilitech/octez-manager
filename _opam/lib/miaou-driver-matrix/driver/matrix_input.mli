(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Terminal input handling for the Matrix driver.

    Reads keyboard and mouse input from the terminal, parsing escape sequences
    into events. Adapted from term_event_reader.ml with simplified interface.
*)

(** Input events. *)
type event =
  | Key of string  (** Named key or character *)
  | Mouse of int * int  (** Click at (row, col), 1-indexed *)
  | Resize  (** Terminal was resized *)
  | Refresh  (** Time for service_cycle - rate limited to ~1/sec *)
  | Idle  (** No input, not time for refresh - just keep rendering *)
  | Quit  (** Exit signal received *)

(** Input reader state. *)
type t

(** Create a new input reader for the given terminal. *)
val create : Matrix_terminal.t -> t

(** Poll for next event with timeout.
    Returns Refresh on timeout if no input available.
    @param timeout_ms Maximum time to wait in milliseconds. *)
val poll : t -> timeout_ms:int -> event

(** Drain consecutive identical navigation keys from buffer.
    Call after receiving Up/Down/Left/Right/Tab to prevent scroll lag.
    Returns count of drained events. *)
val drain_nav_keys : t -> event -> int

(** Drain any pending Esc keys from buffer.
    Call after modal close to prevent double-Esc navigation.
    Returns count of drained events. *)
val drain_esc_keys : t -> int
