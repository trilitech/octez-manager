(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Continual payout mode: automatic payouts when new cycles complete.

    Monitors cycle transitions, applies configurable delay, and triggers
    the generate-pay pipeline automatically.

    {b Insufficient balance handling:} If a payout fails (including due to
    insufficient wallet balance), the cycle remains unpaid. On the next
    scheduler tick, {!cycles_due} will include it again, ensuring automatic
    retry without skipping any cycle. *)

(** {1 State} *)

(** Whether continual mode is active for a given baker instance. *)
val is_active : instance:string -> bool

(** Enable continual mode for a baker instance. *)
val enable : instance:string -> unit

(** Disable continual mode for a baker instance. *)
val disable : instance:string -> unit

(** {1 Cycle check} *)

(** Path to the per-instance payout delay file.
    @param instance Baker instance name *)
val delay_file : instance:string -> string

(** Read the delay timestamp from the per-instance delay file.
    Returns [None] if the file does not exist or contains invalid content.
    @param instance Baker instance name *)
val read_delay_until : instance:string -> float option

(** Check if a cycle should be paid based on interval/offset configuration.
    @param current_cycle The current chain cycle.
    @param interval Pay every [interval] cycles (default 1).
    @param offset Cycle offset within the interval (default 0).
    @return List of unpaid cycles that match the interval. *)
val cycles_due :
  instance:string -> current_cycle:int -> interval:int -> offset:int -> int list

(** {1 Execution} *)

(** Attempt to pay all due cycles for a baker instance.
    Called by the scheduler tick when a cycle transition is detected.
    @return List of [(cycle, result)] pairs. *)
val pay_due_cycles :
  ctx:Payout_executor.context ->
  baker:string ->
  network:string ->
  current_cycle:int ->
  interval:int ->
  offset:int ->
  (int * (unit, string) result) list
