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

(** {1 Cycle check} *)

(** Determine which cycles should be paid on this scheduler tick.

    The [interval] controls how often payouts are triggered: payouts only
    fire when [(current_cycle - offset) mod interval = 0]. When triggered,
    the last [interval] unpaid cycles are returned. This batches rewards
    from skipped cycles into the trigger payout.

    @param current_cycle The current chain cycle.
    @param interval Trigger payout every [interval] cycles (default 1).
    @param offset Cycle offset within the interval (default 0).
    @return List of unpaid cycles to pay (empty if not a trigger cycle). *)
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
  (int * (int * (unit, string) result)) list

(**/**)

module Internal_for_tests : sig
  val is_trigger_cycle : current_cycle:int -> interval:int -> offset:int -> bool

  val collect_due_cycles :
    current_cycle:int -> is_paid:(int -> bool) -> int list
end

(**/**)
