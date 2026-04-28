(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Payout report read/write.

    Writes standard CSV reports and JSON summaries.
    Reads summaries back for history display and paid-cycle checks. *)

(** {1 Report directories} *)

(** Reports directory for a baker instance + cycle.
    E.g., [~/.octez-manager/rewards/<instance>/reports/<cycle>/] *)
val report_dir : instance:string -> cycle:int -> string

(** Dry-run reports directory.
    E.g., [~/.octez-manager/rewards/<instance>/reports/dry/<cycle>/] *)
val dry_report_dir : instance:string -> cycle:int -> string

(** {1 Writing reports} *)

(** Write the payouts CSV (standard columns). *)
val write_payouts_csv :
  dir:string ->
  baker:string ->
  cycle:int ->
  Rewards.payout_result list ->
  (unit, string) result

(** Write the invalid (excluded) delegators CSV. *)
val write_invalid_csv :
  dir:string ->
  baker:string ->
  cycle:int ->
  Rewards.delegator_reward list ->
  (unit, string) result

(** Write the cycle summary JSON. *)
val write_summary_json :
  dir:string -> Rewards.cycle_summary -> (unit, string) result

(** {1 Reading reports} *)

(** Read a cycle summary from disk. *)
val read_summary_json :
  instance:string -> cycle:int -> (Rewards.cycle_summary, string) result

(** Read per-delegator payout results back from [payouts.csv]. Returns an
    empty list if the file does not exist. *)
val read_payouts_csv :
  instance:string -> cycle:int -> (Rewards.payout_result list, string) result

(** Read excluded (pre-execution) delegators back from [invalid.csv]. Returns
    an empty list if the file does not exist. *)
val read_invalid_csv :
  instance:string -> cycle:int -> (Rewards.delegator_reward list, string) result

(** Check whether a cycle has been paid (summary.json exists). *)
val cycle_is_paid : instance:string -> cycle:int -> bool

(** List all cycles that have summary files, sorted descending. *)
val list_paid_cycles : instance:string -> int list
