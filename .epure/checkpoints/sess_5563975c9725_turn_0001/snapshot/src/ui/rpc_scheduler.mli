(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Background scheduler for polling RPC bootstrap status and head level.

    Manages boot state (bootstrapping/ready) and head monitoring
    for all active node instances. *)

open Octez_manager_lib

(** {2 Scheduler Control} *)

(** Spawn the background polling domain and start the worker queue. *)
val start : unit -> unit

(** Poll all active node services and submit RPC requests for due instances. *)
val tick : unit -> unit

(** Stop all monitors and the worker queue. *)
val shutdown : unit -> unit

(** {2 Head Monitoring} *)

(** Open a streaming [/monitor/heads/main] connection for a service. *)
val start_head_monitor : Service.t -> unit

(** Stop the head-monitor for the given instance name. *)
val stop_head_monitor : string -> unit

(** Stop all active head-monitor connections. *)
val stop_all_monitors : unit -> unit

(** {2 Worker Statistics} *)

(** Return current stats (queue length, processed count) from the worker. *)
val get_worker_stats : unit -> Worker_queue.stats

(** {2 Testing Interface} *)

module For_tests : sig
  (** Clear all monitors, boot-state tables, and reset clock stubs. *)
  val reset_state : unit -> unit

  (** Temporarily replace the clock function for the duration of [f]. *)
  val with_now : (unit -> float) -> (unit -> 'a) -> 'a

  (** Temporarily replace the [poll_boot] function for the duration of [f]. *)
  val with_poll_boot : (Service.t -> float -> unit) -> (unit -> 'a) -> 'a

  (** Return the polling interval for an instance based on boot state. *)
  val poll_interval : string -> float

  (** [true] if the instance has never been polled or enough time has elapsed. *)
  val is_due_for_poll : float -> Service.t -> bool

  (** Polling interval when a node is not yet bootstrapped (6 seconds). *)
  val boot_pending_interval : float

  (** Polling interval when a node is bootstrapped (10 seconds). *)
  val boot_ok_interval : float

  (** Directly set the boot state for an instance. *)
  val set_boot_state : string -> bool option -> unit

  (** Directly set the last-poll timestamp for an instance. *)
  val set_boot_at : string -> float -> unit

  (** Prepend ["http://"] if the address does not start with ["http"]. *)
  val normalize_endpoint : string -> string

  (** Return [Some now] if the head level changed, otherwise preserve
      the existing [last_block_time]. *)
  val compute_last_block_time :
    previous_head:int option ->
    head_level:int option ->
    now:float ->
    existing_block_time:float option ->
    float option
end
