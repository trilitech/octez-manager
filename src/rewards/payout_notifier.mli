(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Notification dispatch after payout completion.

    Sends payout summary notifications to configured channels
    (Discord, Telegram, webhook, external). Template variables in
    message templates are substituted with cycle data. *)

(** {1 Template rendering} *)

(** Render a message template by replacing placeholders with values.
    Supported placeholders: [<Cycle>], [<Delegators>], [<TotalPaid>],
    [<DistributedRewards>], [<BakerFee>], [<TxFees>], [<Timestamp>]. *)
val render_template : template:string -> summary:Rewards.cycle_summary -> string

(** {1 Dispatch} *)

(** Send a notification to a single channel.
    @return [Ok ()] on success, [Error msg] on failure. *)
val send :
  channel:Rewards.notification_channel ->
  summary:Rewards.cycle_summary ->
  (unit, string) result

(** Send notifications to all configured channels.
    Returns a list of [(channel_name, result)] for each channel. *)
val notify_all :
  channels:Rewards.notification_channel list ->
  summary:Rewards.cycle_summary ->
  (string * (unit, string) result) list

(** Send a test notification to all configured channels.
    Uses a sample summary with placeholder data. *)
val send_test :
  channels:Rewards.notification_channel list ->
  (string * (unit, string) result) list
