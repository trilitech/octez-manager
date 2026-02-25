(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TzKT cycle rewards data fetching.

    Fetches baker reward data and delegator snapshots from TzKT API
    using [Cmd_runner.run_out_silent] + curl. *)

(** Fetch reward data for a specific baker and cycle.

    Uses [GET /v1/rewards/bakers/{baker}/cycles/{cycle}]. *)
val fetch_cycle :
  tzkt_url:string ->
  baker:string ->
  cycle:int ->
  (Rewards.cycle_rewards, string) result

(** Fetch recent cycle rewards for a baker.

    Uses [GET /v1/rewards/bakers/{baker}?limit=N&sort.desc=cycle]. *)
val fetch_recent_cycles :
  tzkt_url:string ->
  baker:string ->
  limit:int ->
  (Rewards.cycle_rewards list, string) result

(** Fetch the current head cycle number.

    Uses [GET /v1/head]. *)
val fetch_current_cycle : tzkt_url:string -> (int, string) result
