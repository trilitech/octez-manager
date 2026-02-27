(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** TzKT cycle rewards data fetching.

    Fetches baker reward data and delegator snapshots via the {!Indexer}
    module, which handles local-indexer routing and public TzKT fallback. *)

(** Fetch reward data with delegator details for a specific baker and cycle.

    Uses [GET /v1/rewards/split/{baker}/{cycle}] with automatic pagination
    for bakers with more than 10,000 delegators.

    @param preferred_base  Optional base URL tried before local endpoints
                           (e.g. [payout_config.tzkt_url]). [None] uses the
                           default Indexer routing. *)
val fetch_cycle :
  network:string ->
  preferred_base:string option ->
  baker:string ->
  cycle:int ->
  (Rewards.cycle_rewards, string) result

(** Fetch recent cycle rewards for a baker.

    Uses [GET /v1/rewards/bakers/{baker}?limit=N&sort.desc=cycle].

    @param preferred_base  Optional base URL tried before local endpoints. *)
val fetch_recent_cycles :
  network:string ->
  preferred_base:string option ->
  baker:string ->
  limit:int ->
  (Rewards.cycle_rewards list, string) result

(** Fetch the current head cycle number.

    Uses [GET /v1/head].

    @param preferred_base  Optional base URL tried before local endpoints. *)
val fetch_current_cycle :
  network:string -> preferred_base:string option -> (int, string) result
