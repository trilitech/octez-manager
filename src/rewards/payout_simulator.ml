(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Payout simulator: runs the full payout pipeline in dry-run mode. *)

type simulation_result = {
  results : Rewards.payout_result list;
  summary : Rewards.cycle_summary;
  wallet_balance : Int64.t option;
  total_needed : Int64.t;
  sufficient_balance : bool option;
}

let simulate ~ctx ~(blueprint : Rewards.payout_blueprint) ?on_progress
    ?batch_size () =
  (* Check wallet balance *)
  let wallet_balance =
    match Payout_executor.fetch_wallet_balance ~ctx with
    | Ok b -> Some b
    | Error _ -> None
  in
  let total_needed =
    List.fold_left
      (fun acc (r : Rewards.delegator_reward) ->
        match r.status with
        | Rewards.Eligible when Int64.compare r.net_reward 0L > 0 ->
            Int64.add acc r.net_reward
        | _ -> acc)
      0L
      blueprint.delegator_rewards
  in
  let total_needed =
    List.fold_left
      (fun acc (_, amount) -> Int64.add acc amount)
      total_needed
      blueprint.bond_payouts
  in
  let total_needed =
    List.fold_left
      (fun acc (_, amount) -> Int64.add acc amount)
      total_needed
      blueprint.fee_payouts
  in
  let sufficient_balance =
    Option.map
      (fun balance -> Int64.compare balance total_needed >= 0)
      wallet_balance
  in
  (* Run executor in dry-run mode *)
  match
    Payout_executor.execute
      ~ctx
      ~blueprint
      ~dry_run:true
      ?on_progress
      ?batch_size
      ()
  with
  | Ok (results, summary) ->
      Ok {results; summary; wallet_balance; total_needed; sufficient_balance}
  | Error msg -> Error msg
