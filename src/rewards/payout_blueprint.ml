(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

let is_already_paid ~instance ~cycle =
  Payout_report.cycle_is_paid ~instance ~cycle

let generate_from_data ~config ~network ~(cycle_rewards : Rewards.cycle_rewards)
    ~instance ?(force = false) () =
  if (not force) && is_already_paid ~instance ~cycle:cycle_rewards.cycle then
    Error
      (Printf.sprintf
         "Cycle %d has already been paid. Use --force to override."
         cycle_rewards.cycle)
  else Ok (Reward_calculator.generate_blueprint ~config ~network ~cycle_rewards)

let generate ~instance ~baker ~network ~cycle ?(force = false) () =
  if (not force) && is_already_paid ~instance ~cycle then
    Error
      (Printf.sprintf
         "Cycle %d has already been paid. Use --force to override."
         cycle)
  else
    let config =
      match Payout_config.load ~instance with
      | Ok c -> c
      | Error _ -> Payout_config.default ~baker_pkh:baker
    in
    let tzkt_url = config.tzkt_url in
    match Cycle_data.fetch_cycle ~tzkt_url ~baker ~cycle with
    | Error msg -> Error msg
    | Ok cycle_rewards ->
        Ok
          (Reward_calculator.generate_blueprint ~config ~network ~cycle_rewards)
