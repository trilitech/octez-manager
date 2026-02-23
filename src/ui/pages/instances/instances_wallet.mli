(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker wallet modal for managing delegate operations.

    Opens from the instance actions menu for baker-role services. Displays
    wallet balances, delegate status, staking parameters, and provides
    access to wallet operations (stake, unstake, transfer, etc.). *)

(** Open the wallet modal for a baker service.
    Reads wallet data from {!Baker_wallet_data} cache and displays
    balances, delegate status, and an operations menu.

    @param svc The baker service to show wallet for. *)
val wallet_modal : svc:Octez_manager_lib.Service.t -> unit
