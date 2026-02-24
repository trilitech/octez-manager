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

(** {2 Operation tracking} *)

(** Progress of an on-chain operation. *)
type tracking_step =
  | Submitting
  | Submitted of {op_hash : string}
  | Included of {op_hash : string; block_hash : string}
  | Confirmed of {op_hash : string; block_hash : string}
  | Finalized of {op_hash : string; block_hash : string}
  | Failed of string

(** Open a modal that shows a real-time operation checklist
    (submitting → included → confirmed → finalized).
    The modal reads from [step_ref] on each render tick. *)
val open_tracking_modal :
  title:string -> network:string -> step_ref:tracking_step Atomic.t -> unit

(** Poll the chain until the operation is included and finalized.
    Updates [step_ref] at each stage. Blocks the calling fiber. *)
val poll_operation :
  endpoint:string -> op_hash:string -> tracking_step Atomic.t -> unit

(** {2 Wallet modal} *)

(** Open the wallet modal for a baker service.
    Reads wallet data from {!Baker_wallet_data} cache and displays
    balances, delegate status, and an operations menu.

    @param svc The baker service to show wallet for. *)
val wallet_modal : svc:Octez_manager_lib.Service.t -> unit
