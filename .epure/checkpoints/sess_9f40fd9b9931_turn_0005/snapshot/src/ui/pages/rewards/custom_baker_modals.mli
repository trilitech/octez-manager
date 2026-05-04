(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Reusable selection modals for custom-baker fields.

    Used by both the [Add custom baker] flow on the Rewards page and the
    Configuration tab's edit-field flow when editing an existing custom
    baker. *)

(** Open an RPC-endpoint selection modal pre-populated with local node
    instances and cached public nodes whose network matches [network]. The
    [Custom…] entry falls back to a validated text prompt accepting
    [host:port]. *)
val prompt_endpoint :
  ?title:string -> network:string -> on_submit:(string -> unit) -> unit -> unit

(** Open a payout-key selection modal listing aliases known to the
    octez-client wallet at [base_dir] ({!Octez_manager_lib.Keys_reader}).
    Falls back to a validated text prompt if [base_dir] holds no keys, or
    when the user picks the [Type alias directly…] entry. *)
val prompt_payout_key :
  ?title:string -> base_dir:string -> on_submit:(string -> unit) -> unit -> unit
