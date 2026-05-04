(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** JSON folding operations for the RPC Browser.

    Provides fold/unfold operations on the focused pager's JSON content,
    using {!Foldable_json} for the fold state and {!Rpc_browser_pagers} for
    pager slot access. *)

(** Toggle fold at a specific line in the JSON view of the focused pager.
    @param line Line number to toggle fold at *)
val toggle_fold : line:int -> Rpc_browser_types.state -> Rpc_browser_types.state

(** Unfold all JSON sections in the focused pager. *)
val unfold_all : Rpc_browser_types.state -> Rpc_browser_types.state

(** Fold all JSON sections in the focused pager. *)
val fold_all : Rpc_browser_types.state -> Rpc_browser_types.state
