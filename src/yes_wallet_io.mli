(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** I/O layer for yes-wallet: delegate fetching and wallet file writing. *)

(** Fetch active delegates from a node RPC endpoint.

    Calls [/chains/main/blocks/head/context/delegates?active=true&with_minimal_stake=true],
    takes the first [max_delegates], detects curve from address prefix, and
    assigns sequential aliases [delegate-0], [delegate-1], etc.

    @param endpoint Node RPC endpoint (e.g. ["http://127.0.0.1:18732"])
    @param max_delegates Maximum number of delegates to return *)
val fetch_delegates :
  endpoint:string ->
  max_delegates:int ->
  (Yes_wallet.delegate list, [> `Msg of string]) result

(** Write the three wallet JSON files to a directory.

    Creates [wallet_dir] if needed, then writes [public_key_hashs],
    [public_keys], and [secret_keys] files.

    @param wallet_dir Target directory *)
val write_wallet :
  wallet_dir:string ->
  Yes_wallet.delegate list ->
  (unit, [> `Msg of string]) result

(** Add a single account to an existing wallet directory.

    Reads existing wallet files, appends the new delegate, and rewrites.
    Returns error if address already exists or has invalid prefix.

    @param wallet_dir Existing wallet directory
    @param address Tezos address (tz1/tz2/tz3/tz4)
    @param alias Optional alias (default: [delegate-N] where N is next index) *)
val add_account :
  wallet_dir:string ->
  address:string ->
  ?alias:string ->
  unit ->
  (string, [> `Msg of string]) result
