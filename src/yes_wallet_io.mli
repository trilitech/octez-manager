(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** I/O layer for yes-wallet: delegate fetching and wallet file writing. *)

(** Fetch active delegates from a node RPC endpoint, including their
    consensus and companion keys needed for signing.

    Calls [/chains/main/blocks/head/context/delegates?active=true&with_minimal_stake=true],
    takes the first [max_delegates], then fetches the consensus key and companion
    key for each delegate (needed when delegates use BLS consensus keys).

    Returns [(baker_delegates, all_wallet_entries)] where [baker_delegates] is
    the consensus key aliases to pass as baker CLI args (the baker daemon uses
    consensus key hashes to query attestation rights), and [all_wallet_entries]
    is the full list to write to the wallet (includes consensus/companion keys).
    For delegates without a distinct consensus key, the delegate alias is used.

    @param endpoint Node RPC endpoint (e.g. ["http://127.0.0.1:18732"])
    @param max_delegates Maximum number of delegates to return *)
val fetch_delegates :
  endpoint:string ->
  max_delegates:int ->
  ( Yes_wallet.delegate list * Yes_wallet.delegate list,
    [> `Msg of string] )
  result

(** Write the three wallet JSON files to a directory.

    Creates [wallet_dir] if needed, then writes [public_key_hashs],
    [public_keys], and [secret_keys] files.

    @param wallet_dir Target directory *)
val write_wallet :
  wallet_dir:string ->
  Yes_wallet.delegate list ->
  (unit, [> `Msg of string]) result

(** Read the [public_key_hashs] file from a wallet directory.

    Returns a list of [(alias, address)] pairs. Returns [[]] if the file does
    not exist. Used to inspect wallet contents without rewriting. *)
val read_wallet_pkhs :
  wallet_dir:string -> ((string * string) list, [> `Msg of string]) result

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
