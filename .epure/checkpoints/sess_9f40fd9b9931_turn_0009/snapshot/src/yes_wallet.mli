(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure wallet data generation for sandbox mode.

    Generates octez-client wallet files with hardcoded test keys.
    With [yes_crypto] enabled, any valid secret key can sign for any public key,
    so these test keys let a baker impersonate any delegate. *)

(** Elliptic curve type, inferred from address prefix. *)
type curve = Ed25519 | Secp256k1 | P256 | BLS

(** A delegate entry for wallet generation. *)
type delegate = {alias : string; address : string; curve : curve}

(** Detect curve from a Tezos address prefix.

    @return [Some curve] for tz1/tz2/tz3/tz4, [None] for unrecognized. *)
val curve_of_address : string -> curve option

(** Return the hardcoded test (secret_key, public_key) pair for a given curve. *)
val test_keys_for_curve : curve -> string * string

(** Generate the three wallet JSON values for a list of delegates.

    @return [(public_key_hashs, public_keys, secret_keys)] as [Yojson.Safe.t]
    triples. Duplicates (by address) are removed, keeping the first occurrence. *)
val generate_wallet_json :
  delegate list -> Yojson.Safe.t * Yojson.Safe.t * Yojson.Safe.t

(**/**)

module Internal_for_tests : sig
  val test_keys_for_curve : curve -> string * string
end

(**/**)
