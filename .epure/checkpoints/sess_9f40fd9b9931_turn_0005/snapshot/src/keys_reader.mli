(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** {1 Basic key info} *)

type key_info = {name : string; value : string}

(** Read public key hashes from the [public_key_hashs] file in [base_dir].
    Returns a list of key aliases and their hashes. *)
val read_public_key_hashes :
  base_dir:string -> (key_info list, [`Msg of string]) result

(** {1 Enriched key metadata} *)

(** How the key is stored or accessed.

    Derived from the URI scheme in [secret_keys] and [public_keys] files:
    - [unencrypted:] → {!Unencrypted}
    - [encrypted:] → {!Encrypted}
    - [ledger://device] → {!Ledger}
    - [tcp://host:port] → {!Remote}

    Keys present only in [public_key_hashs] are treated as watch-only
    ({!Unencrypted} with [has_secret_key = false]). *)
type key_kind =
  | Unencrypted  (** Plaintext secret key *)
  | Encrypted  (** Password-encrypted secret key *)
  | Ledger of string  (** Hardware wallet (device path) *)
  | Remote of string  (** Remote signer (host:port) *)

(** Enriched key information from cross-referencing all three octez-client
    key files ([public_key_hashs], [public_keys], [secret_keys]). *)
type key_metadata = {
  alias : string;  (** Key alias (unique within a base directory) *)
  pkh : string;  (** Public key hash: tz1/tz2/tz3/tz4 *)
  public_key : string option;  (** Public key (edpk/sppk/p2pk/BLpk) *)
  key_kind : key_kind;  (** How the key is stored *)
  has_secret_key : bool;  (** Whether a secret key is available *)
}

(** Read enriched key metadata from all three key files in [base_dir].

    Cross-references [public_key_hashs], [public_keys], and [secret_keys]
    by alias. Returns one {!key_metadata} per alias found in
    [public_key_hashs]. Missing entries in the other files are handled
    gracefully (e.g. a key only in [public_key_hashs] is watch-only). *)
val read_keys_full :
  base_dir:string -> (key_metadata list, [`Msg of string]) result

(** {1 Testing} *)

(**/**)

module For_tests : sig
  val key_info_of_yojson : Yojson.Safe.t -> (key_info, [`Msg of string]) result

  (** Parse the URI scheme from a key locator string to determine key kind. *)
  val key_kind_of_locator : string -> key_kind
end

(**/**)
