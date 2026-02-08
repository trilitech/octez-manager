(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type key_info = {name : string; value : string}

(** Read public key hashes from the [public_key_hashs] file in [base_dir].
    Returns a list of key aliases and their hashes. *)
val read_public_key_hashes :
  base_dir:string -> (key_info list, [`Msg of string]) result

module For_tests : sig
  (** Parse a single key_info entry from JSON. *)
  val key_info_of_yojson : Yojson.Safe.t -> (key_info, [`Msg of string]) result
end
