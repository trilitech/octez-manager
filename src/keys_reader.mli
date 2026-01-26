(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type key_info = {name : string; value : string}

val read_public_key_hashes :
  base_dir:string -> (key_info list, [`Msg of string]) result

module For_tests : sig
  val key_info_of_yojson : Yojson.Safe.t -> (key_info, [`Msg of string]) result
end
