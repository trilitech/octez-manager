(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type enrichment = {
  balance : string option;
  delegate : string option;
  alias : string option;
}

type validation_result = Valid | Invalid of string

(** Base58check alphabet used by Tezos addresses. *)
let base58_alphabet =
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

let is_base58_char c = String.contains base58_alphabet c

let validate_format pkh =
  let pkh = String.trim pkh in
  let len = String.length pkh in
  if len = 0 then Invalid "Empty address"
  else if len <> 36 then
    Invalid (Printf.sprintf "Expected 36 characters, got %d" len)
  else
    let prefix = String.sub pkh 0 3 in
    if
      not
        (String.equal prefix "tz1" || String.equal prefix "tz2"
       || String.equal prefix "tz3" || String.equal prefix "tz4")
    then Invalid (Printf.sprintf "Invalid prefix: %s (expected tz1-tz4)" prefix)
    else
      let payload = String.sub pkh 3 (len - 3) in
      let all_base58 = String.to_seq payload |> Seq.for_all is_base58_char in
      if not all_base58 then Invalid "Contains invalid characters (not base58)"
      else Valid

let enrich ~pkh:_ ~network:_ =
  (* TODO: https://github.com/trilitech/octez-manager/issues/760
     Implement RPC fetch for balance + delegate, tzkt alias lookup *)
  None
