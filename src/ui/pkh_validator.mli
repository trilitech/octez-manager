(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Public key hash validation and live enrichment.

    Validates PKH format (tz1-4 base58check) and provides background
    enrichment that fetches balance, delegate status, and tzkt alias
    for valid addresses. Used by transfer destination, delegate-to picker,
    and key import flows.

    Enrichment is debounced (500ms) and uses Job_manager to avoid
    blocking the render loop. *)

(** Result of enriching a valid PKH with on-chain data. *)
type enrichment = {
  balance : string option;  (** Spendable balance in mutez *)
  delegate : string option;  (** Delegate PKH, if delegating *)
  alias : string option;  (** tzkt alias, if known *)
}

(** Format validation result. *)
type validation_result =
  | Valid  (** PKH matches tz[1-4] base58check format *)
  | Invalid of string  (** Reason the PKH is invalid *)

(** Strip non-ASCII bytes and trim whitespace. Apply before storing a PKH
    received from clipboard input. *)
val sanitize : string -> string

(** Validate PKH format without any I/O.
    Strips unicode whitespace (e.g. non-breaking spaces inserted by browsers
    when copying addresses) before checking prefix (tz1-4), base58check
    character set, and length. *)
val validate_format : string -> validation_result

(** Enrich a valid PKH with on-chain data.
    Fetches balance, delegate, and tzkt alias from the given network.
    Returns [None] if the address is not found on-chain. *)
val enrich : pkh:string -> network:string -> enrichment option
