(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Import external [config.hjson] into an octez-manager payout configuration.

    Maps supported fields to the internal {!Payout_config.t} type.
    Collects warnings for unsupported features (extensions, social notifications).
    Returns both the imported configuration and a list of human-readable warnings. *)

(** {1 Import result} *)

type import_result = {
  config : Payout_config.t;
  warnings : string list;
  imported_fields : int;
}

(** {1 Import} *)

(** Import a configuration from an HJSON string.
    @param baker_pkh Baker address to associate with the configuration.
    @return Imported configuration with warnings, or an error message. *)
val import_string : baker_pkh:string -> string -> (import_result, string) result

(** Import a configuration from a file path.
    @param baker_pkh Baker address to associate with the configuration.
    @return Imported configuration with warnings, or an error message. *)
val import_file : baker_pkh:string -> string -> (import_result, string) result
