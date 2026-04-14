(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker wallet operations via octez-client.

    Builds and executes octez-client commands for delegate operations:
    register, stake, unstake, finalize unstake, transfer,
    set delegate parameters, update consensus key, and governance voting.

    Each command is executed with a 100-second timeout; the process is
    killed (SIGTERM) if it does not complete in time. *)

(** {2 Types} *)

(** A wallet operation to execute. *)
type wallet_operation =
  | Register
  | Stake of {amount : string}
  | Unstake of {amount : string}
  | Finalize_unstake
  | Transfer of {amount : string; destination : string}
  | Set_delegate_params of {limit : int; edge : int}
  | Update_consensus_key of {delegate_alias : string; key_alias : string}
  | Update_companion_key of {delegate_alias : string; key_alias : string}
  | Submit_proposals of {proposals : string list}
  | Submit_ballot of {proposal : string; ballot : Baker_wallet_data.ballot_vote}

(** Result of executing a wallet operation. *)
type operation_result = {
  success : bool;
  op_hash : string option;
  error : string option;
}

(** {2 Command Building} *)

(** Build the octez-client argv list for an operation without executing it.
    Useful for display in confirmation prompts and testing.
    @param octez_client_bin  Path to the octez-client binary
    @param endpoint          Node RPC endpoint URL
    @param base_dir          Optional base directory for octez-client
    @param password_file     Optional path to password file for encrypted keys
    @param alias             Baker key alias in octez-client
    @param op                The operation to build *)
val build_command :
  octez_client_bin:string ->
  endpoint:string ->
  base_dir:string option ->
  password_file:string option ->
  alias:string ->
  op:wallet_operation ->
  string list

(** {2 Execution} *)

(** Execute a wallet operation via octez-client.
    The process is killed after 100 seconds if it doesn't complete.
    @param instance_name     Instance name (used for logging)
    @param octez_client_bin  Path to the octez-client binary
    @param endpoint          Node RPC endpoint URL
    @param base_dir          Optional base directory for octez-client
    @param password_file     Optional path to password file for encrypted keys
    @param alias             Baker key alias in octez-client
    @param op                The operation to execute
    @return Operation result with hash on success or error message *)
val execute :
  instance_name:string ->
  octez_client_bin:string ->
  endpoint:string ->
  base_dir:string option ->
  password_file:string option ->
  alias:string ->
  op:wallet_operation ->
  operation_result

(** Estimate fees for a wallet operation via octez-client dry-run.
    @param instance_name     Instance name (used for logging)
    @param octez_client_bin  Path to the octez-client binary
    @param endpoint          Node RPC endpoint URL
    @param base_dir          Optional base directory for octez-client
    @param password_file     Optional path to password file for encrypted keys
    @param alias             Baker key alias in octez-client
    @param op                The operation to estimate
    @return Estimated fee string on success, error message on failure *)
val estimate_fee :
  instance_name:string ->
  octez_client_bin:string ->
  endpoint:string ->
  base_dir:string option ->
  password_file:string option ->
  alias:string ->
  op:wallet_operation ->
  (string, string) result

(** {2 Helpers} *)

(** Human-readable description of an operation for confirmation prompts.
    Example: ["Stake 1,000.000000 ꜩ"] *)
val describe_operation : wallet_operation -> string

(** Extract the operation hash from octez-client output.
    Looks for ['oo...'] hashes in lines like ["Operation hash is 'oo...'"].
    Returns [None] if no hash is found. *)
val extract_op_hash : string -> string option
