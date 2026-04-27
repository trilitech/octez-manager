(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Registry of custom (non-managed) baker entries for the Rewards page.

    Custom bakers are bakers whose keys are held by the operator but whose
    Octez services are not managed by octez-manager.  This module persists
    their metadata in
    [<registry_root>/rewards/custom_bakers.json] so that the Rewards page
    can offer payout services without a {!Service_registry} row. *)

(** {1 Entry type} *)

(** A single custom-baker entry. *)
type entry = {
  instance : string;
      (** Synthetic handle: ["custom-<network>-<pkh-prefix-8>"].  Stable
          across restarts; used as a map key throughout the rewards pipeline. *)
  baker_pkh : string;
      (** Baker public-key hash.  Only [tz1]/[tz2]/[tz3]/[tz4] are accepted. *)
  network : string;
      (** Network identifier (e.g. ["mainnet"], ["ghostnet"]).  Sanitized to
          alphanumeric + [_-.] characters. *)
  label : string option;
      (** Optional human-readable label shown in the baker selector. *)
  endpoint : string;  (** RPC endpoint in ["host:port"] format. *)
  payout_key_alias : string;
      (** Alias of the payout key in the [octez-client] wallet at
          [base_dir]. *)
  base_dir : string;  (** [octez-client] base directory (wallet directory). *)
  octez_client_bin : string;
      (** Absolute path to the [octez-client] binary, materialized at creation
          time via the {!resolve_octez_client_bin} cascade. *)
  added_at : string;  (** ISO-8601 timestamp of when the entry was created. *)
}

(** {1 CRUD operations} *)

(** Return all custom-baker entries.  Returns [[]] when the backing file does
    not exist or is empty. *)
val list : unit -> entry list

(** Find a custom-baker entry by its instance handle.
    Returns [None] if not found. *)
val find : instance:string -> entry option

(** Persist a new custom-baker entry.

    Returns [Error msg] when:
    - [entry.instance] already exists in the custom-baker file,
    - [entry.instance] matches a managed-service instance in the service
      registry (detected by scanning [<registry_root>/services/]),
    - [entry.instance] matches a synthetic test-baker instance derived from
      the [OM_TEST_BAKER] environment variable, or
    - [entry.instance] fails {!Systemd_validate.validate_instance_name}. *)
val add : entry -> (unit, string) result

(** Remove the custom-baker entry identified by [instance].
    Returns [Error msg] if the entry does not exist. *)
val remove : instance:string -> (unit, string) result

(** {1 Helper functions} *)

(** [build_instance_handle ~network ~baker_pkh] builds the synthetic instance
    handle ["custom-<network>-<first 8 chars of baker_pkh>"].

    Returns [Error msg] when:
    - [baker_pkh] is not a valid baker PKH (see
      {!Payout_config.is_valid_baker_pkh}), or
    - [network] is empty or contains characters outside alphanumeric + [_-.]. *)
val build_instance_handle :
  network:string -> baker_pkh:string -> (string, string) result

(** [resolve_octez_client_bin ()] resolves the [octez-client] binary via the
    three-tier cascade:

    1. [<registry_root>/rewards/octez_bin_dir] override file (if present),
       looking for [octez-client] inside the recorded directory.
    2. Newest managed Octez version under
       [$XDG_DATA_HOME/octez-manager/binaries/v<version>/octez-client].
    3. [$PATH] lookup of ["octez-client"].

    Returns [Error msg] if none of the three tiers yields a usable executable.

    Note: tiers 1 and 2 rely on raw filesystem paths derived from
    {!Paths.registry_root} / {!Paths.xdg_data_home} rather than the higher-
    level registry modules, because [Custom_baker_registry] lives in the
    [octez_manager_rewards] library which does not depend on
    [octez_manager_lib]. *)
val resolve_octez_client_bin : unit -> (string, string) result

(** [validate_endpoint s] checks that [s] has the form ["host:port"] where:
    - [host] is non-empty, and
    - [port] parses as an integer in the range [1..65535].

    Returns [Ok ()] on success and [Error msg] on validation failure. *)
val validate_endpoint : string -> (unit, string) result
