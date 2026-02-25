(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Sandbox orchestration — create and destroy isolated Tezos environments. *)

(** Generate a unique sandbox name. Appends a numeric suffix if the base name
    already exists. *)
val unique_name : base:string -> string

(** Directory for sandbox wallet files. *)
val wallet_dir : sandbox_name:string -> string

(** Find the node service belonging to a sandbox group. *)
val find_sandbox_node :
  group_name:string -> (Service.t option, Rresult.R.msg) result

(** Find the baker service belonging to a sandbox group. *)
val find_sandbox_baker :
  group_name:string -> (Service.t option, Rresult.R.msg) result

(** Create a complete sandbox environment.

    Steps:
    1. Create group (sandbox=true)
    2. Install node with snapshot, --no-bootstrap-peers, --allow-yes-crypto, yes_crypto env
    3. Start node and wait for RPC to become available
    4. Generate yes-wallet with top N active delegates
    5. Install and start baker with wallet as base_dir and yes_crypto env

    @param on_log Optional callback for step-by-step progress messages
    @param network Network name (e.g., "mainnet", "ghostnet")
    @param name Sandbox name (default: generated from network)
    @param rpc_addr Node RPC address (default: auto-assigned)
    @param snapshot Optional snapshot URI
    @param max_delegates Max delegates to impersonate (default: 20)
    @param bin_source Binary source for node and baker *)
val create :
  ?on_log:(string -> unit) ->
  network:string ->
  ?name:string ->
  ?rpc_addr:string ->
  ?snapshot:string ->
  ?max_delegates:int ->
  bin_source:Binary_registry.bin_source ->
  service_user:string ->
  app_bin_dir:string ->
  unit ->
  (Group.t, Rresult.R.msg) result

(** Destroy a sandbox: stop all services, remove them, delete wallet, remove group. *)
val destroy :
  ?on_log:(string -> unit) ->
  group_name:string ->
  unit ->
  (unit, Rresult.R.msg) result
