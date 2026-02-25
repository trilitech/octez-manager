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

(** Find all node services belonging to a sandbox group. *)
val find_sandbox_nodes :
  group_name:string -> (Service.t list, Rresult.R.msg) result

(** Find all baker services belonging to a sandbox group. *)
val find_sandbox_bakers :
  group_name:string -> (Service.t list, Rresult.R.msg) result

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

(** Add a second (or Nth) node to an existing sandbox.

    Exports a snapshot from node 1, allocates new RPC/P2P ports, and installs
    the new node with [{--peer node1_p2p}] so it connects to the primary node.

    @param on_log Optional callback for step-by-step progress messages
    @param group_name Name of the sandbox group *)
val add_node :
  ?on_log:(string -> unit) ->
  group_name:string ->
  unit ->
  (Service.t, Rresult.R.msg) result

(** Add a baker to an existing sandbox.

    Installs a new baker service with [--force-apply-from-round 0] and
    yes-crypto env, using the sandbox wallet as base_dir.

    @param on_log Optional callback for step-by-step progress messages
    @param group_name Name of the sandbox group
    @param node_instance Node instance the baker should connect to
    @param delegates Consensus key aliases to assign to this baker *)
val add_baker :
  ?on_log:(string -> unit) ->
  group_name:string ->
  node_instance:string ->
  delegates:string list ->
  unit ->
  (Service.t, Rresult.R.msg) result

(** Destroy a sandbox: stop all services, remove them, delete wallet, remove group. *)
val destroy :
  ?on_log:(string -> unit) ->
  group_name:string ->
  unit ->
  (unit, Rresult.R.msg) result
