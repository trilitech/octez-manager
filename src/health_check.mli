(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Health checks for Octez services.

    Provides functions to wait for services to be ready by polling their
    RPC endpoints. Used during sequential restart to ensure services are
    fully operational before starting dependents. *)

type check_result = {ready : bool; elapsed : float; message : string}

(** Default timeouts in seconds *)
val default_node_timeout : float

val default_dal_timeout : float

val default_poll_interval : float

(** Wait for node RPC to be ready.
    @param rpc_addr The RPC address (host:port)
    @param timeout Maximum time to wait in seconds (default: 120)
    @param poll_interval Time between polls in seconds (default: 2)
    @param on_progress Optional callback called during polling with elapsed time
    @return check_result with ready status and timing info *)
val wait_for_node_rpc :
  ?timeout:float ->
  ?poll_interval:float ->
  ?on_progress:(float -> unit) ->
  rpc_addr:string ->
  unit ->
  check_result

(** Wait for DAL node RPC to be ready.
    @param rpc_addr The RPC address (host:port)
    @param timeout Maximum time to wait in seconds (default: 30)
    @param poll_interval Time between polls in seconds (default: 2)
    @param on_progress Optional callback called during polling with elapsed time
    @return check_result with ready status and timing info *)
val wait_for_dal_rpc :
  ?timeout:float ->
  ?poll_interval:float ->
  ?on_progress:(float -> unit) ->
  rpc_addr:string ->
  unit ->
  check_result

(** Wait for a service to be ready based on its role.
    For nodes and DAL nodes, polls the RPC endpoint.
    For other roles (baker, accuser), returns ready immediately.
    @param svc The service to check
    @param timeout Maximum time to wait (default: 120 for node, 30 for DAL)
    @param on_progress Optional progress callback
    @return check_result *)
val wait_for_service :
  ?timeout:float -> ?on_progress:(float -> unit) -> Service.t -> check_result
