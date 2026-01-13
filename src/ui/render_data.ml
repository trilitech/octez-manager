(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Render-safe data accessors.

    All functions in this module read from in-memory caches and NEVER perform
    I/O. This is the single source of truth for data accessed during rendering.

    See AGENTS.md for the TUI architecture documentation. *)

(* Baker configuration - cached by Delegate_scheduler *)
let baker_has_dal = Delegate_scheduler.baker_has_dal

let baker_delegates = Delegate_scheduler.get_baker_delegates

let baker_node_endpoint = Delegate_scheduler.get_baker_node_endpoint

(* Baker highwatermarks - cached by Delegate_scheduler *)
let baker_highwatermarks = Baker_highwatermarks.get

(* Delegate participation data - cached by Delegate_scheduler *)
let delegate_data = Delegate_data.get

let baker_delegate_data = Delegate_scheduler.get_baker_delegate_data

(* DAL node health - cached by System_metrics_scheduler *)
let dal_health = Dal_health.get

(* Node RPC metrics - cached by Rpc_scheduler *)
let node_rpc_metrics = Rpc_metrics.get

(* System metrics - cached by System_metrics_scheduler *)
let cpu_chart = System_metrics_scheduler.render_cpu_chart

let mem_sparkline = System_metrics_scheduler.render_mem_sparkline

let service_version = System_metrics_scheduler.get_version

let service_disk_size = System_metrics_scheduler.get_disk_size

(* Service states - cached by Data module *)
let service_states () = Data.load_service_states ()
