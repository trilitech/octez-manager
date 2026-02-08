(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Persistence for RPC browser dynamic history and recent paths.

    Handles loading and saving dynamic segment history and recent path
    shortcuts to disk as JSON files. *)

(** Load recent paths from disk.
    @return List of recent paths, or empty list on failure. *)
val load_recent_paths : unit -> Rpc_browser_types.recent_path list

(** Save recent paths to disk. Silently ignores errors. *)
val save_recent_paths : Rpc_browser_types.recent_path list -> unit

(** Load dynamic history from disk.
    @return List of dynamic values, or empty list on failure. *)
val load_dynamic_history : unit -> Rpc_browser_types.dynamic_value list

(** Save dynamic history to disk. Silently ignores errors. *)
val save_dynamic_history : Rpc_browser_types.dynamic_value list -> unit
