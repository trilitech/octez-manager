(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC Browser actions.

    Action handlers for RPC Browser page interactions. *)

open Octez_manager_lib

(** {1 Service Grouping} *)

(** Group services by network, returning list of (network_name, services). *)
val group_by_network : Service.t list -> (string * Service.t list) list

(** Build modal items with local/public sections and network grouping.
    Returns a flat list where each item carries its section, network,
    and service. *)
val build_instance_items :
  local:Service.t list ->
  public:Service.t list ->
  (string * string * Service.t) list

(** {1 Service Formatting} *)

(** Format a service for display in the instance picker modal.
    For public nodes, shows both label and URL.
    For local nodes, shows just the label.
    @param is_current Whether this service is the currently selected one *)
val format_service_label : Service.t -> is_current:bool -> string

(** {1 Entry Selection} *)

(** Get the currently selected entry (under cursor).
    @return Entry at cursor position, or None if list empty *)
val get_selected_entry :
  Rpc_browser_state.state -> Rpc_browser_state.entry option

(** {1 Navigation Actions} *)

(** Handle Enter key - navigate or execute based on entry kind.
    - SUB: navigate into subdirectory
    - GET: execute GET request
    - DYN: prompt for dynamic value *)
val handle_enter :
  Rpc_browser_state.state -> (Rpc_browser_state.state -> unit) -> unit

(** {1 RPC Execution} *)

(** Build full URL for RPC call.
    @param service Service with RPC endpoint
    @param path Path segments *)
val build_rpc_url : Service.t -> string list -> string

(** Execute GET request for current path.
    @param state Current state
    @param on_update Callback to update state *)
val execute_get :
  Rpc_browser_state.state -> (Rpc_browser_state.state -> unit) -> unit

(** {1 Dynamic Value Prompts} *)

(** Get smart default for dynamic segment.
    Checks history for recent values first, then falls back to hardcoded defaults.
    @param name Segment name (e.g., "chain_id", "block_id")
    @param typ Type hint from OpenAPI
    @param state Current state (for history lookup)
    @return Default value *)
val default_for_dynamic :
  name:string -> typ:string -> Rpc_browser_state.state -> string

(** Open modal to prompt for dynamic segment value.
    Shows recent values from history as hints.
    Records entered value to history.
    @param name Segment name
    @param typ Type hint
    @param state Current state
    @param on_value Callback with entered value
    @param on_update Callback to update state (for recording history) *)
val prompt_dynamic :
  name:string ->
  typ:string ->
  Rpc_browser_state.state ->
  (string -> unit) ->
  (Rpc_browser_state.state -> unit) ->
  unit

(** {1 Instance Cycling} *)

(** Cycle to next instance.
    @param delta Direction (+1 for next, -1 for previous) *)
val cycle_instance :
  delta:int -> Rpc_browser_state.state -> Rpc_browser_state.state

(** {1 Fetch Entries} *)

(** Fetch entries for current path synchronously. *)
val fetch_entries_sync : Rpc_browser_state.state -> Rpc_browser_state.state

(** Fetch entries for current path and call update callback. *)
val fetch_entries :
  Rpc_browser_state.state -> (Rpc_browser_state.state -> unit) -> unit

(** {1 Quick Access Shortcuts} *)

(** Get shortcuts from LRU or defaults: (key, path, description) list *)
val get_shortcuts : Rpc_browser_state.state -> (string * string * string) list

(** Default shortcuts for when no recent paths exist *)
val default_shortcuts : (string * string) list

(** Execute a shortcut by key.
    @param key Key pressed (e.g., "1", "2")
    @param state Current state
    @param on_update Callback to update state
    @return true if shortcut was handled, false otherwise *)
val execute_shortcut :
  key:string ->
  Rpc_browser_state.state ->
  (Rpc_browser_state.state -> unit) ->
  bool

(** {1 Cached Entry Actions} *)

(** Handle Enter on cached entry in Result mode browser panel.
    Uses the cached cursor position to select the entry.
    For Sub/Dyn entries, navigates while staying in Result mode.
    For Get entries, executes the GET and updates the pager. *)
val handle_cached_enter :
  Rpc_browser_state.state -> (Rpc_browser_state.state -> unit) -> unit

(** Navigate back one level in the cached browser while staying in Result mode. *)
val navigate_cached_back :
  Rpc_browser_state.state -> (Rpc_browser_state.state -> unit) -> unit

(** Fetch entries and update only cached_entries (for Result mode browser). *)
val fetch_cached_entries :
  Rpc_browser_state.state -> (Rpc_browser_state.state -> unit) -> unit
