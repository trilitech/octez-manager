(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Service_state = Data.Service_state
module StringSet = Set.Make (String)

(** Track recent start/restart failures for display.
    Maps instance name to (error_message, timestamp) *)
let recent_failures : (string, string * float) Hashtbl.t = Hashtbl.create 16

let recent_failure_ttl = 30.0 (* seconds to keep showing failure *)

let record_failure ~instance ~error =
  Hashtbl.replace recent_failures instance (error, Unix.gettimeofday ())

let clear_failure ~instance = Hashtbl.remove recent_failures instance

let get_recent_failure ~instance =
  match Hashtbl.find_opt recent_failures instance with
  | Some (error, ts) when Unix.gettimeofday () -. ts < recent_failure_ttl ->
      Some error
  | Some _ ->
      (* Expired, clean up *)
      Hashtbl.remove recent_failures instance ;
      None
  | None -> None

(** Number of menu items before services.
    1 = Install button only (no update available)
    2 = Upgrade + Install buttons (update available) *)
let menu_item_count () =
  if Self_update_scheduler.update_available () then 2 else 1

(** Index where services start (after menu items + separator line) *)
let services_start_idx () = menu_item_count () + 1

type state = {
  services : Service_state.t list;
  external_services : Octez_manager_lib.External_service.t list;
  selected : int;
  folded : StringSet.t; (* managed instance names that are folded *)
  external_folded : StringSet.t; (* external instance names that are folded *)
  last_updated : float;
  (* Matrix layout state *)
  num_columns : int; (* number of columns based on terminal width *)
  active_column : int; (* which column has focus, 0-indexed *)
  column_scroll : int array; (* scroll offset per column *)
}

type msg = unit

type pstate = state Miaou.Core.Navigation.t

let clamp_selection services external_services idx =
  (* Total selectable items: menu + managed services + external services *)
  let len =
    services_start_idx () + List.length services + List.length external_services
  in
  max 0 (min idx (len - 1))

let current_service state =
  let start_idx = services_start_idx () in
  if state.selected < start_idx then None
  else List.nth_opt state.services (state.selected - start_idx)
