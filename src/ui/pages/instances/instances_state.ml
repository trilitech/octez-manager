(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Service_state = Data.Service_state
module StringSet = Set.Make (String)

type view_mode = By_role | By_group

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

(** Number of menu items before services (Install, Binaries, RPCs buttons) *)
let menu_item_count = 3

(** Index where services start (after menu items + separator line) *)
let services_start_idx = menu_item_count + 1

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
  view_mode : view_mode;
  groups : Octez_manager_lib.Group.t list;
}

type msg = unit

type pstate = state Miaou.Core.Navigation.t

let load_groups () =
  match Octez_manager_lib.Group_registry.list () with
  | Ok groups -> groups
  | Error _ -> []

let clamp_selection services external_services idx =
  (* Total selectable items: menu + managed services + external services *)
  let len =
    services_start_idx + List.length services + List.length external_services
  in
  max 0 (min idx (len - 1))

let role_order = function
  | "node" -> 0
  | "baker" -> 1
  | "accuser" -> 2
  | "dal-node" -> 3
  | "signatory" -> 4
  | _ -> 5

let sort_services_by_role services =
  List.sort
    (fun (a : Service_state.t) (b : Service_state.t) ->
      let rc =
        Int.compare
          (role_order a.service.Octez_manager_lib.Service.role)
          (role_order b.service.Octez_manager_lib.Service.role)
      in
      if rc <> 0 then rc
      else
        String.compare
          a.service.Octez_manager_lib.Service.instance
          b.service.Octez_manager_lib.Service.instance)
    services

let display_ordered_services state =
  match state.view_mode with
  | By_role -> state.services (* already sorted by role *)
  | By_group ->
      (* Grouped first (sorted by group name, then by role), then ungrouped *)
      let grouped, ungrouped =
        List.partition
          (fun (st : Service_state.t) ->
            Option.is_some st.service.Octez_manager_lib.Service.group)
          state.services
      in
      let by_group =
        let tbl : (string, Service_state.t list) Hashtbl.t =
          Hashtbl.create 17
        in
        List.iter
          (fun (st : Service_state.t) ->
            match st.service.Octez_manager_lib.Service.group with
            | Some gname ->
                let prev =
                  match Hashtbl.find_opt tbl gname with
                  | Some l -> l
                  | None -> []
                in
                Hashtbl.replace tbl gname (st :: prev)
            | None -> ())
          grouped ;
        let names =
          Hashtbl.fold (fun k _ acc -> k :: acc) tbl []
          |> List.sort String.compare
        in
        List.concat_map
          (fun gname ->
            match Hashtbl.find_opt tbl gname with
            | Some l -> sort_services_by_role l
            | None -> [])
          names
      in
      by_group @ sort_services_by_role ungrouped

let current_service state =
  if state.selected < services_start_idx then None
  else
    let ordered = display_ordered_services state in
    List.nth_opt ordered (state.selected - services_start_idx)
