(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Service_state = Data.Service_state
module StringSet = Set.Make (String)

type display_item = Real_service of Service_state.t | Ghost_add_new of string

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

(** Index where services start. The radio row is visible but not navigable,
    so services occupy all navigation indices starting from 0. *)
let services_start_idx = 0

type state = {
  services : Service_state.t list;
  external_services : Octez_manager_lib.External_service.t list;
  selected : int;
  folded : StringSet.t; (* managed instance names that are folded *)
  external_folded : StringSet.t; (* external instance names that are folded *)
  external_section_folded : bool;
      (* when true, Unmanaged section collapses to a single header line *)
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

(** Helper: Insert ghost "Add new" entry after last instance of a role *)
let insert_ghost_for_role role services =
  let real_services = List.map (fun s -> Real_service s) services in
  real_services @ [Ghost_add_new role]

(** Build display items with ghost entries injected after each role section *)
let display_ordered_items state =
  match state.view_mode with
  | By_role ->
      (* Group by role and inject ghost after each role *)
      let roles =
        ["node"; "baker"; "accuser"; "dal-node"; "signatory"; "index"]
      in
      List.concat_map
        (fun role ->
          let instances =
            List.filter
              (fun (st : Service_state.t) ->
                String.equal st.service.Octez_manager_lib.Service.role role)
              state.services
          in
          if instances = [] then
            (* No instances for this role, show only ghost *)
            [Ghost_add_new role]
          else
            (* Show instances followed by ghost *)
            insert_ghost_for_role role instances)
        roles
  | By_group ->
      (* In group mode, show ghost at the end of each group and ungrouped section *)
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
        (* For grouped mode, we add ghosts for all 5 roles at the end of each group *)
        List.concat_map
          (fun gname ->
            match Hashtbl.find_opt tbl gname with
            | Some l ->
                let sorted = sort_services_by_role l in
                (* Add all role ghosts after the group *)
                List.map (fun s -> Real_service s) sorted
                @ [
                    Ghost_add_new "node";
                    Ghost_add_new "baker";
                    Ghost_add_new "accuser";
                    Ghost_add_new "dal-node";
                    Ghost_add_new "signatory";
                    Ghost_add_new "index";
                  ]
            | None -> [])
          names
      in
      (* Add ungrouped services + ghosts for all roles *)
      let ungrouped_items =
        if ungrouped = [] then
          (* No ungrouped, just show all role ghosts *)
          [
            Ghost_add_new "node";
            Ghost_add_new "baker";
            Ghost_add_new "accuser";
            Ghost_add_new "dal-node";
            Ghost_add_new "signatory";
            Ghost_add_new "index";
          ]
        else
          List.map (fun s -> Real_service s) (sort_services_by_role ungrouped)
          @ [
              Ghost_add_new "node";
              Ghost_add_new "baker";
              Ghost_add_new "accuser";
              Ghost_add_new "dal-node";
              Ghost_add_new "signatory";
              Ghost_add_new "index";
            ]
      in
      by_group @ ungrouped_items

let clamp_selection_with_items items idx =
  let len = services_start_idx + List.length items in
  max 0 (min idx (len - 1))

let current_service state =
  (* Selection is always >= services_start_idx (= 0) *)
  let ordered = display_ordered_items state in
  match List.nth_opt ordered (state.selected - services_start_idx) with
  | Some (Real_service st) -> Some st
  | Some (Ghost_add_new _) | None -> None
