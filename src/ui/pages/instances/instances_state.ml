(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Service_state = Data.Service_state
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

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

(** Number of button menu items before services (none after UI revamp) *)
let menu_item_count = 0

(** Index where services start (after radio row + separator).
    Layout: 0 radio row, 1 separator, 2+ services. *)
let services_start_idx = menu_item_count + 2

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
  display_sections : (string * Service_state.t list) list;
  ordered_services : Service_state.t list;
  ordered_service_indices : int StringMap.t;
  (* Inline create-instance dropdown *)
  create_menu_open : bool;
  create_menu_cursor : int; (* 0-4: Node, Baker, DAL Node, Accuser, Signatory *)
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

let group_by_role services =
  let roles = ["node"; "baker"; "accuser"; "dal-node"; "signatory"] in
  List.filter_map
    (fun role ->
      let instances =
        List.filter
          (fun (st : Service_state.t) ->
            st.service.Octez_manager_lib.Service.role = role)
          services
      in
      if instances = [] then None else Some (role, instances))
    roles

let group_display_title (g : Octez_manager_lib.Group.t) =
  let version =
    match g.Octez_manager_lib.Group.bin_source with
    | Octez_manager_lib.Binary_registry.Managed_octez_version v -> v
    | Octez_manager_lib.Binary_registry.Managed_signatory_version v -> v
    | Octez_manager_lib.Binary_registry.Registered_alias a -> a
    | Octez_manager_lib.Binary_registry.Raw_path p -> Filename.basename p
  in
  Printf.sprintf "%s (%s · %s)" g.name g.network version

let group_by_group ~(groups : Octez_manager_lib.Group.t list)
    (services : Service_state.t list) =
  let grouped, ungrouped =
    List.partition
      (fun (st : Service_state.t) ->
        Option.is_some st.service.Octez_manager_lib.Service.group)
      services
  in
  let group_map =
    List.fold_left
      (fun acc (g : Octez_manager_lib.Group.t) -> (g.name, g) :: acc)
      []
      groups
  in
  let group_services : (string * Service_state.t list) list =
    let tbl : (string, Service_state.t list) Hashtbl.t = Hashtbl.create 17 in
    List.iter
      (fun (st : Service_state.t) ->
        match st.service.Octez_manager_lib.Service.group with
        | Some gname ->
            let prev =
              match Hashtbl.find_opt tbl gname with Some l -> l | None -> []
            in
            Hashtbl.replace tbl gname (st :: prev)
        | None -> ())
      grouped ;
    let names =
      Hashtbl.fold (fun k _ acc -> k :: acc) tbl [] |> List.sort String.compare
    in
    List.map
      (fun gname ->
        let svcs =
          match Hashtbl.find_opt tbl gname with Some l -> l | None -> []
        in
        let svcs = sort_services_by_role svcs in
        let title =
          match List.assoc_opt gname group_map with
          | Some g -> group_display_title g
          | None -> gname
        in
        (title, svcs))
      names
  in
  let ungrouped_section =
    if ungrouped = [] then []
    else [("Ungrouped", sort_services_by_role ungrouped)]
  in
  group_services @ ungrouped_section

let display_sections_of_state state =
  match state.view_mode with
  | By_role -> group_by_role state.services
  | By_group -> group_by_group ~groups:state.groups state.services

let build_index_map services =
  List.fold_left
    (fun acc (idx, (svc : Service_state.t)) ->
      StringMap.add svc.service.Octez_manager_lib.Service.instance idx acc)
    StringMap.empty
    (List.mapi (fun idx svc -> (idx, svc)) services)

let rebuild_display_cache state =
  let display_sections = display_sections_of_state state in
  let ordered_services = List.concat_map snd display_sections in
  let ordered_service_indices = build_index_map ordered_services in
  {state with display_sections; ordered_services; ordered_service_indices}

let display_ordered_services state = state.ordered_services

let current_service state =
  if state.selected < services_start_idx then None
  else List.nth_opt state.ordered_services (state.selected - services_start_idx)
