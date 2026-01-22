(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module Service_state = Data.Service_state
open Instances_state

(** Layout configuration constants *)
let min_column_width = 50

let column_separator = "   "

let role_order = function
  | "node" -> 0
  | "baker" -> 1
  | "accuser" -> 2
  | "dal-node" -> 3
  | "signer" -> 4
  | _ -> 5

(** Role section headers *)
let role_header = function
  | "node" -> "── Nodes ──"
  | "baker" -> "── Bakers ──"
  | "accuser" -> "── Accusers ──"
  | "dal-node" -> "── DAL Nodes ──"
  | "signer" -> "── Signers ──"
  | r -> Printf.sprintf "── %s ──" (String.capitalize_ascii r)

(** Sort services by role, then by instance name *)
let sort_services services =
  List.sort
    (fun (a : Service_state.t) (b : Service_state.t) ->
      let role_cmp =
        compare
          (role_order a.service.Service.role)
          (role_order b.service.Service.role)
      in
      if role_cmp <> 0 then role_cmp
      else String.compare a.service.Service.instance b.service.Service.instance)
    services

let load_services () = Data.load_service_states () |> sort_services

let load_services_fresh () =
  Data.load_service_states ~detail:false () |> sort_services

let load_external_services () = External_services_scheduler.get ()

(** Calculate number of columns based on terminal width *)
let calc_num_columns ~cols ~min_column_width ~column_separator =
  let separator_width = String.length column_separator in
  let available = cols - separator_width in
  (* At least 1 column, max based on available width *)
  max 1 (available / (min_column_width + separator_width))

(** Group services by role, preserving order *)
let group_by_role services =
  let roles = ["node"; "baker"; "accuser"; "dal-node"; "signer"] in
  List.filter_map
    (fun role ->
      let instances =
        List.filter
          (fun (st : Service_state.t) -> st.service.Service.role = role)
          services
      in
      if instances = [] then None else Some (role, instances))
    roles

(** Distribute role groups across columns, balancing by instance count.
    Returns: column index -> list of (role, instances) *)
let distribute_to_columns ~num_columns role_groups =
  if num_columns <= 1 then [|role_groups|]
  else
    let columns = Array.make num_columns [] in
    let column_counts = Array.make num_columns 0 in
    (* Assign each role group to the column with fewest instances *)
    List.iter
      (fun ((_role, instances) as group) ->
        let min_col =
          let min_idx = ref 0 in
          for i = 1 to num_columns - 1 do
            if column_counts.(i) < column_counts.(!min_idx) then min_idx := i
          done ;
          !min_idx
        in
        columns.(min_col) <- columns.(min_col) @ [group] ;
        column_counts.(min_col) <-
          column_counts.(min_col) + List.length instances + 2
        (* +2 for header and spacing *))
      role_groups ;
    columns

(** Get flat list of services for a column, with their global indices *)
type column_item =
  | Header of string
  | Instance of int * Service_state.t (* global index, service *)

let column_items ~column_groups ~global_services =
  List.concat_map
    (fun (role, instances) ->
      let header = Header (role_header role) in
      let items =
        List.map
          (fun (st : Service_state.t) ->
            (* Find global index *)
            let idx =
              List.find_mapi
                (fun i (s : Service_state.t) ->
                  if
                    String.equal
                      s.service.Service.instance
                      st.service.Service.instance
                  then Some i
                  else None)
                global_services
              |> Option.value ~default:0
            in
            Instance (idx, st))
          instances
      in
      header :: items)
    column_groups

(** Get list of global service indices in a column *)
let column_service_indices ~column_groups ~global_services =
  column_items ~column_groups ~global_services
  |> List.filter_map (function
    | Header _ -> None
    | Instance (idx, _) -> Some idx)

(** Get first service index in a column *)
let first_service_in_column ~num_columns ~services col =
  if num_columns <= 1 then 0
  else
    let role_groups = group_by_role services in
    let columns = distribute_to_columns ~num_columns role_groups in
    if col >= Array.length columns then 0
    else
      let indices =
        column_service_indices
          ~column_groups:columns.(col)
          ~global_services:services
      in
      match indices with [] -> 0 | first :: _ -> first

(** Get all service indices in a column *)
let services_in_column ~num_columns ~services col =
  if num_columns <= 1 then List.mapi (fun i _ -> i) services
  else
    let role_groups = group_by_role services in
    let columns = distribute_to_columns ~num_columns role_groups in
    if col >= Array.length columns then []
    else
      column_service_indices
        ~column_groups:columns.(col)
        ~global_services:services

(** Find which column contains a given service index *)
let column_for_service ~num_columns ~services idx =
  if num_columns <= 1 then 0
  else
    let rec find_col col =
      if col >= num_columns then 0
      else
        let indices = services_in_column ~num_columns ~services col in
        if List.mem idx indices then col else find_col (col + 1)
    in
    find_col 0

(** Calculate line position of a service within its column.
    Returns (start_line, line_count) where start_line is 0-indexed
    from the top of the column content (after headers). *)
let service_line_position ~num_columns ~services ~folded svc_idx col =
  if num_columns <= 1 then (0, 1)
  else
    let role_groups = group_by_role services in
    let columns = distribute_to_columns ~num_columns role_groups in
    if col >= Array.length columns then (0, 1)
    else
      let column_groups = columns.(col) in
      let items = column_items ~column_groups ~global_services:services in
      let rec count_lines line_acc is_first = function
        | [] -> (line_acc, 1)
        | Header _ :: rest ->
            (* Header + possible empty line before it *)
            let header_lines = if is_first then 1 else 2 in
            count_lines (line_acc + header_lines) false rest
        | Instance (idx, st) :: rest ->
            let is_folded = StringSet.mem st.service.Service.instance folded in
            let line_count = if is_folded then 2 else 6 in
            (* Approximate: 2 lines folded, ~6 unfolded *)
            if idx = svc_idx then (line_acc, line_count)
            else count_lines (line_acc + line_count) false rest
      in
      count_lines 0 true items

(** Adjust column scroll to keep selection visible.
    Mutates column_scroll array in place. *)
let adjust_column_scroll ~column_scroll ~col ~line_start ~line_count
    ~visible_height =
  let scroll = column_scroll.(col) in
  let new_scroll =
    if line_start < scroll then line_start
    else if line_start + line_count > scroll + visible_height then
      line_start + line_count - visible_height
    else scroll
  in
  column_scroll.(col) <- max 0 new_scroll

(* Mutable reference to track visible height for scroll calculations *)
let last_visible_height_ref = ref 20

(** Find first non-empty column, or None if all empty *)
let find_non_empty_column ~num_columns ~services =
  let rec find col =
    if col >= num_columns then None
    else
      let indices = services_in_column ~num_columns ~services col in
      if indices <> [] then Some col else find (col + 1)
  in
  find 0

(** Ensure active_column points to a non-empty column, adjusting selection if needed.
    If all columns are empty (no services), move selection to menu. *)
let ensure_valid_column state =
  if state.services = [] && state.external_services = [] then
    (* No services at all, go to "Install new instance" *)
    {state with selected = 0; active_column = 0}
  else if state.services = [] then
    (* Only external services, keep selection as-is *)
    state
  else if state.num_columns <= 1 then state
  else
    let current_indices =
      services_in_column
        ~num_columns:state.num_columns
        ~services:state.services
        state.active_column
    in
    if current_indices <> [] then state
    else
      (* Current column is empty, find a non-empty one *)
      match
        find_non_empty_column
          ~num_columns:state.num_columns
          ~services:state.services
      with
      | None ->
          (* All columns empty (shouldn't happen if services <> []) *)
          {state with selected = 0; active_column = 0}
      | Some new_col ->
          let first_svc =
            first_service_in_column
              ~num_columns:state.num_columns
              ~services:state.services
              new_col
          in
          {
            state with
            active_column = new_col;
            selected = first_svc + services_start_idx ();
          }
