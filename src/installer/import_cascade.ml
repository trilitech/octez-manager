(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** {1 Types} *)

type graph_node = {
  service : External_service.t;
  dependencies : string list;
  dependents : string list;
}

type dependency_analysis = {
  nodes : graph_node list;
  import_order : string list;
  cycles : string list list;
  external_dependents : (string * string) list;
}

type validation_error =
  | Cycle_detected of string list
  | External_dependents_exist of (string * string) list
  | Missing_dependencies of (string * string list) list

(** {1 Helpers} *)

(** Get unit name from service *)
let unit_name svc = svc.External_service.config.unit_name

(** Find service by unit name *)
let find_service ~unit_name:name services =
  List.find_opt (fun s -> unit_name s = name) services

(** Get just the unit names from dependency tuples *)
let get_dependency_names svc all_services =
  External_service.get_dependencies svc all_services
  |> List.map (fun (name, _role) -> name)

(** Get just the unit names from dependent tuples *)
let get_dependent_names svc all_services =
  External_service.get_dependents svc all_services
  |> List.map (fun (name, _role) -> name)

(** {1 Graph Construction} *)

(** Build graph nodes from services *)
let build_graph_nodes ~all_services ~target_services =
  List.map
    (fun service ->
      let dependencies = get_dependency_names service all_services in
      let dependents = get_dependent_names service all_services in
      {service; dependencies; dependents})
    target_services

(** {1 Topological Sort} *)

(** Helper to drop first n elements from list *)
let rec list_drop n lst =
  match (n, lst) with
  | 0, _ -> lst
  | _, [] -> []
  | n, _ :: tl when n > 0 -> list_drop (n - 1) tl
  | _, lst -> lst

(** Topological sort using DFS. Returns (sorted_list, cycles). *)
let topological_sort nodes =
  (* Create adjacency map: unit_name -> dependencies *)
  let dep_map = Hashtbl.create 17 in
  List.iter
    (fun node -> Hashtbl.add dep_map (unit_name node.service) node.dependencies)
    nodes ;

  let visited = Hashtbl.create 17 in
  let rec_stack = Hashtbl.create 17 in
  let result = ref [] in
  let cycles = ref [] in

  let rec visit node_name path =
    if Hashtbl.mem rec_stack node_name then
      (* Cycle detected *)
      let cycle_start =
        try List.find_index (fun n -> n = node_name) path |> Option.get
        with _ -> 0
      in
      let cycle = list_drop cycle_start path @ [node_name] in
      cycles := cycle :: !cycles
    else if not (Hashtbl.mem visited node_name) then (
      Hashtbl.add rec_stack node_name true ;
      Hashtbl.add visited node_name true ;
      (* Visit dependencies first *)
      (match Hashtbl.find_opt dep_map node_name with
      | Some deps -> List.iter (fun dep -> visit dep (node_name :: path)) deps
      | None -> ()) ;
      Hashtbl.remove rec_stack node_name ;
      result := node_name :: !result)
  in

  List.iter (fun node -> visit (unit_name node.service) []) nodes ;
  (!result, !cycles)

(** {1 Dependency Analysis} *)

let analyze_dependencies ~services ~target_services =
  let target_names =
    List.map unit_name target_services |> List.sort_uniq String.compare
  in

  (* Build graph *)
  let nodes = build_graph_nodes ~all_services:services ~target_services in

  (* Topological sort *)
  let import_order, cycles = topological_sort nodes in

  (* Find external dependents - services NOT in target set that depend on target services *)
  let external_dependents =
    List.concat_map
      (fun target ->
        let dependents = get_dependent_names target services in
        (* Filter to only external (non-target) dependents *)
        let external_deps =
          List.filter
            (fun dep_name -> not (List.mem dep_name target_names))
            dependents
        in
        List.map (fun dep_name -> (dep_name, unit_name target)) external_deps)
      target_services
  in

  {nodes; import_order; cycles; external_dependents}

(** {1 Dependency Chain} *)

let get_dependency_chain ~service ~all_services =
  (* BFS to find all transitive dependencies *)
  let visited = Hashtbl.create 17 in
  let queue = Queue.create () in
  Queue.add service queue ;
  Hashtbl.add visited (unit_name service) true ;

  let chain = ref [] in

  while not (Queue.is_empty queue) do
    let current = Queue.take queue in
    chain := current :: !chain ;
    let deps = get_dependency_names current all_services in
    List.iter
      (fun dep_name ->
        if not (Hashtbl.mem visited dep_name) then (
          Hashtbl.add visited dep_name true ;
          match find_service ~unit_name:dep_name all_services with
          | Some dep_svc -> Queue.add dep_svc queue
          | None -> ()))
      deps
  done ;

  (* Reverse to get dependencies first *)
  let all_services_in_chain = List.rev !chain in
  (* Do topological sort *)
  let nodes =
    build_graph_nodes ~all_services ~target_services:all_services_in_chain
  in
  let sorted, _cycles = topological_sort nodes in
  (* Map back to services *)
  List.filter_map (fun name -> find_service ~unit_name:name all_services) sorted

(** {1 Validation} *)

let validate_cascade ~services ~target_services ~strategy =
  let analysis = analyze_dependencies ~services ~target_services in

  (* Check for cycles *)
  match analysis.cycles with
  | [] -> (
      (* For Takeover strategy, check for external dependents *)
      match (strategy : Installer_types.import_strategy) with
      | Installer_types.Takeover when analysis.external_dependents <> [] ->
          Error (External_dependents_exist analysis.external_dependents)
      | _ ->
          (* Check for missing dependencies *)
          let target_names =
            List.map unit_name target_services |> List.sort_uniq String.compare
          in
          let missing =
            List.filter_map
              (fun node ->
                let missing_deps =
                  List.filter
                    (fun dep_name -> not (List.mem dep_name target_names))
                    node.dependencies
                in
                if missing_deps <> [] then
                  Some (unit_name node.service, missing_deps)
                else None)
              analysis.nodes
          in
          if missing <> [] then Error (Missing_dependencies missing) else Ok ())
  | cycle :: _ -> Error (Cycle_detected cycle)

(** {1 Pretty Printing} *)

let pp_validation_error fmt = function
  | Cycle_detected cycle ->
      Format.fprintf
        fmt
        "Dependency cycle detected: %s"
        (String.concat " -> " cycle)
  | External_dependents_exist deps ->
      Format.fprintf fmt "External services depend on import targets:" ;
      List.iter
        (fun (dependent, dependency) ->
          Format.fprintf fmt "@.  %s depends on %s" dependent dependency)
        deps ;
      Format.fprintf
        fmt
        "@.Cannot use Takeover strategy - use Clone instead or import \
         dependents too."
  | Missing_dependencies missing ->
      Format.fprintf fmt "Missing dependencies:" ;
      List.iter
        (fun (service, deps) ->
          Format.fprintf
            fmt
            "@.  %s depends on: %s"
            service
            (String.concat ", " deps))
        missing

let pp_analysis fmt analysis =
  Format.fprintf fmt "Dependency Analysis:@." ;
  Format.fprintf fmt "  Services: %d@." (List.length analysis.nodes) ;
  Format.fprintf
    fmt
    "  Import order: %s@."
    (String.concat " → " analysis.import_order) ;
  if analysis.cycles <> [] then (
    Format.fprintf fmt "  ⚠ Cycles detected:@." ;
    List.iter
      (fun cycle -> Format.fprintf fmt "    %s@." (String.concat " → " cycle))
      analysis.cycles) ;
  if analysis.external_dependents <> [] then (
    Format.fprintf fmt "  ⚠ External dependents:@." ;
    List.iter
      (fun (dependent, dependency) ->
        Format.fprintf fmt "    %s → %s@." dependent dependency)
      analysis.external_dependents)
