(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Data loading and item building for the binaries page.

    Functions that load managed versions, registered directories, and
    available-for-download versions from registries and schedulers, and
    build the flat item list for rendering. *)

open Octez_manager_lib
open Binaries_types

let load_managed_versions () =
  match Binary_registry.list_managed_versions () with
  | Error _ -> []
  | Ok versions ->
      List.map
        (fun version ->
          let path = Binary_registry.managed_version_path version in
          let size = File_ops.get_dir_size path in
          let count =
            Service_registry.count_instances_using
              (Binary_registry.Managed_version version)
          in
          (version, size, count))
        versions

let load_registered_dirs () =
  match Binary_registry.load_registered_dirs () with
  | Error _ -> []
  | Ok dirs ->
      List.map
        (fun (ld : Binary_registry.registered_dir) ->
          let count =
            Service_registry.count_instances_using
              (Binary_registry.Registered_alias ld.alias)
          in
          (ld, count))
        dirs

(** Filter versions to only keep the N latest major versions.
    Returns versions from the N most recent major version families. *)
let filter_latest_n_major_versions n versions =
  let extract_major version_str =
    try
      match String.split_on_char '.' version_str with
      | major :: _ -> int_of_string major
      | [] -> 0
    with _ -> 0
  in
  (* Group versions by major version *)
  let major_versions = Hashtbl.create 5 in
  List.iter
    (fun (v : Binary_downloader.version_info) ->
      let major = extract_major v.version in
      let existing = Hashtbl.find_opt major_versions major in
      Hashtbl.replace
        major_versions
        major
        (v :: Option.value ~default:[] existing))
    versions ;
  (* Get the N latest major versions *)
  let all_majors =
    Hashtbl.to_seq_keys major_versions |> List.of_seq |> List.sort compare
  in
  let latest_n_majors =
    List.rev all_majors |> fun l -> List.filteri (fun i _ -> i < n) l
  in
  List.concat_map
    (fun major ->
      Option.value ~default:[] (Hashtbl.find_opt major_versions major))
    latest_n_majors

let load_available_versions () =
  match Versions_scheduler.get_cached () with
  | None -> []
  | Some versions ->
      (* Filter to only the 2 latest major versions *)
      let filtered_versions = filter_latest_n_major_versions 2 versions in
      (* Filter out versions < 23.0 *)
      let filtered_versions =
        List.filter
          (fun (v : Binary_downloader.version_info) ->
            Binary_registry.compare_versions v.version "23.0" >= 0)
          filtered_versions
      in
      (* Filter out already installed versions *)
      let managed =
        match Binary_registry.list_managed_versions () with
        | Ok v -> v
        | Error _ -> []
      in
      List.filter
        (fun (v : Binary_downloader.version_info) ->
          not (List.mem v.version managed))
        filtered_versions

let build_items managed registered available expanded_majors =
  (* Group available versions by major version *)
  let major_groups = Hashtbl.create 10 in
  List.iter
    (fun (v : Binary_downloader.version_info) ->
      (* Parse major version from "24.0" or "23.1-rc1" *)
      match String.split_on_char '.' v.version with
      | major_str :: _ -> (
          try
            let major = int_of_string major_str in
            let existing =
              Hashtbl.find_opt major_groups major |> Option.value ~default:[]
            in
            Hashtbl.replace major_groups major (v :: existing)
          with _ -> ())
      | _ -> ())
    available ;

  (* Build major groups in descending order, with sub-items if expanded *)
  let majors =
    Hashtbl.to_seq_keys major_groups
    |> List.of_seq
    |> List.sort (fun a b -> compare b a)
  in
  let major_items =
    List.concat_map
      (fun major ->
        let versions = Hashtbl.find major_groups major |> List.rev in
        let group = AvailableMajorGroup (major, versions) in
        (* If expanded, add individual version items *)
        if List.mem major expanded_majors then
          group :: List.map (fun v -> AvailableVersion v) versions
        else [group])
      majors
  in

  (* Build items list: managed, registered, register action, then available *)
  List.map (fun (v, s, c) -> ManagedVersion (v, s, c)) managed
  @ List.map (fun (ld, c) -> RegisteredDir (ld, c)) registered
  @ [RegisterAction] @ major_items
