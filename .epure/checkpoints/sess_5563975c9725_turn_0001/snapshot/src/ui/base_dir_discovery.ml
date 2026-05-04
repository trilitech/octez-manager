(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

let default_client_base_dir () =
  Filename.concat (Paths.home_dir ()) ".tezos-client"

let normalize_path path =
  let len = String.length path in
  if len > 1 && String.get path (len - 1) = '/' then String.sub path 0 (len - 1)
  else path

let discover_from_services () =
  let services = Data.load_service_states () in
  List.filter_map
    (fun (st : Data.Service_state.t) ->
      let key =
        match st.service.role with
        | "baker" -> Some "OCTEZ_BAKER_BASE_DIR"
        | "accuser" -> Some "OCTEZ_CLIENT_BASE_DIR"
        | _ -> None
      in
      match key with
      | None -> None
      | Some env_key -> (
          match Node_env.read ~inst:st.service.instance with
          | Ok pairs -> (
              match List.assoc_opt env_key pairs with
              | Some dir when not (String.equal (String.trim dir) "") ->
                  Some (String.trim dir)
              | _ -> None)
          | Error _ -> None))
    services

let list_all () =
  let default_dir = default_client_base_dir () in
  let managed_dirs =
    match Directory_registry.list ~dir_type:Client_base_dir () with
    | Ok entries ->
        List.map
          (fun (e : Directory_registry.directory_entry) -> e.path)
          entries
    | Error _ -> []
  in
  let service_dirs = discover_from_services () in
  let all_dirs = (default_dir :: managed_dirs) @ service_dirs in
  let normalized = List.map normalize_path all_dirs in
  List.sort_uniq String.compare normalized |> List.sort String.compare
