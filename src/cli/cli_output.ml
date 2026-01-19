(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module S = Service

let pp_service fmt svc =
  (* Don't query systemd during list - causes issues with multiple services *)
  (* Status is shown in the UI instead *)
  Format.fprintf
    fmt
    "%-16s %-8s %s (%s)"
    svc.S.instance
    svc.role
    svc.network
    svc.data_dir

let print_services services =
  if services = [] then print_endline "No services registered."
  else (
    List.iter (fun svc -> Format.printf "%a@." pp_service svc) services ;
    Format.print_flush ())

let pp_external_service fmt (ext : External_service.t) =
  let open External_service in
  let cfg = ext.config in
  let role_str =
    match cfg.role.value with Some r -> role_to_string r | None -> "unknown"
  in
  let network_str = match cfg.network.value with Some n -> n | None -> "?" in
  let status_str = status_label (status_of_unit_state cfg.unit_state) in

  (* Line 1: Instance, role, network, status *)
  Format.fprintf
    fmt
    "%-20s %-10s %-12s %s@."
    ext.suggested_instance_name
    role_str
    network_str
    status_str ;

  (* Line 2: Unit name and binary path *)
  let binary_str =
    match cfg.binary_path.value with Some path -> path | None -> "?"
  in
  Format.fprintf fmt "  Unit: %-30s Binary: %s@." cfg.unit_name binary_str ;

  (* Line 3: Role-specific info *)
  (match cfg.role.value with
  | Some Node ->
      let rpc = match cfg.rpc_addr.value with Some r -> r | None -> "?" in
      let p2p = match cfg.net_addr.value with Some n -> n | None -> "?" in
      let data_dir =
        match cfg.data_dir.value with Some d -> d | None -> "?"
      in
      Format.fprintf fmt "  RPC: %-25s P2P: %-25s Data: %s@." rpc p2p data_dir
  | Some Baker ->
      let node_ep =
        match cfg.node_endpoint.value with Some e -> e | None -> "?"
      in
      let base_dir =
        match cfg.base_dir.value with Some b -> b | None -> "?"
      in
      let data_dir =
        match cfg.data_dir.value with Some d -> d | None -> "?"
      in
      Format.fprintf
        fmt
        "  Node: %-24s Base: %-24s Data: %s@."
        node_ep
        base_dir
        data_dir
  | Some Accuser ->
      let node_ep =
        match cfg.node_endpoint.value with Some e -> e | None -> "?"
      in
      let base_dir =
        match cfg.base_dir.value with Some b -> b | None -> "?"
      in
      Format.fprintf fmt "  Node: %-24s Base: %s@." node_ep base_dir
  | Some Dal_node ->
      let node_ep =
        match cfg.node_endpoint.value with Some e -> e | None -> "?"
      in
      let dal_rpc = match cfg.rpc_addr.value with Some r -> r | None -> "?" in
      let data_dir =
        match cfg.data_dir.value with Some d -> d | None -> "?"
      in
      Format.fprintf
        fmt
        "  Node: %-24s DAL RPC: %-20s Data: %s@."
        node_ep
        dal_rpc
        data_dir
  | _ ->
      let data_dir =
        match cfg.data_dir.value with Some d -> d | None -> "?"
      in
      Format.fprintf fmt "  Data: %s@." data_dir) ;

  (* Blank line between services *)
  Format.fprintf fmt "@."

let print_external_services services =
  if services = [] then print_endline "No external services detected."
  else (
    Format.printf "External Octez Services Detected:@.@." ;
    List.iter (fun svc -> Format.printf "%a" pp_external_service svc) services ;
    Format.print_flush ())

let print_all_services ~managed ~external_ =
  if managed = [] && external_ = [] then print_endline "No services found."
  else (
    (* Print managed services *)
    if managed <> [] then (
      Format.printf "=== Managed Services ===@." ;
      List.iter (fun svc -> Format.printf "%a@." pp_service svc) managed ;
      if external_ <> [] then Format.printf "@.") ;

    (* Print external services *)
    if external_ <> [] then (
      Format.printf "=== External Services ===@.@." ;
      List.iter
        (fun svc -> Format.printf "%a" pp_external_service svc)
        external_) ;

    Format.print_flush ())

let pp_logging fmt = function
  | Logging_mode.Journald -> Format.fprintf fmt "journald"

let print_service_details svc =
  (* Read env file for this instance *)
  let env =
    match Node_env.read ~inst:svc.S.instance with
    | Ok pairs -> pairs
    | Error _ -> []
  in
  let lookup key =
    match List.assoc_opt key env with Some v -> String.trim v | None -> ""
  in
  (* Try to map endpoints to managed instances *)
  let resolve_instance_for_endpoint (ep : string) ~(roles : string list) :
      string option =
    if String.trim ep = "" then None
    else
      match Service_registry.list () with
      | Error _ -> None
      | Ok (services : Service.t list) ->
          let rec find_match (lst : Service.t list) =
            match lst with
            | [] -> None
            | s :: rest ->
                if
                  List.exists (fun r -> String.equal s.role r) roles
                  && String.equal (Config.endpoint_of_rpc s.rpc_addr) ep
                then Some s.instance
                else find_match rest
          in
          find_match services
  in
  let print_node_endpoint () =
    let node_inst = lookup "OCTEZ_NODE_INSTANCE" in
    if String.trim node_inst <> "" then
      Format.printf "Node instance : %s@." node_inst
    else
      let node_ep = lookup "OCTEZ_NODE_ENDPOINT" in
      Format.printf
        "Node endpoint : %s@."
        (if node_ep = "" then svc.rpc_addr else node_ep)
  in
  (* Common fields *)
  Format.printf "Instance      : %s@." svc.S.instance ;
  Format.printf "Service name  : %s@." (Systemd.unit_name svc.role svc.instance) ;
  Format.printf "Role          : %s@." svc.role ;
  Format.printf "Network       : %s@." svc.network ;
  (* Role-specific fields *)
  (match svc.role with
  | "node" ->
      Format.printf
        "History mode  : %s@."
        (History_mode.to_string svc.history_mode) ;
      Format.printf "Data dir      : %s@." svc.data_dir ;
      Format.printf "RPC addr      : %s@." svc.rpc_addr ;
      Format.printf "P2P addr      : %s@." svc.net_addr
  | "baker" ->
      let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
      let dal_cfg = lookup "OCTEZ_DAL_CONFIG" in
      let delegates = lookup "OCTEZ_BAKER_DELEGATES_ARGS" in
      let lb_vote = lookup "OCTEZ_BAKER_LB_VOTE" in
      Format.printf "Base dir      : %s@." base_dir ;
      print_node_endpoint () ;
      (match String.lowercase_ascii (String.trim dal_cfg) with
      | "disabled" -> Format.printf "DAL Config    : opt-out@."
      | "" -> Format.printf "DAL Config    : auto@."
      | raw -> (
          match
            resolve_instance_for_endpoint raw ~roles:["dal-node"; "dal"]
          with
          | Some inst -> Format.printf "DAL instance  : %s@." inst
          | None -> Format.printf "DAL endpoint  : %s@." raw)) ;
      if delegates <> "" then Format.printf "Delegates     : %s@." delegates ;
      if lb_vote <> "" then Format.printf "LB Vote       : %s@." lb_vote
  | "accuser" ->
      let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
      Format.printf "Base dir      : %s@." base_dir ;
      print_node_endpoint ()
  | "dal-node" | "dal" ->
      let dal_data_dir = lookup "OCTEZ_DAL_DATA_DIR" in
      Format.printf "DAL data dir  : %s@." dal_data_dir ;
      print_node_endpoint () ;
      if svc.rpc_addr <> "" then
        Format.printf "DAL RPC addr  : %s@." svc.rpc_addr ;
      if svc.net_addr <> "" then
        Format.printf "DAL P2P addr  : %s@." svc.net_addr
  | _ ->
      (* Fallback for unknown roles *)
      Format.printf "Data dir      : %s@." svc.data_dir ;
      Format.printf "RPC addr      : %s@." svc.rpc_addr ;
      Format.printf "P2P addr      : %s@." svc.net_addr) ;
  (* Common footer *)
  Format.printf "Service user  : %s@." svc.service_user ;
  Format.printf "Octez bin dir : %s@." svc.app_bin_dir ;
  Format.printf "Created at    : %s@." svc.created_at ;
  Format.printf "Logging       : %a@." pp_logging svc.logging_mode ;

  (* Files & Paths Section *)
  Format.printf "@.Files & Paths:@." ;

  let service_paths =
    Systemd.get_service_paths ~role:svc.role ~instance:svc.instance
  in
  List.iter
    (fun (label, path) -> Format.printf "  %-16s: %s@." label path)
    service_paths ;

  Format.printf
    "  %-16s: %s@."
    "Service Metadata"
    (Filename.concat
       (Service_registry.services_dir ())
       (svc.instance ^ ".json")) ;

  (match
     Log_viewer.get_daily_log_file ~role:svc.role ~instance:svc.instance
   with
  | Ok path -> Format.printf "  %-16s: %s@." "Log File" path
  | Error _ -> ()) ;

  match svc.role with
  | "node" ->
      Format.printf
        "  %-16s: %s@."
        "Config File"
        (Filename.concat svc.data_dir "config.json") ;
      Format.printf
        "  %-16s: %s@."
        "Identity File"
        (Filename.concat svc.data_dir "identity.json")
  | "baker" ->
      let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
      if base_dir <> "" then (
        Format.printf "  %-16s: %s@." "Base Directory" base_dir ;
        Format.printf
          "  %-16s: %s@."
          "Client Config"
          (Filename.concat base_dir "config"))
  | "accuser" ->
      let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
      if base_dir <> "" then (
        Format.printf "  %-16s: %s@." "Base Directory" base_dir ;
        Format.printf
          "  %-16s: %s@."
          "Client Config"
          (Filename.concat base_dir "config"))
  | "dal-node" | "dal" ->
      let dal_dir = lookup "OCTEZ_DAL_DATA_DIR" in
      if dal_dir <> "" then (
        Format.printf "  %-16s: %s@." "DAL Data Dir" dal_dir ;
        Format.printf
          "  %-16s: %s@."
          "Config File"
          (Filename.concat dal_dir "config.json") ;
        Format.printf
          "  %-16s: %s@."
          "Identity File"
          (Filename.concat dal_dir "identity.json"))
  | _ -> ()

let print_snapshot_entry (entry : Snapshots.entry) =
  Format.printf "%s (%s)@." entry.Snapshots.label entry.slug ;
  (match entry.Snapshots.history_mode with
  | Some hm -> Format.printf "  History mode : %s@." hm
  | None -> ()) ;
  (match entry.Snapshots.download_url with
  | Some url -> Format.printf "  HTTPS        : %s@." url
  | None -> ()) ;
  entry.Snapshots.metadata
  |> List.filter (fun (k, _) -> k <> "History mode" && k <> "HTTPS")
  |> List.iter (fun (k, v) -> Format.printf "  %-12s %s@." (k ^ ":") v) ;
  print_newline ()

let snapshot_entry_to_json (entry : Snapshots.entry) =
  let metadata =
    `List
      (List.map
         (fun (k, v) -> `Assoc [("key", `String k); ("value", `String v)])
         entry.Snapshots.metadata)
  in
  `Assoc
    [
      ("slug", `String entry.slug);
      ("label", `String entry.Snapshots.label);
      ( "history_mode",
        match entry.Snapshots.history_mode with
        | Some hm -> `String hm
        | None -> `Null );
      ( "download_url",
        match entry.Snapshots.download_url with
        | Some url -> `String url
        | None -> `Null );
      ("metadata", metadata);
    ]

let dropin_path_for ~role ~instance =
  let dir =
    if Common.is_root () then
      Printf.sprintf "/etc/systemd/system/octez-%s@%s.service.d" role instance
    else
      let base = Filename.concat (Common.xdg_config_home ()) "systemd/user" in
      Filename.concat
        base
        (Printf.sprintf "octez-%s@%s.service.d" role instance)
  in
  Filename.concat dir "override.conf"

let slurp_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let format_bytes bytes =
  let gb = Int64.to_float bytes /. (1024. *. 1024. *. 1024.) in
  if gb >= 1.0 then Printf.sprintf "%.1f GB" gb
  else
    let mb = Int64.to_float bytes /. (1024. *. 1024.) in
    Printf.sprintf "%.0f MB" mb
