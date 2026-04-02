(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types

let ( let* ) = Result.bind

let yes_crypto_env = [("TEZOS_USE_YES_CRYPTO_I_KNOW_WHAT_I_AM_DOING", "y")]

let sandboxes_root () = Paths.default_data_dir "sandboxes"

let wallet_dir ~sandbox_name =
  Filename.concat (sandboxes_root ()) (Filename.concat sandbox_name "wallet")

(** Per-baker base directory: [<sandboxes_root>/<sandbox_name>/<baker_instance>/].
    Each baker gets its own base dir so highwatermarks are independent. *)
let baker_base_dir ~sandbox_name ~baker_instance =
  Filename.concat
    (sandboxes_root ())
    (Filename.concat sandbox_name baker_instance)

(** Copy the three wallet JSON files into a baker-specific base directory.
    Creates the directory if it does not exist. *)
let copy_wallet_to_base_dir ~wallet_dir ~base_dir =
  (try Unix.mkdir base_dir 0o750 with Unix.Unix_error _ -> ()) ;
  List.fold_left
    (fun acc file ->
      let* () = acc in
      let src = Filename.concat wallet_dir file in
      let dst = Filename.concat base_dir file in
      if not (Sys.file_exists src) then Ok ()
      else
        try
          let ic = open_in src in
          let content = really_input_string ic (in_channel_length ic) in
          close_in ic ;
          let oc = open_out dst in
          output_string oc content ;
          close_out oc ;
          Ok ()
        with Sys_error msg -> Error (`Msg msg))
    (Ok ())
    ["public_key_hashs"; "public_keys"; "secret_keys"]

let unique_name ~base =
  let rec try_name n =
    let candidate = if n = 0 then base else Printf.sprintf "%s-%d" base n in
    match Group_registry.find ~name:candidate with
    | Ok None -> candidate
    | Ok (Some _) -> try_name (n + 1)
    | Error _ -> candidate
  in
  try_name 0

let find_sandbox_node ~group_name =
  let* services = Lifecycle.group_services ~group_name () in
  Ok
    (List.find_opt (fun (s : Service.t) -> String.equal s.role "node") services)

let find_sandbox_baker ~group_name =
  let* services = Lifecycle.group_services ~group_name () in
  Ok
    (List.find_opt
       (fun (s : Service.t) -> String.equal s.role "baker")
       services)

let wait_for_rpc ~endpoint ~timeout_seconds =
  let url = endpoint ^ "/chains/main/blocks/head/header" in
  let cmd =
    Printf.sprintf
      "curl -fsL --max-time 5 --connect-timeout 2 %s >/dev/null 2>&1"
      (Cmd_runner.sh_quote url)
  in
  let rec loop remaining =
    if remaining <= 0 then
      Error
        (`Msg
           (Printf.sprintf
              "Timed out waiting for node RPC at %s after %d seconds"
              endpoint
              timeout_seconds))
    else
      match
        Cmd_runner.run_out ["/bin/sh"; "-c"; cmd ^ " && echo ok || echo fail"]
      with
      | Ok out when String.trim out = "ok" -> Ok ()
      | _ ->
          Unix.sleepf 2.0 ;
          loop (remaining - 2)
  in
  loop timeout_seconds

let create ?(on_log = fun _ -> ()) ~network ?name ?rpc_addr ?snapshot
    ?(max_delegates = 20) ?(num_nodes = 1) ?(num_bakers = 1) ?(accuser = false)
    ~bin_source ~service_user ~app_bin_dir () =
  let num_nodes = max 1 num_nodes in
  let num_bakers = max 1 num_bakers in
  let sandbox_name =
    match name with
    | Some n -> n
    | None -> unique_name ~base:(Printf.sprintf "sandbox-%s" network)
  in
  (* Instance naming: use indexed names when there are multiple of a kind *)
  let node_instance_name k =
    if num_nodes = 1 then Printf.sprintf "%s-node" sandbox_name
    else Printf.sprintf "%s-node-%d" sandbox_name k
  in
  let baker_instance_name k =
    if num_bakers = 1 then Printf.sprintf "%s-baker" sandbox_name
    else Printf.sprintf "%s-baker-%d" sandbox_name k
  in
  let accuser_instance = Printf.sprintf "%s-accuser" sandbox_name in
  let wallet = wallet_dir ~sandbox_name in

  (* Total step count for progress messages *)
  let total_steps = 3 + num_nodes + num_bakers + if accuser then 1 else 0 in
  let step = ref 0 in
  let log_step label =
    incr step ;
    on_log (Printf.sprintf "[%d/%d] %s" !step total_steps label)
  in

  (* Determine RPC and P2P addresses for node 1 *)
  let avoid_rpc, avoid_p2p = Port_validation.ports_from_services () in
  let avoid_rpc_ports = ref (List.map fst avoid_rpc) in
  let avoid_p2p_ports = ref (List.map fst avoid_p2p) in
  let alloc_rpc_port () =
    let port =
      Port_validation.next_free_port ~start:18732 ~avoid:!avoid_rpc_ports
    in
    avoid_rpc_ports := port :: !avoid_rpc_ports ;
    port
  in
  let alloc_p2p_port () =
    let port =
      Port_validation.next_free_port ~start:19732 ~avoid:!avoid_p2p_ports
    in
    avoid_p2p_ports := port :: !avoid_p2p_ports ;
    port
  in
  let rpc_addr_str =
    match rpc_addr with
    | Some addr -> addr
    | None -> Printf.sprintf "127.0.0.1:%d" (alloc_rpc_port ())
  in
  let p2p_addr_str = Printf.sprintf "0.0.0.0:%d" (alloc_p2p_port ()) in
  let endpoint = Config.endpoint_of_rpc rpc_addr_str in
  let node1_instance = node_instance_name 1 in

  (* Step 1: Create group *)
  log_step "Creating group..." ;
  let group =
    Group.make
      ~name:sandbox_name
      ~network
      ~bin_source
      ~service_user
      ~app_bin_dir
      ~sandbox:true
      ()
  in
  let* () = Group_registry.write group in

  let rollback () =
    on_log "Rolling back: removing installed services..." ;
    let services =
      match Lifecycle.group_services ~group_name:sandbox_name () with
      | Ok l -> l
      | Error _ -> []
    in
    List.iter
      (fun (svc : Service.t) ->
        ignore
          (Removal.remove_service
             ~quiet:true
             ~delete_data_dir:true
             ~instance:svc.instance
             ()))
      services ;
    on_log "Rolling back: removing group registry entry..." ;
    ignore (Group_registry.remove ~name:sandbox_name)
  in

  let set_group ~instance =
    match Service_registry.find ~instance with
    | Ok (Some svc) ->
        Service_registry.write {svc with group = Some sandbox_name}
    | Ok None | Error _ -> Ok ()
  in

  let bootstrap =
    match snapshot with
    | Some uri -> Snapshot {src = Some uri}
    | None -> Snapshot {src = None}
  in

  (* Step 2: Install node 1 *)
  log_step "Installing primary node..." ;
  let node1_request : node_request =
    {
      instance = node1_instance;
      network;
      history_mode = History_mode.Rolling;
      data_dir = None;
      rpc_addr = Rpc_addr.of_string rpc_addr_str;
      net_addr = p2p_addr_str;
      service_user;
      app_bin_dir;
      bin_source = Some bin_source;
      logging_mode = Logging_mode.Journald;
      extra_args =
        [
          "--no-bootstrap-peers";
          "--bootstrap-threshold";
          "0";
          "--allow-yes-crypto";
        ];
      extra_env = yes_crypto_env;
      auto_enable = true;
      bootstrap;
      preserve_data = false;
      snapshot_no_check = true;
      tmp_dir = None;
      keep_snapshot = false;
    }
  in
  let result =
    let* _node1_svc = Node.install_node ~on_log node1_request in
    let* () = set_group ~instance:node1_instance in

    (* Step 3: Wait for RPC on node 1 *)
    log_step "Starting primary node, waiting for RPC..." ;
    let* () = wait_for_rpc ~endpoint ~timeout_seconds:300 in

    (* Steps 4..(3+num_nodes-1): Install additional nodes 2..N *)
    let* () =
      List.fold_left
        (fun acc k ->
          let* () = acc in
          log_step (Printf.sprintf "Installing node %d (peered to node 1)..." k) ;
          let node_k_rpc = Printf.sprintf "127.0.0.1:%d" (alloc_rpc_port ()) in
          let node_k_p2p = Printf.sprintf "0.0.0.0:%d" (alloc_p2p_port ()) in
          let node_k_instance = node_instance_name k in
          let node_k_request : node_request =
            {
              instance = node_k_instance;
              network;
              history_mode = History_mode.Rolling;
              data_dir = None;
              rpc_addr = Rpc_addr.of_string node_k_rpc;
              net_addr = node_k_p2p;
              service_user;
              app_bin_dir;
              bin_source = Some bin_source;
              logging_mode = Logging_mode.Journald;
              extra_args =
                [
                  "--no-bootstrap-peers";
                  "--bootstrap-threshold";
                  "0";
                  "--allow-yes-crypto";
                  "--peer";
                  p2p_addr_str;
                ];
              extra_env = yes_crypto_env;
              auto_enable = true;
              bootstrap;
              preserve_data = false;
              snapshot_no_check = true;
              tmp_dir = None;
              keep_snapshot = false;
            }
          in
          let* _svc = Node.install_node ~on_log node_k_request in
          set_group ~instance:node_k_instance)
        (Ok ())
        (List.init (num_nodes - 1) (fun i -> i + 2))
    in

    (* Step (3+num_nodes): Generate yes-wallet *)
    log_step
      (Printf.sprintf "Generating yes-wallet (%d delegates)..." max_delegates) ;
    let* baker_delegates, all_wallet_entries =
      Yes_wallet_io.fetch_delegates ~endpoint ~max_delegates
    in
    let* () =
      Yes_wallet_io.write_wallet ~wallet_dir:wallet all_wallet_entries
    in

    (* Split delegates evenly across bakers *)
    let all_aliases =
      List.map (fun (d : Yes_wallet.delegate) -> d.alias) baker_delegates
    in
    let total_delegates = List.length all_aliases in
    let chunk_size =
      (total_delegates + num_bakers - 1) / num_bakers
      (* ceiling division *)
    in
    let delegate_chunks =
      List.init num_bakers (fun k ->
          let start = k * chunk_size in
          let len = min chunk_size (total_delegates - start) in
          if len <= 0 then []
          else
            List.filteri (fun i _ -> i >= start && i < start + len) all_aliases)
    in

    (* Steps (3+num_nodes+1)..(3+num_nodes+num_bakers): Install bakers *)
    let* () =
      List.fold_left
        (fun acc (k, delegates) ->
          let* () = acc in
          let baker_instance = baker_instance_name k in
          (* Baker K uses node K if available, else node 1 *)
          let node_for_baker =
            if k <= num_nodes then node_instance_name k else node1_instance
          in
          log_step (Printf.sprintf "Installing baker %d..." k) ;
          let baker_base = baker_base_dir ~sandbox_name ~baker_instance in
          let* () =
            copy_wallet_to_base_dir ~wallet_dir:wallet ~base_dir:baker_base
          in
          let baker_request : baker_request =
            {
              instance = baker_instance;
              node_mode = Local_instance node_for_baker;
              base_dir = Some baker_base;
              delegates;
              dal_config = Dal_disabled;
              dal_node = None;
              liquidity_baking_vote = Some "pass";
              signer_mode = Signer_types.Local_keys;
              extra_args = ["--force-apply-from-round"; "0"];
              extra_env = yes_crypto_env;
              service_user;
              app_bin_dir;
              bin_source = Some bin_source;
              logging_mode = Logging_mode.Journald;
              auto_enable = true;
              preserve_data = false;
              extra_nodes = [];
            }
          in
          let* _baker_svc = Baker.install_baker baker_request in
          set_group ~instance:baker_instance)
        (Ok ())
        (List.mapi (fun i delegates -> (i + 1, delegates)) delegate_chunks)
    in

    (* Optional: Install accuser *)
    let* () =
      if not accuser then Ok ()
      else begin
        log_step "Installing accuser..." ;
        let accuser_request : accuser_request =
          {
            instance = accuser_instance;
            node_mode = Local_instance node1_instance;
            base_dir = None;
            extra_args = [];
            service_user;
            app_bin_dir;
            bin_source = Some bin_source;
            logging_mode = Logging_mode.Journald;
            auto_enable = true;
            preserve_data = false;
          }
        in
        let* _acc_svc = Accuser.install_accuser accuser_request in
        set_group ~instance:accuser_instance
      end
    in
    Ok total_delegates
  in
  match result with
  | Error _ as err ->
      rollback () ;
      err
  | Ok delegate_count ->
      on_log
        (Printf.sprintf
           "Sandbox '%s' is ready.\n\
           \  Node RPC: %s\n\
           \  Bakers: %d (total delegates: %d)\n\
           \  Network: %s"
           sandbox_name
           endpoint
           num_bakers
           delegate_count
           network) ;
      Ok group

let find_sandbox_nodes ~group_name =
  let* services = Lifecycle.group_services ~group_name () in
  Ok (List.filter (fun (s : Service.t) -> String.equal s.role "node") services)

let find_sandbox_bakers ~group_name =
  let* services = Lifecycle.group_services ~group_name () in
  Ok (List.filter (fun (s : Service.t) -> String.equal s.role "baker") services)

let add_node ?(on_log = fun _ -> ()) ~group_name () =
  (* Look up group to get bin_source, service_user, app_bin_dir *)
  let* grp =
    match Group_registry.find ~name:group_name with
    | Ok (Some g) -> Ok g
    | Ok None -> Error (`Msg (Printf.sprintf "Group '%s' not found" group_name))
    | Error _ as err -> err
  in
  (* Find primary node (node 1) *)
  let* node1 =
    match find_sandbox_node ~group_name with
    | Ok (Some n) -> Ok n
    | Ok None -> Error (`Msg "No primary node found in sandbox")
    | Error _ as err -> err
  in
  let node1_p2p = node1.Service.net_addr in
  on_log
    (Printf.sprintf
       "Exporting snapshot from node 1 (%s)..."
       node1.Service.instance) ;
  (* Export snapshot from live node *)
  let ts = Int64.to_string (Int64.of_float (Unix.gettimeofday ())) in
  let snap_path =
    Printf.sprintf "/tmp/octez-sandbox-%s-%s.rolling" group_name ts
  in
  let export_cmd =
    Printf.sprintf
      "%s snapshot export --data-dir %s %s"
      (Filename.concat grp.Group.app_bin_dir "octez-node")
      (Cmd_runner.sh_quote node1.Service.data_dir)
      (Cmd_runner.sh_quote snap_path)
  in
  let* () =
    match Cmd_runner.run_out ["/bin/sh"; "-c"; export_cmd] with
    | Ok _ -> Ok ()
    | Error (`Msg msg) ->
        Error (`Msg (Printf.sprintf "Snapshot export failed: %s" msg))
  in
  on_log "Snapshot exported. Installing new node..." ;
  (* Pick new RPC and P2P ports *)
  let avoid_rpc, avoid_p2p = Port_validation.ports_from_services () in
  let avoid_rpc_ports = List.map fst avoid_rpc in
  let avoid_p2p_ports = List.map fst avoid_p2p in
  let rpc_port =
    Port_validation.next_free_port ~start:18732 ~avoid:avoid_rpc_ports
  in
  let p2p_port =
    Port_validation.next_free_port ~start:19732 ~avoid:avoid_p2p_ports
  in
  let rpc_addr_str = Printf.sprintf "127.0.0.1:%d" rpc_port in
  let p2p_addr_str = Printf.sprintf "0.0.0.0:%d" p2p_port in
  (* Determine new node instance name *)
  let* existing_nodes = find_sandbox_nodes ~group_name in
  let node_idx = List.length existing_nodes + 1 in
  let node_instance = Printf.sprintf "%s-node-%d" group_name node_idx in
  let set_group ~instance =
    match Service_registry.find ~instance with
    | Ok (Some svc) -> Service_registry.write {svc with group = Some group_name}
    | Ok None | Error _ -> Ok ()
  in
  let node_request : node_request =
    {
      instance = node_instance;
      network = grp.Group.network;
      history_mode = History_mode.Rolling;
      data_dir = None;
      rpc_addr = Rpc_addr.of_string rpc_addr_str;
      net_addr = p2p_addr_str;
      service_user = grp.Group.service_user;
      app_bin_dir = grp.Group.app_bin_dir;
      bin_source = Some grp.Group.bin_source;
      logging_mode = Logging_mode.Journald;
      extra_args =
        [
          "--no-bootstrap-peers";
          "--bootstrap-threshold";
          "0";
          "--allow-yes-crypto";
          "--peer";
          node1_p2p;
        ];
      extra_env = yes_crypto_env;
      auto_enable = true;
      bootstrap = Snapshot {src = Some (Printf.sprintf "file://%s" snap_path)};
      preserve_data = false;
      snapshot_no_check = true;
      tmp_dir = None;
      keep_snapshot = false;
    }
  in
  let result =
    let* node_svc = Node.install_node ~on_log node_request in
    let* () = set_group ~instance:node_instance in
    on_log (Printf.sprintf "Node %s is ready." node_instance) ;
    Ok node_svc
  in
  (* Clean up temp snapshot regardless of outcome *)
  (if Sys.file_exists snap_path then
     let rm_cmd = Printf.sprintf "rm -f %s" (Cmd_runner.sh_quote snap_path) in
     ignore (Cmd_runner.run_out ["/bin/sh"; "-c"; rm_cmd])) ;
  result

let add_baker ?(on_log = fun _ -> ()) ~group_name ~node_instance ~delegates () =
  let* grp =
    match Group_registry.find ~name:group_name with
    | Ok (Some g) -> Ok g
    | Ok None -> Error (`Msg (Printf.sprintf "Group '%s' not found" group_name))
    | Error _ as err -> err
  in
  let wallet = wallet_dir ~sandbox_name:group_name in
  let* existing_bakers = find_sandbox_bakers ~group_name in
  let baker_idx = List.length existing_bakers + 1 in
  let baker_instance = Printf.sprintf "%s-baker-%d" group_name baker_idx in
  let baker_base = baker_base_dir ~sandbox_name:group_name ~baker_instance in
  let* () = copy_wallet_to_base_dir ~wallet_dir:wallet ~base_dir:baker_base in
  on_log (Printf.sprintf "Installing baker %s..." baker_instance) ;
  let baker_request : baker_request =
    {
      instance = baker_instance;
      node_mode = Local_instance node_instance;
      base_dir = Some baker_base;
      delegates;
      dal_config = Dal_disabled;
      dal_node = None;
      liquidity_baking_vote = Some "pass";
      signer_mode = Signer_types.Local_keys;
      extra_args = ["--force-apply-from"; "0"];
      extra_env = yes_crypto_env;
      service_user = grp.Group.service_user;
      app_bin_dir = grp.Group.app_bin_dir;
      bin_source = Some grp.Group.bin_source;
      logging_mode = Logging_mode.Journald;
      auto_enable = true;
      preserve_data = false;
      extra_nodes = [];
    }
  in
  let set_group ~instance =
    match Service_registry.find ~instance with
    | Ok (Some svc) -> Service_registry.write {svc with group = Some group_name}
    | Ok None | Error _ -> Ok ()
  in
  let* baker_svc = Baker.install_baker baker_request in
  let* () = set_group ~instance:baker_instance in
  on_log (Printf.sprintf "Baker %s is ready." baker_instance) ;
  Ok baker_svc

let destroy ?(on_log = fun _ -> ()) ~group_name () =
  on_log (Printf.sprintf "Destroying sandbox '%s'..." group_name) ;
  (* Stop all services in the group *)
  let _ = Lifecycle.stop_group ~quiet:true ~group_name () in
  (* Remove each service *)
  let* services = Lifecycle.group_services ~group_name () in
  let* () =
    List.fold_left
      (fun acc (svc : Service.t) ->
        let* () = acc in
        on_log (Printf.sprintf "Removed %s: %s" svc.role svc.instance) ;
        Removal.remove_service
          ~quiet:true
          ~delete_data_dir:true
          ~instance:svc.instance
          ())
      (Ok ())
      services
  in
  (* Remove wallet directory *)
  let wallet = wallet_dir ~sandbox_name:group_name in
  if Sys.file_exists wallet then (
    on_log (Printf.sprintf "Removed wallet: %s" wallet) ;
    let cmd = Printf.sprintf "rm -rf %s" (Cmd_runner.sh_quote wallet) in
    ignore (Cmd_runner.run_out ["/bin/sh"; "-c"; cmd])) ;
  (* Also remove the sandboxes/<name> directory if empty *)
  let sandbox_dir = Filename.concat (sandboxes_root ()) group_name in
  if Sys.file_exists sandbox_dir then
    ignore
      (Cmd_runner.run_out
         [
           "/bin/sh";
           "-c";
           Printf.sprintf
             "rmdir %s 2>/dev/null || true"
             (Cmd_runner.sh_quote sandbox_dir);
         ]) ;
  (* Remove group *)
  let* () = Group_registry.remove ~name:group_name in
  on_log (Printf.sprintf "Sandbox '%s' destroyed." group_name) ;
  Ok ()
