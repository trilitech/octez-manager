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
    ?(max_delegates = 20) ~bin_source ~service_user ~app_bin_dir () =
  let sandbox_name =
    match name with
    | Some n -> n
    | None -> unique_name ~base:(Printf.sprintf "sandbox-%s" network)
  in
  let node_instance = Printf.sprintf "%s-node" sandbox_name in
  let baker_instance = Printf.sprintf "%s-baker" sandbox_name in
  let wallet = wallet_dir ~sandbox_name in

  (* Determine RPC and P2P addresses, avoiding all ports already in use *)
  let avoid_rpc, avoid_p2p = Port_validation.ports_from_services () in
  let avoid_rpc_ports = List.map fst avoid_rpc in
  let avoid_p2p_ports = List.map fst avoid_p2p in
  let rpc_addr_str =
    match rpc_addr with
    | Some addr -> addr
    | None ->
        let port =
          Port_validation.next_free_port ~start:18732 ~avoid:avoid_rpc_ports
        in
        Printf.sprintf "127.0.0.1:%d" port
  in
  let p2p_addr_str =
    let port =
      Port_validation.next_free_port ~start:19732 ~avoid:avoid_p2p_ports
    in
    Printf.sprintf "0.0.0.0:%d" port
  in
  let endpoint = Config.endpoint_of_rpc rpc_addr_str in

  (* Step 1: Create group *)
  on_log "[1/5] Creating group..." ;
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

  (* Step 2: Install node *)
  on_log "[2/5] Installing node..." ;
  let bootstrap =
    match snapshot with
    | Some uri -> Snapshot {src = Some uri}
    | None -> Snapshot {src = None}
  in
  let node_request : node_request =
    {
      instance = node_instance;
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
  let set_group ~instance =
    match Service_registry.find ~instance with
    | Ok (Some svc) ->
        Service_registry.write {svc with group = Some sandbox_name}
    | Ok None | Error _ -> Ok ()
  in
  let result =
    let* _node_svc = Node.install_node ~on_log node_request in
    let* () = set_group ~instance:node_instance in

    (* Step 3: Wait for RPC *)
    on_log "[3/5] Starting node, waiting for RPC..." ;
    let* () = wait_for_rpc ~endpoint ~timeout_seconds:300 in

    (* Step 4: Generate yes-wallet *)
    on_log
      (Printf.sprintf
         "[4/5] Generating yes-wallet (%d delegates)..."
         max_delegates) ;
    let* baker_delegates, all_wallet_entries =
      Yes_wallet_io.fetch_delegates ~endpoint ~max_delegates
    in
    let* () =
      Yes_wallet_io.write_wallet ~wallet_dir:wallet all_wallet_entries
    in

    (* Step 5: Install baker *)
    on_log "[5/5] Installing and starting baker..." ;
    let delegate_aliases =
      List.map (fun (d : Yes_wallet.delegate) -> d.alias) baker_delegates
    in
    let baker_request : baker_request =
      {
        instance = baker_instance;
        node_mode = Local_instance node_instance;
        base_dir = Some wallet;
        delegates = delegate_aliases;
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
      }
    in
    let* _baker_svc = Baker.install_baker baker_request in
    let* () = set_group ~instance:baker_instance in
    Ok (List.length baker_delegates)
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
           \  Baker delegates: %d\n\
           \  Network: %s"
           sandbox_name
           endpoint
           delegate_count
           network) ;
      Ok group

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
