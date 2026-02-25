(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** CLI commands for managing sandbox environments.

    Subcommands: create, list, status, start, stop, destroy, add-account. *)

open Cmdliner
open Octez_manager_lib

let ( let* ) = Result.bind

(** Positional argument: sandbox name. *)
let name_arg = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME")

(** Look up a sandbox group by name. *)
let with_sandbox ~name f =
  match Group_registry.find ~name with
  | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
  | Ok None ->
      Cli_helpers.cmdliner_error (Printf.sprintf "Unknown sandbox '%s'" name)
  | Ok (Some grp) ->
      if not grp.Group.sandbox then
        Cli_helpers.cmdliner_error (Printf.sprintf "'%s' is not a sandbox" name)
      else f grp

(* ── create ─────────────────────────────────────────────────────────────── *)

let create_term =
  let network =
    let doc = "Tezos network (e.g. ghostnet, mainnet, or a teztnets URL)." in
    Arg.(
      value & opt string "shadownet" & info ["network"; "n"] ~doc ~docv:"NET")
  in
  let sandbox_name =
    let doc = "Sandbox name (default: auto-generated from network)." in
    Arg.(value & opt (some string) None & info ["name"] ~doc ~docv:"NAME")
  in
  let snapshot =
    let doc = "Snapshot URL or file path (default: auto-fetched)." in
    Arg.(value & opt (some string) None & info ["snapshot"] ~doc ~docv:"URI")
  in
  let rpc_addr =
    let doc =
      "Node RPC address host:port (default: auto-assigned starting at 18732)."
    in
    Arg.(value & opt (some string) None & info ["rpc-addr"] ~doc ~docv:"ADDR")
  in
  let max_delegates =
    let doc = "Max delegates to impersonate via yes-wallet (default: 20)." in
    Arg.(value & opt int 20 & info ["max-delegates"] ~doc ~docv:"N")
  in
  let num_nodes =
    let doc =
      "Number of nodes to create (default: 1). Nodes 2+ peer to node 1."
    in
    Arg.(value & opt int 1 & info ["num-nodes"] ~doc ~docv:"N")
  in
  let num_bakers =
    let doc =
      "Number of bakers to create (default: 1). Delegates split evenly."
    in
    Arg.(value & opt int 1 & info ["num-bakers"] ~doc ~docv:"N")
  in
  let accuser =
    let doc = "Install an octez-accuser service (default: false)." in
    Arg.(value & flag & info ["accuser"] ~doc)
  in
  let app_bin_dir =
    let doc = "Path to directory containing Octez binaries." in
    Arg.(value & opt (some string) None & info ["app-bin-dir"] ~doc ~docv:"DIR")
  in
  let octez_version =
    let doc = "Managed Octez version (e.g. '24.1' or 'latest')." in
    Arg.(
      value
      & opt (some string) None
      & info ["octez-version"] ~doc ~docv:"VERSION")
  in
  let bin_dir_alias =
    let doc = "Registered binary directory alias." in
    Arg.(
      value & opt (some string) None & info ["bin-dir-alias"] ~doc ~docv:"ALIAS")
  in
  let service_user =
    let doc = "System user for services (default: current user or 'tezos')." in
    Arg.(value & opt string "" & info ["service-user"] ~doc ~docv:"USER")
  in
  let run network sandbox_name snapshot rpc_addr max_delegates num_nodes
      num_bakers accuser app_bin_dir octez_version bin_dir_alias service_user =
    let result =
      let* resolved_dir, bin_source =
        Cli_helpers.resolve_app_bin_dir
          ?octez_version
          ?bin_dir_alias
          app_bin_dir
      in
      let service_user =
        if String.equal service_user "" then
          if Paths.is_root () then "tezos"
          else Sys.getenv_opt "USER" |> Option.value ~default:"tezos"
        else service_user
      in
      Result.map_error
        (fun (`Msg s) -> s)
        (Sandbox.create
           ~on_log:(fun msg -> Format.printf "%s@." msg)
           ~network
           ?name:sandbox_name
           ?rpc_addr
           ?snapshot
           ~max_delegates
           ~num_nodes
           ~num_bakers
           ~accuser
           ~bin_source
           ~service_user
           ~app_bin_dir:resolved_dir
           ())
    in
    match result with
    | Ok group ->
        Format.printf "Sandbox '%s' created and running.@." group.Group.name ;
        `Ok ()
    | Error msg -> Cli_helpers.cmdliner_error msg
  in
  Term.(
    ret
      (const run $ network $ sandbox_name $ snapshot $ rpc_addr $ max_delegates
     $ num_nodes $ num_bakers $ accuser $ app_bin_dir $ octez_version
     $ bin_dir_alias $ service_user))

let create_cmd =
  let info =
    Cmd.info "create" ~doc:"Create a sandbox with a running node and baker."
  in
  Cmd.v info create_term

(* ── list ────────────────────────────────────────────────────────────────── *)

let list_term =
  let run () =
    match Group_registry.list_sandboxes () with
    | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
    | Ok [] ->
        Format.printf "No sandboxes registered.@." ;
        `Ok ()
    | Ok sandboxes ->
        Format.printf "%-20s  %-20s  %-10s@." "NAME" "NETWORK" "CREATED" ;
        List.iter
          (fun (g : Group.t) ->
            Format.printf "%-20s  %-20s  %-10s@." g.name g.network g.created_at)
          sandboxes ;
        `Ok ()
  in
  Term.(ret (const run $ const ()))

let list_cmd =
  let info = Cmd.info "list" ~doc:"List all sandboxes." in
  Cmd.v info list_term

(* ── status ──────────────────────────────────────────────────────────────── *)

let status_term =
  let run name =
    with_sandbox ~name (fun grp ->
        Format.printf "Sandbox:   %s@." grp.Group.name ;
        Format.printf "Network:   %s@." grp.Group.network ;
        Format.printf "Created:   %s@." grp.Group.created_at ;
        (match Sandbox.find_sandbox_node ~group_name:name with
        | Ok (Some svc) -> (
            Format.printf
              "Node:      %s  (%s)@."
              svc.Service.instance
              (Rpc_addr.to_endpoint svc.Service.rpc_addr) ;
            match Sandbox.find_sandbox_baker ~group_name:name with
            | Ok (Some baker) ->
                Format.printf "Baker:     %s@." baker.Service.instance
            | Ok None -> Format.printf "Baker:     (none)@."
            | Error (`Msg msg) -> Format.printf "Baker:     (error: %s)@." msg)
        | Ok None -> Format.printf "Node:      (none)@."
        | Error (`Msg msg) -> Format.printf "Node:      (error: %s)@." msg) ;
        `Ok ())
  in
  Term.(ret (const run $ name_arg))

let status_cmd =
  let info = Cmd.info "status" ~doc:"Show sandbox status." in
  Cmd.v info status_term

(* ── start ───────────────────────────────────────────────────────────────── *)

let start_term =
  let run name =
    with_sandbox ~name (fun _grp ->
        match Lifecycle.start_group ~quiet:false ~group_name:name () with
        | Ok started ->
            Format.printf
              "Started %d service(s) in sandbox '%s': %s@."
              (List.length started)
              name
              (String.concat ", " started) ;
            `Ok ()
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
  in
  Term.(ret (const run $ name_arg))

let start_cmd =
  let info = Cmd.info "start" ~doc:"Start all services in a sandbox." in
  Cmd.v info start_term

(* ── stop ────────────────────────────────────────────────────────────────── *)

let stop_term =
  let run name =
    with_sandbox ~name (fun _grp ->
        match Lifecycle.stop_group ~quiet:false ~group_name:name () with
        | Ok stopped ->
            Format.printf
              "Stopped %d service(s) in sandbox '%s': %s@."
              (List.length stopped)
              name
              (String.concat ", " stopped) ;
            `Ok ()
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
  in
  Term.(ret (const run $ name_arg))

let stop_cmd =
  let info = Cmd.info "stop" ~doc:"Stop all services in a sandbox." in
  Cmd.v info stop_term

(* ── destroy ─────────────────────────────────────────────────────────────── *)

let destroy_term =
  let yes_flag =
    Arg.(value & flag & info ["yes"; "y"] ~doc:"Skip confirmation prompt.")
  in
  let run name yes =
    with_sandbox ~name (fun _grp ->
        let confirmed =
          yes
          || Cli_helpers.prompt_yes_no
               (Printf.sprintf
                  "Permanently destroy sandbox '%s'? This cannot be undone."
                  name)
               ~default:false
        in
        if not confirmed then (
          Format.printf "Aborted.@." ;
          `Ok ())
        else
          match
            Sandbox.destroy
              ~on_log:(fun msg -> Format.printf "%s@." msg)
              ~group_name:name
              ()
          with
          | Ok () ->
              Format.printf "Sandbox '%s' destroyed.@." name ;
              `Ok ()
          | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
  in
  Term.(ret (const run $ name_arg $ yes_flag))

let destroy_cmd =
  let info =
    Cmd.info
      "destroy"
      ~doc:"Destroy a sandbox (stops services and removes all data)."
  in
  Cmd.v info destroy_term

(* ── add-account ─────────────────────────────────────────────────────────── *)

let add_account_term =
  let address =
    let doc = "Tezos address to add (tz1/tz2/tz3/tz4)." in
    Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"ADDRESS")
  in
  let alias =
    let doc = "Alias for the account (default: derived from address)." in
    Arg.(value & opt (some string) None & info ["alias"] ~doc ~docv:"ALIAS")
  in
  let run name address alias =
    with_sandbox ~name (fun _grp ->
        match Yes_wallet.curve_of_address address with
        | None ->
            Cli_helpers.cmdliner_error
              (Printf.sprintf
                 "Invalid Tezos address '%s' (must start with tz1/tz2/tz3/tz4)"
                 address)
        | Some _ -> (
            let wallet = Sandbox.wallet_dir ~sandbox_name:name in
            let result =
              Result.map_error
                (fun (`Msg s) -> s)
                (Yes_wallet_io.add_account
                   ~wallet_dir:wallet
                   ~address
                   ?alias
                   ())
            in
            match result with
            | Ok used_alias ->
                Format.printf
                  "Added account '%s' (alias: %s) to sandbox '%s'.@."
                  address
                  used_alias
                  name ;
                `Ok ()
            | Error msg -> Cli_helpers.cmdliner_error msg))
  in
  Term.(ret (const run $ name_arg $ address $ alias))

let add_account_cmd =
  let info =
    Cmd.info "add-account" ~doc:"Add an account to a sandbox wallet."
  in
  Cmd.v info add_account_term

(* ── sandbox command group ───────────────────────────────────────────────── *)

let sandbox_cmd =
  let info = Cmd.info "sandbox" ~doc:"Manage sandbox environments." in
  Cmd.group
    info
    [
      create_cmd;
      list_cmd;
      status_cmd;
      start_cmd;
      stop_cmd;
      destroy_cmd;
      add_account_cmd;
    ]
