(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib

let ( let* ) = Result.bind

(** Look up a group by name, or return a cmdliner error. *)
let with_group ~name f =
  match Group_registry.find ~name with
  | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
  | Ok None ->
      Cli_helpers.cmdliner_error (Printf.sprintf "Unknown group '%s'" name)
  | Ok (Some grp) -> f grp

(** Validate group name characters (alphanumeric, dash, underscore). *)
let validate_group_name name =
  if String.length name = 0 then Error "Group name cannot be empty"
  else
    let valid =
      String.to_seq name
      |> Seq.for_all (fun c ->
          (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c = '-' || c = '_')
    in
    if valid then Ok ()
    else
      Error
        (Printf.sprintf
           "Group name '%s' contains invalid characters (use alphanumeric, \
            dash, underscore)"
           name)

(** Cmdliner positional argument for group name. *)
let name_arg = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME")

(* ── create ─────────────────────────────────────────────────── *)

let create_term =
  let network =
    let doc = "Network for services in this group (e.g. mainnet, ghostnet)." in
    Arg.(required & opt (some string) None & info ["network"] ~doc ~docv:"NET")
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
  let app_bin_dir =
    let doc = "Path to binaries directory." in
    Arg.(value & opt (some string) None & info ["app-bin-dir"] ~doc ~docv:"DIR")
  in
  let service_user =
    let doc = "System user for services (default: tezos)." in
    Arg.(value & opt string "tezos" & info ["service-user"] ~doc ~docv:"USER")
  in
  let run name network octez_version bin_dir_alias app_bin_dir service_user =
    let result =
      let* () = validate_group_name name in
      let* () =
        match Group_registry.find ~name with
        | Ok (Some _) -> Error (Printf.sprintf "Group '%s' already exists" name)
        | Ok None -> Ok ()
        | Error (`Msg msg) -> Error msg
      in
      let* resolved_dir, bin_source =
        Cli_helpers.resolve_app_bin_dir
          ?octez_version
          ?bin_dir_alias
          app_bin_dir
      in
      let group =
        Group.make
          ~name
          ~network
          ~bin_source
          ~service_user
          ~app_bin_dir:resolved_dir
          ()
      in
      Result.map_error (fun (`Msg s) -> s) (Group_registry.write group)
    in
    match result with
    | Ok () ->
        Format.printf "Group '%s' created.@." name ;
        `Ok ()
    | Error msg -> Cli_helpers.cmdliner_error msg
  in
  Term.(
    ret
      (const run $ name_arg $ network $ octez_version $ bin_dir_alias
     $ app_bin_dir $ service_user))

let create_cmd =
  let info = Cmd.info "create" ~doc:"Create a new instance group." in
  Cmd.v info create_term

(* ── list ───────────────────────────────────────────────────── *)

let list_term =
  let json_flag =
    Arg.(value & flag & info ["json"] ~doc:"Output in JSON format.")
  in
  let run json =
    match Group_registry.list () with
    | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
    | Ok groups ->
        if json then (
          let json_list = `List (List.map Group.to_yojson groups) in
          print_endline (Yojson.Safe.pretty_to_string json_list) ;
          `Ok ())
        else (
          (match groups with
          | [] -> print_endline "No groups registered."
          | _ ->
              List.iter
                (fun (g : Group.t) ->
                  Format.printf
                    "%-20s  %-15s  %-20s  user=%s@."
                    g.name
                    g.network
                    (Binary_registry.bin_source_to_string g.bin_source)
                    g.service_user)
                groups) ;
          `Ok ())
  in
  Term.(ret (const run $ json_flag))

let list_cmd =
  let info = Cmd.info "list" ~doc:"List all instance groups." in
  Cmd.v info list_term

(* ── show ───────────────────────────────────────────────────── *)

let show_term =
  let json_flag =
    Arg.(value & flag & info ["json"] ~doc:"Output in JSON format.")
  in
  let run name json =
    with_group ~name (fun grp ->
        if json then (
          print_endline (Yojson.Safe.pretty_to_string (Group.to_yojson grp)) ;
          `Ok ())
        else (
          Format.printf "Name:         %s@." grp.name ;
          Format.printf "Network:      %s@." grp.network ;
          Format.printf
            "Binary:       %s@."
            (Binary_registry.bin_source_to_string grp.bin_source) ;
          Format.printf "App bin dir:  %s@." grp.app_bin_dir ;
          Format.printf "Service user: %s@." grp.service_user ;
          Format.printf "Created at:   %s@." grp.created_at ;
          (* List services belonging to this group *)
          (match Service_registry.list () with
          | Ok services -> (
              let group_services =
                List.filter
                  (fun (svc : Service.t) ->
                    Option.equal String.equal svc.group (Some grp.name))
                  services
              in
              match group_services with
              | [] -> ()
              | _ ->
                  Format.printf "@.Services:@." ;
                  List.iter
                    (fun (svc : Service.t) ->
                      Format.printf "  - %s (%s)@." svc.instance svc.role)
                    group_services)
          | Error _ -> ()) ;
          `Ok ()))
  in
  Term.(ret (const run $ name_arg $ json_flag))

let show_cmd =
  let info = Cmd.info "show" ~doc:"Show details of an instance group." in
  Cmd.v info show_term

(* ── delete ─────────────────────────────────────────────────── *)

let delete_term =
  let cascade =
    Arg.(
      value & flag & info ["cascade"] ~doc:"Delete all services in the group.")
  in
  let ungroup =
    Arg.(
      value & flag
      & info ["ungroup"] ~doc:"Remove group but keep services (ungroup them).")
  in
  let run name cascade ungroup =
    with_group ~name (fun _grp ->
        let result =
          match Service_registry.list () with
          | Error (`Msg msg) -> Error msg
          | Ok services -> (
              let group_services =
                List.filter
                  (fun (svc : Service.t) ->
                    Option.equal String.equal svc.group (Some name))
                  services
              in
              if cascade && ungroup then
                Error "--cascade and --ungroup are mutually exclusive"
              else
                match group_services with
                | [] ->
                    Result.map_error
                      (fun (`Msg s) -> s)
                      (Group_registry.remove ~name)
                | _ when cascade ->
                    (* Remove in reverse dependency order *)
                    let sorted =
                      List.sort
                        (fun (a : Service.t) (b : Service.t) ->
                          Int.compare
                            (Lifecycle.role_order b.role)
                            (Lifecycle.role_order a.role))
                        group_services
                    in
                    List.iter
                      (fun (svc : Service.t) ->
                        match
                          Removal.remove_service
                            ~quiet:false
                            ~delete_data_dir:false
                            ~instance:svc.instance
                            ()
                        with
                        | Ok () ->
                            Format.printf "Removed service '%s'.@." svc.instance
                        | Error (`Msg msg) ->
                            Format.eprintf
                              "Warning: failed to remove '%s': %s@."
                              svc.instance
                              msg)
                      sorted ;
                    Result.map_error
                      (fun (`Msg s) -> s)
                      (Group_registry.remove ~name)
                | _ when ungroup ->
                    List.iter
                      (fun (svc : Service.t) ->
                        let updated = {svc with group = None} in
                        match Service_registry.write updated with
                        | Ok () ->
                            Format.printf
                              "Ungrouped service '%s'.@."
                              svc.instance
                        | Error (`Msg msg) ->
                            Format.eprintf
                              "Warning: failed to ungroup '%s': %s@."
                              svc.instance
                              msg)
                      group_services ;
                    Result.map_error
                      (fun (`Msg s) -> s)
                      (Group_registry.remove ~name)
                | _ ->
                    Error
                      (Printf.sprintf
                         "Group '%s' has %d service(s). Use --cascade to \
                          delete them or --ungroup to keep them."
                         name
                         (List.length group_services)))
        in
        match result with
        | Ok () ->
            Format.printf "Group '%s' deleted.@." name ;
            `Ok ()
        | Error msg -> Cli_helpers.cmdliner_error msg)
  in
  Term.(ret (const run $ name_arg $ cascade $ ungroup))

let delete_cmd =
  let info = Cmd.info "delete" ~doc:"Delete an instance group." in
  Cmd.v info delete_term

(* ── add (add existing service to group) ────────────────────── *)

let add_term =
  let instance =
    let doc = "Instance name to add to the group." in
    Arg.(
      required & opt (some string) None & info ["instance"] ~doc ~docv:"INST")
  in
  let run name instance =
    with_group ~name (fun grp ->
        match Service_registry.find ~instance with
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
        | Ok None ->
            Cli_helpers.cmdliner_error
              (Printf.sprintf "Unknown instance '%s'" instance)
        | Ok (Some svc) -> (
            if not (String.equal svc.Service.network grp.Group.network) then
              Cli_helpers.cmdliner_error
                (Printf.sprintf
                   "Network mismatch: instance '%s' is on '%s' but group '%s' \
                    is on '%s'"
                   instance
                   svc.Service.network
                   name
                   grp.Group.network)
            else
              let updated = {svc with Service.group = Some name} in
              match Service_registry.write updated with
              | Ok () ->
                  Format.printf
                    "Added instance '%s' to group '%s'.@."
                    instance
                    name ;
                  `Ok ()
              | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg))
  in
  Term.(ret (const run $ name_arg $ instance))

let add_cmd =
  let info = Cmd.info "add" ~doc:"Add an existing service to a group." in
  Cmd.v info add_term

(* ── remove (remove service from group) ─────────────────────── *)

let remove_term =
  let instance =
    let doc = "Instance name to remove from the group." in
    Arg.(
      required & opt (some string) None & info ["instance"] ~doc ~docv:"INST")
  in
  let run name instance =
    with_group ~name (fun _grp ->
        match Service_registry.find ~instance with
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
        | Ok None ->
            Cli_helpers.cmdliner_error
              (Printf.sprintf "Unknown instance '%s'" instance)
        | Ok (Some svc) -> (
            if not (Option.equal String.equal svc.Service.group (Some name))
            then
              Cli_helpers.cmdliner_error
                (Printf.sprintf
                   "Instance '%s' is not in group '%s'"
                   instance
                   name)
            else
              let updated = {svc with Service.group = None} in
              match Service_registry.write updated with
              | Ok () ->
                  Format.printf
                    "Removed instance '%s' from group '%s'.@."
                    instance
                    name ;
                  `Ok ()
              | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg))
  in
  Term.(ret (const run $ name_arg $ instance))

let remove_cmd =
  let info =
    Cmd.info "remove" ~doc:"Remove a service from a group (keeps the service)."
  in
  Cmd.v info remove_term

(* ── start ──────────────────────────────────────────────────── *)

let start_term =
  let run name =
    with_group ~name (fun _grp ->
        match Lifecycle.start_group ~quiet:false ~group_name:name () with
        | Ok started ->
            Format.printf
              "Started %d service(s) in group '%s': %s@."
              (List.length started)
              name
              (String.concat ", " started) ;
            `Ok ()
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
  in
  Term.(ret (const run $ name_arg))

let start_cmd =
  let info =
    Cmd.info "start" ~doc:"Start all services in a group (dependency order)."
  in
  Cmd.v info start_term

(* ── stop ───────────────────────────────────────────────────── *)

let stop_term =
  let run name =
    with_group ~name (fun _grp ->
        match Lifecycle.stop_group ~quiet:false ~group_name:name () with
        | Ok stopped ->
            Format.printf
              "Stopped %d service(s) in group '%s': %s@."
              (List.length stopped)
              name
              (String.concat ", " stopped) ;
            `Ok ()
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
  in
  Term.(ret (const run $ name_arg))

let stop_cmd =
  let info =
    Cmd.info
      "stop"
      ~doc:"Stop all services in a group (reverse dependency order)."
  in
  Cmd.v info stop_term

(* ── restart ────────────────────────────────────────────────── *)

let restart_term =
  let run name =
    with_group ~name (fun _grp ->
        match Lifecycle.restart_group ~quiet:false ~group_name:name () with
        | Ok restarted ->
            Format.printf
              "Restarted %d service(s) in group '%s': %s@."
              (List.length restarted)
              name
              (String.concat ", " restarted) ;
            `Ok ()
        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg)
  in
  Term.(ret (const run $ name_arg))

let restart_cmd =
  let info =
    Cmd.info
      "restart"
      ~doc:"Restart all services in a group (stop all, then start all)."
  in
  Cmd.v info restart_term

(* ── upgrade ────────────────────────────────────────────────── *)

let upgrade_term =
  let octez_version =
    let doc = "New managed Octez version (e.g. '24.1' or 'latest')." in
    Arg.(
      value
      & opt (some string) None
      & info ["octez-version"] ~doc ~docv:"VERSION")
  in
  let bin_dir_alias =
    let doc = "New registered binary directory alias." in
    Arg.(
      value & opt (some string) None & info ["bin-dir-alias"] ~doc ~docv:"ALIAS")
  in
  let app_bin_dir =
    let doc = "New path to binaries directory." in
    Arg.(value & opt (some string) None & info ["app-bin-dir"] ~doc ~docv:"DIR")
  in
  let run name octez_version bin_dir_alias app_bin_dir =
    with_group ~name (fun grp ->
        let result =
          let* resolved_dir, bin_source =
            Cli_helpers.resolve_app_bin_dir
              ?octez_version
              ?bin_dir_alias
              app_bin_dir
          in
          (* Update the group *)
          let updated_grp =
            {grp with Group.bin_source; app_bin_dir = resolved_dir}
          in
          let* () =
            Result.map_error
              (fun (`Msg s) -> s)
              (Group_registry.write updated_grp)
          in
          (* Update all services in the group *)
          let* svcs =
            Result.map_error
              (fun (`Msg s) -> s)
              (Lifecycle.group_services ~group_name:name ())
          in
          List.iter
            (fun (svc : Service.t) ->
              let updated_svc =
                {
                  svc with
                  bin_source = Some bin_source;
                  app_bin_dir = resolved_dir;
                }
              in
              match Service_registry.write updated_svc with
              | Ok () ->
                  Format.printf "Updated '%s' to %s@." svc.instance resolved_dir
              | Error (`Msg msg) ->
                  Format.eprintf
                    "Warning: failed to update '%s': %s@."
                    svc.instance
                    msg)
            svcs ;
          Ok ()
        in
        match result with
        | Ok () ->
            Format.printf "Group '%s' upgraded.@." name ;
            `Ok ()
        | Error msg -> Cli_helpers.cmdliner_error msg)
  in
  Term.(
    ret (const run $ name_arg $ octez_version $ bin_dir_alias $ app_bin_dir))

let upgrade_cmd =
  let info =
    Cmd.info
      "upgrade"
      ~doc:"Upgrade binary version for all services in a group."
  in
  Cmd.v info upgrade_term

(* ── group command group ────────────────────────────────────── *)

let group_cmd =
  let info = Cmd.info "group" ~doc:"Manage instance groups." in
  Cmd.group
    info
    [
      create_cmd;
      list_cmd;
      show_cmd;
      delete_cmd;
      add_cmd;
      remove_cmd;
      start_cmd;
      stop_cmd;
      restart_cmd;
      upgrade_cmd;
    ]
