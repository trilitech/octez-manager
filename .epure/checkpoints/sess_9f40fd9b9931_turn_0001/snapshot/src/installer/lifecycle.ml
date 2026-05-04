(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Helpers

let start_service ?quiet ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Check parent dependency is running *)
      let* () =
        match svc.depends_on with
        | None -> Ok ()
        | Some parent_instance -> (
            match Service_registry.find ~instance:parent_instance with
            | Ok (Some parent) -> (
                match
                  Systemd.is_active ~role:parent.role ~instance:parent_instance
                with
                | Ok true -> Ok ()
                | Ok false ->
                    R.error_msgf
                      "Cannot start %s: dependency '%s' is not running.\n\
                       Start it first with: octez-manager instance %s start"
                      instance
                      parent_instance
                      parent_instance
                | Error _ ->
                    R.error_msgf
                      "Cannot start %s: dependency '%s' is not running.\n\
                       Start it first with: octez-manager instance %s start"
                      instance
                      parent_instance
                      parent_instance)
            | _ ->
                (* Parent not found in registry, skip check *)
                Ok ())
      in
      Systemd.start ?quiet ~role:svc.role ~instance ()

let stop_service_cascade ?quiet ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Stop dependents first *)
      let* () =
        if svc.dependents <> [] then (
          if not (Option.value ~default:false quiet) then
            Printf.printf
              "Stopping dependents: %s\n"
              (String.concat ", " svc.dependents) ;
          List.fold_left
            (fun acc dep ->
              let* () = acc in
              (* Silently ignore missing dependents during cascade *)
              match Service_registry.find ~instance:dep with
              | Ok (Some dep_svc) ->
                  Systemd.stop ?quiet ~role:dep_svc.role ~instance:dep ()
              | _ -> Ok ())
            (Ok ())
            svc.dependents)
        else Ok ()
      in
      Systemd.stop ?quiet ~role:svc.role ~instance ()

let stop_service ?quiet ~instance () = stop_service_cascade ?quiet ~instance ()

let get_stopped_dependencies ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some _svc ->
      (* Collect all stopped parent dependencies *)
      let rec collect_deps acc inst =
        match Service_registry.find ~instance:inst with
        | Ok (Some s) -> (
            match s.depends_on with
            | None -> Ok acc
            | Some parent_inst -> (
                match Service_registry.find ~instance:parent_inst with
                | Ok (Some parent) -> (
                    match
                      Systemd.is_active ~role:parent.role ~instance:parent_inst
                    with
                    | Ok true -> collect_deps acc parent_inst
                    | Ok false | Error _ ->
                        (* Parent is stopped, add it and check its dependencies *)
                        collect_deps (parent :: acc) parent_inst)
                | _ -> Ok acc))
        | _ -> Ok acc
      in
      let* deps = collect_deps [] instance in
      (* Return in order: topmost parent first *)
      Ok (List.rev deps)

let get_stopped_dependents ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | None -> R.error_msgf "Instance '%s' not found" instance
  | Some svc ->
      (* Collect all stopped dependents *)
      let stopped =
        List.filter_map
          (fun dep_inst ->
            match Service_registry.find ~instance:dep_inst with
            | Ok (Some dep) -> (
                match Systemd.is_active ~role:dep.role ~instance:dep_inst with
                | Ok true -> None
                | Ok false | Error _ -> Some dep)
            | _ -> None)
          svc.dependents
      in
      Ok stopped

let restart_service ?quiet ~instance () =
  let* svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | Some svc -> Systemd.restart ?quiet ~role:svc.role ~instance ()
  | None -> R.error_msgf "Instance '%s' not found" instance

(** Sort services by role order for dependency-aware start/stop. *)
let role_order = function
  | "node" -> 0
  | "baker" -> 1
  | "accuser" -> 2
  | "dal-node" -> 3
  | "signatory" -> 4
  | _ -> 5

let group_services ~group_name () =
  let* services = Service_registry.list () in
  let group_svcs =
    List.filter
      (fun (svc : Service.t) ->
        Option.equal String.equal svc.group (Some group_name))
      services
  in
  let sorted =
    List.sort
      (fun (a : Service.t) (b : Service.t) ->
        let rc = Int.compare (role_order a.role) (role_order b.role) in
        if rc <> 0 then rc else String.compare a.instance b.instance)
      group_svcs
  in
  Ok sorted

let start_group ?quiet ~group_name () =
  let* svcs = group_services ~group_name () in
  match svcs with
  | [] -> R.error_msgf "No services in group '%s'" group_name
  | _ ->
      (* Start in dependency order (nodes first) — fail fast *)
      let rec start_all acc = function
        | [] -> Ok (List.rev acc)
        | (svc : Service.t) :: rest -> (
            match start_service ?quiet ~instance:svc.instance () with
            | Ok () -> start_all (svc.instance :: acc) rest
            | Error _ as err -> err)
      in
      start_all [] svcs

let stop_group ?quiet ~group_name () =
  let* svcs = group_services ~group_name () in
  match svcs with
  | [] -> R.error_msgf "No services in group '%s'" group_name
  | _ ->
      (* Stop in reverse dependency order (children first) — best effort *)
      let rev_svcs = List.rev svcs in
      let stopped =
        List.filter_map
          (fun (svc : Service.t) ->
            match stop_service_cascade ?quiet ~instance:svc.instance () with
            | Ok () -> Some svc.instance
            | Error (`Msg msg) ->
                if not (Option.value ~default:false quiet) then
                  Printf.eprintf
                    "Warning: failed to stop '%s': %s\n%!"
                    svc.instance
                    msg ;
                None)
          rev_svcs
      in
      Ok stopped

let restart_group ?quiet ~group_name () =
  let* _stopped = stop_group ?quiet ~group_name () in
  start_group ?quiet ~group_name ()
