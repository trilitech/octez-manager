(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
include Helpers

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
