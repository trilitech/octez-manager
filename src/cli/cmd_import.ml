(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib
open Import

let import_cmd =
  let term =
    let run external_name instance_override network_override strategy cascade
        dry_run =
      Capabilities.register () ;

      (* 1. Find the external service by name *)
      match External_service_detector.detect () with
      | Error msg ->
          Cli_helpers.cmdliner_error
            (Printf.sprintf "Failed to detect external services: %s" msg)
      | Ok services -> (
          match
            List.find_opt
              (fun (svc : External_service.t) ->
                svc.config.unit_name = external_name
                || svc.suggested_instance_name = external_name)
              services
          with
          | None ->
              Cli_helpers.cmdliner_error
                (Printf.sprintf
                   "External service '%s' not found. Use 'list --external' to \
                    see available services."
                   external_name)
          | Some external_svc -> (
              (* 2. Validate importability *)
              match Import.validate_importable external_svc with
              | Error (`Msg msg) ->
                  Cli_helpers.cmdliner_error
                    (Printf.sprintf "Cannot import service: %s" msg)
              | Ok () -> (
                  (* 3. Check for missing required fields *)
                  let missing = Import.missing_required_fields external_svc in
                  if missing <> [] && not dry_run then (
                    Format.eprintf
                      "Warning: Missing required fields: %s@."
                      (String.concat ", " missing) ;
                    if network_override = None then
                      Format.eprintf
                        "Use --network to specify the network if not \
                         detected.@.") ;

                  (* 4. Validate strategy *)
                  match strategy with
                  | Some s when s <> "clone" && s <> "takeover" ->
                      Cli_helpers.cmdliner_error
                        (Printf.sprintf
                           "Unknown strategy '%s'. Use 'takeover' or 'clone'."
                           s)
                  | _ -> (
                      let import_strategy =
                        match strategy with
                        | Some "clone" -> Import.Clone
                        | _ -> Import.Takeover
                      in
                      let overrides : Import.field_overrides =
                        {
                          network = network_override;
                          data_dir = None;
                          base_dir = None;
                          rpc_addr = None;
                          net_addr = None;
                          delegates = None;
                        }
                      in
                      let options : Import.import_options =
                        {
                          strategy = import_strategy;
                          new_instance_name = instance_override;
                          overrides;
                          dry_run;
                          preserve_data = true;
                          quiet = false;
                        }
                      in

                      (* 5. Perform import *)
                      let log_fn msg = Format.printf "%s@." msg in
                      if cascade then
                        (* Cascade import: import entire dependency chain *)
                        match
                          Import.import_cascade
                            ~on_log:log_fn
                            ~options
                            ~external_svc
                            ~all_services:services
                            ()
                        with
                        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
                        | Ok results ->
                            if dry_run then (
                              Format.printf
                                "@.Dry run complete. No changes made.@." ;
                              `Ok ())
                            else (
                              Format.printf "@.Cascade import successful!@." ;
                              Format.printf
                                "  Imported %d services:@."
                                (List.length results) ;
                              List.iter
                                (fun (r : Import.import_result) ->
                                  Format.printf
                                    "    %s → %s@."
                                    r.original_unit
                                    r.new_instance)
                                results ;
                              `Ok ())
                      else
                        (* Single service import *)
                        match
                          Import.import_service
                            ~on_log:log_fn
                            ~options
                            ~external_svc
                            ()
                        with
                        | Error (`Msg msg) -> Cli_helpers.cmdliner_error msg
                        | Ok result ->
                            if dry_run then (
                              Format.printf
                                "@.Dry run complete. No changes made.@." ;
                              `Ok ())
                            else (
                              Format.printf "@.Import successful!@." ;
                              Format.printf
                                "  Original unit: %s@."
                                result.original_unit ;
                              Format.printf
                                "  New instance: %s@."
                                result.new_instance ;
                              if result.warnings <> [] then (
                                Format.printf "@.Warnings:@." ;
                                List.iter
                                  (fun w -> Format.printf "  - %s@." w)
                                  result.warnings) ;
                              Format.printf
                                "@.Use 'octez-manager %s start' to start the \
                                 service.@."
                                result.new_instance ;
                              `Ok ())))))
    in
    let external_name_arg =
      Arg.(
        required
        & pos 0 (some string) None
        & info
            []
            ~docv:"EXTERNAL_NAME"
            ~doc:"Name of external service to import")
    in
    let instance_arg =
      let doc =
        "Instance name for the imported service (default: auto-generated)"
      in
      Arg.(
        value & opt (some string) None & info ["as"; "name"] ~docv:"NAME" ~doc)
    in
    let network_arg =
      let doc = "Override network (if not detected)" in
      Arg.(
        value
        & opt (some string) None
        & info ["network"; "n"] ~docv:"NETWORK" ~doc)
    in
    let strategy_arg =
      let doc =
        "Import strategy: 'takeover' (default, disable original) or 'clone' \
         (keep original)"
      in
      Arg.(
        value
        & opt (some string) None
        & info ["strategy"; "s"] ~docv:"STRATEGY" ~doc)
    in
    let cascade_flag =
      let doc =
        "Import service and all its dependencies (cascade import). Analyzes \
         dependency graph and imports in correct order (dependencies first)."
      in
      Arg.(value & flag & info ["cascade"; "c"] ~doc)
    in
    let dry_run_flag =
      let doc = "Show import plan without executing" in
      Arg.(value & flag & info ["dry-run"; "d"] ~doc)
    in
    Term.(
      ret
        (const run $ external_name_arg $ instance_arg $ network_arg
       $ strategy_arg $ cascade_flag $ dry_run_flag))
  in
  let info =
    Cmd.info
      "import"
      ~doc:"Import an external Octez service"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Import an existing Octez service detected on the system into \
             managed services.";
          `P
            "By default, uses the 'takeover' strategy which disables the \
             original systemd unit and gives octez-manager full control.";
          `P
            "Use 'clone' strategy to keep the original service intact while \
             creating a managed copy (useful for testing).";
          `P "Use --dry-run to preview the import plan without making changes.";
          `P
            "Use --cascade to import a service along with all its dependencies \
             in the correct order.";
          `S Manpage.s_examples;
          `P "Import an external node:";
          `Pre "  octez-manager import tezos-node-mainnet";
          `P "Import with custom name:";
          `Pre "  octez-manager import tezos-node-mainnet --as my-node";
          `P "Preview import plan:";
          `Pre "  octez-manager import tezos-baker-ghostnet --dry-run";
          `P "Clone instead of takeover:";
          `Pre "  octez-manager import my-baker --strategy clone";
          `P "Import baker with all dependencies (node + DAL):";
          `Pre "  octez-manager import my-baker --cascade";
        ]
  in
  Cmd.v info term
