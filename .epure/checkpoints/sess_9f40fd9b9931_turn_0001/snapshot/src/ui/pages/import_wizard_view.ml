(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the import wizard page. No Eio calls. *)

open Octez_manager_lib
module Widgets = Miaou_widgets_display.Widgets

type step = SelectService | ConfigureImport | ReviewImport | Importing

type state = {
  step : step;
  external_services : External_service.t list;
  selected_idx : int;
  selected_service : External_service.t option;
  strategy : Import.import_strategy;
  custom_name : string option;
  network_override : string option;
  error : string option;
  cascade : bool;
  cascade_chain : External_service.t list;
  cascade_analysis : Import_cascade.dependency_analysis option;
}

let header s =
  let step_text =
    match s.step with
    | SelectService -> "Step 1/3: Select Service"
    | ConfigureImport -> "Step 2/3: Configure Import"
    | ReviewImport -> "Step 3/3: Review & Confirm"
    | Importing -> "Importing..."
  in
  [
    Widgets.themed_primary (" Import Wizard \xc2\xb7 " ^ step_text);
    Widgets.themed_muted "";
  ]

let view s ~focus:_ ~size =
  let body_lines =
    match s.step with
    | SelectService ->
        if s.external_services = [] then
          [
            "";
            "No external services detected.";
            "";
            "Press 'r' to refresh, Esc to cancel.";
          ]
        else
          s.external_services
          |> List.mapi (fun idx (svc : External_service.t) ->
              let is_selected = idx = s.selected_idx in
              let marker =
                if is_selected then Widgets.themed_accent "\xe2\x96\xb8 "
                else "  "
              in
              let name = Widgets.themed_emphasis svc.suggested_instance_name in
              let role =
                match svc.config.role.value with
                | Some r ->
                    Widgets.themed_secondary (External_service.role_to_string r)
                | None -> "unknown"
              in
              let conf =
                match svc.config.role.confidence with
                | External_service.Detected -> Widgets.themed_success "\xe2\x97\x8f\xe2\x97\x8f\xe2\x97\x8f"
                | External_service.Inferred -> Widgets.themed_warning "\xe2\x97\x8f\xe2\x97\x8f\xe2\x97\x8b"
                | External_service.Permission_denied | External_service.Unknown
                  ->
                    Widgets.themed_error "\xe2\x97\x8f\xe2\x97\x8b\xe2\x97\x8b"
              in
              let line =
                Printf.sprintf "%s%-25s %-12s %s" marker name role conf
              in
              if is_selected then
                [
                  line;
                  Widgets.themed_muted
                    (Printf.sprintf "    Unit: %s" svc.config.unit_name);
                  Widgets.themed_muted
                    (Printf.sprintf
                       "    Network: %s"
                       (External_service.value_or
                          ~default:"not detected"
                          svc.config.network));
                  "";
                ]
              else [line])
          |> List.flatten
          |> List.append [""; ""]
          |> List.append
               (if s.error <> None then
                  [
                    Widgets.themed_error
                      (Printf.sprintf "Error: %s" (Option.get s.error));
                    "";
                  ]
                else [])
          |> List.append
               ["\xe2\x86\x91/\xe2\x86\x93: Navigate  Enter: Select  r: Refresh  Esc: Cancel"]
    | ConfigureImport -> (
        match s.selected_service with
        | None -> ["Error: No service selected"]
        | Some svc ->
            let final_name =
              Option.value s.custom_name ~default:svc.suggested_instance_name
            in
            let strategy_text =
              match s.strategy with
              | Import.Takeover -> "Takeover (disable original)"
              | Import.Clone -> "Clone (keep original)"
            in
            let cascade_text = if s.cascade then "Yes" else "No" in
            [
              "";
              Printf.sprintf
                "Service: %s"
                (Widgets.themed_emphasis svc.suggested_instance_name);
              "";
              Printf.sprintf "  Instance name: %s" final_name;
              Printf.sprintf "  Strategy:      %s" strategy_text;
              Printf.sprintf
                "  Network:       %s"
                (Option.value
                   s.network_override
                   ~default:
                     (External_service.value_or
                        ~default:"(auto-detect)"
                        svc.config.network));
              Printf.sprintf "  Cascade:       %s" cascade_text;
              "";
            ]
            @ (let missing =
                 Import.missing_required_fields
                   ?network_override:s.network_override
                   svc
               in
               if missing <> [] then
                 [
                   Widgets.themed_warning "\xe2\x9a\xa0 Missing fields:";
                   Widgets.themed_warning
                     (Printf.sprintf "  %s" (String.concat ", " missing));
                   "";
                 ]
               else [])
            @ (if s.cascade then
                 match s.cascade_analysis with
                 | None ->
                     [""; Widgets.themed_muted "  Computing cascade..."; ""]
                 | Some analysis ->
                     let chain_count = List.length s.cascade_chain in
                     let order_str =
                       String.concat
                         " \xe2\x86\x92 "
                         (List.map
                            (fun unit_name ->
                              match
                                List.find_opt
                                  (fun (svc : External_service.t) ->
                                    String.equal svc.config.unit_name unit_name)
                                  s.external_services
                              with
                              | Some svc -> svc.suggested_instance_name
                              | None -> unit_name)
                            analysis.import_order)
                     in
                     [
                       "";
                       Widgets.themed_emphasis
                         (Printf.sprintf
                            "Cascade import: %d services"
                            chain_count);
                       Widgets.themed_muted (Printf.sprintf "  %s" order_str);
                       "";
                     ]
               else [])
            @ [
                "";
                "Space: Toggle strategy  c: Toggle cascade  Enter: Next  Esc: \
                 Back";
              ])
    | ReviewImport -> (
        match s.selected_service with
        | None -> ["Error: No service selected"]
        | Some svc ->
            let final_name =
              Option.value s.custom_name ~default:svc.suggested_instance_name
            in
            if s.cascade then
              (* Cascade import review *)
              let chain_count = List.length s.cascade_chain in
              [
                "";
                Widgets.themed_success "Ready to cascade import:";
                "";
                Printf.sprintf
                  "  Target service: %s"
                  (Widgets.themed_emphasis svc.suggested_instance_name);
                Printf.sprintf "  Services to import: %d" chain_count;
                Printf.sprintf
                  "  Strategy:       %s"
                  (match s.strategy with
                  | Import.Takeover ->
                      Widgets.themed_warning "Takeover (will disable originals)"
                  | Import.Clone ->
                      Widgets.themed_success "Clone (keep originals)");
                "";
                "Import order:";
              ]
              @ (match s.cascade_analysis with
                | None -> ["  (analysis not available)"]
                | Some analysis ->
                    List.mapi
                      (fun i unit_name ->
                        match
                          List.find_opt
                            (fun (svc : External_service.t) ->
                              String.equal svc.config.unit_name unit_name)
                            s.external_services
                        with
                        | Some svc ->
                            let role_str =
                              match svc.config.role.value with
                              | Some r -> External_service.role_to_string r
                              | None -> "unknown"
                            in
                            Printf.sprintf
                              "  %d. %s (%s)"
                              (i + 1)
                              svc.suggested_instance_name
                              role_str
                        | None -> Printf.sprintf "  %d. %s" (i + 1) unit_name)
                      analysis.import_order)
              @ [""; "Enter: Confirm  Esc: Back"]
            else
              (* Single service import review *)
              [
                "";
                Widgets.themed_success "Ready to import:";
                "";
                Printf.sprintf "  Original unit: %s" svc.config.unit_name;
                Printf.sprintf
                  "  New instance:  %s"
                  (Widgets.themed_emphasis final_name);
                Printf.sprintf
                  "  Strategy:      %s"
                  (match s.strategy with
                  | Import.Takeover ->
                      Widgets.themed_warning "Takeover (will disable original)"
                  | Import.Clone ->
                      Widgets.themed_success "Clone (keep original)");
                "";
                "What will happen:";
              ]
              @ (match s.strategy with
                | Import.Takeover ->
                    [
                      "  1. Stop external service";
                      "  2. Create managed service";
                      "  3. Disable original systemd unit";
                      "  4. Start managed service";
                    ]
                | Import.Clone ->
                    [
                      "  1. Create managed service (copy config)";
                      "  2. Keep original service running";
                      "  3. Start managed service";
                    ])
              @ [""; "Enter: Confirm  Esc: Back"])
    | Importing -> (
        (* Show live progress from Job_manager *)
        let all_jobs = Job_manager.list () in
        let num_jobs = List.length all_jobs in
        match Job_manager.get_running_job () with
        | Some job ->
            let phase =
              if job.phase <> "" then Printf.sprintf " - %s" job.phase else ""
            in
            let num_logs = List.length job.log in
            let log_lines =
              List.rev job.log
              |> (fun lines ->
              if List.length lines > 10 then
                List.filteri (fun i _ -> i < 10) lines
              else lines)
              |> List.map (fun line -> "  " ^ line)
            in
            [""; Printf.sprintf "\xe2\x8f\xb3  Importing...%s" phase; ""]
            @ log_lines
            @ [
                "";
                Widgets.themed_muted
                  (Printf.sprintf
                     "  (job #%d, %d log lines, %d jobs total)"
                     job.id
                     num_logs
                     num_jobs);
              ]
        | None -> (
            (* Fallback if job already finished or not found *)
            let latest = Job_manager.get_latest_job () in
            match latest with
            | None ->
                [
                  "";
                  "";
                  "  \xe2\x8f\xb3  Importing...";
                  "";
                  Widgets.themed_muted
                    (Printf.sprintf "  Debug: No jobs (%d total)" num_jobs);
                  "";
                ]
            | Some j -> (
                match j.status with
                | Job_manager.Running ->
                    [""; "  \xe2\x8f\xb3  Importing..."; ""; "  Status: Running"; ""]
                | Job_manager.Pending ->
                    [""; "  \xe2\x8f\xb3  Importing..."; ""; "  Status: Pending"; ""]
                | Job_manager.Succeeded ->
                    [
                      "";
                      Widgets.themed_success "  \xe2\x9c\x93 Import succeeded!";
                      "";
                      "  Returning to instances page...";
                      "";
                    ]
                | Job_manager.Failed msg ->
                    let log_lines =
                      List.rev j.log
                      |> (fun lines ->
                      let len = List.length lines in
                      if len > 15 then
                        List.filteri (fun i _ -> i >= len - 15) lines
                      else lines)
                      |> List.map (fun line -> "  " ^ line)
                    in
                    [""; Widgets.themed_error "  \xe2\x9c\x97 Import failed"; ""]
                    @ (if msg <> "" then ["  Error: " ^ msg; ""] else [])
                    @ (if log_lines <> [] then
                         ["  Log output:"; ""] @ log_lines @ [""]
                       else ["  (no log output)"; ""])
                    @ [""; "Press Esc to go back"])))
  in
  Themed_page.render_layout ~size ~header:(header s) ~footer:[] ~child:(fun _ ->
      String.concat "\n" body_lines)
