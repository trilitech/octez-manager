(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Import wizard - convert external services to managed instances *)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
open Octez_manager_lib

let name = "import_wizard"

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
}

type msg = unit

type pstate = state Navigation.t

let load_external_services () = External_services_scheduler.get ()

let init () =
  let external_services = load_external_services () in
  Navigation.make
    {
      step = SelectService;
      external_services;
      selected_idx = 0;
      selected_service = None;
      strategy = Import.Takeover;
      custom_name = None;
      network_override = None;
      error = None;
    }

let update ps _ =
  (* Check for pending navigation (e.g., from job completion callback) *)
  match Context.consume_navigation () with
  | Some page -> Navigation.goto page ps
  | None -> ps

let refresh ps =
  (* Check for pending navigation (e.g., from job completion callback) *)
  let ps =
    match Context.consume_navigation () with
    | Some page -> Navigation.goto page ps
    | None -> ps
  in
  Navigation.update
    (fun s ->
      let external_services = load_external_services () in
      let selected_idx =
        min s.selected_idx (max 0 (List.length external_services - 1))
      in
      {s with external_services; selected_idx; error = None})
    ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps =
  let s = ps.Navigation.s in
  match s.step with
  | SelectService -> Navigation.back ps
  | ConfigureImport ->
      Navigation.update
        (fun s -> {s with step = SelectService; error = None})
        ps
  | ReviewImport ->
      Navigation.update
        (fun s -> {s with step = ConfigureImport; error = None})
        ps
  | Importing ->
      (* Allow exiting from Importing state *)
      Navigation.back ps

let with_selected_service s on_error on_success =
  match List.nth_opt s.external_services s.selected_idx with
  | None -> on_error "No service selected"
  | Some svc -> on_success svc

let rec next_step ps =
  let s = ps.Navigation.s in
  match s.step with
  | SelectService ->
      with_selected_service
        s
        (fun msg -> Navigation.update (fun s -> {s with error = Some msg}) ps)
        (fun svc ->
          match Import.validate_importable svc with
          | Error (`Msg msg) ->
              Navigation.update (fun s -> {s with error = Some msg}) ps
          | Ok () ->
              Navigation.update
                (fun s ->
                  {
                    s with
                    step = ConfigureImport;
                    selected_service = Some svc;
                    error = None;
                  })
                ps)
  | ConfigureImport ->
      Navigation.update (fun s -> {s with step = ReviewImport; error = None}) ps
  | ReviewImport -> start_import ps
  | Importing -> ps

and start_import ps =
  let s = ps.Navigation.s in
  match s.selected_service with
  | None ->
      Navigation.update
        (fun s -> {s with error = Some "No service selected"})
        ps
  | Some external_svc ->
      let import_task ~append_log () =
        let overrides : Import.field_overrides =
          {
            network = s.network_override;
            data_dir = None;
            base_dir = None;
            rpc_addr = None;
            net_addr = None;
            delegates = None;
          }
        in
        let options : Import.import_options =
          {
            strategy = s.strategy;
            new_instance_name = s.custom_name;
            overrides;
            dry_run = false;
            interactive = false;
            preserve_data = true;
            quiet = false;
          }
        in
        match
          Import.import_service ~on_log:append_log ~options ~external_svc ()
        with
        | Ok _result -> Ok ()
        | Error e -> Error e
      in
      Job_manager.submit
        ~on_complete:(fun status ->
          match status with
          | Job_manager.Succeeded ->
              Cache.invalidate_all () ;
              Context.toast_success "Service imported successfully!" ;
              Context.navigate "instances"
          | Job_manager.Failed msg ->
              Context.toast_error (Printf.sprintf "Import failed: %s" msg)
          | Job_manager.Pending | Job_manager.Running -> ())
        ~description:"Import external service"
        import_task ;
      Navigation.update (fun s -> {s with step = Importing; error = None}) ps

let handled_keys () = Miaou.Core.Keys.[Escape; Enter; Up; Down]

let keymap _ =
  let kb key action help =
    {Miaou.Core.Tui_page.key; action; help; display_only = false}
  in
  [kb "Esc" back "Back / Previous"; kb "Enter" next_step "Select / Next"]

let move_selection ps delta =
  Navigation.update
    (fun s ->
      let len = List.length s.external_services in
      if len = 0 then s
      else
        let selected_idx = (s.selected_idx + delta + len) mod len in
        {s with selected_idx})
    ps

let toggle_strategy ps =
  Navigation.update
    (fun s ->
      let strategy =
        match s.strategy with
        | Import.Takeover -> Import.Clone
        | Import.Clone -> Import.Takeover
      in
      {s with strategy})
    ps

let header s =
  let step_text =
    match s.step with
    | SelectService -> "Step 1/3: Select Service"
    | ConfigureImport -> "Step 2/3: Configure Import"
    | ReviewImport -> "Step 3/3: Review & Confirm"
    | Importing -> "Importing..."
  in
  [Widgets.title_highlight (" Import Wizard · " ^ step_text); Widgets.dim ""]

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
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
              let marker = if is_selected then Widgets.fg 14 "▸ " else "  " in
              let name = Widgets.bold svc.suggested_instance_name in
              let role =
                match svc.config.role.value with
                | Some r -> Widgets.fg 12 (External_service.role_to_string r)
                | None -> "unknown"
              in
              let conf =
                match svc.config.role.confidence with
                | External_service.Detected -> Widgets.fg 10 "●●●"
                | External_service.Inferred -> Widgets.fg 11 "●●○"
                | External_service.Permission_denied | External_service.Unknown
                  ->
                    Widgets.fg 9 "●○○"
              in
              let line =
                Printf.sprintf "%s%-25s %-12s %s" marker name role conf
              in
              if is_selected then
                [
                  line;
                  Widgets.dim
                    (Printf.sprintf "    Unit: %s" svc.config.unit_name);
                  Widgets.dim
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
                    Widgets.fg
                      9
                      (Printf.sprintf "Error: %s" (Option.get s.error));
                    "";
                  ]
                else [])
          |> List.append
               ["↑/↓: Navigate  Enter: Select  r: Refresh  Esc: Cancel"]
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
            [
              "";
              Printf.sprintf
                "Service: %s"
                (Widgets.bold svc.suggested_instance_name);
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
              "";
            ]
            @ (let missing = Import.missing_required_fields svc in
               if missing <> [] then
                 [
                   Widgets.fg 11 "⚠ Missing fields:";
                   Widgets.fg
                     11
                     (Printf.sprintf "  %s" (String.concat ", " missing));
                   "";
                 ]
               else [])
            @ [""; "Space: Toggle strategy  Enter: Next  Esc: Back"])
    | ReviewImport -> (
        match s.selected_service with
        | None -> ["Error: No service selected"]
        | Some svc ->
            let final_name =
              Option.value s.custom_name ~default:svc.suggested_instance_name
            in
            [
              "";
              Widgets.fg 10 "Ready to import:";
              "";
              Printf.sprintf "  Original unit: %s" svc.config.unit_name;
              Printf.sprintf "  New instance:  %s" (Widgets.bold final_name);
              Printf.sprintf
                "  Strategy:      %s"
                (match s.strategy with
                | Import.Takeover ->
                    Widgets.fg 11 "Takeover (will disable original)"
                | Import.Clone -> Widgets.fg 10 "Clone (keep original)");
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
            [""; Printf.sprintf "⏳  Importing...%s" phase; ""]
            @ log_lines
            @ [
                "";
                Widgets.dim
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
                  "  ⏳  Importing...";
                  "";
                  Widgets.dim
                    (Printf.sprintf "  Debug: No jobs (%d total)" num_jobs);
                  "";
                ]
            | Some j -> (
                match j.status with
                | Job_manager.Running ->
                    [""; "  ⏳  Importing..."; ""; "  Status: Running"; ""]
                | Job_manager.Pending ->
                    [""; "  ⏳  Importing..."; ""; "  Status: Pending"; ""]
                | Job_manager.Succeeded ->
                    [
                      "";
                      Widgets.fg 10 "  ✓ Import succeeded!";
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
                    [""; Widgets.fg 9 "  ✗ Import failed"; ""]
                    @ (if msg <> "" then ["  Error: " ^ msg; ""] else [])
                    @ (if log_lines <> [] then
                         ["  Log output:"; ""] @ log_lines @ [""]
                       else ["  (no log output)"; ""])
                    @ [""; "Press Esc to go back"])))
  in
  Vsection.render ~size ~header:(header s) ~content_footer:[] ~child:(fun _ ->
      String.concat "\n" body_lines)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    let s = ps.Navigation.s in
    match (s.step, Keys.of_string key) with
    | _, Some (Keys.Char "r") -> refresh ps
    | SelectService, Some Keys.Up -> move_selection ps (-1)
    | SelectService, Some Keys.Down -> move_selection ps 1
    | SelectService, Some Keys.Enter -> next_step ps
    | SelectService, Some Keys.Escape -> Navigation.back ps
    | ConfigureImport, Some (Keys.Char " ") -> toggle_strategy ps
    | ConfigureImport, Some Keys.Enter -> next_step ps
    | ConfigureImport, Some Keys.Escape -> back ps
    | ReviewImport, Some Keys.Enter -> next_step ps
    | ReviewImport, Some Keys.Escape -> back ps
    | Importing, Some Keys.Escape ->
        (* Allow exiting from Importing state *)
        Navigation.back ps
    | _ -> ps

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let handled_keys = handled_keys

  let keymap = keymap

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let has_modal = has_modal
end

module Page =
  Monitored_page.Make
    (Page_Impl)
    (struct
      let page_name = name
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
