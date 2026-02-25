(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Rewards page: TUI dashboard for reward distribution. *)

open Octez_manager_lib
module Widgets = Miaou_widgets_display.Widgets
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation

let name = "rewards"

open Rewards_state

type msg = unit

type state = Rewards_state.state

type pstate = state Navigation.t

(* Load baker instances from service registry *)
let load_baker_instances () =
  let bakers =
    Data.load_service_states ()
    |> List.filter (fun (st : Data.Service_state.t) ->
        String.equal st.service.Service.role "baker")
  in
  List.filter_map
    (fun (st : Data.Service_state.t) ->
      let instance = st.service.Service.instance in
      let delegates = Delegate_scheduler.get_baker_delegates ~instance in
      match delegates with pkh :: _ -> Some (instance, pkh) | [] -> None)
    bakers

let init () =
  let baker_instances = load_baker_instances () in
  Navigation.make
    {
      Rewards_state.baker_instances;
      selected_baker = 0;
      active_tab = Rewards_state.Overview;
      selected_cycle = None;
      current_cycle = Rewards_scheduler.get_current_cycle ();
      delegator_cursor = 0;
      delegator_sort = Rewards_state.SortBalance;
      delegator_filter = Rewards_state.FilterAll;
      search_query = "";
      search_active = false;
      blueprint = None;
      history_cursor = 0;
      loading = false;
      error = None;
    }

let update ps _ = ps

let refresh ps =
  match Context.consume_navigation () with
  | Some (Context.Goto page) -> Navigation.goto page ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      Navigation.update
        (fun s ->
          let baker_instances = load_baker_instances () in
          let current_cycle = Rewards_scheduler.get_current_cycle () in
          {s with baker_instances; current_cycle})
        ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps = Navigation.back ps

(* Tab rendering *)

let render_tab_bar (s : Rewards_state.state) ~cols =
  let tabs =
    List.map
      (fun tab ->
        let label = Rewards_state.tab_label tab in
        if tab = s.active_tab then
          Widgets.themed_emphasis (Printf.sprintf " [%s] " label)
        else Widgets.themed_muted (Printf.sprintf "  %s  " label))
      Rewards_state.all_tabs
  in
  let _ = cols in
  String.concat "" tabs

let render_baker_header (s : Rewards_state.state) =
  match Rewards_state.selected_baker_instance s with
  | None -> Widgets.themed_warning "No baker instances found"
  | Some (instance, pkh) ->
      let short_pkh =
        if String.length pkh > 12 then
          String.sub pkh 0 7 ^ "..." ^ String.sub pkh (String.length pkh - 4) 4
        else pkh
      in
      Widgets.themed_primary
        (Printf.sprintf " Rewards - %s (%s) " instance short_pkh)

let render_overview (s : Rewards_state.state) =
  match Rewards_state.selected_baker_pkh s with
  | None -> Widgets.themed_muted "Select a baker to view rewards"
  | Some baker -> (
      let current =
        match s.current_cycle with
        | Some c -> Printf.sprintf "Current cycle: %d" c
        | None -> "Current cycle: loading..."
      in
      let recent = Rewards_scheduler.get_recent_cycles ~baker in
      match recent with
      | [] ->
          String.concat
            "\n"
            [
              Widgets.themed_text current;
              "";
              Widgets.themed_muted "No cycle data available yet";
              Widgets.themed_muted "Waiting for data from TzKT...";
            ]
      | _ ->
          let cycle_lines =
            List.map
              (fun (cr : Octez_manager_rewards.Rewards.cycle_rewards) ->
                let rewards =
                  Octez_manager_rewards.Rewards.format_tez
                    (Int64.add cr.block_rewards cr.block_fees)
                in
                let delegators = List.length cr.delegators in
                Printf.sprintf
                  "  %d    %s    %d delegators"
                  cr.cycle
                  rewards
                  delegators)
              recent
          in
          String.concat
            "\n"
            ([
               Widgets.themed_text current;
               "";
               Widgets.themed_accent "Recent Cycles:";
               Widgets.themed_muted "  CYCLE  EARNED           DELEGATORS";
             ]
            @ List.map Widgets.themed_text cycle_lines))

let render_placeholder tab_name =
  String.concat
    "\n"
    [
      ""; Widgets.themed_muted (Printf.sprintf "  %s tab — coming soon" tab_name);
    ]

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let cols = size.LTerm_geom.cols in
  let header_line = render_baker_header s in
  let tab_bar = render_tab_bar s ~cols in
  let content =
    match s.active_tab with
    | Rewards_state.Overview -> render_overview s
    | Rewards_state.Delegators -> render_placeholder "Delegators"
    | Rewards_state.History -> render_placeholder "History"
    | Rewards_state.Configuration -> render_placeholder "Configuration"
  in
  let hint = Widgets.themed_muted "1-4 tabs · b baker · r refresh · Esc back" in
  Themed_page.render_layout
    ~size
    ~header:[header_line; tab_bar; ""]
    ~footer:[hint]
    ~child:(fun _ -> content)

(** Count filtered delegators for cursor bounds. *)
let delegator_count s =
  match s.blueprint with
  | None -> 0
  | Some bp ->
      let ds = bp.Rewards.delegator_rewards in
      let ds = Rewards_delegators.apply_filter s.delegator_filter ds in
      let ds =
        if s.search_active || String.length s.search_query > 0 then
          Rewards_delegators.apply_search s.search_query ds
        else ds
      in
      List.length ds

(** Handle keys when search mode is active. *)
let handle_search_key ps key =
  match Keys.of_string key with
  | Some Keys.Escape ->
      Navigation.update
        (fun s -> {s with search_active = false; search_query = ""})
        ps
  | Some Keys.Enter ->
      Navigation.update (fun s -> {s with search_active = false}) ps
  | Some Keys.Backspace ->
      Navigation.update
        (fun s ->
          let len = String.length s.search_query in
          let search_query =
            if len > 0 then String.sub s.search_query 0 (len - 1)
            else s.search_query
          in
          {s with search_query; delegator_cursor = 0})
        ps
  | Some (Keys.Char c) when String.length c = 1 ->
      Navigation.update
        (fun s ->
          {s with search_query = s.search_query ^ c; delegator_cursor = 0})
        ps
  | _ -> ps

(** Handle keys specific to the Delegators tab. *)
let handle_delegator_key ps key =
  let s = ps.Navigation.s in
  match Keys.of_string key with
  | Some (Keys.Char "j") | Some Keys.Down ->
      let count = delegator_count s in
      Navigation.update
        (fun s ->
          {
            s with
            delegator_cursor = min (s.delegator_cursor + 1) (max 0 (count - 1));
          })
        ps
  | Some (Keys.Char "k") | Some Keys.Up ->
      Navigation.update
        (fun s -> {s with delegator_cursor = max (s.delegator_cursor - 1) 0})
        ps
  | Some (Keys.Char "g") ->
      Navigation.update (fun s -> {s with delegator_cursor = 0}) ps
  | Some (Keys.Char "G") ->
      let count = delegator_count s in
      Navigation.update
        (fun s -> {s with delegator_cursor = max 0 (count - 1)})
        ps
  | Some (Keys.Char "/") ->
      Navigation.update (fun s -> {s with search_active = true}) ps
  | Some (Keys.Char "s") ->
      Navigation.update
        (fun s ->
          {
            s with
            delegator_sort = Rewards_state.next_sort_column s.delegator_sort;
            delegator_cursor = 0;
          })
        ps
  | Some (Keys.Char "f") ->
      Navigation.update
        (fun s ->
          {
            s with
            delegator_filter = Rewards_state.next_filter_mode s.delegator_filter;
            delegator_cursor = 0;
          })
        ps
  | Some (Keys.Char "c") -> (
      (* Open cycle selector modal *)
      match Rewards_state.selected_baker_instance s with
      | None -> ps
      | Some (instance, baker) -> (
          let recent = Rewards_scheduler.get_recent_cycles ~baker in
          let cycles =
            List.map (fun (cr : Rewards.cycle_rewards) -> cr.cycle) recent
          in
          match cycles with
          | [] -> ps
          | _ ->
              let current_cycle =
                Rewards_scheduler.get_current_cycle ~instance
              in
              Modal_helpers.open_choice_modal
                ~title:"Select Cycle"
                ~items:cycles
                ~to_string:(fun c ->
                  match current_cycle with
                  | Some cc when Int.equal c cc ->
                      Printf.sprintf "Cycle %d  (current)" c
                  | _ -> Printf.sprintf "Cycle %d" c)
                ~on_select:(fun cycle ->
                  Rewards_scheduler.ensure_cycle_detail ~instance ~baker ~cycle ;
                  pending_cycle := Some cycle)
                () ;
              ps))
  | _ -> ps

(** Handle keys specific to the Configuration tab. *)
let handle_config_key ps key =
  let s = ps.Navigation.s in
  match Keys.of_string key with
  | Some (Keys.Char "j") | Some Keys.Down ->
      Navigation.update
        (fun s ->
          {
            s with
            config_cursor =
              min (s.config_cursor + 1) (Rewards_config_tab.field_count - 1);
          })
        ps
  | Some (Keys.Char "k") | Some Keys.Up ->
      Navigation.update
        (fun s -> {s with config_cursor = max (s.config_cursor - 1) 0})
        ps
  | Some Keys.Enter -> (
      match s.config with
      | Some config -> (
          match List.nth_opt Rewards_config_tab.all_fields s.config_cursor with
          | Some field ->
              Rewards_config_tab.edit_field config field ;
              ps
          | None -> ps)
      | None -> ps)
  | Some (Keys.Char "s") -> (
      match (s.config, Rewards_state.selected_instance_name s) with
      | Some config, Some instance ->
          Rewards_config_tab.save_config ~instance config ;
          Navigation.update (fun s -> {s with config_dirty = false}) ps
      | _ -> ps)
  | Some (Keys.Char "?") ->
      Navigation.update
        (fun s -> {s with config_show_hint = not s.config_show_hint})
        ps
  | Some (Keys.Char "r") -> (
      match Rewards_state.selected_baker_pkh s with
      | Some baker_pkh ->
          Rewards_config_tab.reset_config ~baker_pkh ;
          ps
      | None -> ps)
  | Some (Keys.Char "i") -> (
      (* Import external config.hjson *)
      match Rewards_state.selected_baker_instance s with
      | None -> ps
      | Some (instance, baker_pkh) ->
          Modal_helpers.open_file_browser_modal
            ~dirs_only:false
            ~require_writable:false
            ~on_select:(fun path ->
              match Config_import.import_file ~baker_pkh path with
              | Error msg ->
                  Context.toast_error (Printf.sprintf "Import failed: %s" msg)
              | Ok result ->
                  (match Payout_config.save ~instance result.config with
                  | Ok () ->
                      Rewards_config_tab.set_pending_config result.config ;
                      let msg =
                        Printf.sprintf
                          "Imported %d fields from external config"
                          result.imported_fields
                      in
                      Context.toast_info msg
                  | Error msg ->
                      Context.toast_error (Printf.sprintf "Save failed: %s" msg)) ;
                  List.iter (fun w -> Context.toast_warn w) result.warnings)
            () ;
          ps)
  | Some (Keys.Char "n") -> (
      (* Notification channel test *)
      match Rewards_state.selected_instance_name s with
      | None -> ps
      | Some instance ->
          let channels =
            match Payout_config.load ~instance with
            | Ok c -> c.notifications
            | Error _ -> []
          in
          if channels = [] then (
            Context.toast_info "No notification channels configured" ;
            ps)
          else (
            Modal_helpers.open_choice_modal
              ~title:"Notification Channels"
              ~items:
                (List.map
                   (fun ch ->
                     match ch with
                     | Rewards.Discord _ -> "Test Discord"
                     | Rewards.Telegram _ -> "Test Telegram"
                     | Rewards.Webhook _ -> "Test Webhook"
                     | Rewards.External _ -> "Test External")
                   channels
                @ ["Cancel"])
              ~to_string:Fun.id
              ~on_select:(fun choice ->
                if not (String.equal choice "Cancel") then
                  let results = Payout_notifier.send_test ~channels in
                  List.iter
                    (fun (name, result) ->
                      match result with
                      | Ok () ->
                          Context.toast_info
                            (Printf.sprintf "%s: test sent" name)
                      | Error msg ->
                          Context.toast_warn (Printf.sprintf "%s: %s" name msg))
                    results)
              () ;
            ps))
  | _ -> ps

(** Handle keys specific to the History tab. *)
let handle_history_key ps key =
  let s = ps.Navigation.s in
  let count =
    match Rewards_state.selected_baker_pkh s with
    | None -> 0
    | Some baker ->
        Rewards_history.cycle_count (Rewards_scheduler.get_recent_cycles ~baker)
  in
  match Keys.of_string key with
  | Some (Keys.Char "j") | Some Keys.Down ->
      Navigation.update
        (fun s ->
          {
            s with
            history_cursor = min (s.history_cursor + 1) (max 0 (count - 1));
          })
        ps
  | Some (Keys.Char "k") | Some Keys.Up ->
      Navigation.update
        (fun s -> {s with history_cursor = max (s.history_cursor - 1) 0})
        ps
  | Some Keys.Enter -> (
      (* Navigate to the selected cycle's Overview/Delegators view *)
      match Rewards_state.selected_baker_pkh s with
      | None -> ps
      | Some baker -> (
          let recent = Rewards_scheduler.get_recent_cycles ~baker in
          match List.nth_opt recent s.history_cursor with
          | None -> ps
          | Some (cr : Rewards.cycle_rewards) ->
              let instance =
                match Rewards_state.selected_instance_name s with
                | Some i -> i
                | None -> ""
              in
              Rewards_scheduler.ensure_cycle_detail
                ~instance
                ~baker
                ~cycle:cr.cycle ;
              Navigation.update
                (fun s ->
                  {
                    s with
                    selected_cycle = Some cr.cycle;
                    active_tab = Rewards_state.Overview;
                    blueprint = None;
                    overview_preview = false;
                  })
                ps))
  | _ -> ps

(* ── Payout execution helper ──────────────────────────────── *)

let run_payout_in_background ~instance ~pkh ~network ~cycle ~dry_run =
  let svc_opt = Service_registry.find ~instance in
  match svc_opt with
  | Ok (Some svc) -> (
      let octez_client_bin =
        Filename.concat svc.Service.app_bin_dir "octez-client"
      in
      let node_endpoint =
        Delegate_scheduler.get_baker_node_endpoint ~instance
        |> Option.value
             ~default:("http://" ^ Rpc_addr.to_string svc.Service.rpc_addr)
      in
      let config =
        match Payout_config.load ~instance with
        | Ok c -> c
        | Error _ -> Payout_config.default ~baker_pkh:pkh
      in
      let ctx : Payout_executor.context =
        {
          octez_client_bin;
          endpoint = node_endpoint;
          base_dir = None;
          password_file = None;
          payout_key_alias = config.payout_key_alias;
          instance;
        }
      in
      match
        Payout_blueprint.generate
          ~instance
          ~baker:pkh
          ~network
          ~cycle
          ~force:dry_run
          ()
      with
      | Error msg ->
          Context.toast_error (Printf.sprintf "Generate failed: %s" msg)
      | Ok blueprint ->
          Context.toast_info
            (Printf.sprintf
               "%s cycle %d..."
               (if dry_run then "Dry-running" else "Paying")
               cycle) ;
          if not dry_run then
            Rewards_scheduler.mark_in_progress ~instance ~cycle ;
          ignore
            (Domain_pool.submit (fun () ->
                 match
                   Payout_executor.execute
                     ~ctx
                     ~blueprint
                     ~dry_run
                     ~on_progress:(fun p ->
                       if p.current mod 10 = 0 || p.current = p.total then
                         Context.toast_info
                           (Printf.sprintf "Progress: %d/%d" p.current p.total))
                     ~batch_size:config.sim_batch_size
                     ()
                 with
                 | Ok (results, summary) ->
                     let ok =
                       List.filter
                         (fun (r : Rewards.payout_result) -> r.success)
                         results
                     in
                     let total = List.length results in
                     let succeeded = List.length ok in
                     if succeeded = total then
                       Context.toast_info
                         (Printf.sprintf
                            "%s complete: %d/%d succeeded"
                            (if dry_run then "Dry-run" else "Payout")
                            succeeded
                            total)
                     else
                       Context.toast_warn
                         (Printf.sprintf
                            "%s partial: %d/%d succeeded"
                            (if dry_run then "Dry-run" else "Payout")
                            succeeded
                            total) ;
                     (* Send notifications for real payouts *)
                     (if (not dry_run) && succeeded > 0 then
                        let channels =
                          match Payout_config.load ~instance with
                          | Ok c -> c.notifications
                          | Error _ -> []
                        in
                        if channels <> [] then
                          let results =
                            Payout_notifier.notify_all ~channels ~summary
                          in
                          List.iter
                            (fun (name, result) ->
                              match result with
                              | Error msg ->
                                  Context.toast_warn
                                    (Printf.sprintf
                                       "Notification %s failed: %s"
                                       name
                                       msg)
                              | Ok () -> ())
                            results) ;
                     Rewards_scheduler.clear_in_progress ~instance ~cycle ;
                     Rewards_scheduler.refresh_payout_status ~instance ~cycle ;
                     Rewards_scheduler.refresh_baker ~instance
                 | Error msg ->
                     Rewards_scheduler.clear_in_progress ~instance ~cycle ;
                     Rewards_scheduler.refresh_payout_status ~instance ~cycle ;
                     Context.toast_error
                       (Printf.sprintf "Payout failed: %s" msg))))
  | _ -> Context.toast_error "Cannot resolve baker service"

(** Given a column position in the tab bar (1-indexed), return the tab index.
    Each tab has width [String.length label + 4] (for padding/brackets). *)
let tab_at_col col =
  let rec find idx pos = function
    | [] -> None
    | tab :: rest ->
        let label = Rewards_state.tab_label tab in
        let w = String.length label + 4 in
        if col >= pos && col < pos + w then Some idx
        else find (idx + 1) (pos + w) rest
  in
  find 0 1 Rewards_state.all_tabs

let handle_key ps key ~size:_ =
  let s = ps.Navigation.s in
  match Keys.of_string key with
  | Some Keys.Escape -> back ps
  | Some (Keys.Char "1") ->
      Navigation.update
        (fun s -> {s with active_tab = Rewards_state.Overview})
        ps
  | Some (Keys.Char "2") ->
      Navigation.update
        (fun s -> {s with active_tab = Rewards_state.Delegators})
        ps
  | Some (Keys.Char "3") ->
      Navigation.update
        (fun s -> {s with active_tab = Rewards_state.History})
        ps
  | Some (Keys.Char "4") ->
      Navigation.update
        (fun s -> {s with active_tab = Rewards_state.Configuration})
        ps
  | Some (Keys.Char "b") ->
      (* Cycle baker selection *)
      let n = List.length s.baker_instances in
      if n > 1 then
        Navigation.update
          (fun s -> {s with selected_baker = (s.selected_baker + 1) mod n})
          ps
      else ps
  | Some (Keys.Char "r") -> refresh ps
  | _ -> ps

let keymap _ps =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "1-4" "Switch tab"; kb "b" "Baker"; kb "r" "Refresh"; kb "Esc" "Back"]

let handled_keys () =
  Keys.[Escape; Char "1"; Char "2"; Char "3"; Char "4"; Char "b"; Char "r"]

let has_modal _ = false

let handle_modal_key ps _ ~size:_ = ps

module Page : Miaou.Core.Tui_page.PAGE_SIG = struct
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

  let keymap = keymap

  let handled_keys = handled_keys

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let on_key ps key ~size =
    let ps' = handle_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let key_hints _ps =
    Miaou.Core.Tui_page.
      [
        {key = "1-4"; help = "Tab"};
        {key = "b"; help = "Baker"};
        {key = "r"; help = "Refresh"};
        {key = "Esc"; help = "Back"};
      ]

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
