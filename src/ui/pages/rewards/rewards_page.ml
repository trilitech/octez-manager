(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Rewards page: TUI dashboard for reward distribution. *)

open Octez_manager_lib
open Octez_manager_rewards
module Widgets = Miaou_widgets_display.Widgets
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation

let name = "rewards"

open Rewards_state

type msg = unit

type state = Rewards_state.state

type pstate = state Navigation.t

(* Pending cycle selection from modal callback.
   Written by on_select, consumed by refresh. *)
let pending_cycle : int option ref = ref None

(* Load baker instances from service registry.
   Prefer the auto-detected baker address from the scheduler cache,
   falling back to the first delegate if not yet detected.
   Also includes test bakers from OM_TEST_BAKER env var. *)
let load_baker_instances () =
  let from_services =
    let bakers =
      Data.load_service_states ()
      |> List.filter (fun (st : Data.Service_state.t) ->
          st.service.Service.role = "baker")
    in
    List.filter_map
      (fun (st : Data.Service_state.t) ->
        let instance = st.service.Service.instance in
        match Rewards_scheduler.get_baker_for_instance ~instance with
        | Some pkh -> Some (instance, pkh)
        | None ->
            let delegates = Delegate_scheduler.get_baker_delegates ~instance in
            List.nth_opt delegates 0 |> Option.map (fun pkh -> (instance, pkh)))
      bakers
  in
  let from_env =
    match Sys.getenv_opt "OM_TEST_BAKER" with
    | None | Some "" -> []
    | Some s ->
        String.split_on_char ',' s
        |> List.filter_map (fun entry ->
            let entry = String.trim entry in
            match String.index_opt entry '/' with
            | None -> None
            | Some i ->
                let network = String.sub entry 0 i in
                let pkh =
                  String.sub entry (i + 1) (String.length entry - i - 1)
                in
                if String.length network > 0 && String.length pkh > 0 then
                  let instance = Printf.sprintf "test-%s" network in
                  Some (instance, pkh)
                else None)
  in
  let from_custom =
    Custom_baker_registry.list ()
    |> List.map (fun (e : Custom_baker_registry.entry) ->
        (e.instance, e.baker_pkh))
  in
  (* De-duplicate by instance handle: services and OM_TEST_BAKER take
     precedence; custom entries with a colliding instance are silently dropped. *)
  let existing_instances =
    List.map fst (from_services @ from_env) |> List.sort_uniq String.compare
  in
  let from_custom =
    List.filter
      (fun (inst, _) ->
        not (List.exists (fun ei -> String.equal ei inst) existing_instances))
      from_custom
  in
  from_services @ from_env @ from_custom

let config_exists_for_selected baker_instances selected_baker =
  match List.nth_opt baker_instances selected_baker with
  | Some (instance, _) -> Payout_config.exists ~instance
  | None -> false

let init () =
  let baker_instances = load_baker_instances () in
  let active_tab =
    match Context.take_pending_rewards_tab () with
    | Some "configuration" -> Rewards_state.Configuration
    | Some "delegators" -> Rewards_state.Delegators
    | Some "history" -> Rewards_state.History
    | _ -> Rewards_state.Overview
  in
  let selected_baker =
    match Context.take_pending_baker_instance () with
    | Some pending_instance ->
        List.find_index
          (fun (inst, _) -> String.equal inst pending_instance)
          baker_instances
        |> Option.value ~default:0
    | None -> 0
  in
  let config_exists =
    config_exists_for_selected baker_instances selected_baker
  in
  Navigation.make
    {
      Rewards_state.baker_instances;
      selected_baker;
      active_tab;
      selected_cycle = None;
      current_cycle = None;
      delegator_cursor = 0;
      delegator_sort = Rewards_state.SortBalance;
      delegator_filter = Rewards_state.FilterAll;
      search_query = "";
      search_active = false;
      blueprint = None;
      overview_preview = false;
      config = None;
      config_cursor = 0;
      config_dirty = false;
      config_exists;
      history_cursor = 0;
      loading = false;
      error = None;
    }

let update ps _ = ps

(** Compute a payout blueprint for the delegators tab if needed.
    Only runs when the tab is active and cached data is available.
    Uses the loaded config if available, otherwise default config. *)
let needs_blueprint s =
  s.active_tab = Rewards_state.Delegators
  || (s.active_tab = Rewards_state.Overview && s.overview_preview)

let maybe_compute_blueprint s =
  if not (needs_blueprint s) then s
  else
    match Rewards_state.selected_baker_instance s with
    | None -> {s with blueprint = None}
    | Some (instance, pkh) -> (
        let cycle_opt =
          match s.selected_cycle with
          | Some c -> Some c
          | None -> (
              match Rewards_scheduler.get_recent_cycles ~instance with
              | cr :: _ -> Some cr.Rewards.cycle
              | [] -> None)
        in
        match cycle_opt with
        | None -> {s with blueprint = None}
        | Some cycle -> (
            let cached = Rewards_scheduler.get_cycle_data ~instance ~cycle in
            let stale_blueprint =
              match (s.blueprint, cached) with
              | Some bp, Some cr
                when bp.Rewards.cycle = cycle
                     && bp.Rewards.delegator_rewards = []
                     && cr.Rewards.delegators <> [] ->
                  true
              | _ -> false
            in
            match s.blueprint with
            | Some bp when bp.Rewards.cycle = cycle && not stale_blueprint -> s
            | _ -> (
                match cached with
                | None -> {s with blueprint = None}
                | Some cr ->
                    let network =
                      match Service_registry.find ~instance with
                      | Ok (Some svc) -> svc.Service.network
                      | _ -> "unknown"
                    in
                    let config =
                      match s.config with
                      | Some c -> c
                      | None -> Payout_config.default ~baker_pkh:pkh
                    in
                    let bp =
                      Reward_calculator.generate_blueprint
                        ~config
                        ~network
                        ~cycle_rewards:cr
                    in
                    {s with blueprint = Some bp; selected_cycle = Some cycle})))

(** Load payout config from disk when first viewing the Configuration tab.
    Only loads once; subsequent changes are in-memory until saved. *)
let maybe_load_config s =
  if s.active_tab <> Rewards_state.Configuration then s
  else if Option.is_some s.config then s
  else
    match Rewards_state.selected_baker_instance s with
    | None -> s
    | Some (instance, pkh) ->
        let config =
          match Payout_config.load ~instance with
          | Ok c -> c
          | Error _ -> Payout_config.default ~baker_pkh:pkh
        in
        {s with config = Some config}

let apply_pending_config s =
  match Rewards_config_tab.consume_pending_config () with
  | Some config -> {s with config = Some config; config_dirty = true}
  | None -> (
      match Rewards_config_tab.consume_pending_config_clean () with
      | Some config -> {s with config = Some config}
      | None -> s)

let refresh ps =
  match Context.consume_navigation () with
  | Some (Context.Goto page) -> Navigation.goto page ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      Navigation.update
        (fun s ->
          let baker_instances = load_baker_instances () in
          let current_cycle =
            match Rewards_state.selected_baker_instance s with
            | Some (instance, _) ->
                Rewards_scheduler.get_current_cycle ~instance
            | None -> None
          in
          let config_exists =
            config_exists_for_selected baker_instances s.selected_baker
          in
          let s = {s with baker_instances; current_cycle; config_exists} in
          let s = maybe_compute_blueprint s in
          let s = maybe_load_config s in
          let s = apply_pending_config s in
          match !pending_cycle with
          | Some c ->
              pending_cycle := None ;
              {
                s with
                selected_cycle = Some c;
                blueprint = None;
                delegator_cursor = 0;
              }
          | None -> s)
        ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps =
  Context.set_pending_tab Context.Tab_instances ;
  Navigation.back ps

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

(** Render baker selector radio row (only when multiple bakers exist). *)
let render_baker_selector s =
  let n = List.length s.baker_instances in
  if n < 2 then None
  else
    let box selected label =
      let tick = if selected then "◉" else "○" in
      tick ^ " " ^ label
    in
    let options =
      List.mapi
        (fun i (instance, _pkh) -> box (i = s.selected_baker) instance)
        s.baker_instances
    in
    Some
      (Widgets.title_highlight "Baker"
      ^ ":" ^ "  "
      ^ String.concat "   " options
      ^ Widgets.themed_muted "  [b: switch baker]")

let hint_for_tab _tab = ""

let keymap ps =
  let s = ps.Navigation.s in
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  let common =
    let base = [kb "Tab" "Next tab"; kb "Esc" "Back"] in
    if List.length s.baker_instances > 1 then kb "b" "Baker" :: base else base
  in
  let tab_keys =
    match s.active_tab with
    | Rewards_state.Overview ->
        if s.config_exists then
          [
            kb "g" "Generate";
            kb "p" "Pay";
            kb "d" "Dry-run";
            kb "t" "Continual";
            kb "r" "Refresh";
          ]
        else [kb "r" "Refresh"]
    | Rewards_state.Delegators ->
        [
          kb "j/k" "Navigate";
          kb "/" "Search";
          kb "s" "Sort";
          kb "f" "Filter";
          kb "c" "Cycle";
        ]
    | Rewards_state.History -> [kb "j/k" "Navigate"; kb "Enter" "View"]
    | Rewards_state.Configuration ->
        let save_key =
          if s.config_exists then kb "s" "Save" else kb "c" "Create"
        in
        [
          kb "j/k" "Navigate";
          kb "Enter" "Edit";
          save_key;
          kb "r" "Reset";
          kb "i" "Import";
          kb "n" "Notify";
        ]
  in
  tab_keys @ common

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  (* Register keymap for help modal *)
  let keymap_pairs =
    List.map
      (fun (kb : state Miaou.Core.Tui_page.key_binding_desc) ->
        (kb.key, kb.help))
      (keymap ps)
  in
  Context.register_active_page_keymap (fun () -> keymap_pairs) ;
  let cols = size.LTerm_geom.cols in
  Context.tick_spinner () ;
  Context.tick_toasts () ;
  let header_line = render_baker_header s in
  let baker_selector = render_baker_selector s in
  let tab_bar = render_tab_bar s ~cols in
  let hint = hint_for_tab s.active_tab in
  let toast = Context.render_toasts ~cols in
  let footer = if String.length toast > 0 then [toast; hint] else [hint] in
  let header =
    match baker_selector with
    | None -> [header_line; tab_bar; ""]
    | Some selector -> [header_line; selector; tab_bar; ""]
  in
  Themed_page.render_layout ~size ~header ~footer ~child:(fun avail ->
      let cols = avail.LTerm_geom.cols in
      let rows = avail.LTerm_geom.rows in
      match s.active_tab with
      | Rewards_state.Overview -> Rewards_overview.render ~state:s ~cols
      | Rewards_state.Delegators ->
          Rewards_delegators.render ~state:s ~cols ~rows
      | Rewards_state.History -> Rewards_history.render ~state:s ~cols ~rows
      | Rewards_state.Configuration ->
          Rewards_config_tab.render ~state:s ~cols ~_rows:rows)

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
          let recent = Rewards_scheduler.get_recent_cycles ~instance in
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
              min (s.config_cursor + 1) Rewards_config_tab.field_count;
          })
        ps
  | Some (Keys.Char "k") | Some Keys.Up ->
      Navigation.update
        (fun s -> {s with config_cursor = max (s.config_cursor - 1) 0})
        ps
  | Some Keys.Enter -> (
      if s.config_cursor = Rewards_config_tab.field_count then (
        (* Payout service action *)
        match Rewards_state.selected_instance_name s with
        | None -> ps
        | Some instance ->
            Rewards_config_tab.open_payout_service_actions
              ~instance
              ~baker_pkh:
                (match Rewards_state.selected_baker_pkh s with
                | Some p -> p
                | None -> "")
              ~config:s.config ;
            ps)
      else
        match s.config with
        | Some config ->
            let field =
              List.nth Rewards_config_tab.all_fields s.config_cursor
            in
            let network =
              match Rewards_state.selected_instance_name s with
              | None -> None
              | Some instance -> (
                  match Service_registry.find ~instance with
                  | Ok (Some svc) -> Some svc.Service.network
                  | _ -> None)
            in
            Rewards_config_tab.edit_field ?network config field ;
            ps
        | None -> ps)
  | Some (Keys.Char "s") | Some (Keys.Char "c") -> (
      match (s.config, Rewards_state.selected_instance_name s) with
      | Some config, Some instance ->
          Rewards_config_tab.save_config ~instance config ;
          Navigation.update
            (fun s ->
              {
                s with
                config_dirty = false;
                config_exists = Payout_config.exists ~instance;
              })
            ps
      | _ -> ps)
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
    match Rewards_state.selected_baker_instance s with
    | None -> 0
    | Some (instance, _) ->
        Rewards_history.cycle_count
          (Rewards_scheduler.get_recent_cycles ~instance)
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
      match Rewards_state.selected_baker_instance s with
      | None -> ps
      | Some (instance, baker) -> (
          let recent = Rewards_scheduler.get_recent_cycles ~instance in
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
        |> Option.value ~default:(Rpc_addr.to_endpoint svc.Service.rpc_addr)
      in
      let config =
        match Payout_config.load ~instance with
        | Ok c -> c
        | Error _ -> Payout_config.default ~baker_pkh:pkh
      in
      let base_dir =
        match Node_env.read ~inst:instance with
        | Error _ -> None
        | Ok pairs -> List.assoc_opt "OCTEZ_BAKER_BASE_DIR" pairs
      in
      let ctx : Payout_executor.context =
        {
          octez_client_bin;
          endpoint = node_endpoint;
          base_dir;
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

(** Given a column position in the baker selector row (1-indexed), return the baker index.
    Format: "Baker:  ◉ name1   ○ name2   [b: switch baker]"
    The prefix "Baker:  " is 8 chars. Each option is "◉ name   " or "○ name   ". *)
let baker_at_col s col =
  let n = List.length s.baker_instances in
  if n < 2 then None
  else
    let prefix_len = 8 in
    (* "Baker:  " *)
    if col < prefix_len then None
    else
      let rec find idx pos = function
        | [] -> None
        | (instance, _pkh) :: rest ->
            let tick_len = 2 in
            (* "◉ " or "○ " *)
            let name_len = String.length instance in
            let spacing_len = 3 in
            (* "   " between options *)
            let option_len = tick_len + name_len + spacing_len in
            if col >= pos && col < pos + tick_len + name_len then Some idx
            else find (idx + 1) (pos + option_len) rest
      in
      find 0 prefix_len s.baker_instances

let handle_key ps key ~size:_ =
  Metrics.mark_input_event () ;
  let s = ps.Navigation.s in
  (* Search mode captures all input *)
  if s.search_active then handle_search_key ps key
  else
    (* Check for mouse clicks on baker selector (row 2) or tab bar (row 2 or 3 depending on baker count) *)
    match Miaou_helpers.Mouse.parse_click key with
    | Some {row = 2; col} when List.length s.baker_instances > 1 -> (
        (* Row 2 is baker selector when multiple bakers exist *)
        match baker_at_col s col with
        | Some idx ->
            Navigation.update
              (fun s ->
                {
                  s with
                  selected_baker = idx;
                  blueprint = None;
                  overview_preview = false;
                  config = None;
                  config_dirty = false;
                  selected_cycle = None;
                  delegator_cursor = 0;
                })
              ps
        | None -> ps)
    | Some {row; col}
      when row = if List.length s.baker_instances > 1 then 3 else 2 -> (
        (* Tab bar is on row 3 if baker selector present, otherwise row 2 *)
        match tab_at_col col with
        | Some idx ->
            let tab = Rewards_state.tab_of_index idx in
            Navigation.update (fun s -> {s with active_tab = tab}) ps
        | None -> ps)
    | Some _ -> ps (* other mouse clicks: ignore *)
    | None -> (
        match Keys.of_string key with
        | Some Keys.Escape ->
            if Option.is_some s.selected_cycle then
              Navigation.update
                (fun s ->
                  {
                    s with
                    selected_cycle = None;
                    active_tab = Rewards_state.History;
                    blueprint = None;
                    overview_preview = false;
                  })
                ps
            else back ps
        | Some Keys.Tab ->
            Navigation.update
              (fun s ->
                let next_idx =
                  (Rewards_state.tab_index s.active_tab + 1) mod 4
                in
                let next = Rewards_state.tab_of_index next_idx in
                if next_idx = 0 then
                  (* Wrapping from Configuration back to Overview: reset view state *)
                  {
                    s with
                    active_tab = next;
                    selected_cycle = None;
                    blueprint = None;
                    overview_preview = false;
                    delegator_cursor = 0;
                    history_cursor = 0;
                  }
                else {s with active_tab = next})
              ps
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
            (* Cycle baker selection, reset blueprint and config *)
            let n = List.length s.baker_instances in
            if n > 1 then
              Navigation.update
                (fun s ->
                  {
                    s with
                    selected_baker = (s.selected_baker + 1) mod n;
                    blueprint = None;
                    overview_preview = false;
                    config = None;
                    config_dirty = false;
                    selected_cycle = None;
                    delegator_cursor = 0;
                  })
                ps
            else ps
        | Some (Keys.Char "p") when s.active_tab = Rewards_state.Overview -> (
            (* Trigger payout confirmation *)
            let s = ps.Navigation.s in
            match Rewards_state.selected_baker_instance s with
            | None -> ps
            | Some (instance, pkh) -> (
                let cycle_opt =
                  match s.selected_cycle with
                  | Some c -> Some c
                  | None -> (
                      match Rewards_scheduler.get_recent_cycles ~instance with
                      | cr :: _ -> Some cr.Rewards.cycle
                      | [] -> None)
                in
                match cycle_opt with
                | None ->
                    Context.toast_warn "No cycle data available" ;
                    ps
                | Some cycle ->
                    if Payout_blueprint.is_already_paid ~instance ~cycle then begin
                      Context.toast_warn
                        (Printf.sprintf "Cycle %d already paid" cycle) ;
                      ps
                    end
                    else
                      let network =
                        match Service_registry.find ~instance with
                        | Ok (Some svc) -> svc.Service.network
                        | _ -> "unknown"
                      in
                      Modal_helpers.open_choice_modal
                        ~title:
                          (Printf.sprintf
                             "Pay Cycle %d — %s (%s)"
                             cycle
                             instance
                             network)
                        ~items:["Execute payout"; "Dry-run only"; "Cancel"]
                        ~to_string:Fun.id
                        ~on_select:(fun choice ->
                          let dry_run = String.equal choice "Dry-run only" in
                          if
                            String.equal choice "Execute payout"
                            || String.equal choice "Dry-run only"
                          then
                            run_payout_in_background
                              ~instance
                              ~pkh
                              ~network
                              ~cycle
                              ~dry_run)
                        () ;
                      ps))
        | Some (Keys.Char "d") when s.active_tab = Rewards_state.Overview -> (
            (* Direct dry-run without modal *)
            let s = ps.Navigation.s in
            match Rewards_state.selected_baker_instance s with
            | None -> ps
            | Some (instance, pkh) -> (
                let cycle_opt =
                  match s.selected_cycle with
                  | Some c -> Some c
                  | None -> (
                      match Rewards_scheduler.get_recent_cycles ~instance with
                      | cr :: _ -> Some cr.Rewards.cycle
                      | [] -> None)
                in
                match cycle_opt with
                | None ->
                    Context.toast_warn "No cycle data available" ;
                    ps
                | Some cycle ->
                    let network =
                      match Service_registry.find ~instance with
                      | Ok (Some svc) -> svc.Service.network
                      | _ -> "unknown"
                    in
                    run_payout_in_background
                      ~instance
                      ~pkh
                      ~network
                      ~cycle
                      ~dry_run:true ;
                    ps))
        | Some (Keys.Char "t") when s.active_tab = Rewards_state.Overview -> (
            (* Toggle continual mode *)
            match Rewards_state.selected_baker_instance s with
            | None -> ps
            | Some (instance, pkh) ->
                let currently_active =
                  Systemd.is_payout_timer_active ~instance
                in
                if currently_active then (
                  (* Disable timer *)
                  (match Systemd.disable_payout_timer ~instance with
                  | Ok () -> ()
                  | Error (`Msg msg) ->
                      Context.toast_warn
                        (Printf.sprintf "Failed to disable timer: %s" msg)) ;
                  let config =
                    match Payout_config.load ~instance with
                    | Ok c -> c
                    | Error _ -> Payout_config.default ~baker_pkh:pkh
                  in
                  let config =
                    {config with Payout_config.continual_enabled = false}
                  in
                  ignore (Payout_config.save ~instance config) ;
                  Context.toast_info
                    (Printf.sprintf "Continual mode disabled for %s" instance) ;
                  ps)
                else (
                  (* Enable timer - need to set up systemd units first *)
                  Context.toast_warn
                    "Use CLI 'rewards continual start' to enable continual mode" ;
                  ps))
        | Some (Keys.Char "g") when s.active_tab = Rewards_state.Overview -> (
            (* Generate payout preview on Overview tab *)
            let s = ps.Navigation.s in
            match Rewards_state.selected_baker_instance s with
            | None -> ps
            | Some (instance, _) -> (
                (* Check double-payment prevention *)
                let cycle_opt =
                  match s.selected_cycle with
                  | Some c -> Some c
                  | None -> s.current_cycle
                in
                match cycle_opt with
                | None ->
                    Context.toast_warn "No cycle data available" ;
                    ps
                | Some cycle ->
                    if Payout_blueprint.is_already_paid ~instance ~cycle then begin
                      Context.toast_warn
                        (Printf.sprintf "Cycle %d already paid" cycle) ;
                      ps
                    end
                    else
                      Navigation.update
                        (fun s ->
                          {s with overview_preview = true; blueprint = None})
                        ps))
        | Some (Keys.Char "r") when s.active_tab <> Rewards_state.Configuration
          ->
            refresh ps
        | _ when s.active_tab = Rewards_state.Delegators ->
            handle_delegator_key ps key
        | _ when s.active_tab = Rewards_state.Configuration ->
            handle_config_key ps key
        | _ when s.active_tab = Rewards_state.History ->
            handle_history_key ps key
        | _ -> ps)

let handled_keys () =
  Keys.
    [
      Escape;
      Enter;
      Backspace;
      Tab;
      Up;
      Down;
      Char "1";
      Char "2";
      Char "3";
      Char "4";
      Char "b";
      Char "r";
      Char "j";
      Char "k";
      Char "g";
      Char "G";
      Char "/";
      Char "s";
      Char "f";
      Char "c";
      Char "g";
      Char "p";
      Char "d";
      Char "t";
      Char "i";
      Char "n";
      Char "c";
    ]

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

  let key_hints ps =
    let s = ps.Navigation.s in
    let kh key help = Miaou.Core.Tui_page.{key; help} in
    let common =
      let base = [kh "Tab" "Next tab"; kh "Esc" "Back"] in
      if List.length s.baker_instances > 1 then kh "b" "Baker" :: base else base
    in
    let tab_keys =
      match s.active_tab with
      | Rewards_state.Overview ->
          if s.config_exists then
            [
              kh "g" "Generate";
              kh "p" "Pay";
              kh "d" "Dry-run";
              kh "t" "Continual";
              kh "r" "Refresh";
            ]
          else [kh "r" "Refresh"]
      | Rewards_state.Delegators ->
          [
            kh "j/k" "Navigate";
            kh "/" "Search";
            kh "s" "Sort";
            kh "f" "Filter";
            kh "c" "Cycle";
          ]
      | Rewards_state.History -> [kh "j/k" "Navigate"; kh "Enter" "View"]
      | Rewards_state.Configuration ->
          let save_hint =
            if s.config_exists then kh "s" "Save" else kh "c" "Create"
          in
          [
            kh "j/k" "Navigate";
            kh "Enter" "Edit";
            save_hint;
            kh "r" "Reset";
            kh "i" "Import";
            kh "n" "Notify";
          ]
    in
    tab_keys @ common

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
