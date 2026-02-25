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

(* Load baker instances from service registry *)
let load_baker_instances () =
  let bakers =
    Data.load_service_states ()
    |> List.filter (fun (st : Data.Service_state.t) ->
        st.service.Service.role = "baker")
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
      overview_preview = false;
      config = None;
      config_cursor = 0;
      config_dirty = false;
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
        let baker = pkh in
        let cycle_opt =
          match s.selected_cycle with
          | Some c -> Some c
          | None -> (
              match Rewards_scheduler.get_recent_cycles ~baker with
              | cr :: _ -> Some cr.Rewards.cycle
              | [] -> None)
        in
        match cycle_opt with
        | None -> {s with blueprint = None}
        | Some cycle -> (
            match s.blueprint with
            | Some bp when bp.Rewards.cycle = cycle -> s
            | _ -> (
                match Rewards_scheduler.get_cycle_data ~baker ~cycle with
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

(** Apply pending config edits from modal callbacks. *)
let apply_pending_config s =
  match Rewards_config_tab.consume_pending_config () with
  | Some config -> {s with config = Some config; config_dirty = true}
  | None -> s

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
          let s = {s with baker_instances; current_cycle} in
          let s = maybe_compute_blueprint s in
          let s = maybe_load_config s in
          apply_pending_config s)
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

let hint_for_tab = function
  | Rewards_state.Delegators ->
      Widgets.themed_muted
        "j/k nav \xc2\xb7 / search \xc2\xb7 s sort \xc2\xb7 f filter \xc2\xb7 \
         c cycle \xc2\xb7 1-4 tabs \xc2\xb7 Esc back"
  | Rewards_state.Configuration ->
      Widgets.themed_muted
        "j/k nav \xc2\xb7 Enter edit \xc2\xb7 s save \xc2\xb7 r reset \xc2\xb7 \
         1-4 tabs \xc2\xb7 Esc back"
  | Rewards_state.Overview ->
      Widgets.themed_muted
        "g generate \xc2\xb7 1-4 tabs \xc2\xb7 b baker \xc2\xb7 r refresh \
         \xc2\xb7 Esc back"
  | Rewards_state.History ->
      Widgets.themed_muted
        "j/k nav \xc2\xb7 Enter view cycle \xc2\xb7 1-4 tabs \xc2\xb7 Esc back"

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let cols = size.LTerm_geom.cols in
  let header_line = render_baker_header s in
  let tab_bar = render_tab_bar s ~cols in
  let hint = hint_for_tab s.active_tab in
  Themed_page.render_layout
    ~size
    ~header:[header_line; tab_bar; ""]
    ~footer:[hint]
    ~child:(fun avail ->
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
      (* Cycle through recent cycles *)
      match Rewards_state.selected_baker_pkh s with
      | None -> ps
      | Some baker -> (
          let recent = Rewards_scheduler.get_recent_cycles ~baker in
          let cycles =
            List.map (fun (cr : Rewards.cycle_rewards) -> cr.cycle) recent
          in
          match cycles with
          | [] -> ps
          | _ ->
              let current =
                match s.selected_cycle with
                | Some c -> c
                | None -> List.hd cycles
              in
              let rec find_next = function
                | [] -> List.hd cycles
                | [_] -> List.hd cycles
                | x :: y :: _ when x = current -> y
                | _ :: rest -> find_next rest
              in
              let next_cycle = find_next cycles in
              Navigation.update
                (fun s ->
                  {
                    s with
                    selected_cycle = Some next_cycle;
                    blueprint = None;
                    delegator_cursor = 0;
                  })
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
      | Some config ->
          let field = List.nth Rewards_config_tab.all_fields s.config_cursor in
          Rewards_config_tab.edit_field config field ;
          ps
      | None -> ps)
  | Some (Keys.Char "s") -> (
      match (s.config, Rewards_state.selected_instance_name s) with
      | Some config, Some instance ->
          Rewards_config_tab.save_config ~instance config ;
          Navigation.update (fun s -> {s with config_dirty = false}) ps
      | _ -> ps)
  | Some (Keys.Char "r") -> (
      match Rewards_state.selected_baker_pkh s with
      | Some baker_pkh ->
          Rewards_config_tab.reset_config ~baker_pkh ;
          ps
      | None -> ps)
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

let handle_key ps key ~size:_ =
  let s = ps.Navigation.s in
  (* Search mode captures all input *)
  if s.search_active then handle_search_key ps key
  else
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
    | Some (Keys.Char "r") when s.active_tab <> Rewards_state.Configuration ->
        refresh ps
    | _ when s.active_tab = Rewards_state.Delegators ->
        handle_delegator_key ps key
    | _ when s.active_tab = Rewards_state.Configuration ->
        handle_config_key ps key
    | _ when s.active_tab = Rewards_state.History -> handle_history_key ps key
    | _ -> ps

let keymap _ps =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [
    kb "1-4" "Switch tab";
    kb "b" "Baker";
    kb "r" "Refresh";
    kb "j/k" "Navigate";
    kb "/" "Search";
    kb "s" "Sort";
    kb "f" "Filter";
    kb "c" "Cycle";
    kb "Esc" "Back";
  ]

let handled_keys () =
  Keys.
    [
      Escape;
      Enter;
      Backspace;
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

  let key_hints _ps =
    Miaou.Core.Tui_page.
      [
        {key = "1-4"; help = "Tab"};
        {key = "b"; help = "Baker"};
        {key = "r"; help = "Refresh"};
        {key = "j/k"; help = "Navigate"};
        {key = "/"; help = "Search"};
        {key = "s"; help = "Sort"};
        {key = "f"; help = "Filter"};
        {key = "c"; help = "Cycle"};
        {key = "Esc"; help = "Back"};
      ]

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
