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
