(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Binaries management page.

    State manipulation, key handling, PAGE_SIG implementation.
    Data loading, actions, and rendering are delegated to
    {!Binaries_data}, {!Binaries_actions}, and {!Binaries_view}. *)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
include Binaries_types

let name = "binaries"

let init () =
  let managed_octez = Binaries_data.load_managed_octez_versions () in
  let managed_signatory = Binaries_data.load_managed_signatory_versions () in
  let registered = Binaries_data.load_registered_dirs () in
  let available_octez = Binaries_data.load_available_octez_versions () in
  let available_signatory =
    Binaries_data.load_available_signatory_versions ()
  in
  let expanded_managed_octez = true in
  let expanded_managed_signatory = true in
  let expanded_available_octez = true in
  let expanded_available_signatory = true in
  let expanded_octez_majors = [] in
  let expanded_managed_octez_items = [] in
  let expanded_registered = [] in
  let items =
    Binaries_data.build_items
      managed_octez
      managed_signatory
      registered
      available_octez
      available_signatory
      ~expanded_managed_octez
      ~expanded_managed_signatory
      ~expanded_available_octez
      ~expanded_available_signatory
      ~expanded_octez_majors
  in
  Navigation.make
    {
      managed_octez_versions = managed_octez;
      managed_signatory_versions = managed_signatory;
      registered_dirs = registered;
      available_octez_versions = available_octez;
      available_signatory_versions = available_signatory;
      items;
      selected = 0;
      loading_remote = false;
      expanded_managed_octez;
      expanded_managed_signatory;
      expanded_available_octez;
      expanded_available_signatory;
      expanded_octez_majors;
      expanded_managed_octez_items;
      expanded_registered;
    }

let update ps _ = ps

let refresh_data s =
  let managed_octez = Binaries_data.load_managed_octez_versions () in
  let managed_signatory = Binaries_data.load_managed_signatory_versions () in
  let registered = Binaries_data.load_registered_dirs () in
  let available_octez = Binaries_data.load_available_octez_versions () in
  let available_signatory =
    Binaries_data.load_available_signatory_versions ()
  in
  let items =
    Binaries_data.build_items
      managed_octez
      managed_signatory
      registered
      available_octez
      available_signatory
      ~expanded_managed_octez:s.expanded_managed_octez
      ~expanded_managed_signatory:s.expanded_managed_signatory
      ~expanded_available_octez:s.expanded_available_octez
      ~expanded_available_signatory:s.expanded_available_signatory
      ~expanded_octez_majors:s.expanded_octez_majors
  in
  {
    managed_octez_versions = managed_octez;
    managed_signatory_versions = managed_signatory;
    registered_dirs = registered;
    available_octez_versions = available_octez;
    available_signatory_versions = available_signatory;
    items;
    selected = s.selected;
    loading_remote = false;
    expanded_managed_octez = s.expanded_managed_octez;
    expanded_managed_signatory = s.expanded_managed_signatory;
    expanded_available_octez = s.expanded_available_octez;
    expanded_available_signatory = s.expanded_available_signatory;
    expanded_octez_majors = s.expanded_octez_majors;
    expanded_managed_octez_items = s.expanded_managed_octez_items;
    expanded_registered = s.expanded_registered;
  }

let refresh ps = Navigation.update refresh_data ps

let auto_refresh ps =
  (* Auto-refresh if data has been marked dirty (e.g., after remove/download) *)
  if Context.consume_instances_dirty () then refresh ps else ps

let toggle_major_expansion s major =
  let expanded_octez_majors =
    if List.mem major s.expanded_octez_majors then
      List.filter (( <> ) major) s.expanded_octez_majors
    else major :: s.expanded_octez_majors
  in
  (* Rebuild items with new expansion state *)
  let items =
    Binaries_data.build_items
      s.managed_octez_versions
      s.managed_signatory_versions
      s.registered_dirs
      s.available_octez_versions
      s.available_signatory_versions
      ~expanded_managed_octez:s.expanded_managed_octez
      ~expanded_managed_signatory:s.expanded_managed_signatory
      ~expanded_available_octez:s.expanded_available_octez
      ~expanded_available_signatory:s.expanded_available_signatory
      ~expanded_octez_majors
  in
  {s with expanded_octez_majors; items}

let toggle_managed_expansion s version =
  let expanded_managed_octez_items =
    if List.mem version s.expanded_managed_octez_items then
      List.filter (( <> ) version) s.expanded_managed_octez_items
    else version :: s.expanded_managed_octez_items
  in
  {s with expanded_managed_octez_items}

let toggle_registered_expansion s alias =
  let expanded_registered =
    if List.mem alias s.expanded_registered then
      List.filter (( <> ) alias) s.expanded_registered
    else alias :: s.expanded_registered
  in
  {s with expanded_registered}

let toggle_managed_group s kind =
  match kind with
  | Octez ->
      let expanded = not s.expanded_managed_octez in
      let items =
        Binaries_data.build_items
          s.managed_octez_versions
          s.managed_signatory_versions
          s.registered_dirs
          s.available_octez_versions
          s.available_signatory_versions
          ~expanded_managed_octez:expanded
          ~expanded_managed_signatory:s.expanded_managed_signatory
          ~expanded_available_octez:s.expanded_available_octez
          ~expanded_available_signatory:s.expanded_available_signatory
          ~expanded_octez_majors:s.expanded_octez_majors
      in
      {s with expanded_managed_octez = expanded; items}
  | Signatory ->
      let expanded = not s.expanded_managed_signatory in
      let items =
        Binaries_data.build_items
          s.managed_octez_versions
          s.managed_signatory_versions
          s.registered_dirs
          s.available_octez_versions
          s.available_signatory_versions
          ~expanded_managed_octez:s.expanded_managed_octez
          ~expanded_managed_signatory:expanded
          ~expanded_available_octez:s.expanded_available_octez
          ~expanded_available_signatory:s.expanded_available_signatory
          ~expanded_octez_majors:s.expanded_octez_majors
      in
      {s with expanded_managed_signatory = expanded; items}

let toggle_available_group s kind =
  match kind with
  | Octez ->
      let expanded = not s.expanded_available_octez in
      let items =
        Binaries_data.build_items
          s.managed_octez_versions
          s.managed_signatory_versions
          s.registered_dirs
          s.available_octez_versions
          s.available_signatory_versions
          ~expanded_managed_octez:s.expanded_managed_octez
          ~expanded_managed_signatory:s.expanded_managed_signatory
          ~expanded_available_octez:expanded
          ~expanded_available_signatory:s.expanded_available_signatory
          ~expanded_octez_majors:s.expanded_octez_majors
      in
      {s with expanded_available_octez = expanded; items}
  | Signatory ->
      let expanded = not s.expanded_available_signatory in
      let items =
        Binaries_data.build_items
          s.managed_octez_versions
          s.managed_signatory_versions
          s.registered_dirs
          s.available_octez_versions
          s.available_signatory_versions
          ~expanded_managed_octez:s.expanded_managed_octez
          ~expanded_managed_signatory:s.expanded_managed_signatory
          ~expanded_available_octez:s.expanded_available_octez
          ~expanded_available_signatory:expanded
          ~expanded_octez_majors:s.expanded_octez_majors
      in
      {s with expanded_available_signatory = expanded; items}

let move_up s =
  let selected = if s.selected > 0 then s.selected - 1 else s.selected in
  {s with selected}

let move_down s =
  let max_idx = List.length s.items - 1 in
  let selected = if s.selected < max_idx then s.selected + 1 else s.selected in
  {s with selected}

let move_selection ps dir =
  match dir with
  | `Up -> Navigation.update move_up ps
  | `Down -> Navigation.update move_down ps
  | _ -> ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps = Navigation.back ps

(** Key handling *)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some Keys.Escape -> back ps
    | Some (Keys.Char "r") -> refresh ps
    | Some (Keys.Char "d") ->
        Navigation.update
          (fun s ->
            if s.available_octez_versions <> [] then
              Binaries_actions.download_octez_version
                (List.hd s.available_octez_versions) ;
            s)
          ps
    | Some (Keys.Char "l") ->
        Binaries_actions.register_directory () ;
        ps
    | Some (Keys.Char "p") -> Navigation.update Binaries_actions.prune_unused ps
    | Some Keys.Enter ->
        Navigation.update
          (Binaries_actions.handle_action
             ~toggle_managed_expansion
             ~toggle_registered_expansion
             ~toggle_major_expansion
             ~toggle_managed_group
             ~toggle_available_group)
          ps
    | Some Keys.Tab ->
        Navigation.update
          (Binaries_actions.toggle_current_group
             ~toggle_managed_expansion
             ~toggle_registered_expansion
             ~toggle_major_expansion
             ~toggle_managed_group
             ~toggle_available_group)
          ps
    | Some Keys.Up -> move_selection ps `Up
    | Some Keys.Down -> move_selection ps `Down
    | _ -> ps

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

let handled_keys () = Miaou.Core.Keys.[Escape]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [
    kb "Esc" "Back";
    kb "r" "Refresh";
    kb "d" "Download latest";
    kb "l" "Register directory";
    kb "p" "Prune unused";
    kb "Enter" "Action";
    kb "Tab" "Expand/Collapse";
    kb "↑/↓" "Navigate";
    kb "?" "Help";
  ]

let _header =
  let open Miaou_widgets_display.Widgets in
  [
    title_highlight " Binaries Management ";
    dim "Manage Octez binary versions and registered directories";
  ]

let _footer = []

module Page_Impl :
  Miaou.Core.Tui_page.PAGE_SIG with type state = state and type msg = msg =
struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init () = init ()

  let update = update

  let refresh = auto_refresh

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let handled_keys () = handled_keys ()

  let keymap = keymap

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let has_modal = has_modal

  let view = Binaries_view.view

  let on_key ps key ~size =
    let ps' = handle_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  (* Footer is rendered by binaries_view using Themed_page.render_themed_footer,
     so return empty key_hints to prevent Miaou from rendering a duplicate footer *)
  let key_hints _ps = []
end

module Page =
  Themed_page.Make
    (Page_Impl)
    (struct
      let page_name = "binaries"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page

(** For testing *)
module For_tests = struct
  let filter_latest_n_major_versions =
    Binaries_data.filter_latest_n_major_versions

  let format_size = String_utils.format_size

  let build_items = Binaries_data.build_items

  let move_up = move_up

  let move_down = move_down

  let toggle_major_expansion = toggle_major_expansion

  let toggle_managed_expansion = toggle_managed_expansion

  let toggle_registered_expansion = toggle_registered_expansion
end
