(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation

let name = "topology"

type state = {services : Data.Service_state.t list}

type msg = unit

type pstate = state Navigation.t

let init () = Navigation.make {services = Data.load_service_states ()}

let update ps _ = ps

let refresh ps =
  match Context.consume_navigation () with
  | Some (Context.Goto p) -> Navigation.goto p ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      if Context.consume_instances_dirty () then
        Navigation.update
          (fun _s -> {services = Data.load_service_states ()})
          ps
      else ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps = Navigation.back ps

let handled_keys () = Miaou.Core.Keys.[Escape]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "Esc" "Back"; kb "?" "Help"]

let view ps ~focus ~size =
  let s = ps.Navigation.s in
  let vs : Topology_view.state = {Topology_view.services = s.services} in
  Topology_view.view vs ~focus ~size

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some Keys.Escape | Some (Keys.Char "q") -> Navigation.back ps
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
      [{key = "Esc"; help = "Back"}; {key = "?"; help = "Help"}]

  let has_modal = has_modal
end

module Page =
  Themed_page.Make
    (Page_Impl)
    (struct
      let page_name = "topology"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
