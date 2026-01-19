(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
open Octez_manager_lib

let name = "snapshots"

type state = {
  network : string;
  entries : Snapshots.entry list;
  selected : int;
  error : string option;
}

type msg = unit

type pstate = state Navigation.t

let load_snapshots network =
  match Snapshots.list ~network_slug:network with
  | Ok entries -> entries
  | Error (`Msg _e) -> []

let init () =
  let network = "mainnet" in
  let entries = load_snapshots network in
  Navigation.make {network; entries; selected = 0; error = None}

let update ps _ = ps

let refresh ps = ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps = Navigation.back ps

let handled_keys () = Miaou.Core.Keys.[Escape]

let keymap _ =
  let noop ps = ps in
  let kb key action help =
    {Miaou.Core.Tui_page.key; action; help; display_only = false}
  in
  [
    kb "Esc" back "Back";
    {
      Miaou.Core.Tui_page.key = "?";
      action = noop;
      help = "Help";
      display_only = true;
    };
  ]

let header s =
  [
    Widgets.title_highlight (" Snapshots · " ^ s.network);
    Widgets.dim "n: select network";
  ]

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let body =
    if s.entries = [] then ["No snapshots found or error loading."]
    else
      s.entries
      |> List.mapi (fun i (entry : Snapshots.entry) ->
          let marker = if i = s.selected then Widgets.bold ">" else " " in
          Printf.sprintf
            "%s %-20s %s"
            marker
            (Widgets.bold entry.label)
            (Widgets.dim (Option.value ~default:"" entry.download_url)))
  in
  Vsection.render ~size ~header:(header s) ~content_footer:[] ~child:(fun _ ->
      String.concat "\n" body)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let move_selection ps delta =
  Navigation.update
    (fun s ->
      let len = List.length s.entries in
      if len = 0 then s
      else
        let selected = max 0 (min (len - 1) (s.selected + delta)) in
        {s with selected})
    ps

let select_network ps =
  Modal_helpers.open_choice_modal
    ~title:"Select Network"
    ~items:["mainnet"; "ghostnet"; "weeklynet"]
    ~to_string:(fun x -> x)
    ~on_select:(fun _network ->
      (* TODO: Implement network selection properly *)
      ())
    () ;
  ps

let import_snapshot ps =
  let s = ps.Navigation.s in
  if s.entries = [] then ps
  else
    let entry = List.nth s.entries s.selected in
    (* TODO: Implement import flow *)
    Modal_helpers.show_error
      ~title:"Not Implemented"
      ("Import " ^ entry.label ^ " not implemented yet") ;
    ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") | Some (Keys.Char "q") -> Navigation.back ps
    | Some Keys.Up | Some (Keys.Char "k") -> move_selection ps (-1)
    | Some Keys.Down | Some (Keys.Char "j") -> move_selection ps 1
    | Some (Keys.Char "n") -> select_network ps
    | Some Keys.Enter -> import_snapshot ps
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

  let has_modal = has_modal
end

module Page =
  Monitored_page.Make
    (Page_Impl)
    (struct
      let page_name = "snapshots"
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
