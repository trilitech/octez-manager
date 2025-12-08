(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
open Octez_manager_lib

let name = "snapshots"

type state = {
  network : string;
  entries : Snapshots.entry list;
  selected : int;
  error : string option;
  next_page : string option;
}

type msg = unit

let load_snapshots network =
  match Snapshots.list ~network_slug:network with
  | Ok entries -> entries
  | Error (`Msg _e) -> []

let init () =
  let network = "mainnet" in
  let entries = load_snapshots network in
  {network; entries; selected = 0; error = None; next_page = None}

let update s _ = s

let refresh s = s

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = s

let back s = {s with next_page = Some "__BACK__"}

let keymap _ = [("Esc", back, "Back")]

let header s =
  [
    Widgets.title_highlight (" Snapshots · " ^ s.network);
    Widgets.dim "n: select network";
  ]

let footer = [Widgets.dim "Enter: import  n: network  Esc: back"]

let view s ~focus:_ ~size =
  let body =
    if s.entries = [] then ["No snapshots found or error loading."]
    else
      s.entries
      |> List.mapi (fun i (entry : Snapshots.entry) ->
          let marker = if i = s.selected then Widgets.bold "➤" else " " in
          Printf.sprintf
            "%s %-20s %s"
            marker
            (Widgets.bold entry.label)
            (Widgets.dim (Option.value ~default:"" entry.download_url)))
  in
  Vsection.render ~size ~header:(header s) ~footer ~child:(fun _ ->
      String.concat "\n" body)

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  s

let move_selection s delta =
  let len = List.length s.entries in
  if len = 0 then s
  else
    let selected = max 0 (min (len - 1) (s.selected + delta)) in
    {s with selected}

let select_network s =
  Modal_helpers.open_choice_modal
    ~title:"Select Network"
    ~items:["mainnet"; "ghostnet"; "weeklynet"]
    ~to_string:(fun x -> x)
    ~on_select:(fun _network ->
      (* We can't update state here directly, but we can navigate to self with new param?
         Or we need a way to signal update.
         For now, we can't easily update state from modal callback without message passing.
         But we can use a mutable ref or Context?
         Or we can just reload in refresh if we store network in Context?
      *)
      ()
      (* TODO: Implement network selection properly *)) ;
  s

let import_snapshot s =
  if s.entries = [] then s
  else
    let entry = List.nth s.entries s.selected in
    (* TODO: Implement import flow *)
    Modal_helpers.show_error
      ~title:"Not Implemented"
      ("Import " ^ entry.label ^ " not implemented yet") ;
    s

let handle_key s key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    s)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") | Some (Keys.Char "q") ->
        {s with next_page = Some "__BACK__"}
    | Some Keys.Up | Some (Keys.Char "k") -> move_selection s (-1)
    | Some Keys.Down | Some (Keys.Char "j") -> move_selection s 1
    | Some (Keys.Char "n") -> select_network s
    | Some Keys.Enter -> import_snapshot s
    | _ -> s

let next_page s = s.next_page

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let enter = enter

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let keymap = keymap

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let next_page = next_page

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
