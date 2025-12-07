module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
open Octez_manager_lib

let name = "installers"

type state = {
  services : Service.t list;
  selected : int;
}

type msg = unit

let next_page_ref = ref None

let installers =
  [
    ("Node", "Run a Tezos node (layer 1)");
    ("Baker", "Bake blocks (requires a node)");
    ("Accuser", "Monitor for double signing");
    ("DAL Node", "Data Availability Layer node");
    ("Smart Rollup Node", "Smart Rollup execution node");
    ("Signer", "Remote signer for keys");
  ]

let init () =
  let services =
    match Service_registry.list () with Ok l -> l | Error _ -> []
  in
  {services; selected = 0}

let check_navigation s =
  match Context.consume_navigation () with
  | Some p ->
      next_page_ref := Some p ;
      s
  | None -> s

let refresh s =
  let services =
    match Service_registry.list () with Ok l -> l | Error _ -> []
  in
  check_navigation {s with services}

let move_selection s delta =
  let len = List.length installers in
  let selected = max 0 (min (len - 1) (s.selected + delta)) in
  {s with selected}

let select_installer s =
  let name, _ = List.nth installers s.selected in
  (match name with
  | "Node" -> Context.navigate Install_node_form.name
  | "Baker" -> Context.navigate Install_baker_form.name
  | "Accuser" ->
      Flows.create_accuser_flow ~on_success:Context.mark_instances_dirty
  | "DAL Node" ->
      Flows.create_dal_node_flow ~on_success:Context.mark_instances_dirty
  | "Smart Rollup Node" ->
      Flows.create_smart_rollup_node_flow
        ~on_success:Context.mark_instances_dirty
  | "Signer" ->
      Flows.create_signer_flow ~on_success:Context.mark_instances_dirty
  | _ ->
      Modal_helpers.show_error
        ~title:"Not Implemented"
        "This installer is not yet implemented.") ;
  s

let enter s =
  let s = select_installer s in
  check_navigation s

let update s _ = s

let move s _ = s

let service_select s _ = s

let service_cycle s _ = refresh s

let back s =
  next_page_ref := Some "__BACK__" ;
  s

let keymap _ = [("Enter", enter, "Select"); ("Esc", back, "Back")]

let header = [Widgets.title_highlight " Installers Hub "]

let footer = [Widgets.dim "Enter: select  Esc: back"]

let view s ~focus:_ ~size =
  let body =
    installers
    |> List.mapi (fun i (name, desc) ->
        let marker = if i = s.selected then Widgets.bold "➤" else " " in
        Printf.sprintf
          "%s %-20s %s"
          marker
          (Widgets.bold name)
          (Widgets.dim desc))
  in
  Vsection.render ~size ~header ~footer ~child:(fun _ ->
      String.concat "\n" body)

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  check_navigation s

let handle_key s key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation s)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") | Some (Keys.Char "q") -> back s
    | Some Keys.Up | Some (Keys.Char "k") -> move_selection s (-1)
    | Some Keys.Down | Some (Keys.Char "j") -> move_selection s 1
    | Some Keys.Enter ->
        let s = select_installer s in
        check_navigation s
    | _ -> s

let next_page _ =
  let p = !next_page_ref in
  next_page_ref := None ;
  p

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
