module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
open Octez_manager_lib

let name = "networks"

type state = {
  networks : Teztnets.network_info list;
  selected : int;
  next_page : string option;
}

type msg = unit

let init () =
  let networks =
    match Teztnets.list_networks () with Ok infos -> infos | Error _ -> []
  in
  {networks; selected = 0; next_page = None}

let update s _ = s

let check_navigation s =
  match Context.consume_navigation () with
  | Some p -> {s with next_page = Some p}
  | None -> s

let refresh s = check_navigation s

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = refresh s

let back s = {s with next_page = Some "__BACK__"}

let keymap _ = [("Esc", back, "Back")]

let header s =
  [
    Widgets.title_highlight " Networks ";
    Widgets.dim
      (Printf.sprintf "%d networks available" (List.length s.networks));
  ]

let footer = [Widgets.dim "Enter: details  c: copy config  Esc: back"]

let view s ~focus:_ ~size =
  let body =
    if s.networks = [] then ["No networks found."]
    else
      s.networks
      |> List.mapi (fun i (n : Teztnets.network_info) ->
          let marker = if i = s.selected then Widgets.bold "➤" else " " in
          Printf.sprintf
            "%s %-20s %s"
            marker
            (Widgets.bold n.alias)
            (Widgets.dim (Option.value ~default:"" n.human_name)))
  in
  Vsection.render ~size ~header:(header s) ~footer ~child:(fun _ ->
      String.concat "\n" body)

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  check_navigation s

let move_selection s delta =
  let len = List.length s.networks in
  if len = 0 then s
  else
    let selected = max 0 (min (len - 1) (s.selected + delta)) in
    {s with selected}

let show_details s =
  if s.networks = [] then s
  else
    let n = List.nth s.networks s.selected in
    let lines =
      [
        "Alias: " ^ n.alias;
        "URL: " ^ n.network_url;
        "Name: " ^ Option.value ~default:"-" n.human_name;
        "Description: " ^ Option.value ~default:"-" n.description;
        "Category: " ^ Option.value ~default:"-" n.category;
        "RPC: " ^ Option.value ~default:"-" n.rpc_url;
        "Faucet: " ^ Option.value ~default:"-" n.faucet_url;
        "Docker: " ^ Option.value ~default:"-" n.docker_build;
        "Git Ref: " ^ Option.value ~default:"-" n.git_ref;
        "Last Updated: " ^ Option.value ~default:"-" n.last_updated;
      ]
    in
    Modal_helpers.open_text_modal ~title:("Network · " ^ n.alias) ~lines ;
    s

let copy_config s =
  if s.networks = [] then s
  else
    let n = List.nth s.networks s.selected in
    let lines =
      [
        "# octez-node config init";
        Printf.sprintf "octez-node config init --network %s" n.network_url;
        "";
        "# Environment variables";
        Printf.sprintf "export OCTEZ_NETWORK=%s" n.network_url;
      ]
    in
    Modal_helpers.open_text_modal ~title:("Config · " ^ n.alias) ~lines ;
    s

let handle_key s key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation s)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") | Some (Keys.Char "q") ->
        {s with next_page = Some "__BACK__"}
    | Some Keys.Up | Some (Keys.Char "k") -> move_selection s (-1)
    | Some Keys.Down | Some (Keys.Char "j") -> move_selection s 1
    | Some Keys.Enter -> show_details s
    | Some (Keys.Char "c") -> copy_config s
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
