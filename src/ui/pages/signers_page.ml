module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
open Octez_manager_lib
open Rresult

let ( let* ) = Result.bind

let name = "signers"

type state = {
  signers : Data.Service_state.t list;
  selected : int;
  next_page : string option;
}

type msg = unit

let load_signers () =
  let services = Data.load_service_states () in
  List.filter
    (fun st -> st.Data.Service_state.service.Service.role = "signer")
    services

let init () =
  let signers = load_signers () in
  {signers; selected = 0; next_page = None}

let update s _ = s

let check_navigation s =
  match Context.consume_navigation () with
  | Some p -> {s with next_page = Some p}
  | None -> s

let refresh s =
  let s = check_navigation s in
  if Context.consume_instances_dirty () then {s with signers = load_signers ()}
  else s

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = refresh s

let back s = {s with next_page = Some "__BACK__"}

let keymap _ = [("Esc", back, "Back")]

let header s =
  [
    Widgets.title_highlight " Signers ";
    Widgets.dim (Printf.sprintf "%d signers managed" (List.length s.signers));
  ]

let footer = [Widgets.dim "Enter: actions  Esc: back"]

let view s ~focus:_ ~size =
  let body =
    if s.signers = [] then ["No signer instances found."]
    else
      s.signers
      |> List.mapi (fun i (st : Data.Service_state.t) ->
          let marker = if i = s.selected then Widgets.bold "➤" else " " in
          let svc = st.Data.Service_state.service in
          Printf.sprintf
            "%s %-20s %s"
            marker
            (Widgets.bold svc.Service.instance)
            (Widgets.dim svc.Service.rpc_addr))
  in
  Vsection.render ~size ~header:(header s) ~footer ~child:(fun _ ->
      String.concat "\n" body)

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  check_navigation s

let move_selection s delta =
  let len = List.length s.signers in
  if len = 0 then s
  else
    let selected = max 0 (min (len - 1) (s.selected + delta)) in
    {s with selected}

let require_tezos_node_manager () =
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Tezos_node_manager_capability.key
  with
  | Some cap ->
      let module I =
        (val (cap : Manager_interfaces.Tezos_node_manager_capability.t))
      in
      Ok (module I : Manager_interfaces.Tezos_node_manager)
  | None -> Error (`Msg "Tezos node manager capability not available")

let require_tezos_client_manager () =
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Tezos_client_manager_capability.key
  with
  | Some cap ->
      let module I =
        (val (cap : Manager_interfaces.Tezos_client_manager_capability.t))
      in
      Ok (module I : Manager_interfaces.Tezos_client_manager)
  | None -> Error (`Msg "Tezos client manager capability not available")

let authorize_key_modal s instance =
  Modal_helpers.prompt_text_modal
    ~title:("Authorize Key on " ^ instance)
    ~placeholder:(Some "edpk... or tz...")
    ~on_submit:(fun key ->
      Modal_helpers.prompt_text_modal
        ~title:"Key Alias (optional)"
        ~on_submit:(fun alias ->
          let alias_opt = if String.trim alias = "" then None else Some alias in
          let res =
            let* (module CM) = require_tezos_client_manager () in
            CM.add_authorized_key ~instance ~key ~name:alias_opt
          in
          match res with
          | Ok () ->
              Modal_helpers.show_success ~title:"Success" "Key authorized."
          | Error (`Msg e) -> Modal_helpers.show_error ~title:"Error" e)
        ())
    () ;
  s

let generate_key_modal s instance =
  Modal_helpers.prompt_text_modal
    ~title:("Generate Secret Key on " ^ instance)
    ~placeholder:(Some "alias")
    ~on_submit:(fun alias ->
      if String.trim alias = "" then
        Modal_helpers.show_error ~title:"Error" "Alias required"
      else
        let res =
          let* (module NM) = require_tezos_node_manager () in
          NM.generate_secret_key ~instance ~alias
        in
        match res with
        | Ok () ->
            Modal_helpers.show_success
              ~title:"Success"
              ("Key " ^ alias ^ " generated.")
        | Error (`Msg e) -> Modal_helpers.show_error ~title:"Error" e)
    () ;
  s

let list_keys_modal s instance =
  let res =
    let* (module CM) = require_tezos_client_manager () in
    CM.list_keys ~instance
  in
  match res with
  | Ok output ->
      Modal_helpers.open_text_modal
        ~title:("Keys · " ^ instance)
        ~lines:(String.split_on_char '\n' output) ;
      s
  | Error (`Msg e) ->
      Modal_helpers.show_error ~title:"Error" e ;
      s

let actions_modal s =
  if s.signers = [] then s
  else
    let st = List.nth s.signers s.selected in
    let instance = st.Data.Service_state.service.Service.instance in
    Modal_helpers.open_choice_modal
      ~title:("Actions · " ^ instance)
      ~items:[`Authorize; `Generate; `ListKeys]
      ~to_string:(function
        | `Authorize -> "Authorize Key"
        | `Generate -> "Generate Secret Key"
        | `ListKeys -> "List Keys")
      ~on_select:(function
        | `Authorize -> authorize_key_modal s instance |> ignore
        | `Generate -> generate_key_modal s instance |> ignore
        | `ListKeys -> list_keys_modal s instance |> ignore) ;
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
    | Some Keys.Enter -> actions_modal s
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
