module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
open Octez_manager_lib

let name = "settings"

type state = {settings : Settings.t; next_page : string option}

type msg = unit

let init () =
  let settings =
    match Settings.load () with
    | Ok s -> s
    | Error _ ->
        {
          app_bin_dir = None;
          default_history_mode = None;
          default_logging_mode = None;
        }
  in
  {settings; next_page = None}

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

let header _ = [Widgets.title_highlight " Settings "]

let footer = [Widgets.dim "d: detect binaries  l: logging policy  Esc: back"]

let view s ~focus:_ ~size =
  let settings = s.settings in
  let app_bin_dir = Option.value ~default:"(auto)" settings.app_bin_dir in
  let history_mode =
    match settings.default_history_mode with
    | Some h -> History_mode.to_string h
    | None -> "(default)"
  in
  let logging_mode =
    match settings.default_logging_mode with
    | Some l -> Logging_mode.to_string l
    | None -> "(default)"
  in
  let lines =
    [
      "Global Defaults";
      "---------------";
      Printf.sprintf "Octez Binaries: %s" app_bin_dir;
      Printf.sprintf "History Mode:   %s" history_mode;
      Printf.sprintf "Logging Mode:   %s" logging_mode;
    ]
  in
  Vsection.render ~size ~header:(header s) ~footer ~child:(fun _ ->
      String.concat "\n" lines)

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  check_navigation s

let detect_binaries s =
  match Common.which "octez-node" with
  | Some path -> (
      let dir = Filename.dirname path in
      let new_settings = {s.settings with app_bin_dir = Some dir} in
      match Settings.save new_settings with
      | Ok () ->
          Modal_helpers.show_success
            ~title:"Detected"
            ("Found octez-node in " ^ dir) ;
          {s with settings = new_settings}
      | Error (`Msg e) ->
          Modal_helpers.show_error ~title:"Error saving settings" e ;
          s)
  | None ->
      Modal_helpers.show_error ~title:"Not Found" "octez-node not found in PATH" ;
      s

let logging_policy_modal s =
  Modal_helpers.open_choice_modal
    ~title:"Default Logging Policy"
    ~items:[`Journald; `File]
    ~to_string:(function
      | `Journald -> "Journald (systemd)" | `File -> "File (with rotation)")
    ~on_select:(fun choice ->
      let mode =
        match choice with
        | `Journald -> Logging_mode.Journald
        | `File -> Logging_mode.File {path = ""; rotate = true}
      in
      let new_settings = {s.settings with default_logging_mode = Some mode} in
      match Settings.save new_settings with
      | Ok () ->
          Modal_helpers.show_success ~title:"Saved" "Logging policy updated."
      | Error (`Msg e) -> Modal_helpers.show_error ~title:"Error" e) ;
  s

let handle_key s key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation s)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") | Some (Keys.Char "q") ->
        {s with next_page = Some "__BACK__"}
    | Some (Keys.Char "d") -> detect_binaries s
    | Some (Keys.Char "l") -> logging_policy_modal s
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
