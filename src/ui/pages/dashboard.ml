module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys

let name = "dashboard"

let diagnostics_modal_title = "Diagnostics"

let activity_modal_title = "Activity"

let open_diagnostics_modal () =
  let states = Data.load_service_states ~detail:true () in
  let lines = Data.diagnostics_lines states in
  Modal_helpers.open_text_modal ~title:diagnostics_modal_title ~lines

let open_activity_modal () =
  let states = Data.load_service_states () in
  let lines = Data.activity_lines states in
  Modal_helpers.open_text_modal ~title:activity_modal_title ~lines

let open_menu_modal () =
  Modal_helpers.open_choice_modal
    ~title:"Main Menu"
    ~items:
      [
        `Instances;
        `Installers;
        `Diagnostics;
        `Activity;
        `Networks;
        `Snapshots;
        `Signers;
        `Settings;
        `Jobs;
        `Refresh;
        `Quit;
      ]
    ~to_string:(function
      | `Instances -> "Instances"
      | `Installers -> "Installers Hub"
      | `Diagnostics -> "Diagnostics"
      | `Activity -> "Activity"
      | `Networks -> "Available Networks"
      | `Snapshots -> "Browse Snapshots"
      | `Signers -> "Signers"
      | `Settings -> "Settings"
      | `Jobs -> "Background Jobs"
      | `Refresh -> "Refresh"
      | `Quit -> "Quit")
    ~on_select:(function
      | `Instances -> Context.navigate "instances"
      | `Installers -> Context.navigate Installers.name
      | `Diagnostics -> open_diagnostics_modal ()
      | `Activity -> open_activity_modal ()
      | `Networks -> Context.navigate Networks_page.name
      | `Snapshots -> Context.navigate Snapshots_page.name
      | `Signers -> Context.navigate Signers_page.name
      | `Settings -> Context.navigate Settings_page.name
      | `Jobs -> Context.navigate Jobs_page.name
      | `Refresh -> ()
      | `Quit -> Context.navigate "__BACK__")

let summary_line (summary : Data.Summary.t) =
  let metric label value color =
    Printf.sprintf
      "%s %s"
      (Widgets.dim (label ^ ":"))
      (color (string_of_int value))
  in
  String.concat
    "   "
    [
      metric "total" summary.total Widgets.bold;
      metric "running" summary.running Widgets.green;
      metric "stopped" summary.stopped Widgets.yellow;
      metric "unknown" summary.unknown Widgets.red;
    ]

let services_preview services =
  match Data.spotlight_lines services ~limit:5 with
  | [] -> ["No services registered yet."]
  | lines -> lines

module Page : Miaou.Core.Tui_page.PAGE_SIG = struct
  type state = {
    summary : Data.Summary.t;
    services : Data.Service_state.t list;
    last_updated : float;
    next_page : string option;
  }

  type msg = unit

  let load () =
    let services = Data.load_service_states () in
    let summary = Data.summarize services in
    (summary, services)

  let init () =
    let summary, services = load () in
    {summary; services; last_updated = Unix.gettimeofday (); next_page = None}

  let update s _ = s

  let refresh s =
    let now = Unix.gettimeofday () in
    let next_page =
      match Context.consume_navigation () with
      | Some p -> Some p
      | None -> None
    in
    if now -. s.last_updated > 5. then
      let summary, services = load () in
      {summary; services; last_updated = now; next_page}
    else {s with next_page}

  let force_refresh s =
    let summary, services = load () in
    {
      summary;
      services;
      last_updated = Unix.gettimeofday ();
      next_page = s.next_page;
    }

  let view s ~focus:_ ~size =
    let header =
      [
        Widgets.title_highlight " Octez Manager – Dashboard ";
        Widgets.dim
          (Printf.sprintf
             "Updated %s"
             (Data.formatted_timestamp s.last_updated));
      ]
    in
    let body_lines =
      [summary_line s.summary; ""; "Services"; "---------"]
      @ services_preview s.services
    in
    let footer =
      [
        Widgets.dim "d: diagnostics  a: activity  r: refresh  m: menu  Esc: quit";
      ]
    in
    Vsection.render ~size ~header ~footer ~child:(fun _ ->
        String.concat "\n" body_lines)

  let move s _ = s

  let enter s = s

  let service_select s _ = s

  let service_cycle s _ = refresh s

  let back s = {s with next_page = Some "__BACK__"}

  let keymap _ =
    [
      ( "d",
        (fun s ->
          open_diagnostics_modal () ;
          s),
        "Diagnostics" );
      ( "a",
        (fun s ->
          open_activity_modal () ;
          s),
        "Activity" );
      ("i", (fun s -> {s with next_page = Some "instances"}), "Instances");
      ( "m",
        (fun s ->
          open_menu_modal () ;
          s),
        "Menu" );
      ("r", force_refresh, "Refresh");
    ]

  let check_navigation s =
    match Context.consume_navigation () with
    | Some p -> {s with next_page = Some p}
    | None -> {s with next_page = None}

  let handle_modal_key s key ~size:_ =
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation s

  let handle_key s key ~size:_ =
    let s =
      if Miaou.Core.Modal_manager.has_active () then (
        Miaou.Core.Modal_manager.handle_key key ;
        s)
      else
        match Keys.of_string key with
        | Some (Miaou.Core.Keys.Char "d") ->
            open_diagnostics_modal () ;
            s
        | Some (Miaou.Core.Keys.Char "a") ->
            open_activity_modal () ;
            s
        | Some (Miaou.Core.Keys.Char "i") ->
            {s with next_page = Some "instances"}
        | Some (Miaou.Core.Keys.Char "m") ->
            open_menu_modal () ;
            s
        | Some (Miaou.Core.Keys.Char "r") -> force_refresh s
        | Some (Miaou.Core.Keys.Char "q")
        | Some (Miaou.Core.Keys.Char "Esc")
        | Some (Miaou.Core.Keys.Char "Escape") ->
            {s with next_page = Some "__BACK__"}
        | _ -> s
    in
    check_navigation s

  let next_page s = s.next_page

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  match Miaou.Core.Registry.find name with
  | Some _ -> ()
  | None -> Miaou.Core.Registry.register name page
