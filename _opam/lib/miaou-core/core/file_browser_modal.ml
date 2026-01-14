(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

module Navigation = Navigation
module Modal_manager = Modal_manager
module File_browser_widget = Miaou_widgets_layout.File_browser_widget

let open_modal ?(title = "Select path") ?path ?dirs_only ?require_writable
    ?select_dirs ?show_hidden ?(on_cancel = fun () -> ()) ~on_select () =
  let module Page : Tui_page.PAGE_SIG = struct
    type state = {browser : File_browser_widget.t}

    type msg = unit

    type pstate = state Navigation.t

    type key_binding = state Tui_page.key_binding_desc

    let init () =
      let browser =
        File_browser_widget.open_centered
          ?path
          ?dirs_only
          ?require_writable
          ?select_dirs
          ?show_hidden
          ()
      in
      Navigation.make {browser}

    let update ps _ = ps

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ = ps

    let back ps = ps

    let with_browser ps f =
      Navigation.update (fun st -> {browser = f st.browser}) ps
    (* Render widget and propagate any pending updates back to state. *)

    let view ps ~focus:_ ~size =
      let ps = with_browser ps File_browser_widget.apply_pending_updates in
      let browser = ps.Navigation.s.browser in
      let rendered =
        File_browser_widget.render_with_size browser ~focus:true ~size
      in
      rendered

    let keymap ps =
      let browser = ps.Navigation.s.browser in
      File_browser_widget.key_hints browser
      |> List.map (fun (key, help) ->
          {Tui_page.key; action = (fun ps -> ps); help; display_only = true})

    let handled_keys () =
      [
        Keys.Up;
        Keys.Down;
        Keys.PageUp;
        Keys.PageDown;
        Keys.Char " ";
        Keys.Enter;
        Keys.Escape;
        Keys.Backspace;
        Keys.Tab;
        Keys.Char "h";
        Keys.Char "n";
      ]

    let rec handle_modal_key ps key ~size = handle_key ps key ~size

    and handle_key ps key ~size:_ =
      let st = ps.Navigation.s in
      let browser' = File_browser_widget.handle_key st.browser ~key in
      let ps' = Navigation.update (fun _ -> {browser = browser'}) ps in
      let browser' = ps'.Navigation.s.browser in
      if File_browser_widget.is_cancelled browser' then (
        on_cancel () ;
        Modal_manager.close_top `Cancel ;
        ps')
      else
        match File_browser_widget.get_selection browser' with
        | Some path ->
            on_select path ;
            Modal_manager.close_top `Commit ;
            ps'
        | None -> ps'

    let has_modal _ = false
  end in
  Modal_manager.push
    (module Page)
    ~init:(Page.init ())
    ~ui:{title; left = None; max_width = None; dim_background = true}
    ~commit_on:[]
    ~cancel_on:[]
    ~on_close:(fun (_ : Page.pstate) _ -> ())
