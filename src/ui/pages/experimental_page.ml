(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Experimental features directory page — lists and navigates to experimental features.

    This page acts as a launcher for experimental/beta features that are not yet
    stable enough for prominent placement in the main menu. *)

module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
module Flex = Miaou_widgets_layout.Flex_layout
module T = Themed_text

let name = "experimental"

(* ─── State ────────────────────────────────────────────────────────────── *)

type feature_info = {
  title : string;
  description : string;
  badge : string;
  target_page : string;
}

type state = {features : feature_info list; cursor : int}

type msg = unit

type pstate = state Navigation.t

(* ─── Feature Definitions ──────────────────────────────────────────────── *)

let feature_list =
  [
    {
      title = "Sandbox";
      description =
        "Create and manage local test networks with nodes and bakers";
      badge = "BETA";
      target_page = Sandbox_page.name;
    };
  ]

(* ─── Init / Lifecycle ──────────────────────────────────────────────────── *)

let init () = Navigation.make {features = feature_list; cursor = 0}

let update ps _ = ps

let refresh ps =
  match Context.consume_navigation () with
  | Some (Context.Goto p) -> Navigation.goto p ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None -> ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps = Navigation.back ps

(* ─── Rendering ─────────────────────────────────────────────────────────── *)

let render_feature_item ~selected ~(info : feature_info) =
  let arrow = if selected then T.warning "▶" else " " in
  let badge = T.concat [T.accent "["; T.accent "%s" info.badge; T.accent "]"] in
  let title_line = T.concat [arrow; " "; T.text "%s" info.title; "  "; badge] in
  let desc_line = T.muted "  %s" info.description in
  T.concat [title_line; "\n"; desc_line]

let render_list ~features ~cursor ~size =
  let rows_per_item = 2 in
  let feature_rows =
    List.mapi
      (fun i info ->
        let selected = cursor = i in
        {
          Flex.render = (fun ~size:_ -> render_feature_item ~selected ~info);
          basis = Flex.Px rows_per_item;
          cross = None;
        })
      features
  in
  Flex.create
    ~direction:Flex.Column
    ~padding:{Flex.left = 2; right = 1; top = 1; bottom = 0}
    feature_rows
  |> fun f -> Flex.render f ~size

let render_detail ~(info : feature_info) ~size =
  Flex.create
    ~direction:Flex.Column
    ~padding:{Flex.left = 2; right = 1; top = 2; bottom = 0}
    [
      {
        Flex.render = (fun ~size:_ -> T.text "%s" info.title);
        basis = Flex.Px 1;
        cross = None;
      };
      {
        Flex.render = (fun ~size:_ -> T.muted "%s" info.description);
        basis = Flex.Px 1;
        cross = None;
      };
      {
        Flex.render = (fun ~size:_ -> T.text "");
        basis = Flex.Px 1;
        cross = None;
      };
      {
        Flex.render =
          (fun ~size:_ -> T.muted "Press Enter to open this feature.");
        basis = Flex.Px 1;
        cross = None;
      };
    ]
  |> fun f -> Flex.render f ~size

(* ─── Page Layout ───────────────────────────────────────────────────────── *)

let header = ["  Experimental Features"; ""]

let key_hint_pairs = [("Enter", "open"); ("j/k", "nav"); ("Esc", "back")]

let list_width total_cols = max 30 (total_cols / 3)

let render_content s ~size =
  let cols = size.LTerm_geom.cols in
  let rows = size.LTerm_geom.rows in
  let lw = list_width cols in
  let selected_feature =
    if s.cursor >= 0 && s.cursor < List.length s.features then
      Some (List.nth s.features s.cursor)
    else None
  in
  let layout =
    Flex.create
      ~direction:Flex.Row
      [
        {
          Flex.render =
            (fun ~size ->
              render_list ~features:s.features ~cursor:s.cursor ~size);
          basis = Flex.Px lw;
          cross = None;
        };
        {
          Flex.render =
            (fun ~size ->
              let h = size.LTerm_geom.rows in
              String.concat "\n" (List.init h (fun _ -> T.muted "|")));
          basis = Flex.Px 1;
          cross = None;
        };
        {
          Flex.render =
            (fun ~size ->
              match selected_feature with
              | Some info -> render_detail ~info ~size
              | None -> "");
          basis = Flex.Fill;
          cross = None;
        };
      ]
  in
  Flex.render layout ~size:{LTerm_geom.rows; cols}

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  Context.tick_spinner () ;
  Context.tick_toasts () ;
  let cols = size.LTerm_geom.cols in
  let toast = Context.render_toasts ~cols in
  let footer =
    let hints = Themed_page.render_themed_footer ~cols key_hint_pairs in
    if String.length toast > 0 then [toast] @ hints else hints
  in
  Themed_page.render_layout ~size ~header ~footer ~child:(fun avail ->
      render_content s ~size:avail)

(* ─── Key Handling ──────────────────────────────────────────────────────── *)

let clamp_cursor features cursor =
  let n = List.length features in
  if n = 0 then 0 else max 0 (min cursor (n - 1))

let selected_feature s =
  if s.cursor >= 0 && s.cursor < List.length s.features then
    Some (List.nth s.features s.cursor)
  else None

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match key with
    | "Escape" | "q" -> Navigation.back ps
    | "j" | "Down" ->
        Navigation.update
          (fun s -> {s with cursor = clamp_cursor s.features (s.cursor + 1)})
          ps
    | "k" | "Up" ->
        Navigation.update
          (fun s -> {s with cursor = clamp_cursor s.features (s.cursor - 1)})
          ps
    | "Return" | "Enter" -> (
        match selected_feature ps.Navigation.s with
        | Some info ->
            Context.navigate info.target_page ;
            ps
        | None -> ps)
    | _ -> ps

(* ─── PAGE_SIG ──────────────────────────────────────────────────────────── *)

let handled_keys () =
  Keys.[Escape; Char "q"; Char "j"; Char "k"; Down; Up; Enter]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "Enter" "Open"; kb "j/k" "Navigate"; kb "Esc" "Back"]

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

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
    List.map (fun (key, help) -> Miaou.Core.Tui_page.{key; help}) key_hint_pairs

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()
end

module Page =
  Themed_page.Make
    (Page_Impl)
    (struct
      let page_name = name
    end)

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
