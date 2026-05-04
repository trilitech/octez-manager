(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets

type tab = {id : string; labels : string list}

let tab ~id ~labels =
  if labels = [] then
    invalid_arg "Responsive_tabs_widget.tab: labels must be non-empty" ;
  {id; labels}

type t = {tabs : tab list; selected : int}

let make tabs = if tabs = [] then {tabs; selected = 0} else {tabs; selected = 0}

let current t =
  match List.nth_opt t.tabs t.selected with
  | Some tab -> Some tab
  | None -> None

let select t ~id =
  match List.find_index (fun tab -> String.equal tab.id id) t.tabs with
  | Some i -> {t with selected = i}
  | None -> t

let move t dir =
  let n = List.length t.tabs in
  if n = 0 then t
  else
    match dir with
    | `Left -> {t with selected = max 0 (t.selected - 1)}
    | `Right -> {t with selected = min (n - 1) (t.selected + 1)}
    | `First -> {t with selected = 0}
    | `Last -> {t with selected = n - 1}

(** Return label at the given shortening level (0 = full, higher = shorter). *)
let label_at_level (tab : tab) level =
  let n = List.length tab.labels in
  List.nth tab.labels (min level (n - 1))

(** Total render width for a given per-tab shortening level array.
    Each tab renders as " <label> " with " | " between tabs (3 extra chars per
    separator = 1 space + "|" + 1 space but we include the space in each tab
    as " <label> ").  The separator " | " is 3 chars between tabs.
    Simplified: each tab costs (String.length label + 2) for " label ", plus
    3 for " | " separator between tabs (n-1 times). *)
let render_width tabs levels =
  let n = List.length tabs in
  if n = 0 then 0
  else
    let tab_widths =
      List.mapi
        (fun i tab ->
          let level = List.nth levels i in
          String.length (label_at_level tab level) + 2)
        tabs
    in
    let total = List.fold_left ( + ) 0 tab_widths in
    (* separators: (n-1) * 3 chars " | " *)
    total + ((n - 1) * 3)

(** Compute the shortening level for each tab so the bar fits within [cols].
    Shortens from the rightmost tab outward. *)
let fit_levels tabs cols =
  let n = List.length tabs in
  let levels = Array.make n 0 in
  let max_level_for tab = List.length tab.labels - 1 in
  let get_levels () = Array.to_list levels in
  let fits () = render_width tabs (get_levels ()) <= cols in
  if not (fits ()) then begin
    let changed = ref true in
    while !changed && not (fits ()) do
      changed := false ;
      for i = n - 1 downto 0 do
        if not (fits ()) then begin
          let tab = List.nth tabs i in
          if levels.(i) < max_level_for tab then begin
            levels.(i) <- levels.(i) + 1 ;
            changed := true
          end
        end
      done
    done
  end ;
  Array.to_list levels

let render t ~focus ~cols =
  let tabs = t.tabs in
  if tabs = [] then ""
  else
    let levels = fit_levels tabs cols in
    List.mapi
      (fun i tab ->
        let label = label_at_level tab (List.nth levels i) in
        let is_selected = Int.equal i t.selected in
        let rendered =
          if is_selected then
            if focus then Widgets.themed_emphasis (" " ^ label ^ " ")
            else Widgets.themed_primary (" " ^ label ^ " ")
          else Widgets.themed_muted (" " ^ label ^ " ")
        in
        rendered)
      tabs
    |> String.concat (Widgets.themed_muted " | ")

let handle_event ?(bubble_unhandled = true) t ~key ~cols:_ =
  let n = List.length t.tabs in
  (* Number key 1-9 selects tab by index *)
  let digit_select () =
    if String.length key = 1 then
      let c = key.[0] in
      if c >= '1' && c <= '9' then
        let idx = Char.code c - Char.code '1' in
        if idx < n then Some {t with selected = idx} else None
      else None
    else None
  in
  match digit_select () with
  | Some t' -> (t', `Handled)
  | None -> (
      match key with
      | "Left" | "h" -> (move t `Left, `Handled)
      | "Right" | "l" -> (move t `Right, `Handled)
      | "Home" -> (move t `First, `Handled)
      | "End" -> (move t `Last, `Handled)
      | _ -> if bubble_unhandled then (t, `Bubble) else (t, `Handled))
