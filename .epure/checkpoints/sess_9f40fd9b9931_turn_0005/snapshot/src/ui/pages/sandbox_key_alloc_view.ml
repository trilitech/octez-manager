(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the sandbox key allocation page.

    LAYOUT RULE: Grid_layout for the table, Flex_layout for the page.
    No Eio calls. *)

module Flex = Miaou_widgets_layout.Flex_layout
module Grid = Miaou_widgets_layout.Grid_layout
module T = Themed_text

(* ─── Types ─────────────────────────────────────────────────────────────── *)

type edit_field = EKeys | EPct

type baker_row = {
  instance : string;
  keys : int;
  is_new : bool;
  to_delete : bool;
}

type state = {
  group_name : string;
  rows : baker_row list;
  total_delegates : int;
  delegate_balances : (float array * float) option;
  cursor : int;
  editing : (int * edit_field * string) option;
  user_modified : bool;
}

(* ─── Helpers ─────────────────────────────────────────────────────────── *)

let allocated_keys rows =
  List.fold_left (fun acc r -> if r.to_delete then acc else acc + r.keys) 0 rows

let slice_stake_pct ~delegate_balances ~offset ~count =
  if count = 0 then 0.0
  else
    match delegate_balances with
    | None -> 0.0
    | Some (balances, total) when total > 0.0 ->
        let sum = ref 0.0 in
        for k = 0 to count - 1 do
          let idx = offset + k in
          if idx < Array.length balances then sum := !sum +. balances.(idx)
        done ;
        !sum /. total *. 100.0
    | Some _ -> 0.0

(* ─── Rendering ─────────────────────────────────────────────────────────── *)

let render_table s ~size =
  let nrows = List.length s.rows in
  if nrows = 0 then T.muted "No bakers — press [a] to add one."
  else
    let header =
      [
        Grid.cell ~row:0 ~col:0 (fun ~size:_ -> T.muted "Baker");
        Grid.cell ~row:0 ~col:1 (fun ~size:_ -> T.muted "Keys");
        Grid.cell ~row:0 ~col:2 (fun ~size:_ -> T.muted "Stake%%");
      ]
    in
    let cells =
      let offset = ref 0 in
      List.concat_map
        (fun (i, row) ->
          let r = i + 1 in
          let selected = i = s.cursor in
          let arrow = if selected then T.warning "▶ " else "  " in
          let inst_str =
            if row.to_delete then
              arrow ^ T.concat [T.error "✗ "; T.muted "%s" row.instance]
            else if row.is_new then arrow ^ T.success "[+ %s]" row.instance
            else arrow ^ T.text "%s" row.instance
          in
          let cur_offset = !offset in
          if not row.to_delete then offset := !offset + row.keys ;
          let keys_str =
            match s.editing with
            | Some (idx, EKeys, txt) when idx = i -> T.warning "[%s_]" txt
            | _ -> if row.to_delete then T.muted "–" else T.text "%d" row.keys
          in
          let stake_str =
            match s.editing with
            | Some (idx, EPct, txt) when idx = i -> T.warning "[%s%%_]" txt
            | _ ->
                if row.to_delete then T.muted "–"
                else
                  let pct =
                    slice_stake_pct
                      ~delegate_balances:s.delegate_balances
                      ~offset:cur_offset
                      ~count:row.keys
                  in
                  if pct < 0.001 && s.delegate_balances = None then
                    T.muted "loading…"
                  else if pct < 0.001 then T.muted "–"
                  else T.text "%.1f%%" pct
          in
          [
            Grid.cell ~row:r ~col:0 (fun ~size:_ -> inst_str);
            Grid.cell ~row:r ~col:1 (fun ~size:_ -> keys_str);
            Grid.cell ~row:r ~col:2 (fun ~size:_ -> stake_str);
          ])
        (List.mapi (fun i r -> (i, r)) s.rows)
    in
    let grid =
      Grid.create
        ~rows:(Grid.Px 1 :: List.init nrows (fun _ -> Grid.Px 1))
        ~cols:[Grid.Fr 1.; Grid.Px 6; Grid.Px 10]
        ~col_gap:2
        (header @ cells)
    in
    Grid.render grid ~size

let header = ["  Key Allocation"; ""]

let render_content s ~size =
  let rows = size.LTerm_geom.rows in
  let cols = size.LTerm_geom.cols in
  let total = s.total_delegates in
  let alloc = allocated_keys s.rows in
  let unalloc = total - alloc in
  let summary =
    T.concat
      [
        T.muted "Sandbox: ";
        T.text "%s" s.group_name;
        T.muted "  Delegates: ";
        T.text "%d total" total;
        T.muted "  ";
        (if unalloc < 0 then T.error "Over-allocated: +%d" (abs unalloc)
         else if unalloc = 0 then T.success "Fully allocated"
         else T.warning "Available: %d" unalloc);
      ]
  in
  let layout =
    Flex.create
      ~direction:Flex.Column
      ~padding:{Flex.left = 1; right = 1; top = 1; bottom = 0}
      [
        {Flex.render = (fun ~size:_ -> summary); basis = Flex.Px 1; cross = None};
        {Flex.render = (fun ~size:_ -> ""); basis = Flex.Px 1; cross = None};
        {
          Flex.render = (fun ~size -> render_table s ~size);
          basis = Flex.Fill;
          cross = None;
        };
      ]
  in
  Flex.render layout ~size:{LTerm_geom.rows; cols}

let key_hint_pairs =
  [
    ("Enter", "edit keys");
    ("p", "edit %");
    ("a", "add baker");
    ("d", "del/toggle");
    ("Tab", "redistribute");
    ("c", "apply");
    ("Esc", "back");
  ]

(** Render the full key allocation page view.

    [toast] is a pre-rendered toast string (may be empty). *)
let view s ~toast ~focus:_ ~size =
  let cols = size.LTerm_geom.cols in
  let footer =
    let hints = Page_layout.render_themed_footer ~cols key_hint_pairs in
    if String.length toast > 0 then [toast] @ hints else hints
  in
  Page_layout.render_layout ~size ~header ~footer ~child:(fun avail ->
      render_content s ~size:avail)
