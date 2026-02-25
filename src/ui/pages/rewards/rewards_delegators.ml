(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Delegators tab for the Rewards page. *)

open Octez_manager_rewards
module Widgets = Miaou_widgets_display.Widgets
module Box = Miaou_widgets_layout.Box_widget
module Desc_list = Miaou_widgets_display.Description_list

let tez_symbol = "\xEA\x9C\xA9"

let short_address addr =
  let len = String.length addr in
  if len > 14 then String.sub addr 0 8 ^ ".." ^ String.sub addr (len - 4) 4
  else addr

let format_tez_compact mutez =
  let tez = Int64.to_float mutez /. 1_000_000.0 in
  if Float.abs tez >= 10_000.0 then Printf.sprintf "%.0f" tez
  else if Float.abs tez >= 1.0 then Printf.sprintf "%.2f" tez
  else Printf.sprintf "%.4f" tez

let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let found = ref false in
    let i = ref 0 in
    while (not !found) && !i <= hlen - nlen do
      if String.equal (String.sub haystack !i nlen) needle then found := true
      else incr i
    done ;
    !found

(* {1 Filtering, sorting, searching} *)

let apply_filter filter (delegators : Rewards.delegator_reward list) =
  match filter with
  | Rewards_state.FilterAll -> delegators
  | Rewards_state.FilterEligible ->
      List.filter
        (fun (d : Rewards.delegator_reward) ->
          match d.status with Rewards.Eligible -> true | _ -> false)
        delegators
  | Rewards_state.FilterExcluded ->
      List.filter
        (fun (d : Rewards.delegator_reward) ->
          match d.status with
          | Rewards.Ignored | Rewards.Override_excluded | Rewards.Emptied ->
              true
          | _ -> false)
        delegators
  | Rewards_state.FilterBelowMin ->
      List.filter
        (fun (d : Rewards.delegator_reward) ->
          match d.status with
          | Rewards.Below_minimum_payout | Rewards.Below_minimum_balance -> true
          | _ -> false)
        delegators

let apply_search query (delegators : Rewards.delegator_reward list) =
  if String.length query = 0 then delegators
  else
    let q = String.lowercase_ascii query in
    List.filter
      (fun (d : Rewards.delegator_reward) ->
        string_contains ~needle:q (String.lowercase_ascii d.delegator)
        || string_contains ~needle:q (String.lowercase_ascii d.recipient))
      delegators

let apply_sort sort (delegators : Rewards.delegator_reward list) =
  List.sort
    (fun (a : Rewards.delegator_reward) (b : Rewards.delegator_reward) ->
      match sort with
      | Rewards_state.SortAddress -> String.compare a.delegator b.delegator
      | Rewards_state.SortBalance ->
          Int64.compare b.delegated_balance a.delegated_balance
      | Rewards_state.SortReward -> Int64.compare b.net_reward a.net_reward
      | Rewards_state.SortStatus ->
          String.compare
            (Rewards.string_of_delegator_status a.status)
            (Rewards.string_of_delegator_status b.status))
    delegators

let prepare_delegators (state : Rewards_state.state)
    (delegators : Rewards.delegator_reward list) =
  let filtered = apply_filter state.delegator_filter delegators in
  let searched =
    if state.search_active || String.length state.search_query > 0 then
      apply_search state.search_query filtered
    else filtered
  in
  apply_sort state.delegator_sort searched

(* {1 Detail panel} *)

let render_detail ~box_width (d : Rewards.delegator_reward) =
  let recipient_label =
    if String.equal d.recipient d.delegator then d.recipient ^ " (self)"
    else d.recipient
  in
  let items =
    [
      ("Address", d.delegator);
      ( "Delegated Balance",
        Rewards.format_tez d.delegated_balance ^ " " ^ tez_symbol );
      ("Staked Balance", Rewards.format_tez d.staked_balance ^ " " ^ tez_symbol);
      ("Gross Reward", Rewards.format_tez d.gross_reward ^ " " ^ tez_symbol);
      ("Fee Rate", Printf.sprintf "%.1f%%" (d.fee_rate *. 100.0));
      ("Fee Amount", Rewards.format_tez d.fee_amount ^ " " ^ tez_symbol);
      ("Net Reward", Rewards.format_tez d.net_reward ^ " " ^ tez_symbol);
      ("Recipient", recipient_label);
      ("Status", Rewards.string_of_delegator_status d.status);
    ]
  in
  let desc =
    Desc_list.create ~key_width:18 ~items ()
    |> Desc_list.render ~cols:(box_width - 4) ~wrap:true ~focus:false
  in
  let title = Printf.sprintf "Detail: %s" (short_address d.delegator) in
  Box.render ~title ~style:Rounded ~width:box_width desc

(* {1 List rendering} *)

let render_row ~is_selected (d : Rewards.delegator_reward) =
  let addr = short_address d.delegator in
  let balance = format_tez_compact d.delegated_balance ^ " " ^ tez_symbol in
  let reward = format_tez_compact d.net_reward ^ " " ^ tez_symbol in
  let fee = Printf.sprintf "%.1f%%" (d.fee_rate *. 100.0) in
  let indicator = if is_selected then "\xe2\x96\xb8" else " " in
  let warning =
    match d.status with
    | Rewards.Below_minimum_payout | Rewards.Below_minimum_balance ->
        " \xe2\x9a\xa0"
    | _ -> ""
  in
  let line =
    Printf.sprintf
      "%s %-14s %14s %14s %5s%s"
      indicator
      addr
      balance
      reward
      fee
      warning
  in
  if is_selected then Widgets.themed_emphasis line
  else
    match d.status with
    | Rewards.Eligible -> Widgets.themed_text line
    | _ -> Widgets.themed_muted line

(* {1 Main render} *)

let render ~(state : Rewards_state.state) ~cols ~rows =
  let box_width = min (cols - 2) 72 in
  match state.blueprint with
  | None ->
      String.concat
        "\n"
        [
          "";
          Widgets.themed_muted "  No delegator data available.";
          Widgets.themed_muted
            "  Select a baker and wait for cycle data to load.";
        ]
  | Some bp ->
      let delegators = prepare_delegators state bp.Rewards.delegator_rewards in
      let count = List.length delegators in
      let cursor =
        if count = 0 then 0 else max 0 (min state.delegator_cursor (count - 1))
      in
      (* Indicators line *)
      let indicators =
        Printf.sprintf
          "  Sort: %s  Filter: %s"
          (Widgets.themed_accent
             (Rewards_state.sort_label state.delegator_sort))
          (Widgets.themed_accent
             (Rewards_state.filter_label state.delegator_filter))
      in
      (* Search line *)
      let search_line =
        if state.search_active then
          Some
            (Widgets.themed_text
               (Printf.sprintf "  Search: %s_" state.search_query))
        else if String.length state.search_query > 0 then
          Some
            (Widgets.themed_muted
               (Printf.sprintf "  Search: %s  (/ to edit)" state.search_query))
        else None
      in
      (* List header *)
      let header =
        Widgets.themed_muted
          (Printf.sprintf
             "  %-14s %14s %14s %5s"
             "ADDRESS"
             "BALANCE"
             "REWARD"
             "FEE")
      in
      (* Compute visible window. Reserve space for indicators, search,
         header, scroll indicator, detail panel, and padding. *)
      let detail_height = 13 in
      let chrome_height = 6 in
      let max_visible = max 3 (rows - detail_height - chrome_height) in
      let scroll_offset =
        if count <= max_visible then 0
        else if cursor < max_visible / 2 then 0
        else if cursor > count - ((max_visible + 1) / 2) then
          max 0 (count - max_visible)
        else cursor - (max_visible / 2)
      in
      (* Render visible rows *)
      let visible_rows =
        let acc = ref [] in
        List.iteri
          (fun i d ->
            if i >= scroll_offset && i < scroll_offset + max_visible then
              acc := render_row ~is_selected:(i = cursor) d :: !acc)
          delegators ;
        List.rev !acc
      in
      let scroll_indicator =
        if count > max_visible then
          Some
            (Widgets.themed_muted
               (Printf.sprintf
                  "  showing %d\xe2\x80\x93%d of %d"
                  (scroll_offset + 1)
                  (min (scroll_offset + max_visible) count)
                  count))
        else None
      in
      (* List box *)
      let list_lines = header :: visible_rows in
      let list_lines =
        match scroll_indicator with
        | Some s -> list_lines @ [s]
        | None -> list_lines
      in
      let list_content = String.concat "\n" list_lines in
      let cycle_title =
        Printf.sprintf "Delegators \xe2\x80\x94 Cycle %d (%d)" bp.cycle count
      in
      let list_box =
        Box.render
          ~title:cycle_title
          ~style:Rounded
          ~width:box_width
          list_content
      in
      (* Detail panel for selected delegator *)
      let detail =
        match List.nth_opt delegators cursor with
        | Some d -> render_detail ~box_width d
        | None -> ""
      in
      (* Assemble output *)
      let parts = [indicators] in
      let parts =
        match search_line with Some s -> parts @ [s] | None -> parts
      in
      let parts = parts @ [""; list_box] in
      let parts = if detail <> "" then parts @ [""; detail] else parts in
      String.concat "\n" parts
