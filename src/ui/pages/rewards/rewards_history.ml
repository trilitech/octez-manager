(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** History tab for the Rewards page. *)

open Octez_manager_rewards
module Widgets = Miaou_widgets_display.Widgets
module Box = Miaou_widgets_layout.Box_widget
module Sparkline = Miaou_widgets_display.Sparkline_widget

let format_tez_short mutez =
  let tez = Int64.to_float mutez /. 1_000_000.0 in
  Printf.sprintf "%.2f" tez

let make_sparkline ~width data =
  let spark = Sparkline.create ~width ~max_points:30 ~min_value:0.0 () in
  List.iter (Sparkline.push spark) data ;
  spark

let render_sparklines ~box_width (cycles : Rewards.cycle_rewards list) ~instance
    =
  let width = max 10 (box_width - 6) in
  (* Collect data in ascending order for sparklines *)
  let sorted = List.rev cycles in
  let earned_data =
    List.map
      (fun (cr : Rewards.cycle_rewards) ->
        Int64.to_float
          (List.fold_left
             Int64.add
             0L
             [
               cr.block_rewards;
               cr.attestation_rewards;
               cr.other_rewards;
               cr.block_fees;
             ]))
      sorted
  in
  let delegator_data =
    List.map
      (fun (cr : Rewards.cycle_rewards) -> Float.of_int cr.num_delegators)
      sorted
  in
  let distributed_data =
    List.map
      (fun (cr : Rewards.cycle_rewards) ->
        match Payout_report.read_summary_json ~instance ~cycle:cr.cycle with
        | Ok s -> Int64.to_float s.distributed_rewards
        | Error _ -> 0.0)
      sorted
  in
  let earned_spark = make_sparkline ~width earned_data in
  let dist_spark = make_sparkline ~width distributed_data in
  let del_spark = make_sparkline ~width delegator_data in
  let earned_line =
    Sparkline.render_with_label earned_spark ~label:"Earned  " ~focus:false ()
  in
  let dist_line =
    Sparkline.render_with_label dist_spark ~label:"Distrib " ~focus:false ()
  in
  let del_line =
    Sparkline.render_with_label del_spark ~label:"Delegs  " ~focus:false ()
  in
  let content = String.concat "\n" [earned_line; dist_line; del_line] in
  Box.render
    ~title:"Trends (Last 30 Cycles)"
    ~style:Rounded
    ~width:box_width
    content

let render_history_table ~box_width ~(state : Rewards_state.state) ~instance
    (cycles : Rewards.cycle_rewards list) =
  let header =
    Printf.sprintf
      "  %-7s %-14s %-14s %-12s %-5s %-8s"
      "CYCLE"
      "EARNED"
      "DISTRIBUTED"
      "FEE INCOME"
      "DELEG"
      "STATUS"
  in
  let rows =
    List.mapi
      (fun i (cr : Rewards.cycle_rewards) ->
        let earned =
          format_tez_short
            (List.fold_left
               Int64.add
               0L
               [
                 cr.block_rewards;
                 cr.attestation_rewards;
                 cr.other_rewards;
                 cr.block_fees;
               ])
          ^ " \xEA\x9C\xA9"
        in
        let status =
          Rewards_scheduler.get_payout_status ~instance ~cycle:cr.cycle
        in
        let distributed, fee_income =
          match status with
          | Rewards.Paid -> (
              match
                Payout_report.read_summary_json ~instance ~cycle:cr.cycle
              with
              | Ok s ->
                  ( format_tez_short s.distributed_rewards ^ " \xEA\x9C\xA9",
                    format_tez_short s.fee_income ^ " \xEA\x9C\xA9" )
              | Error _ -> ("\xe2\x80\x94", "\xe2\x80\x94"))
          | _ -> ("\xe2\x80\x94", "\xe2\x80\x94")
        in
        let status_str =
          match status with
          | Rewards.Paid -> Widgets.themed_success "paid"
          | Rewards.Unpaid -> Widgets.themed_muted "unpaid"
          | Rewards.Partial -> Widgets.themed_warning "partial"
          | Rewards.In_progress -> Widgets.themed_accent "active"
        in
        let delegators = string_of_int cr.num_delegators in
        let indicator =
          if i = state.history_cursor then "\xe2\x96\xb8" else " "
        in
        let line =
          Printf.sprintf
            "%s %-7d %-14s %-14s %-12s %-5s %s"
            indicator
            cr.cycle
            earned
            distributed
            fee_income
            delegators
            status_str
        in
        if i = state.history_cursor then Widgets.themed_emphasis line
        else Widgets.themed_text line)
      cycles
  in
  let content =
    String.concat
      "\n"
      (Widgets.themed_muted header :: List.map (fun r -> r) rows)
  in
  Box.render
    ~title:(Printf.sprintf "Cycle History (%d)" (List.length cycles))
    ~style:Rounded
    ~width:box_width
    content

let cycle_count (cycles : Rewards.cycle_rewards list) = List.length cycles

let render ~(state : Rewards_state.state) ~cols ~rows:_ =
  let box_width = min (cols - 2) 90 in
  match Rewards_state.selected_baker_instance state with
  | None ->
      String.concat
        "\n"
        [
          "";
          Widgets.themed_muted "  No baker selected.";
          Widgets.themed_muted "  Select a baker to view history.";
        ]
  | Some (instance, baker) -> (
      let cycles = Rewards_scheduler.get_recent_cycles ~baker in
      match cycles with
      | [] ->
          String.concat
            "\n"
            [
              "";
              Widgets.themed_muted "  No cycle history available.";
              Widgets.themed_muted
                "  Waiting for scheduler to fetch reward data...";
            ]
      | _ ->
          let sparklines = render_sparklines ~box_width cycles ~instance in
          let table = render_history_table ~box_width ~state ~instance cycles in
          String.concat "\n" [""; sparklines; ""; table])
