(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Overview tab for the Rewards page. *)

open Octez_manager_rewards
module Widgets = Miaou_widgets_display.Widgets
module Box = Miaou_widgets_layout.Box_widget
module Desc_list = Miaou_widgets_display.Description_list
module Grid = Miaou_widgets_layout.Grid_layout
module Display = Rewards_display_utils

let format_tez_short mutez =
  let tez = Int64.to_float mutez /. 1_000_000.0 in
  let s = Printf.sprintf "%.2f" tez in
  (* Add thousands separators *)
  let parts = String.split_on_char '.' s in
  match parts with
  | [int_part; dec_part] ->
      let negative = String.length int_part > 0 && int_part.[0] = '-' in
      let digits =
        if negative then String.sub int_part 1 (String.length int_part - 1)
        else int_part
      in
      let len = String.length digits in
      let buf = Buffer.create (len + (len / 3) + 4) in
      if negative then Buffer.add_char buf '-' ;
      for i = 0 to len - 1 do
        if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',' ;
        Buffer.add_char buf digits.[i]
      done ;
      Buffer.add_char buf '.' ;
      Buffer.add_string buf dec_part ;
      Buffer.contents buf
  | _ -> s

let render_network_badge network =
  let lower = String.lowercase_ascii network in
  if String.equal lower "mainnet" then
    Widgets.themed_warning
      (Printf.sprintf " %s " (String.uppercase_ascii network))
  else Widgets.themed_accent (Printf.sprintf " %s " network)

let render_current_cycle_box ~box_width ~instance current_cycle =
  let cycle_line =
    match current_cycle with
    | Some _cycle -> Widgets.themed_text "Status: In progress"
    | None -> Widgets.themed_muted "Loading cycle data..."
  in
  let continual_active = Rewards_scheduler.get_payout_timer_active ~instance in
  let continual_line =
    if continual_active then
      let interval_str =
        match Rewards_scheduler.get_continual_interval ~instance with
        | Some interval when interval > 1 ->
            Printf.sprintf " (every %d cycles)" interval
        | _ -> ""
      in
      Widgets.themed_success (Printf.sprintf "Continual: Active%s" interval_str)
    else "Continual: " ^ Widgets.themed_error "Inactive"
  in
  let content = String.concat "\n" [cycle_line; continual_line] in
  let title =
    match current_cycle with
    | Some c -> Printf.sprintf "Current Cycle: %d" c
    | None -> "Current Cycle"
  in
  Box.render ~title ~style:Rounded ~width:box_width content

let render_last_completed_box ~box_width ~instance
    (cr : Rewards.cycle_rewards option) =
  match cr with
  | None ->
      Box.render
        ~title:"Last Completed"
        ~style:Rounded
        ~width:box_width
        (Widgets.themed_muted "No completed cycle data")
  | Some cr ->
      let total_rewards = Rewards.total_earned cr in
      let delegator_count = cr.num_delegators in
      let status =
        Rewards_scheduler.get_payout_status ~instance ~cycle:cr.cycle
      in
      let status_label =
        if delegator_count = 0 then Widgets.themed_muted "N/A (no delegators)"
        else
          match status with
          | Rewards.Paid -> Widgets.themed_success "Paid"
          | Rewards.Unpaid -> Widgets.themed_warning "Unpaid"
          | Rewards.Partial -> Widgets.themed_warning "Partial"
          | Rewards.In_progress -> Widgets.themed_accent "In progress"
      in
      let items =
        [
          ("Earned Rewards", Rewards.format_tez total_rewards ^ " \xEA\x9C\xA9");
          ( "Block Rewards",
            Rewards.format_tez cr.block_rewards ^ " \xEA\x9C\xA9" );
          ( "Attestation",
            Rewards.format_tez cr.attestation_rewards ^ " \xEA\x9C\xA9" );
          ("DAL Rewards", Rewards.format_tez cr.dal_rewards ^ " \xEA\x9C\xA9");
          ("Block Fees", Rewards.format_tez cr.block_fees ^ " \xEA\x9C\xA9");
          ("Delegators", string_of_int delegator_count);
          ("Payout Status", status_label);
        ]
      in
      let desc =
        Desc_list.create ~key_width:16 ~items ()
        |> Desc_list.render ~cols:(box_width - 4) ~wrap:true ~focus:false
      in
      Box.render
        ~title:(Printf.sprintf "Last Completed: Cycle %d" cr.cycle)
        ~style:Rounded
        ~width:box_width
        desc

let render_recent_cycles_box ~box_width ~instance ~current_cycle
    ~(state : Rewards_state.state) (cycles : Rewards.cycle_rewards list) =
  match cycles with
  | [] ->
      Box.render
        ~title:"Recent Cycles"
        ~style:Rounded
        ~width:box_width
        (Widgets.themed_muted "No cycle data available")
  | _ ->
      let header =
        "  "
        ^ Display.pad_right 7 "CYCLE"
        ^ " "
        ^ Display.pad_right 14 "EARNED"
        ^ " "
        ^ Display.pad_right 14 "DISTRIBUTED"
        ^ " "
        ^ Display.pad_right 12 "FEE INCOME"
        ^ " "
        ^ Display.pad_right 5 "DELEG"
        ^ " STATUS"
      in
      let rows =
        List.mapi
          (fun i (cr : Rewards.cycle_rewards) ->
            let earned =
              format_tez_short (Rewards.total_earned cr) ^ " \xEA\x9C\xA9"
            in
            let status =
              Rewards_scheduler.get_payout_status ~instance ~cycle:cr.cycle
            in
            let distributed, fee_income =
              match
                Rewards_scheduler.get_payout_summary ~instance ~cycle:cr.cycle
              with
              | Some s ->
                  ( format_tez_short s.distributed_rewards ^ " \xEA\x9C\xA9",
                    format_tez_short s.fee_income ^ " \xEA\x9C\xA9" )
              | None -> ("\xe2\x80\x94", "\xe2\x80\x94")
            in
            let status_str =
              match status with
              | Rewards.Paid -> Widgets.themed_success "paid"
              | Rewards.Unpaid -> Widgets.themed_muted "unpaid"
              | Rewards.Partial -> Widgets.themed_warning "partial"
              | Rewards.In_progress -> Widgets.themed_accent "active"
            in
            let is_current =
              match current_cycle with
              | Some cc -> Int.equal cr.cycle cc
              | None -> false
            in
            let cycle_label =
              if is_current then string_of_int cr.cycle ^ " \xe2\x97\x80"
              else string_of_int cr.cycle
            in
            let delegators = string_of_int cr.num_delegators in
            let indicator =
              if i = state.history_cursor then "\xe2\x96\xb8 " else "  "
            in
            let line =
              indicator
              ^ Display.pad_right 7 cycle_label
              ^ " "
              ^ Display.pad_right 14 earned
              ^ " "
              ^ Display.pad_right 14 distributed
              ^ " "
              ^ Display.pad_right 12 fee_income
              ^ " "
              ^ Display.pad_right 5 delegators
              ^ " " ^ status_str
            in
            if i = state.history_cursor then Widgets.themed_emphasis line
            else if is_current then Widgets.themed_accent line
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

let render_blueprint_box ~box_width (bp : Rewards.payout_blueprint) =
  let total_distributable =
    List.fold_left
      (fun acc (r : Rewards.delegator_reward) ->
        match r.status with
        | Rewards.Eligible -> Int64.add acc r.net_reward
        | _ -> acc)
      0L
      bp.delegator_rewards
  in
  let items =
    [
      ("Cycle", string_of_int bp.cycle);
      ("Total Delegators", string_of_int bp.total_delegators);
      ("Eligible Delegators", string_of_int bp.eligible_delegators);
      ( "Total Distributable",
        format_tez_short total_distributable ^ " \xEA\x9C\xA9" );
      ( "Baker Bond Income",
        format_tez_short bp.baker_bond_income ^ " \xEA\x9C\xA9" );
      ( "Baker Fee Income",
        format_tez_short bp.baker_fee_income ^ " \xEA\x9C\xA9" );
      ("Est. TX Fees", format_tez_short bp.estimated_tx_fees ^ " \xEA\x9C\xA9");
    ]
  in
  let desc =
    Desc_list.create ~key_width:20 ~items ()
    |> Desc_list.render ~cols:(box_width - 4) ~wrap:true ~focus:false
  in
  Box.render ~title:"Payout Preview" ~style:Rounded ~width:box_width desc

let render_payout_report_box ~box_width ~instance ~cycle =
  match Rewards_scheduler.get_payout_results ~instance ~cycle with
  | None | Some [] -> ""
  | Some results ->
      let summary = Rewards_scheduler.get_payout_summary ~instance ~cycle in
      let summary_line =
        match summary with
        | Some s ->
            let status_text =
              if s.paid_delegators < s.delegators then
                Widgets.themed_warning
                  (Printf.sprintf
                     "%d/%d delegators paid (partial)"
                     s.paid_delegators
                     s.delegators)
              else
                Widgets.themed_success
                  (Printf.sprintf
                     "%d/%d delegators paid"
                     s.paid_delegators
                     s.delegators)
            in
            status_text
        | None -> ""
      in
      let header =
        "  "
        ^ Display.pad_right 18 "DELEGATOR"
        ^ " "
        ^ Display.pad_right 14 "AMOUNT"
        ^ " "
        ^ Display.pad_right 8 "STATUS"
        ^ " NOTE"
      in
      let rows =
        List.map
          (fun (r : Rewards.payout_result) ->
            let short_addr =
              let len = String.length r.delegator in
              if len > 15 then
                String.sub r.delegator 0 10
                ^ "..."
                ^ String.sub r.delegator (len - 4) 4
              else r.delegator
            in
            let amount_str = Rewards.format_tez r.amount ^ " \xEA\x9C\xA9" in
            let status_str =
              if r.success then Widgets.themed_success "\xE2\x9C\x93"
              else Widgets.themed_error "\xE2\x9C\x97 fail"
            in
            let note_str =
              if String.length r.note > 0 then Widgets.themed_muted r.note
              else ""
            in
            "  "
            ^ Display.pad_right 18 short_addr
            ^ " "
            ^ Display.pad_right 14 amount_str
            ^ " " ^ status_str ^ " " ^ note_str)
          results
      in
      let content = String.concat "\n" ([summary_line; ""; header] @ rows) in
      Box.render ~title:"Payout Report" ~style:Rounded ~width:box_width content

let render_cycle_detail ~box_width ~instance ~baker:_
    (state : Rewards_state.state) cycle =
  let cr = Rewards_scheduler.get_cycle_data ~instance ~cycle in
  let header =
    Widgets.themed_primary (Printf.sprintf " Cycle %d Detail " cycle)
  in
  let back_hint = Widgets.themed_muted "  Press Esc to return to dashboard" in
  let detail_box =
    match cr with
    | None ->
        Box.render
          ~title:(Printf.sprintf "Cycle %d" cycle)
          ~style:Rounded
          ~width:box_width
          (Widgets.themed_muted "No data available for this cycle")
    | Some cr ->
        let total_rewards = Rewards.total_earned cr in
        let delegator_count = cr.num_delegators in
        let status =
          Rewards_scheduler.get_payout_status ~instance ~cycle:cr.cycle
        in
        let status_label =
          if delegator_count = 0 then Widgets.themed_muted "N/A (no delegators)"
          else
            match status with
            | Rewards.Paid -> Widgets.themed_success "Paid"
            | Rewards.Unpaid -> Widgets.themed_warning "Unpaid"
            | Rewards.Partial -> Widgets.themed_warning "Partial"
            | Rewards.In_progress -> Widgets.themed_accent "In progress"
        in
        let items =
          [
            ( "Earned Rewards",
              Rewards.format_tez total_rewards ^ " \xEA\x9C\xA9" );
            ( "Block Rewards",
              Rewards.format_tez cr.block_rewards ^ " \xEA\x9C\xA9" );
            ( "Attestation",
              Rewards.format_tez cr.attestation_rewards ^ " \xEA\x9C\xA9" );
            ("DAL Rewards", Rewards.format_tez cr.dal_rewards ^ " \xEA\x9C\xA9");
            ("Block Fees", Rewards.format_tez cr.block_fees ^ " \xEA\x9C\xA9");
            ("Delegators", string_of_int delegator_count);
            ("Payout Status", status_label);
          ]
        in
        let desc =
          Desc_list.create ~key_width:16 ~items ()
          |> Desc_list.render ~cols:(box_width - 4) ~wrap:true ~focus:false
        in
        Box.render
          ~title:(Printf.sprintf "Cycle %d" cycle)
          ~style:Rounded
          ~width:box_width
          desc
  in
  let preview_box =
    match state.blueprint with
    | Some bp when bp.Rewards.cycle = cycle ->
        render_blueprint_box ~box_width bp
    | _ -> Widgets.themed_muted "  Press g to generate payout preview"
  in
  let report_box = render_payout_report_box ~box_width ~instance ~cycle in
  let parts = [header; back_hint; ""; detail_box; ""; preview_box] in
  let parts =
    if String.length report_box > 0 then parts @ [""; report_box] else parts
  in
  String.concat "\n" parts

let resolve_network ~instance =
  match Octez_manager_lib.Service_registry.find ~instance with
  | Ok (Some svc) -> svc.Octez_manager_lib.Service.network
  | _ -> (
      match Rewards_scheduler.get_network_for_instance ~instance with
      | Some n -> n
      | None -> "unknown")

let cycle_count ~instance =
  let cycles = Rewards_scheduler.get_recent_cycles ~instance in
  List.length cycles

let render_dashboard ~box_width ~instance ~baker:_ (state : Rewards_state.state)
    =
  let network = resolve_network ~instance in
  let recent = Rewards_scheduler.get_recent_cycles ~instance in
  let current_cycle = state.current_cycle in
  let last_completed =
    match current_cycle with
    | Some cc ->
        List.find_opt (fun (cr : Rewards.cycle_rewards) -> cr.cycle < cc) recent
    | None -> List.nth_opt recent 0
  in
  let network_line = render_network_badge network in
  let current_box =
    render_current_cycle_box ~box_width ~instance current_cycle
  in
  let completed_box =
    render_last_completed_box ~box_width ~instance last_completed
  in
  let recent_box =
    render_recent_cycles_box ~box_width ~instance ~current_cycle ~state recent
  in
  let parts =
    [network_line; ""; current_box; ""; completed_box; ""; recent_box]
  in
  let parts =
    if state.overview_preview then
      match state.blueprint with
      | Some bp -> parts @ [""; render_blueprint_box ~box_width bp]
      | None -> parts @ [""; Widgets.themed_muted "  Generating preview..."]
    else parts
  in
  String.concat "\n" parts

let render ~(state : Rewards_state.state) ~cols =
  let box_width = min (cols - 2) 90 in
  match Rewards_state.selected_baker_instance state with
  | None ->
      String.concat
        "\n"
        [
          "";
          Widgets.themed_muted "  No baker instances configured.";
          Widgets.themed_muted "  Install a baker to view reward data.";
        ]
  | Some (instance, baker) -> (
      match state.selected_cycle with
      | Some cycle ->
          render_cycle_detail ~box_width ~instance ~baker state cycle
      | None -> render_dashboard ~box_width ~instance ~baker state)
