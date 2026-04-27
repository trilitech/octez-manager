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

let render_recent_cycles_box ~box_width ~instance ~current_cycle ~cursor
    (cycles : Rewards.cycle_rewards list) =
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
        ^ Display.pad_right 16 "EARNED"
        ^ " "
        ^ Display.pad_right 14 "DISTRIBUTED"
        ^ " STATUS"
      in
      let rows =
        List.mapi
          (fun i (cr : Rewards.cycle_rewards) ->
            let is_current =
              match current_cycle with
              | Some cc -> Int.equal cr.cycle cc
              | None -> false
            in
            let cycle_label =
              if is_current then string_of_int cr.cycle ^ " \xe2\x97\x80"
              else string_of_int cr.cycle
            in
            let earned =
              format_tez_short (Rewards.total_earned cr) ^ " \xEA\x9C\xA9"
            in
            let status =
              Rewards_scheduler.get_payout_status ~instance ~cycle:cr.cycle
            in
            let distributed =
              match
                Rewards_scheduler.get_payout_summary ~instance ~cycle:cr.cycle
              with
              | Some s ->
                  format_tez_short s.distributed_rewards ^ " \xEA\x9C\xA9"
              | None -> "\xE2\x80\x94"
            in
            let status_str =
              match status with
              | Rewards.Paid -> Widgets.themed_success "paid"
              | Rewards.Unpaid -> Widgets.themed_muted "unpaid"
              | Rewards.Partial -> Widgets.themed_warning "partial"
              | Rewards.In_progress -> Widgets.themed_accent "in progress"
            in
            let indicator = if i = cursor then "\xe2\x96\xb8 " else "  " in
            let line =
              indicator
              ^ Display.pad_right 7 cycle_label
              ^ " "
              ^ Display.pad_right 16 earned
              ^ " "
              ^ Display.pad_right 14 distributed
              ^ " " ^ status_str
            in
            if i = cursor then Widgets.themed_emphasis line
            else if is_current then Widgets.themed_accent line
            else Widgets.themed_text line)
          cycles
      in
      let content = String.concat "\n" (Widgets.themed_muted header :: rows) in
      Box.render ~title:"Recent Cycles" ~style:Rounded ~width:box_width content

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
  String.concat "\n" [header; back_hint; ""; detail_box; ""; preview_box]

let render_setup_cta_box ~box_width =
  let lines =
    [
      Widgets.themed_text "No payout configuration yet for this baker.";
      "";
      Widgets.themed_text "Press Tab to open the Configuration tab and set:";
      Widgets.themed_muted
        "  - payout key alias       (which key signs payouts)";
      Widgets.themed_muted "  - baker fee              (% kept by the baker)";
      Widgets.themed_muted "  - min payout / balance   (skip dust)";
      Widgets.themed_muted "  - notification channels  (Discord/Telegram/...)";
      "";
      Widgets.themed_muted
        "Once saved, Generate / Pay / Dry-run become available here.";
    ]
  in
  Box.render
    ~title:"Set up rewards"
    ~style:Rounded
    ~width:box_width
    (String.concat "\n" lines)

let resolve_network ~instance =
  match Octez_manager_lib.Service_registry.find ~instance with
  | Ok (Some svc) -> svc.Octez_manager_lib.Service.network
  | _ -> (
      match Rewards_scheduler.get_network_for_instance ~instance with
      | Some n -> n
      | None -> "unknown")

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
    render_recent_cycles_box
      ~box_width
      ~instance
      ~current_cycle
      ~cursor:state.cycle_cursor
      recent
  in
  let setup_cta =
    if state.config_exists then [] else [render_setup_cta_box ~box_width; ""]
  in
  let parts =
    setup_cta
    @ [network_line; ""; current_box; ""; completed_box; ""; recent_box]
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
  let box_width = min (cols - 2) 72 in
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
