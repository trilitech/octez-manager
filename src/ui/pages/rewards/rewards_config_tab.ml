(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Configuration tab for the Rewards page. *)

open Octez_manager_rewards
module Widgets = Miaou_widgets_display.Widgets
module Box = Miaou_widgets_layout.Box_widget
module Desc_list = Miaou_widgets_display.Description_list

(* {1 Pending config edits from modal callbacks} *)

let pending_config : Payout_config.t option ref = ref None

let set_pending_config config = pending_config := Some config

let consume_pending_config () =
  let v = !pending_config in
  pending_config := None ;
  v

(* {1 Field definitions} *)

type field_id =
  | BakerFee
  | PayoutMode
  | PayoutKeyAlias
  | MinPayout
  | MinBalance
  | BelowMinDest
  | OverdelegationProtect
  | BakerPaysTxFee
  | BakerPaysAllocFee
  | IgnoreContracts
  | ContinualEnabled
  | ContinualInterval
  | ContinualOffset

let all_fields =
  [
    BakerFee;
    PayoutMode;
    PayoutKeyAlias;
    MinPayout;
    MinBalance;
    BelowMinDest;
    OverdelegationProtect;
    BakerPaysTxFee;
    BakerPaysAllocFee;
    IgnoreContracts;
    ContinualEnabled;
    ContinualInterval;
    ContinualOffset;
  ]

let field_count = List.length all_fields

let field_label = function
  | BakerFee -> "Baker Fee"
  | PayoutMode -> "Payout Mode"
  | PayoutKeyAlias -> "Payout Key"
  | MinPayout -> "Min Payout"
  | MinBalance -> "Min Balance"
  | BelowMinDest -> "Below Min Dest"
  | OverdelegationProtect -> "Overdelegation Prot"
  | BakerPaysTxFee -> "Baker Pays TX Fee"
  | BakerPaysAllocFee -> "Baker Pays Alloc Fee"
  | IgnoreContracts -> "Ignore Contracts"
  | ContinualEnabled -> "Continual Mode"
  | ContinualInterval -> "Continual Interval"
  | ContinualOffset -> "Continual Offset"

let field_hint = function
  | BakerFee ->
      "Percentage fee deducted from delegator rewards as baker compensation."
  | PayoutMode ->
      "Actual: pay based on real rewards received. Ideal: pay based on \
       expected rewards regardless of missed blocks."
  | PayoutKeyAlias -> "octez-client key alias used to sign payout transactions."
  | MinPayout ->
      "Delegators whose reward is below this threshold will not receive a \
       payout."
  | MinBalance ->
      "Delegators whose delegated balance is below this threshold are excluded."
  | BelowMinDest ->
      "What happens to rewards below the minimum: baker keeps them or they are \
       redistributed to eligible delegators."
  | OverdelegationProtect ->
      "When enabled, caps rewards if total delegation exceeds the baker's \
       staking capacity."
  | BakerPaysTxFee ->
      "When enabled, transaction fees are paid by the baker rather than \
       deducted from delegator rewards."
  | BakerPaysAllocFee ->
      "When enabled, the baker pays the 0.06 tz allocation fee for new \
       accounts."
  | IgnoreContracts ->
      "When enabled, smart contract delegators are excluded from payouts."
  | ContinualEnabled ->
      "Automatically trigger payouts when new cycles complete."
  | ContinualInterval ->
      "Pay every N cycles (e.g. 1 = every cycle, 2 = every other cycle)."
  | ContinualOffset ->
      "Offset within the interval to stagger payments (0 = first eligible \
       cycle)."

let tez_symbol = "\xEA\x9C\xA9"

let field_value (config : Payout_config.t) = function
  | BakerFee -> Printf.sprintf "%.1f%%" (config.baker_fee *. 100.0)
  | PayoutMode -> (
      match config.payout_mode with
      | Rewards.Actual -> "Actual"
      | Rewards.Ideal -> "Ideal")
  | PayoutKeyAlias -> config.payout_key_alias
  | MinPayout ->
      Rewards.format_tez config.min_payout ^ " " ^ tez_symbol ^ " (mutez)"
  | MinBalance ->
      Rewards.format_tez config.min_balance ^ " " ^ tez_symbol ^ " (mutez)"
  | BelowMinDest -> (
      match config.below_min_dest with
      | Rewards.Baker_keeps -> "Baker keeps"
      | Rewards.Redistribute -> "Redistribute")
  | OverdelegationProtect ->
      if config.overdelegation_protect then "\xe2\x9c\x93 Enabled"
      else "\xe2\x9c\x97 Disabled"
  | BakerPaysTxFee ->
      if config.baker_pays_tx_fee then "\xe2\x9c\x93 Yes" else "\xe2\x9c\x97 No"
  | BakerPaysAllocFee ->
      if config.baker_pays_alloc_fee then "\xe2\x9c\x93 Yes"
      else "\xe2\x9c\x97 No"
  | IgnoreContracts ->
      if config.ignore_contracts then "\xe2\x9c\x93 Yes" else "\xe2\x9c\x97 No"
  | ContinualEnabled ->
      if config.continual_enabled then "\xe2\x9c\x93 Enabled"
      else "\xe2\x9c\x97 Disabled"
  | ContinualInterval -> string_of_int config.continual_interval
  | ContinualOffset -> string_of_int config.continual_offset

(* {1 Field editing} *)

let edit_field (config : Payout_config.t) field =
  match field with
  | BakerFee ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Baker Fee (%)"
        ~initial:(Printf.sprintf "%.1f" (config.baker_fee *. 100.0))
        ~validator:(fun s ->
          match Float.of_string_opt s with
          | None -> Error "Must be a valid number"
          | Some f ->
              if f >= 0.0 && f <= 100.0 then Ok ()
              else Error "Must be between 0 and 100")
        ~on_submit:(fun s ->
          match Float.of_string_opt s with
          | Some f ->
              pending_config := Some {config with baker_fee = f /. 100.0}
          | None -> ())
        ()
  | PayoutMode ->
      Modal_helpers.open_choice_modal
        ~title:"Payout Mode"
        ~items:["Actual"; "Ideal"]
        ~to_string:(fun s -> s)
        ~on_select:(fun choice ->
          let mode =
            match choice with "Ideal" -> Rewards.Ideal | _ -> Rewards.Actual
          in
          pending_config := Some {config with payout_mode = mode})
        ()
  | PayoutKeyAlias ->
      Modal_helpers.prompt_text_modal
        ~title:"Payout Key Alias"
        ~initial:config.payout_key_alias
        ~on_submit:(fun s ->
          pending_config := Some {config with payout_key_alias = s})
        ()
  | MinPayout ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Min Payout (mutez)"
        ~initial:(Int64.to_string config.min_payout)
        ~validator:(fun s ->
          match Int64.of_string_opt s with
          | None -> Error "Must be a valid number"
          | Some i -> if i >= 0L then Ok () else Error "Must be non-negative")
        ~on_submit:(fun s ->
          match Int64.of_string_opt s with
          | Some i -> pending_config := Some {config with min_payout = i}
          | None -> ())
        ()
  | MinBalance ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Min Balance (mutez)"
        ~initial:(Int64.to_string config.min_balance)
        ~validator:(fun s ->
          match Int64.of_string_opt s with
          | None -> Error "Must be a valid number"
          | Some i -> if i >= 0L then Ok () else Error "Must be non-negative")
        ~on_submit:(fun s ->
          match Int64.of_string_opt s with
          | Some i -> pending_config := Some {config with min_balance = i}
          | None -> ())
        ()
  | BelowMinDest ->
      Modal_helpers.open_choice_modal
        ~title:"Below Minimum Destination"
        ~items:["Baker keeps"; "Redistribute"]
        ~to_string:(fun s -> s)
        ~on_select:(fun choice ->
          let dest =
            match choice with
            | "Redistribute" -> Rewards.Redistribute
            | _ -> Rewards.Baker_keeps
          in
          pending_config := Some {config with below_min_dest = dest})
        ()
  | OverdelegationProtect ->
      pending_config :=
        Some
          {
            config with
            overdelegation_protect = not config.overdelegation_protect;
          }
  | BakerPaysTxFee ->
      pending_config :=
        Some {config with baker_pays_tx_fee = not config.baker_pays_tx_fee}
  | BakerPaysAllocFee ->
      pending_config :=
        Some
          {config with baker_pays_alloc_fee = not config.baker_pays_alloc_fee}
  | IgnoreContracts ->
      pending_config :=
        Some {config with ignore_contracts = not config.ignore_contracts}
  | ContinualEnabled ->
      pending_config :=
        Some {config with continual_enabled = not config.continual_enabled}
  | ContinualInterval ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Continual Interval (cycles)"
        ~initial:(string_of_int config.continual_interval)
        ~validator:(fun s ->
          match int_of_string_opt s with
          | None -> Error "Must be a positive integer"
          | Some i -> if i >= 1 then Ok () else Error "Must be >= 1")
        ~on_submit:(fun s ->
          match int_of_string_opt s with
          | Some i when i >= 1 ->
              pending_config := Some {config with continual_interval = i}
          | _ -> ())
        ()
  | ContinualOffset ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Continual Offset (cycles)"
        ~initial:(string_of_int config.continual_offset)
        ~validator:(fun s ->
          match int_of_string_opt s with
          | None -> Error "Must be a non-negative integer"
          | Some i ->
              if i >= 0 && i < config.continual_interval then Ok ()
              else
                Error
                  (Printf.sprintf
                     "Must be in [0, %d)"
                     config.continual_interval))
        ~on_submit:(fun s ->
          match int_of_string_opt s with
          | Some i when i >= 0 ->
              pending_config := Some {config with continual_offset = i}
          | _ -> ())
        ()

(* {1 Save / Reset actions} *)

let save_config ~instance config =
  match Payout_config.validate config with
  | Error msg -> Context.toast_warn (Printf.sprintf "Validation error: %s" msg)
  | Ok () -> (
      match Payout_config.save ~instance config with
      | Ok () -> Context.toast_info "Configuration saved"
      | Error msg -> Context.toast_warn (Printf.sprintf "Save failed: %s" msg))

let reset_config ~baker_pkh =
  Modal_helpers.confirm_modal
    ~message:"Reset all settings to defaults?"
    ~on_result:(fun confirmed ->
      if confirmed then begin
        pending_config := Some (Payout_config.default ~baker_pkh) ;
        Context.toast_info "Configuration reset to defaults"
      end)
    ()

(* {1 Rendering} *)

let render ~(state : Rewards_state.state) ~cols ~_rows =
  let box_width = min (cols - 2) 72 in
  match state.config with
  | None ->
      String.concat
        "\n"
        [
          "";
          Widgets.themed_muted "  No baker selected.";
          Widgets.themed_muted "  Select a baker to configure payouts.";
        ]
  | Some config ->
      (* Render field list with cursor *)
      let field_lines =
        List.mapi
          (fun i field ->
            let label = field_label field in
            let value = field_value config field in
            let indicator =
              if i = state.config_cursor then "\xe2\x96\xb8" else " "
            in
            let line = Printf.sprintf "%s %-20s  %s" indicator label value in
            if i = state.config_cursor then Widgets.themed_emphasis line
            else Widgets.themed_text line)
          all_fields
      in
      let general_content = String.concat "\n" field_lines in
      let general_box =
        Box.render
          ~title:"General"
          ~style:Rounded
          ~width:box_width
          general_content
      in
      (* Hint panel: left-bordered block for the selected field *)
      let hint_box =
        if state.config_show_hint then
          let field = List.nth all_fields state.config_cursor in
          let bar = Widgets.themed_muted "\xe2\x94\x82 " in
          let title_line = bar ^ Widgets.themed_emphasis (field_label field) in
          let hint_width = max 20 (box_width - 6) in
          let wrapped =
            Widgets.wrap_text ~width:hint_width (field_hint field)
          in
          let text_lines =
            List.map (fun l -> bar ^ Widgets.themed_text l) wrapped
          in
          String.concat
            "\n"
            (("  " ^ title_line) :: List.map (fun l -> "  " ^ l) text_lines)
        else ""
      in
      (* Delegator overrides section *)
      let override_count = List.length config.delegator_overrides in
      let override_content =
        if override_count = 0 then Widgets.themed_muted "  No overrides defined"
        else
          let lines =
            List.map
              (fun (addr, (ov : Rewards.delegator_override)) ->
                let short =
                  if String.length addr > 14 then
                    String.sub addr 0 8 ^ ".."
                    ^ String.sub addr (String.length addr - 4) 4
                  else addr
                in
                let fee_str =
                  match ov.custom_fee with
                  | Some f -> Printf.sprintf "fee: %.1f%%" (f *. 100.0)
                  | None -> ""
                in
                let redirect_str =
                  match ov.redirect_to with
                  | Some r ->
                      let short_r =
                        if String.length r > 14 then
                          String.sub r 0 8 ^ ".."
                          ^ String.sub r (String.length r - 4) 4
                        else r
                      in
                      "redirect: " ^ short_r
                  | None -> ""
                in
                let parts =
                  List.filter
                    (fun s -> String.length s > 0)
                    [fee_str; redirect_str]
                in
                let detail =
                  if parts = [] then "" else "  " ^ String.concat "  " parts
                in
                Widgets.themed_text (Printf.sprintf "  %s%s" short detail))
              config.delegator_overrides
          in
          String.concat "\n" lines
      in
      let override_title =
        Printf.sprintf "Delegator Overrides (%d)" override_count
      in
      let override_box =
        Box.render
          ~title:override_title
          ~style:Rounded
          ~width:box_width
          override_content
      in
      (* Dirty indicator *)
      let dirty_indicator =
        if state.config_dirty then Widgets.themed_warning "  * Unsaved changes"
        else ""
      in
      let parts =
        if String.length hint_box > 0 then
          [""; general_box; hint_box; ""; override_box]
        else [""; general_box; ""; override_box]
      in
      let parts =
        if dirty_indicator <> "" then parts @ [dirty_indicator] else parts
      in
      String.concat "\n" parts
