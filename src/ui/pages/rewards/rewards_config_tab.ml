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

(* Pending config that doesn't trigger dirty flag (for install/remove operations
   where config is already saved to disk) *)
let pending_config_clean : Payout_config.t option ref = ref None

let set_pending_config_clean config = pending_config_clean := Some config

let consume_pending_config_clean () =
  let v = !pending_config_clean in
  pending_config_clean := None ;
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

(* {1 Payout Service Modals and Actions} *)

let open_payout_service_detail_only ~instance =
  let lines = ref [] in
  let add s = lines := s :: !lines in
  let add_blank () = add "" in
  add "═══ Payout Service Status ═══" ;
  add_blank () ;
  let timer_active =
    Octez_manager_lib.Systemd.is_payout_timer_active ~instance
  in
  add
    (Printf.sprintf
       "  Timer:      %s"
       (if timer_active then "\xe2\x97\x8f Active" else "\xe2\x97\x8b Inactive")) ;
  (match Octez_manager_lib.Systemd.get_payout_last_run ~instance with
  | Some info ->
      let status =
        if info.success then "\xe2\x9c\x93 Success" else "\xe2\x9c\x97 Failed"
      in
      add (Printf.sprintf "  Last run:   %s" info.timestamp) ;
      add (Printf.sprintf "  Result:     %s" status)
  | None -> add "  Last run:   Never") ;
  add_blank () ;
  add "═══ Timer Details ═══" ;
  add_blank () ;
  (match Octez_manager_lib.Systemd.get_payout_timer_next ~instance with
  | Some next -> add (Printf.sprintf "  Next trigger: %s" next)
  | None -> add "  Next trigger: Unknown") ;
  add_blank () ;
  add "═══ Service Configuration ═══" ;
  add_blank () ;
  (match Octez_manager_lib.Systemd.cat_payout_service ~instance with
  | Ok content ->
      String.split_on_char '\n' content |> List.iter (fun l -> add ("  " ^ l))
  | Error (`Msg msg) ->
      add (Printf.sprintf "  (Could not read unit file: %s)" msg)) ;
  Modal_helpers.open_text_modal
    ~title:"Payout Service Details"
    ~lines:(List.rev !lines)

let open_payout_service_logs ~instance =
  match Octez_manager_lib.Systemd.get_payout_service_logs ~instance ~n:200 with
  | Ok output ->
      let lines =
        if String.length (String.trim output) = 0 then
          ["(No log entries found)"]
        else String.split_on_char '\n' output
      in
      Modal_helpers.open_text_modal ~title:"Payout Service Logs" ~lines
  | Error (`Msg msg) ->
      Modal_helpers.open_text_modal
        ~title:"Payout Service Logs"
        ~lines:[Printf.sprintf "Could not fetch logs: %s" msg]

let install_payout_service ~instance ~baker_pkh ~config =
  Modal_helpers.confirm_modal
    ~title:"Install Payout Service"
    ~message:
      "This will install a systemd timer that automatically\n\
       pays delegator rewards every 5 minutes when due cycles\n\
       are detected.\n\n\
       The service runs independently of octez-manager.\n\
       You can remove it later from this menu."
    ~on_result:(fun confirmed ->
      if confirmed then
        let octez_manager_bin =
          match Sys.executable_name with "" -> "octez-manager" | path -> path
        in
        let service_user =
          if Paths.is_root () then
            Octez_manager_lib.Systemd.get_service_user ~role:"baker" ~instance
          else None
        in
        let open Rresult.R.Infix in
        match
          Octez_manager_lib.Systemd.write_payout_service
            ~instance
            ~octez_manager_bin
            ~service_user
            ()
          >>= fun () ->
          Octez_manager_lib.Systemd.write_payout_timer ~instance ()
          >>= fun () -> Octez_manager_lib.Systemd.enable_payout_timer ~instance
        with
        | Ok () -> (
            let cfg =
              match config with
              | Some c -> c
              | None -> Payout_config.default ~baker_pkh
            in
            let cfg = {cfg with Payout_config.continual_enabled = true} in
            match Payout_config.save ~instance cfg with
            | Ok () ->
                set_pending_config_clean cfg ;
                Rewards_scheduler.set_payout_timer_active ~instance ~active:true ;
                Rewards_scheduler.set_continual_interval
                  ~instance
                  ~interval:cfg.continual_interval ;
                Context.toast_info "Payout service installed and enabled"
            | Error msg ->
                Context.toast_error
                  (Printf.sprintf "Config save failed: %s" msg))
        | Error (`Msg msg) ->
            Context.toast_error (Printf.sprintf "Install failed: %s" msg))
    ()

let remove_payout_service ~instance ~config =
  Modal_helpers.confirm_modal
    ~title:"Remove Payout Service"
    ~message:
      "Remove the payout systemd timer and service?\n\
       This will stop automatic payouts."
    ~on_result:(fun confirmed ->
      if confirmed then begin
        (match Octez_manager_lib.Systemd.disable_payout_timer ~instance with
        | Ok () -> ()
        | Error (`Msg msg) ->
            Context.toast_warn (Printf.sprintf "Timer disable warning: %s" msg)) ;
        Octez_manager_lib.Systemd.remove_payout_units ~instance ;
        let cfg =
          match config with
          | Some c -> c
          | None -> Payout_config.default ~baker_pkh:""
        in
        let cfg = {cfg with Payout_config.continual_enabled = false} in
        match Payout_config.save ~instance cfg with
        | Ok () ->
            set_pending_config_clean cfg ;
            Rewards_scheduler.set_payout_timer_active ~instance ~active:false ;
            Context.toast_info "Payout service removed"
        | Error msg ->
            Context.toast_error (Printf.sprintf "Config save failed: %s" msg)
      end)
    ()

let open_payout_service_actions ~instance ~baker_pkh ~config =
  let timer_active =
    Octez_manager_lib.Systemd.is_payout_timer_active ~instance
  in
  let items =
    if timer_active then ["Details"; "Logs"; "Remove"]
    else ["Install"; "Details"; "Logs"]
  in
  (* Filter: only show Details/Logs if service was ever installed *)
  let has_service =
    timer_active
    ||
    match config with
    | Some c -> c.Payout_config.continual_enabled
    | None -> false
  in
  let items = if has_service then items else ["Install"] in
  Modal_helpers.open_choice_modal
    ~title:"Payout Service"
    ~items
    ~to_string:Fun.id
    ~on_select:(fun choice ->
      match choice with
      | "Details" -> open_payout_service_detail_only ~instance
      | "Logs" -> open_payout_service_logs ~instance
      | "Install" -> install_payout_service ~instance ~baker_pkh ~config
      | "Remove" -> remove_payout_service ~instance ~config
      | _ -> ())
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
        if state.config_cursor = field_count then
          (* Payout service is selected — show hint for it *)
          let bar = Widgets.themed_muted "\xe2\x94\x82 " in
          let title_line = bar ^ Widgets.themed_emphasis "Payout Service" in
          let hint_width = max 20 (box_width - 6) in
          let wrapped =
            Widgets.wrap_text
              ~width:hint_width
              "Manage the systemd payout timer. Press Enter to view details, \
               logs, or install/remove the service."
          in
          let text_lines =
            List.map (fun l -> bar ^ Widgets.themed_text l) wrapped
          in
          String.concat
            "\n"
            (("  " ^ title_line) :: List.map (fun l -> "  " ^ l) text_lines)
        else
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
      (* Payout Service section *)
      let payout_service_box =
        match Rewards_state.selected_instance_name state with
        | None -> ""
        | Some instance ->
            let is_selected = state.config_cursor = field_count in
            let timer_active =
              Rewards_scheduler.get_payout_timer_active ~instance
            in
            let status_line =
              if timer_active then
                let interval_str =
                  match Rewards_scheduler.get_continual_interval ~instance with
                  | Some interval when interval > 1 ->
                      Printf.sprintf " (every %d cycles)" interval
                  | _ -> ""
                in
                "  "
                ^ Widgets.themed_success "\xe2\x97\x8f Active"
                ^ interval_str
              else if config.continual_enabled then
                "  " ^ Widgets.themed_error "\xe2\x97\x8b Stopped"
              else "  " ^ Widgets.themed_muted "\xe2\x97\x8b Not installed"
            in
            let last_run_line =
              if timer_active then
                match Rewards_scheduler.get_payout_last_run ~instance with
                | Some info ->
                    let status_icon =
                      if info.success then "\xe2\x9c\x93" else "\xe2\x9c\x97"
                    in
                    let style =
                      if info.success then Widgets.themed_success
                      else Widgets.themed_error
                    in
                    "  "
                    ^ style
                        (Printf.sprintf
                           "%s Last run: %s"
                           status_icon
                           info.timestamp)
                | None -> "  " ^ Widgets.themed_muted "No runs yet"
              else ""
            in
            let hint_line = Widgets.themed_muted "  Press Enter for actions" in
            let content_parts =
              let parts = [status_line] in
              let parts =
                if String.length last_run_line > 0 then parts @ [last_run_line]
                else parts
              in
              parts @ [""; hint_line]
            in
            let content = String.concat "\n" content_parts in
            let title =
              let indicator = if is_selected then "\xe2\x96\xb8 " else "" in
              Printf.sprintf "%sPayout Service" indicator
            in
            let style = if is_selected then Box.Double else Box.Rounded in
            Box.render ~title ~style ~width:box_width content
      in
      (* Dirty indicator *)
      let dirty_indicator =
        if state.config_dirty then
          Widgets.themed_warning "  * Unsaved changes"
          ^ Widgets.themed_muted "  [s: save]"
        else ""
      in
      let parts = [""; general_box; hint_box; ""; override_box] in
      let parts =
        if String.length payout_service_box > 0 then
          parts @ [""; payout_service_box]
        else parts
      in
      let parts =
        if dirty_indicator <> "" then parts @ [dirty_indicator] else parts
      in
      String.concat "\n" parts
