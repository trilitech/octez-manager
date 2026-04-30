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
  (* Custom-baker registry fields (custom bakers only) *)
  | CustomBakerPkh
  | CustomNetwork
  | CustomLabel
  | CustomEndpoint
  | CustomBaseDir
  | CustomPayoutKey
  (* Payout config fields (all bakers) *)
  | BakerFee
  | PayoutKeyAlias
  | IndexerUrl
  | MinPayout
  | MinBalance
  | BelowMinDest
  | OverdelegationProtect
  | IgnoreContracts
  | ContinualEnabled
  | ContinualInterval
  | ContinualOffset

let custom_baker_fields =
  [
    CustomBakerPkh;
    CustomNetwork;
    CustomLabel;
    CustomEndpoint;
    CustomBaseDir;
    CustomPayoutKey;
  ]

let payout_fields =
  [
    BakerFee;
    PayoutKeyAlias;
    IndexerUrl;
    MinPayout;
    MinBalance;
    BelowMinDest;
    OverdelegationProtect;
    IgnoreContracts;
    ContinualEnabled;
    ContinualInterval;
    ContinualOffset;
  ]

let is_read_only = function
  | CustomBakerPkh | CustomNetwork -> true
  | _ -> false

(** Look up the [Custom_baker_registry] entry for the currently selected
    baker, if it is a custom baker. *)
let custom_entry_for_state (state : Rewards_state.state) =
  match Rewards_state.selected_instance_name state with
  | None -> None
  | Some instance -> Custom_baker_registry.find ~instance

let fields_for_state state =
  match custom_entry_for_state state with
  | Some _ -> custom_baker_fields @ payout_fields
  | None -> payout_fields

let field_count_for_state state = List.length (fields_for_state state)

let field_label = function
  | CustomBakerPkh -> "Baker PKH"
  | CustomNetwork -> "Network"
  | CustomLabel -> "Label"
  | CustomEndpoint -> "RPC Endpoint"
  | CustomBaseDir -> "Base Directory"
  | CustomPayoutKey -> "Payout Key (wallet)"
  | BakerFee -> "Baker Fee"
  | PayoutKeyAlias -> "Payout Key"
  | IndexerUrl -> "Indexer URL"
  | MinPayout -> "Min Payout"
  | MinBalance -> "Min Balance"
  | BelowMinDest -> "Below Min Dest"
  | OverdelegationProtect -> "Overdelegation Prot"
  | IgnoreContracts -> "Ignore Contracts"
  | ContinualEnabled -> "Continual Mode"
  | ContinualInterval -> "Continual Interval"
  | ContinualOffset -> "Continual Offset"

let field_hint = function
  | CustomBakerPkh ->
      "Public key hash of the baker. Read-only — to change it, remove and \
       re-add this custom baker."
  | CustomNetwork ->
      "Network identifier of the baker. Read-only — to change it, remove and \
       re-add this custom baker."
  | CustomLabel -> "Optional human-readable label shown in the baker selector."
  | CustomEndpoint ->
      "RPC endpoint used to query the baker's chain (host:port)."
  | CustomBaseDir ->
      "octez-client base directory holding the wallet that signs payouts."
  | CustomPayoutKey ->
      "Alias of the wallet key used to sign payout transactions, looked up in \
       the base directory above."
  | BakerFee ->
      "Percentage fee deducted from delegator rewards as baker compensation."
  | PayoutKeyAlias -> "octez-client key alias used to sign payout transactions."
  | IndexerUrl ->
      "Base URL of the indexer used to fetch cycle rewards and delegator data. \
       Accepts a public TzKT instance (e.g. https://api.tzkt.io) or a \
       self-hosted TzKT-compatible indexer such as octez-index."
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

let custom_field_value (entry : Custom_baker_registry.entry) = function
  | CustomBakerPkh -> entry.baker_pkh
  | CustomNetwork -> entry.network
  | CustomLabel -> Option.value ~default:"(none)" entry.label
  | CustomEndpoint -> entry.endpoint
  | CustomBaseDir -> entry.base_dir
  | CustomPayoutKey -> entry.payout_key_alias
  | _ -> ""

let field_value ?custom (config : Payout_config.t) field =
  match field with
  | CustomBakerPkh | CustomNetwork | CustomLabel | CustomEndpoint
  | CustomBaseDir | CustomPayoutKey -> (
      match custom with
      | Some entry -> custom_field_value entry field
      | None -> "")
  | BakerFee -> Printf.sprintf "%.1f%%" (config.baker_fee *. 100.0)
  | PayoutKeyAlias -> config.payout_key_alias
  | IndexerUrl -> config.tzkt_url
  | MinPayout -> Rewards.format_tez config.min_payout ^ " " ^ tez_symbol
  | MinBalance -> Rewards.format_tez config.min_balance ^ " " ^ tez_symbol
  | BelowMinDest -> (
      match config.below_min_dest with
      | Rewards.Baker_keeps -> "Baker keeps"
      | Rewards.Redistribute -> "Redistribute")
  | OverdelegationProtect ->
      if config.overdelegation_protect then "\xe2\x9c\x93 Enabled"
      else "\xe2\x9c\x97 Disabled"
  | IgnoreContracts ->
      if config.ignore_contracts then "\xe2\x9c\x93 Yes" else "\xe2\x9c\x97 No"
  | ContinualEnabled ->
      if config.continual_enabled then "\xe2\x9c\x93 Enabled"
      else "\xe2\x9c\x97 Disabled"
  | ContinualInterval -> string_of_int config.continual_interval
  | ContinualOffset -> string_of_int config.continual_offset

(* {1 Field editing} *)

type indexer_choice =
  | Local_indexer of {instance : string; endpoint : string}
  | Tzkt_default of string
  | Custom_url

let prompt_custom_indexer_url config =
  Modal_helpers.prompt_validated_text_modal
    ~title:"Indexer URL (TzKT or octez-index)"
    ~initial:config.Payout_config.tzkt_url
    ~validator:(fun s ->
      let s = String.trim s in
      if String.length s = 0 then Error "URL must not be empty"
      else if
        (not (String.starts_with ~prefix:"http://" s))
        && not (String.starts_with ~prefix:"https://" s)
      then Error "URL must start with http:// or https://"
      else Ok ())
    ~on_submit:(fun s ->
      let url = String.trim s in
      let url =
        if String.length url > 0 && url.[String.length url - 1] = '/' then
          String.sub url 0 (String.length url - 1)
        else url
      in
      pending_config := Some {config with tzkt_url = url})
    ()

(** Extract a network slug from values like ["https://teztnets.com/bakingnet"]
    or ["bakingnet"]. Returns [None] if the value does not look like a usable
    slug (e.g. an unrecognised URL with extra path components). *)
let network_slug network =
  let n = Network_name.normalize network in
  let looks_like_slug s =
    String.length s > 0
    && (not (String.contains s '/'))
    && not (String.contains s ':')
  in
  if looks_like_slug n then Some n
  else
    match String.rindex_opt n '/' with
    | Some i when i + 1 < String.length n ->
        let tail = String.sub n (i + 1) (String.length n - i - 1) in
        if looks_like_slug tail then Some tail else None
    | _ -> None

let local_indexers_for_network ~network =
  (* The baker's [network] may be a slug ("tallinnnet") while a local indexer
     service's [svc.network] is often a full teztnets URL
     ("https://teztnets.com/tallinnnet"). Normalize both to a slug via
     {!network_slug} before comparing — falling back to lowercased exact
     equality when one side cannot be reduced to a slug. *)
  let net_match svc_net baker_net =
    match (network_slug svc_net, network_slug baker_net) with
    | Some a, Some b ->
        String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)
    | _ ->
        String.equal
          (String.lowercase_ascii svc_net)
          (String.lowercase_ascii baker_net)
  in
  match Octez_manager_lib.Service_registry.list () with
  | Error _ -> []
  | Ok svcs ->
      List.filter
        (fun (svc : Octez_manager_lib.Service.t) ->
          String.equal svc.role "index"
          &&
          match network with
          | None -> true
          | Some n -> net_match svc.network n)
        svcs

(** Apply a mutation to an existing custom-baker entry: write it back via
    {!Custom_baker_registry.update} and refresh the Rewards page on success. *)
let update_custom_entry (entry : Custom_baker_registry.entry) =
  match Custom_baker_registry.update entry with
  | Ok () ->
      Context.toast_info "Custom baker updated" ;
      Context.navigate "rewards"
  | Error msg ->
      Context.toast_error
        (Printf.sprintf "Failed to update custom baker: %s" msg)

let edit_custom_field (entry : Custom_baker_registry.entry) field =
  match field with
  | CustomBakerPkh | CustomNetwork ->
      (* Read-only — changing these would change the synthetic instance handle
         that keys per-instance state across the rewards pipeline. *)
      ()
  | CustomLabel ->
      Modal_helpers.prompt_text_modal
        ~title:"Label"
        ~initial:(Option.value ~default:"" entry.label)
        ~placeholder:(Some "(optional)")
        ~on_submit:(fun s ->
          let label =
            let s = String.trim s in
            if String.length s = 0 then None else Some s
          in
          update_custom_entry {entry with label})
        ()
  | CustomEndpoint ->
      Custom_baker_modals.prompt_endpoint
        ~title:"RPC Endpoint"
        ~network:entry.network
        ~on_submit:(fun endpoint -> update_custom_entry {entry with endpoint})
        ()
  | CustomBaseDir ->
      Modal_helpers.select_client_base_dir_modal
        ~on_select:(fun base_dir -> update_custom_entry {entry with base_dir})
        ()
  | CustomPayoutKey ->
      Custom_baker_modals.prompt_payout_key
        ~title:"Payout Key"
        ~base_dir:entry.base_dir
        ~on_submit:(fun payout_key_alias ->
          update_custom_entry {entry with payout_key_alias})
        ()
  | _ -> ()

let edit_field ?network ?custom (config : Payout_config.t) field =
  match field with
  | CustomBakerPkh | CustomNetwork | CustomLabel | CustomEndpoint
  | CustomBaseDir | CustomPayoutKey -> (
      match custom with
      | Some entry -> edit_custom_field entry field
      | None -> ())
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
  | PayoutKeyAlias ->
      Modal_helpers.select_client_base_dir_modal
        ~on_select:(fun base_dir ->
          Custom_baker_modals.prompt_payout_key
            ~title:"Payout Key"
            ~base_dir
            ~on_submit:(fun s ->
              pending_config := Some {config with payout_key_alias = s})
            ())
        ()
  | IndexerUrl ->
      let indexers = local_indexers_for_network ~network in
      let tzkt_choice =
        let slug =
          Option.bind network network_slug |> Option.value ~default:"mainnet"
        in
        Tzkt_default (Payout_config.tzkt_base_url_for_network slug)
      in
      let choices =
        List.map
          (fun (svc : Octez_manager_lib.Service.t) ->
            Local_indexer
              {
                instance = svc.instance;
                endpoint = Octez_manager_lib.Rpc_addr.to_endpoint svc.rpc_addr;
              })
          indexers
        @ [tzkt_choice; Custom_url]
      in
      Modal_helpers.open_choice_modal
        ~title:"Indexer URL"
        ~items:choices
        ~to_string:(function
          | Local_indexer {instance; endpoint} ->
              Printf.sprintf "%s  (%s)" instance endpoint
          | Tzkt_default url -> Printf.sprintf "TzKT  (%s)" url
          | Custom_url -> "Custom URL...")
        ~on_select:(fun choice ->
          match choice with
          | Local_indexer {endpoint; _} ->
              pending_config := Some {config with tzkt_url = endpoint}
          | Tzkt_default url ->
              pending_config := Some {config with tzkt_url = url}
          | Custom_url -> prompt_custom_indexer_url config)
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
      | "Details" ->
          Context.set_pending_payout_service instance ;
          Payout_service_page.set_initial_tab Details ;
          Context.navigate Payout_service_page.name
      | "Logs" ->
          Context.set_pending_payout_service instance ;
          Payout_service_page.set_initial_tab Logs ;
          Context.navigate Payout_service_page.name
      | "Install" -> install_payout_service ~instance ~baker_pkh ~config
      | "Remove" -> remove_payout_service ~instance ~config
      | _ -> ())
    ()

(* {1 Rendering} *)

let render ~(state : Rewards_state.state) ~cols ~_rows =
  let box_width = min (cols - 2) 72 in
  let custom = custom_entry_for_state state in
  let fields = fields_for_state state in
  let field_count = List.length fields in
  (* When a custom baker without a saved Payout_config is selected, synthesize
     a default config so the payout rows still render with sensible values.
     Seed [payout_key_alias] from the registry entry so it shows the alias the
     user picked during [Add custom baker], not the baker PKH default. *)
  let config_opt =
    match (state.config, custom) with
    | Some c, _ -> Some c
    | None, Some entry ->
        Some
          {
            (Payout_config.default ~baker_pkh:entry.baker_pkh) with
            payout_key_alias = entry.payout_key_alias;
          }
    | None, None -> None
  in
  match config_opt with
  | None ->
      String.concat
        "\n"
        [
          "";
          Widgets.themed_muted "  No baker selected.";
          Widgets.themed_muted "  Select a baker to configure payouts.";
        ]
  | Some config ->
      (* Render the custom-baker section as a separate box when applicable. *)
      let custom_box =
        match custom with
        | None -> ""
        | Some _ ->
            let custom_lines =
              List.mapi
                (fun i field ->
                  let label = field_label field in
                  let value = field_value ?custom config field in
                  let indicator =
                    if i = state.config_cursor then "\xe2\x96\xb8" else " "
                  in
                  let line =
                    Printf.sprintf "%s %-20s  %s" indicator label value
                  in
                  if i = state.config_cursor then Widgets.themed_emphasis line
                  else if is_read_only field then Widgets.themed_muted line
                  else Widgets.themed_text line)
                custom_baker_fields
            in
            Box.render
              ~title:"Custom Baker"
              ~style:Rounded
              ~width:box_width
              (String.concat "\n" custom_lines)
      in
      (* Render the payout-config section. Cursor offset accounts for any
         custom-baker rows rendered above. *)
      let payout_offset =
        match custom with
        | Some _ -> List.length custom_baker_fields
        | None -> 0
      in
      let field_lines =
        List.mapi
          (fun i field ->
            let cursor_idx = i + payout_offset in
            let label = field_label field in
            let value = field_value ?custom config field in
            let indicator =
              if cursor_idx = state.config_cursor then "\xe2\x96\xb8" else " "
            in
            let line = Printf.sprintf "%s %-20s  %s" indicator label value in
            if cursor_idx = state.config_cursor then
              Widgets.themed_emphasis line
            else Widgets.themed_text line)
          payout_fields
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
          let field = List.nth fields state.config_cursor in
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
      (* Status indicator: create (never saved) takes priority over dirty *)
      let status_indicator =
        if not state.config_exists then
          Widgets.themed_warning "  * Not yet saved"
          ^ Widgets.themed_muted "  [c: create]"
        else if state.config_dirty then
          Widgets.themed_warning "  * Unsaved changes"
          ^ Widgets.themed_muted "  [s: save]"
        else ""
      in
      let parts =
        if String.length custom_box > 0 then
          [""; custom_box; ""; general_box; hint_box; ""; override_box]
        else [""; general_box; hint_box; ""; override_box]
      in
      let parts =
        if String.length payout_service_box > 0 then
          parts @ [""; payout_service_box]
        else parts
      in
      let parts =
        if status_indicator <> "" then parts @ [status_indicator] else parts
      in
      String.concat "\n" parts
