(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker wallet modal implementation.

    Displays wallet balances, delegate status, staking parameters, and an
    operations menu. Data is read from Baker_wallet_data cache (no I/O). *)

open Octez_manager_lib
module Widgets = Miaou_widgets_display.Widgets
module Box = Miaou_widgets_layout.Box_widget
module Desc_list = Miaou_widgets_display.Description_list
module Navigation = Miaou.Core.Navigation
module Select_widget = Miaou_widgets_input.Select_widget
module Flex = Miaou_widgets_layout.Flex_layout
module Keys = Miaou.Core.Keys
module Modal_manager = Miaou.Core.Modal_manager

(* ── Wallet action menu items ────────────────────────────── *)

type wallet_action =
  | Stake
  | Unstake
  | Finalize_unstake
  | Transfer
  | Set_delegate_params
  | Update_consensus_key
  | Register
  | Vote

(* ── Helpers ─────────────────────────────────────────────── *)

let truncate_pkh pkh =
  if String.length pkh > 12 then
    String.sub pkh 0 7 ^ "..." ^ String.sub pkh (String.length pkh - 4) 4
  else pkh

(* ── Wallet modal view rendering ─────────────────────────── *)

let render_balance_box ~cols (data : Baker_wallet_data.t) =
  let items =
    [
      ("Spendable", Baker_wallet_data.format_tez data.spendable_balance);
      ("Staked", Baker_wallet_data.format_tez data.staked_balance);
    ]
    @ (if not (String.equal data.unstaked_frozen "0") then
         [
           ("Unstaked frozen", Baker_wallet_data.format_tez data.unstaked_frozen);
         ]
       else [])
    @ [("Full balance", Baker_wallet_data.format_tez data.full_balance)]
  in
  let dl =
    Desc_list.create ~key_width:18 ~items ()
    |> Desc_list.render ~cols:(cols - 6) ~wrap:false ~focus:false
  in
  Box.render ~title:"Balance" ~style:Rounded ~width:(cols - 2) dl

let render_status_line (data : Baker_wallet_data.t) =
  let status =
    if data.deactivated then Widgets.themed_error "● deactivated"
    else if data.is_registered then Widgets.themed_success "● registered"
    else Widgets.themed_error "○ not registered"
  in
  let key_info =
    let short = truncate_pkh data.active_consensus_key in
    if String.equal data.active_consensus_key data.pkh then
      Printf.sprintf "Key: %s (default)" short
    else Printf.sprintf "Key: %s" short
  in
  Printf.sprintf "  Status: %s      %s" status (Widgets.themed_muted key_info)

let render_staking_params (data : Baker_wallet_data.t) =
  match data.staking_parameters with
  | None -> ""
  | Some params ->
      Printf.sprintf
        "  %s"
        (Widgets.themed_muted
           (Printf.sprintf
              "Staking limit: %s       Baking edge: %s"
              (Baker_wallet_data.format_staking_limit
                 params.limit_of_staking_over_baking)
              (Baker_wallet_data.format_baking_edge
                 params.edge_of_baking_over_staking)))

let render_pending_unstakes (data : Baker_wallet_data.t) =
  match
    data.unstake_requests.finalizable
    @ List.map
        (fun (r : Baker_wallet_data.unfinalizable_request) ->
          ({cycle = r.cycle; amount = r.amount}
            : Baker_wallet_data.finalizable_request))
        data.unstake_requests.unfinalizable
  with
  | [] -> ""
  | requests ->
      let parts =
        List.map
          (fun (r : Baker_wallet_data.finalizable_request) ->
            Printf.sprintf
              "%s (cycle %d)"
              (Baker_wallet_data.format_tez r.amount)
              r.cycle)
          requests
      in
      Printf.sprintf
        "  %s"
        (Widgets.themed_muted ("Pending unstake: " ^ String.concat ", " parts))

let build_operations_list (data : Baker_wallet_data.t) ~node_endpoint:_ =
  if not data.is_registered then [Register; Transfer]
  else
    let items = [Stake; Unstake] in
    let items =
      match data.unstake_requests.finalizable with
      | [] -> items
      | _ -> items @ [Finalize_unstake]
    in
    items @ [Transfer; Set_delegate_params; Update_consensus_key] @ [Vote]

let action_to_string (data : Baker_wallet_data.t) ~node_endpoint action =
  match action with
  | Register -> "Register as Delegate"
  | Stake -> "Stake"
  | Unstake -> "Unstake"
  | Finalize_unstake ->
      let total =
        List.fold_left
          (fun acc (r : Baker_wallet_data.finalizable_request) ->
            match int_of_string_opt r.amount with
            | Some v -> acc + v
            | None -> acc)
          0
          data.unstake_requests.finalizable
      in
      Printf.sprintf
        "Finalize Unstake (%s available)"
        (Baker_wallet_data.format_tez (string_of_int total))
  | Transfer -> "Transfer"
  | Set_delegate_params -> "Set Delegate Parameters"
  | Update_consensus_key -> "Update Consensus Key"
  | Vote -> (
      match Baker_wallet_data.get_voting_info ~node_endpoint with
      | Some info ->
          let already_voted =
            List.exists (fun (p, _) -> String.equal p data.pkh) info.ballots
          in
          let period =
            Baker_wallet_data.string_of_voting_period_kind info.period_kind
          in
          if already_voted then
            Printf.sprintf
              "Vote (%s period) %s"
              period
              (Widgets.themed_muted "(already voted)")
          else Printf.sprintf "Vote (%s period)" period
      | None -> "Vote")

(* ── Operation execution helpers ──────────────────────────── *)

let octez_client_bin (svc : Service.t) =
  Filename.concat svc.app_bin_dir "octez-client"

(** Shared helper for all wallet operations.
    Flow: spinner (estimate fee) → confirmation modal → spinner (execute) → result.
    Reused by register, stake, unstake, transfer, etc. *)
let baker_base_dir (svc : Service.t) =
  match Node_env.read ~inst:svc.instance with
  | Error _ -> None
  | Ok pairs -> List.assoc_opt "OCTEZ_BAKER_BASE_DIR" pairs

let run_wallet_operation ~svc ~pkh ~op =
  let instance = svc.Service.instance in
  let endpoint =
    match Delegate_scheduler.get_baker_node_endpoint ~instance with
    | Some ep -> ep
    | None -> ""
  in
  let client_bin = octez_client_bin svc in
  let base_dir = baker_base_dir svc in
  let description = Baker_ops.describe_operation op in
  let fee_ref = ref "~0.001" in
  (* Step 1: Estimate fee *)
  Modal_helpers.show_spinner_modal
    ~title:description
    ~label:"Estimating fee..."
    ~work:(fun () ->
      match
        Baker_ops.estimate_fee
          ~instance_name:instance
          ~octez_client_bin:client_bin
          ~endpoint
          ~base_dir
          ~alias:pkh
          ~op
      with
      | Ok fee ->
          fee_ref := fee ;
          Ok ()
      | Error msg -> Error (`Msg msg))
    ~on_complete:(fun status ->
      match status with
      | `Failed msg ->
          Modal_helpers.show_error ~title:description msg ;
          Context.toast_error (description ^ ": fee estimation failed")
      | `Cancelled -> ()
      | `Succeeded ->
          (* Step 2: Confirmation modal *)
          let summary_items =
            [
              ("Operation", description);
              ("Delegate", truncate_pkh pkh);
              ("Instance", instance);
              ("Est. fee", !fee_ref ^ " ꜩ");
            ]
          in
          let summary =
            let dl =
              Desc_list.create ~key_width:14 ~items:summary_items ()
              |> Desc_list.render ~cols:50 ~wrap:false ~focus:false
            in
            Box.render ~title:"" ~style:Rounded ~width:54 dl
          in
          ignore summary ;
          Modal_helpers.open_choice_modal
            ~title:"Confirm Operation"
            ~items:[`Confirm; `Cancel]
            ~to_string:(function `Confirm -> "Confirm" | `Cancel -> "Cancel")
            ~on_select:(function
              | `Cancel -> ()
              | `Confirm ->
                  (* Step 3: Execute operation *)
                  Modal_helpers.show_spinner_modal
                    ~title:description
                    ~label:"Submitting operation..."
                    ~work:(fun () ->
                      let result =
                        Baker_ops.execute
                          ~instance_name:instance
                          ~octez_client_bin:client_bin
                          ~endpoint
                          ~base_dir
                          ~alias:pkh
                          ~op
                      in
                      if result.success then Ok ()
                      else
                        Error
                          (`Msg
                             (Option.value
                                ~default:"Unknown error"
                                result.error)))
                    ~on_complete:(fun exec_status ->
                      match exec_status with
                      | `Succeeded ->
                          Modal_helpers.show_success
                            ~title:description
                            "Operation submitted successfully" ;
                          Context.toast_success
                            (description ^ ": operation submitted")
                      | `Failed msg ->
                          Modal_helpers.show_error ~title:description msg ;
                          Context.toast_error
                            (description ^ ": operation failed")
                      | `Cancelled -> ())
                    ())
            ())
    ()

(* ── Dispatch operation ──────────────────────────────────── *)

let dispatch_action svc pkh _data ~node_endpoint action =
  match action with
  | Register -> run_wallet_operation ~svc ~pkh ~op:Baker_ops.Register
  | Stake ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Stake"
        ~placeholder:(Some "e.g. 1000 or 500.5")
        ~validator:(fun s ->
          match float_of_string_opt s with
          | Some f when f > 0.0 -> Ok ()
          | _ -> Error "Enter a positive amount")
        ~on_submit:(fun amount ->
          run_wallet_operation ~svc ~pkh ~op:(Baker_ops.Stake {amount}))
        ()
  | Unstake ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Unstake"
        ~placeholder:(Some "e.g. 500 or everything")
        ~validator:(fun s ->
          if String.equal s "everything" then Ok ()
          else
            match float_of_string_opt s with
            | Some f when f > 0.0 -> Ok ()
            | _ -> Error "Enter a positive amount or \"everything\"")
        ~on_submit:(fun amount ->
          run_wallet_operation ~svc ~pkh ~op:(Baker_ops.Unstake {amount}))
        ()
  | Finalize_unstake ->
      run_wallet_operation ~svc ~pkh ~op:Baker_ops.Finalize_unstake
  | Transfer ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Transfer · Amount"
        ~placeholder:(Some "e.g. 100")
        ~validator:(fun s ->
          match float_of_string_opt s with
          | Some f when f > 0.0 -> Ok ()
          | _ -> Error "Enter a positive amount")
        ~on_submit:(fun amount ->
          Modal_helpers.prompt_validated_text_modal
            ~title:"Transfer · Destination"
            ~placeholder:(Some "tz1... or KT1...")
            ~validator:(fun s ->
              let len = String.length s in
              if
                len >= 36
                && (String.sub s 0 3 = "tz1"
                   || String.sub s 0 3 = "tz2"
                   || String.sub s 0 3 = "tz3"
                   || String.sub s 0 3 = "KT1")
              then Ok ()
              else Error "Enter a valid tz1/tz2/tz3/KT1 address")
            ~on_submit:(fun destination ->
              run_wallet_operation
                ~svc
                ~pkh
                ~op:(Baker_ops.Transfer {amount; destination}))
            ())
        ()
  | Set_delegate_params ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Staking Limit"
        ~placeholder:(Some "0-9 (0 = reject external staking)")
        ~validator:(fun s ->
          match int_of_string_opt s with
          | Some n when n >= 0 && n <= 9 -> Ok ()
          | _ -> Error "Enter an integer from 0 to 9")
        ~on_submit:(fun limit_s ->
          Modal_helpers.prompt_validated_text_modal
            ~title:"Baking Edge"
            ~placeholder:(Some "0-100 (% of staker rewards to baker)")
            ~validator:(fun s ->
              match int_of_string_opt s with
              | Some n when n >= 0 && n <= 100 -> Ok ()
              | _ -> Error "Enter an integer from 0 to 100")
            ~on_submit:(fun edge_s ->
              let limit = int_of_string limit_s * 1000000 in
              let edge = int_of_string edge_s * 10000000 in
              run_wallet_operation
                ~svc
                ~pkh
                ~op:(Baker_ops.Set_delegate_params {limit; edge}))
            ())
        ()
  | Update_consensus_key ->
      Modal_helpers.prompt_validated_text_modal
        ~title:"Update Consensus Key"
        ~placeholder:(Some "tz1... key alias or pkh")
        ~validator:(fun s ->
          let len = String.length s in
          if
            len >= 36
            && (String.sub s 0 3 = "tz1"
               || String.sub s 0 3 = "tz2"
               || String.sub s 0 3 = "tz3")
          then Ok ()
          else Error "Enter a valid tz1/tz2/tz3 public key hash")
        ~on_submit:(fun key ->
          run_wallet_operation
            ~svc
            ~pkh
            ~op:(Baker_ops.Update_consensus_key {key}))
        ()
  | Vote -> (
      match Baker_wallet_data.get_voting_info ~node_endpoint with
      | None ->
          Context.toast_error
            "Voting info unavailable — node may be unreachable"
      | Some info -> (
          (* Check if delegate already voted *)
          let already_voted =
            List.exists (fun (p, _) -> String.equal p pkh) info.ballots
          in
          if already_voted then
            Modal_helpers.show_error
              ~title:"Vote"
              "You have already voted in this period."
          else
            match info.period_kind with
            | Baker_wallet_data.Proposal ->
                (* Proposal period: select a proposal to upvote *)
                let proposals = info.proposals in
                if List.length proposals = 0 then
                  Modal_helpers.show_error
                    ~title:"Vote"
                    "No proposals available in this period."
                else
                  Modal_helpers.open_choice_modal
                    ~title:"Vote · Proposal Period"
                    ~items:proposals
                    ~to_string:(fun (hash, count) ->
                      Printf.sprintf
                        "%s (%d supporter%s)"
                        (truncate_pkh hash)
                        count
                        (if count = 1 then "" else "s"))
                    ~on_select:(fun (hash, _count) ->
                      run_wallet_operation
                        ~svc
                        ~pkh
                        ~op:(Baker_ops.Submit_proposals {proposals = [hash]}))
                    ()
            | Baker_wallet_data.Exploration | Baker_wallet_data.Promotion ->
                (* Ballot period: vote yay/nay/pass on current proposal *)
                let period_name =
                  Baker_wallet_data.string_of_voting_period_kind
                    info.period_kind
                in
                let proposal =
                  Option.value ~default:"(unknown)" info.current_proposal
                in
                Modal_helpers.open_choice_modal
                  ~title:
                    (Printf.sprintf
                       "Vote · %s%s Period"
                       (String.make 1 (Char.uppercase_ascii period_name.[0]))
                       (String.sub
                          period_name
                          1
                          (String.length period_name - 1)))
                  ~items:
                    [
                      Baker_wallet_data.Yay;
                      Baker_wallet_data.Nay;
                      Baker_wallet_data.Pass;
                    ]
                  ~to_string:(fun ballot ->
                    String.capitalize_ascii
                      (Baker_wallet_data.string_of_ballot_vote ballot))
                  ~on_select:(fun ballot ->
                    run_wallet_operation
                      ~svc
                      ~pkh
                      ~op:(Baker_ops.Submit_ballot {proposal; ballot}))
                  ()
            | Baker_wallet_data.Cooldown | Baker_wallet_data.Adoption ->
                let period_name =
                  Baker_wallet_data.string_of_voting_period_kind
                    info.period_kind
                in
                Modal_helpers.show_error
                  ~title:"Vote"
                  (Printf.sprintf
                     "No voting action available during %s period."
                     period_name)))

(* ── Wallet header rendering ──────────────────────────────── *)

let render_wallet_header ~pkh ~delegates ~cols =
  match Baker_wallet_data.get ~pkh with
  | None ->
      String.concat
        "\n"
        [
          Printf.sprintf "  Delegate: %s" (Widgets.themed_muted pkh);
          "";
          Widgets.themed_error
            "  Unable to fetch wallet data — node may be unreachable";
          "";
        ]
  | Some data ->
      let delegate_line =
        Printf.sprintf
          "  Delegate: %s%s"
          data.pkh
          (if List.length delegates > 1 then
             "                   " ^ Widgets.themed_muted "[Tab] to switch"
           else "")
      in
      let parts =
        [
          delegate_line;
          "";
          render_balance_box ~cols data;
          "";
          render_status_line data;
        ]
        @ (let p = render_staking_params data in
           if p = "" then [] else [p])
        @ (let u = render_pending_unstakes data in
           if u = "" then [] else [u])
        @ [""]
      in
      String.concat "\n" parts

let count_lines s =
  let n = ref 1 in
  String.iter (fun c -> if c = '\n' then incr n) s ;
  !n

(* ── Wallet modal ────────────────────────────────────────── *)

let wallet_modal ~svc =
  let instance = svc.Service.instance in
  let delegates = Delegate_scheduler.get_baker_delegates ~instance in
  let node_endpoint =
    match Delegate_scheduler.get_baker_node_endpoint ~instance with
    | Some ep -> ep
    | None -> ""
  in
  let initial_pkh = match delegates with first :: _ -> first | [] -> "" in
  let build_select pkh :
      [`Error_state | `Action of wallet_action] Select_widget.t =
    match Baker_wallet_data.get ~pkh with
    | None ->
        Select_widget.open_centered
          ~title:""
          ~items:
            ([`Error_state] : [`Error_state | `Action of wallet_action] list)
          ~to_string:(function
            | `Error_state -> Widgets.themed_error "Unable to fetch wallet data"
            | `Action _ -> "...")
          ()
    | Some data ->
        let actions = build_operations_list data ~node_endpoint in
        Select_widget.open_centered
          ~title:""
          ~items:(List.map (fun a -> `Action a) actions)
          ~to_string:(function
            | `Error_state -> Widgets.themed_error "Unable to fetch wallet data"
            | `Action action -> action_to_string data ~node_endpoint action)
          ()
  in
  let title = Printf.sprintf "Wallet · %s" instance in
  let module Wallet_modal = struct
    type state = {
      current_pkh : string;
      select : [`Error_state | `Action of wallet_action] Select_widget.t;
    }

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () =
      Navigation.make
        {current_pkh = initial_pkh; select = build_select initial_pkh}

    let update ps _ = ps

    let view ps ~focus ~size =
      let s = ps.Navigation.s in
      let modal_cols = size.LTerm_geom.cols in
      let header =
        render_wallet_header ~pkh:s.current_pkh ~delegates ~cols:modal_cols
      in
      let header_rows = count_lines header in
      let layout =
        Flex.create
          ~direction:Flex.Column
          [
            {
              render = (fun ~size:_ -> header);
              basis = Flex.Px header_rows;
              cross = None;
            };
            {
              render =
                (fun ~size ->
                  Select_widget.render_with_size s.select ~focus ~size);
              basis = Flex.Fill;
              cross = None;
            };
          ]
      in
      Flex.render layout ~size

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ =
      (* Rebuild select widget to pick up cache changes *)
      let s = ps.Navigation.s in
      Navigation.update
        (fun s -> {s with select = build_select s.current_pkh})
        {ps with s}

    let back ps = ps

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key ps key ~size =
      let s = ps.Navigation.s in
      let key_parsed = Keys.of_string key in
      match key_parsed with
      | Some Keys.Enter -> (
          match Select_widget.get_selection s.select with
          | Some (`Action action) -> (
              match Baker_wallet_data.get ~pkh:s.current_pkh with
              | None ->
                  Context.toast_error "No wallet data available" ;
                  ps
              | Some data ->
                  Modal_manager.set_consume_next_key () ;
                  Modal_manager.close_top `Commit ;
                  dispatch_action svc s.current_pkh data ~node_endpoint action ;
                  ps)
          | Some `Error_state | None -> ps)
      | Some Keys.Escape ->
          Modal_manager.set_consume_next_key () ;
          Modal_manager.close_top `Cancel ;
          ps
      | Some Keys.Tab ->
          (* Cycle to next delegate *)
          let rec next = function
            | [] -> initial_pkh
            | [_] -> initial_pkh
            | x :: y :: _ when String.equal x s.current_pkh -> y
            | _ :: rest -> next rest
          in
          let new_pkh = next delegates in
          Navigation.update
            (fun _ -> {current_pkh = new_pkh; select = build_select new_pkh})
            ps
      | _ ->
          Navigation.update
            (fun s ->
              {
                s with
                select = Select_widget.handle_key_with_size s.select ~key ~size;
              })
            ps

    let handle_key = handle_modal_key

    let on_key ps key ~size =
      let ps' = handle_key ps (Keys.to_string key) ~size in
      (ps', Miaou_interfaces.Key_event.Handled)

    let on_modal_key ps key ~size =
      let ps' = handle_modal_key ps (Keys.to_string key) ~size in
      (ps', Miaou_interfaces.Key_event.Handled)

    let key_hints _ps = []

    let has_modal _ = true
  end in
  let ui : Modal_manager.ui =
    {title; left = None; max_width = Some (Fixed 68); dim_background = true}
  in
  Modal_manager.push
    (module Wallet_modal)
    ~init:(Wallet_modal.init ())
    ~ui
    ~commit_on:[]
    ~cancel_on:[]
    ~on_close:(fun _ _ -> ())
