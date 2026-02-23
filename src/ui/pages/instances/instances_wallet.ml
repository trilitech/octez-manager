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

let build_operations_list (data : Baker_wallet_data.t) ~_node_endpoint =
  if not data.is_registered then [Register; Transfer]
  else
    let items = [Stake; Unstake] in
    let items =
      match data.unstake_requests.finalizable with
      | [] -> items
      | _ -> items @ [Finalize_unstake]
    in
    items @ [Transfer; Set_delegate_params; Update_consensus_key] @ [Vote]

let action_to_string (data : Baker_wallet_data.t) ~_node_endpoint action =
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
      let vi =
        Baker_wallet_data.get_voting_info
          ~node_endpoint:
            (match
               Delegate_scheduler.get_baker_node_endpoint
                 ~instance:"" (* placeholder *)
             with
            | Some ep -> ep
            | None -> "")
      in
      match vi with
      | Some info ->
          Printf.sprintf
            "Vote (%s period)"
            (Baker_wallet_data.string_of_voting_period_kind info.period_kind)
      | None -> "Vote")

(* ── Dispatch operation ──────────────────────────────────── *)

let dispatch_action _svc _pkh _data _action =
  (* Operations will be wired in Phase 3-10 tasks (T014+) *)
  Context.toast_info "Operation not yet implemented"

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
  let current_pkh = ref initial_pkh in
  (* Build the modal content dynamically using open_choice_modal with on_tick *)
  let get_items () =
    match Baker_wallet_data.get ~pkh:!current_pkh with
    | None -> [`Error_state]
    | Some data ->
        List.map
          (fun a -> `Action a)
          (build_operations_list data ~_node_endpoint:node_endpoint)
  in
  let render_header () =
    match Baker_wallet_data.get ~pkh:!current_pkh with
    | None ->
        String.concat
          "\n"
          [
            Printf.sprintf "  Delegate: %s" (Widgets.themed_muted !current_pkh);
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
            render_balance_box ~cols:60 data;
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
  in
  (* Use a choice modal for the operations list *)
  let items = get_items () in
  let title = Printf.sprintf "Wallet · %s" instance in
  Modal_helpers.open_choice_modal
    ~title
    ~items
    ~to_string:(fun item ->
      match item with
      | `Error_state -> Widgets.themed_error "Unable to fetch wallet data"
      | `Action action -> (
          match Baker_wallet_data.get ~pkh:!current_pkh with
          | None -> "..."
          | Some data ->
              action_to_string data ~_node_endpoint:node_endpoint action))
    ~on_tick:(fun () ->
      (* Refresh the header display *)
      ignore (render_header ()))
    ~on_select:(fun item ->
      match item with
      | `Error_state -> ()
      | `Action action -> (
          match Baker_wallet_data.get ~pkh:!current_pkh with
          | None -> Context.toast_error "No wallet data available"
          | Some data -> dispatch_action svc !current_pkh data action))
    ()
