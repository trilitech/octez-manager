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

(** Extract password file path from the baker's extra_args.
    Looks for [-f path] or [--password-filename path] in the service JSON.
    Falls back to parsing OCTEZ_BAKER_GLOBAL_ARGS from the env file. *)
let baker_password_file (svc : Service.t) =
  let rec find = function
    | ("-f" | "--password-filename") :: path :: _ when path <> "" -> Some path
    | _ :: rest -> find rest
    | [] -> None
  in
  match find svc.extra_args with
  | Some _ as result -> result
  | None -> (
      match Node_env.read ~inst:svc.instance with
      | Error _ -> None
      | Ok pairs -> (
          match List.assoc_opt "OCTEZ_BAKER_GLOBAL_ARGS" pairs with
          | None -> None
          | Some global_str -> find (String.split_on_char ' ' global_str)))

let tzkt_base_url ~network =
  if String.equal network "mainnet" then "https://tzkt.io"
  else Printf.sprintf "https://%s.tzkt.io" network

(* ── Operation tracking ──────────────────────────────────── *)

type tracking_step =
  | Submitting
  | Submitted of {op_hash : string}
  | Included of {op_hash : string; block_hash : string}
  | Confirmed of {op_hash : string; block_hash : string}
  | Finalized of {op_hash : string; block_hash : string}
  | Failed of string

let rpc_get_json ~endpoint path =
  let url = Printf.sprintf "%s%s" endpoint path in
  match Cmd_runner.run_out_silent ["curl"; "-sfL"; "--max-time"; "10"; url] with
  | Error _ -> None
  | Ok body -> ( try Some (Yojson.Safe.from_string body) with _ -> None)

(** Poll the chain until the operation is included and finalized.
    Updates [step_ref] at each stage. Called from a background thread. *)
let poll_operation ~endpoint ~op_hash (step_ref : tracking_step Atomic.t) =
  let open Yojson.Safe.Util in
  (* Poll for inclusion: check operation_hashes every 5s, up to ~120s *)
  let rec wait_included attempts =
    if attempts > 24 then
      Atomic.set step_ref (Failed "Timed out waiting for inclusion")
    else
      match
        rpc_get_json ~endpoint "/chains/main/blocks/head/operation_hashes"
      with
      | None ->
          Eio_unix.sleep 5.0 ;
          wait_included (attempts + 1)
      | Some json ->
          let all_hashes =
            try
              json |> to_list
              |> List.concat_map (fun pass -> pass |> to_list |> filter_string)
            with _ -> []
          in
          if List.exists (String.equal op_hash) all_hashes then (
            match rpc_get_json ~endpoint "/chains/main/blocks/head/header" with
            | Some header_json ->
                let block_hash =
                  try header_json |> member "hash" |> to_string
                  with _ -> "unknown"
                in
                let inclusion_level =
                  try header_json |> member "level" |> to_int with _ -> 0
                in
                Atomic.set step_ref (Included {op_hash; block_hash}) ;
                wait_confirmed ~block_hash ~inclusion_level 0
            | None ->
                Atomic.set step_ref (Included {op_hash; block_hash = "unknown"}) ;
                (* Cannot determine level, stop tracking *)
                ())
          else (
            Eio_unix.sleep 5.0 ;
            wait_included (attempts + 1))
  and wait_confirmed ~block_hash ~inclusion_level attempts =
    if attempts > 24 then
      Atomic.set step_ref (Failed "Timed out waiting for confirmation")
    else (
      Eio_unix.sleep 5.0 ;
      match rpc_get_json ~endpoint "/chains/main/blocks/head/header" with
      | Some header_json ->
          let head_level =
            try header_json |> member "level" |> to_int with _ -> 0
          in
          if head_level > inclusion_level then (
            Atomic.set step_ref (Confirmed {op_hash; block_hash}) ;
            wait_finalized ~block_hash ~inclusion_level 0)
          else wait_confirmed ~block_hash ~inclusion_level (attempts + 1)
      | None -> wait_confirmed ~block_hash ~inclusion_level (attempts + 1))
  and wait_finalized ~block_hash ~inclusion_level attempts =
    if attempts > 24 then
      Atomic.set step_ref (Failed "Timed out waiting for finalization")
    else (
      Eio_unix.sleep 5.0 ;
      match rpc_get_json ~endpoint "/chains/main/blocks/head/header" with
      | Some header_json ->
          let head_level =
            try header_json |> member "level" |> to_int with _ -> 0
          in
          if head_level > inclusion_level + 1 then
            Atomic.set step_ref (Finalized {op_hash; block_hash})
          else wait_finalized ~block_hash ~inclusion_level (attempts + 1)
      | None -> wait_finalized ~block_hash ~inclusion_level (attempts + 1))
  in
  wait_included 0

let render_tracking_checklist ~step ~network ~cols =
  let done_sym = Widgets.themed_success "✓"
  and spin_sym = Context.render_spinner ""
  and pending_sym = Widgets.themed_muted "○" in
  let op_hash_opt =
    match step with
    | Submitting | Failed _ -> None
    | Submitted {op_hash}
    | Included {op_hash; _}
    | Confirmed {op_hash; _}
    | Finalized {op_hash; _} ->
        Some op_hash
  in
  let line sym label detail =
    match detail with
    | Some d -> Printf.sprintf "  %s %s %s" sym label (Widgets.themed_muted d)
    | None -> Printf.sprintf "  %s %s" sym label
  in
  let lines =
    match step with
    | Submitting ->
        [
          line spin_sym "Submitting operation..." None;
          line pending_sym "Included in block" None;
          line pending_sym "Confirmed +1" None;
          line pending_sym "Finalized +2" None;
        ]
    | Submitted _ ->
        [
          line done_sym "Submitted" None;
          line spin_sym "Waiting for inclusion..." None;
          line pending_sym "Confirmed +1" None;
          line pending_sym "Finalized +2" None;
        ]
    | Included {block_hash; _} ->
        let short_block = truncate_pkh block_hash in
        [
          line done_sym "Submitted" None;
          line done_sym "Included in block" (Some short_block);
          line spin_sym "Waiting for confirmation..." None;
          line pending_sym "Finalized +2" None;
        ]
    | Confirmed {block_hash; _} ->
        let short_block = truncate_pkh block_hash in
        [
          line done_sym "Submitted" None;
          line done_sym "Included in block" (Some short_block);
          line done_sym "Confirmed +1" None;
          line spin_sym "Waiting for finalization..." None;
        ]
    | Finalized {block_hash; _} ->
        let short_block = truncate_pkh block_hash in
        [
          line done_sym "Submitted" None;
          line done_sym "Included in block" (Some short_block);
          line done_sym "Confirmed +1" None;
          line done_sym "Finalized +2" None;
        ]
    | Failed msg ->
        [
          line (Widgets.themed_error "✗") "Failed" (Some msg);
          line pending_sym "Included in block" None;
          line pending_sym "Confirmed +1" None;
          line pending_sym "Finalized +2" None;
        ]
  in
  let hash_lines =
    match op_hash_opt with
    | Some hash ->
        let short_hash = truncate_pkh hash in
        let url = Printf.sprintf "%s/%s" (tzkt_base_url ~network) hash in
        let osc8_link =
          Printf.sprintf
            "  \027]8;;%s\027\\%s\027]8;;\027\\"
            url
            (Widgets.themed_muted
               (Printf.sprintf "%s  (Ctrl+click to open)" short_hash))
        in
        [""; osc8_link]
    | None -> []
  in
  let hint_line =
    [
      "";
      Printf.sprintf
        "  %s"
        (Widgets.themed_muted
           (match step with
           | Finalized _ | Failed _ -> "[Esc] close"
           | _ -> "[Esc] close (tracking continues in background)"));
    ]
  in
  let all_lines = [""] @ lines @ hash_lines @ hint_line @ [""] in
  let content = String.concat "\n" all_lines in
  ignore cols ;
  content

let open_tracking_modal ~title ~network ~step_ref =
  let close_ref : (unit -> unit) option ref = ref None in
  let module Tracking_modal = struct
    type state = unit

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () = Navigation.make ()

    let update ps _ = ps

    let view _ps ~focus:_ ~size =
      let step = Atomic.get step_ref in
      let cols = size.LTerm_geom.cols in
      render_tracking_checklist ~step ~network ~cols

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ = ps

    let back ps = ps

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key ps key ~size:_ =
      let key_parsed = Keys.of_string key in
      (match key_parsed with
      | Some Keys.Escape -> (
          Modal_manager.set_consume_next_key () ;
          match !close_ref with
          | Some close -> close ()
          | None -> Modal_manager.close_top `Commit)
      | _ -> ()) ;
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
    {
      title;
      left = None;
      max_width = Some (Clamped {ratio = 0.6; min = 50; max = 68});
      dim_background = true;
    }
  in
  close_ref := Some (fun () -> Modal_manager.close_top `Commit) ;
  Modal_manager.push
    (module Tracking_modal)
    ~init:(Tracking_modal.init ())
    ~ui
    ~commit_on:[]
    ~cancel_on:[]
    ~on_close:(fun _ _ -> close_ref := None)

let run_wallet_operation ~svc ~pkh ~op =
  let instance = svc.Service.instance in
  let network = svc.Service.network in
  let endpoint =
    match Delegate_scheduler.get_baker_node_endpoint ~instance with
    | Some ep -> ep
    | None -> ""
  in
  let client_bin = octez_client_bin svc in
  let base_dir = baker_base_dir svc in
  let password_file = baker_password_file svc in
  let description = Baker_ops.describe_operation op in
  (* Show confirmation modal directly — no dry-run fee estimation
     to avoid blocking the node's RPC worker with a simulation. *)
  Modal_helpers.open_choice_modal
    ~title:("Confirm: " ^ description)
    ~items:[`Confirm; `Cancel]
    ~to_string:(function
      | `Confirm ->
          Printf.sprintf "Confirm  (%s · %s)" (truncate_pkh pkh) instance
      | `Cancel -> "Cancel")
    ~on_select:(function
      | `Cancel -> ()
      | `Confirm ->
          let step_ref = Atomic.make Submitting in
          open_tracking_modal ~title:description ~network ~step_ref ;
          Job_manager.submit
            ~timeout:None
            ~description
            (fun ~append_log:_ () ->
              let result =
                Baker_ops.execute
                  ~instance_name:instance
                  ~octez_client_bin:client_bin
                  ~endpoint
                  ~base_dir
                  ~password_file
                  ~alias:pkh
                  ~op
              in
              if result.success then (
                Baker_wallet_data.remove ~pkh ;
                match result.op_hash with
                | Some op_hash ->
                    Atomic.set step_ref (Submitted {op_hash}) ;
                    Context.toast_success (description ^ ": operation submitted") ;
                    poll_operation ~endpoint ~op_hash step_ref ;
                    Ok ()
                | None ->
                    Atomic.set step_ref (Failed "No operation hash returned") ;
                    Error (`Msg "No operation hash returned"))
              else
                let msg = Option.value ~default:"Unknown error" result.error in
                Atomic.set step_ref (Failed msg) ;
                Context.toast_error (description ^ ": operation failed") ;
                Error (`Msg msg))
            ~on_complete:(fun _status -> ()))
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
              let limit = int_of_string limit_s in
              let edge = int_of_string edge_s in
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
  match delegates with
  | [] ->
      String.concat
        "\n"
        [
          "  " ^ Widgets.themed_muted "No delegates found in wallet";
          "";
          "  Import or generate keys to manage this baker.";
          "";
        ]
  | _ -> (
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
               if String.equal p "" then [] else [p])
            @ (let u = render_pending_unstakes data in
               if String.equal u "" then [] else [u])
            @ [""]
          in
          String.concat "\n" parts)

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
  let to_string_item ~action_display = function
    | `Empty_wallet -> Widgets.themed_muted "No operations available"
    | `Error_state -> Widgets.themed_error "Unable to fetch wallet data"
    | `Action action -> action_display action
  in
  let build_select pkh :
      [`Error_state | `Action of wallet_action | `Empty_wallet] Select_widget.t
      =
    match delegates with
    | [] ->
        Select_widget.open_centered
          ~title:""
          ~items:
            ([`Empty_wallet]
              : [`Error_state | `Action of wallet_action | `Empty_wallet] list)
          ~to_string:(to_string_item ~action_display:(fun _ -> "..."))
          ()
    | _ -> (
        match Baker_wallet_data.get ~pkh with
        | None ->
            Select_widget.open_centered
              ~title:""
              ~items:
                ([`Error_state]
                  : [`Error_state | `Action of wallet_action | `Empty_wallet]
                    list)
              ~to_string:(to_string_item ~action_display:(fun _ -> "..."))
              ()
        | Some data ->
            let actions = build_operations_list data ~node_endpoint in
            Select_widget.open_centered
              ~title:""
              ~items:
                (List.map (fun a -> `Action a) actions
                  : [`Error_state | `Action of wallet_action | `Empty_wallet]
                    list)
              ~to_string:
                (to_string_item
                   ~action_display:(action_to_string data ~node_endpoint))
              ())
  in
  let title = Printf.sprintf "Wallet · %s" instance in
  let module Wallet_modal = struct
    type state = {
      current_pkh : string;
      select :
        [`Error_state | `Action of wallet_action | `Empty_wallet]
        Select_widget.t;
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
          | Some `Error_state | Some `Empty_wallet | None -> ps)
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

(** {1 Testing} *)

module Internal_for_tests = struct
  let render_wallet_header = render_wallet_header
end
