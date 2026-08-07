(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the wallets page. No Eio calls. *)

open Octez_manager_lib
module Widgets = Miaou_widgets_display.Widgets
module Grid = Miaou_widgets_layout.Grid_layout
module Box = Miaou_widgets_layout.Box_widget
module Desc_list = Miaou_widgets_display.Description_list

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

module StringSet = Set.Make (String)

(** A group of keys from one base directory, with enriched metadata. *)
type enriched_group = {
  base_dir : string;
  keys : Keys_reader.key_metadata list;
  error : string option;
  services : string list;
  networks : string list;
  sandbox_name : string option;
}

(** Items in the flattened navigation list. *)
type nav_item =
  | GroupHeader of enriched_group
  | KeyItem of enriched_group * Keys_reader.key_metadata

type focus_panel = ListPanel | DetailPanel

type sort_mode = SortAlias | SortBalance | SortNetwork

let sort_mode_label = function
  | SortAlias -> "Alias A-Z"
  | SortBalance -> "Balance"
  | SortNetwork -> "Network"

type state = {
  groups : enriched_group list;
  nav_items : nav_item list;
  cursor : int;
  folded : StringSet.t;
  focus_panel : focus_panel;
  scroll_offset : int;
  total_keys : int;
  search_query : string;
  sort_mode : sort_mode;
  multi_select : bool;
  selected : StringSet.t;
}

(* ================================================================ *)
(* Shared helpers (used by rendering and page logic)                 *)
(* ================================================================ *)

(** Check if [haystack] contains [needle] as a substring. *)
let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let found = ref false in
    let i = ref 0 in
    while !i <= hlen - nlen && not !found do
      if String.sub haystack !i nlen = needle then found := true ;
      incr i
    done ;
    !found

(** Resolve the display alias for a key: OM alias if set, else octez-client
    alias. *)
let display_alias ~base_dir (key : Keys_reader.key_metadata) =
  match Key_aliases.get ~base_dir ~pkh:key.pkh with
  | Some alias -> alias
  | None -> key.alias

(** Strip URL scheme/host from network identifiers for display.
    "https://teztnets.com/shadownet" → "shadownet", "ghostnet" → "ghostnet". *)
let pretty_network_name network =
  if contains_substring network "://" then
    match List.rev (String.split_on_char '/' network) with
    | name :: _ when String.length name > 0 -> name
    | _ -> network
  else network

(** Format mutez as tez with 6 decimal places. *)
let format_tez mutez_str =
  match int_of_string_opt mutez_str with
  | None -> mutez_str
  | Some mutez ->
      let tez = mutez / 1_000_000 in
      let frac = abs (mutez mod 1_000_000) in
      Printf.sprintf "%d.%06d \xea\x9c\xa9" tez frac

(** Format a short PKH for display: first 7 + last 4 chars. *)
let short_pkh pkh =
  let len = String.length pkh in
  if len <= 15 then pkh
  else String.sub pkh 0 7 ^ ".." ^ String.sub pkh (len - 4) 4

(* ================================================================ *)
(* Rendering helpers                                                 *)
(* ================================================================ *)

(** Short indicator for key kind (emoji). *)
let key_kind_tag = function
  | Keys_reader.Unencrypted -> ""
  | Keys_reader.Encrypted -> "\xF0\x9F\x94\x92" (* 🔒 *)
  | Keys_reader.Ledger _ -> "\xF0\x9F\x94\x90" (* 🔐 *)
  | Keys_reader.Remote _ -> "\xE2\x9C\x92\xEF\xB8\x8F" (* ✒️  *)

(** Truncate a PKH to fit available space, keeping prefix and suffix. *)
let truncate_pkh ~max_len pkh =
  let len = String.length pkh in
  if len <= max_len then pkh
  else if max_len < 12 then String.sub pkh 0 max_len
  else
    let prefix_len = 8 in
    let suffix_len = max_len - prefix_len - 2 in
    String.sub pkh 0 prefix_len
    ^ ".."
    ^ String.sub pkh (len - suffix_len) suffix_len

(** Render a single key row for the left panel. *)
let render_key_row ~is_selected ~is_focused ~multi_selected ~cols ~base_dir
    (key : Keys_reader.key_metadata) =
  let select_indicator =
    if multi_selected then Widgets.themed_accent "[*]" else ""
  in
  let marker =
    if is_selected then
      if is_focused then Widgets.themed_accent "> "
      else Widgets.themed_muted "> "
    else "  "
  in
  (* Ownership icon: 🔑 signable, 📋 watch-only *)
  let own_icon =
    if key.has_secret_key then "\xF0\x9F\x94\x91" (* 🔑 *)
    else "\xF0\x9F\x93\x8B" (* 📋 *)
  in
  let kind_tag = key_kind_tag key.key_kind in
  (* Baker icon: 🍞 if registered delegate on any network *)
  let baker_icon =
    let wd = Keys_scheduler.get_wallet_data ~pkh:key.pkh in
    if List.exists (fun (w : Keys_scheduler.wallet_data) -> w.is_registered) wd
    then "\xF0\x9F\x8D\x9E" (* 🍞 *)
    else ""
  in
  let tags =
    String.concat
      ""
      (List.filter
         (fun s -> not (String.equal s ""))
         [own_icon; kind_tag; baker_icon])
  in
  let alias_width = min 20 (cols / 3) in
  let name = display_alias ~base_dir key in
  let raw_alias =
    if String.length name > alias_width then
      String.sub name 0 (alias_width - 1) ^ "~"
    else name
  in
  let alias = Printf.sprintf "%-*s" alias_width raw_alias in
  let alias_styled =
    if is_selected then Widgets.themed_emphasis alias else alias
  in
  let pkh_avail = cols - alias_width - 6 - String.length tags in
  let pkh = truncate_pkh ~max_len:(max 10 pkh_avail) key.pkh in
  let pkh_styled = Widgets.themed_muted pkh in
  Printf.sprintf
    "  %s%s%s %s %s"
    marker
    select_indicator
    alias_styled
    pkh_styled
    tags

(** Render a group header for the left panel. *)
let render_group_header ~is_selected ~is_focused ~is_folded ~cols group =
  let marker =
    if is_selected then
      if is_focused then Widgets.themed_accent "> "
      else Widgets.themed_muted "> "
    else "  "
  in
  let fold_indicator = if is_folded then "\xe2\x96\xb8 " else "\xe2\x96\xbe " in
  let dir_display =
    let full = group.base_dir in
    let max_dir_len = cols - 12 in
    if String.length full <= max_dir_len then full
    else
      let home = Paths.home_dir () in
      let home_len = String.length home in
      if
        String.length full > home_len
        && String.equal (String.sub full 0 home_len) home
      then "~" ^ String.sub full home_len (String.length full - home_len)
      else full
  in
  let svc_dots =
    if group.services = [] then ""
    else
      let svc_states = Data.load_service_states () in
      let dots =
        List.map
          (fun svc_name ->
            match
              List.find_opt
                (fun (st : Data.Service_state.t) ->
                  String.equal st.service.instance svc_name)
                svc_states
            with
            | Some st -> (
                match st.status with
                | Data.Service_state.Running -> Widgets.green "\xe2\x97\x8f"
                | Stopped -> Widgets.red "\xe2\x97\x8f"
                | Unknown _ -> Widgets.yellow "\xe2\x97\x8f")
            | None -> Widgets.themed_muted "\xe2\x97\x8b")
          group.services
      in
      " " ^ String.concat "" dots
  in
  let key_count =
    let n = List.length group.keys in
    Widgets.themed_muted
      (Printf.sprintf " (%d key%s)" n (if n = 1 then "" else "s"))
  in
  let label =
    match group.sandbox_name with
    | None -> Widgets.themed_primary dir_display
    | Some name ->
        Widgets.themed_accent (Printf.sprintf "Sandbox \xc2\xb7 %s" name)
        ^ "  "
        ^ Widgets.themed_muted dir_display
  in
  Printf.sprintf "%s%s%s%s%s" marker fold_indicator label svc_dots key_count

(* ================================================================ *)
(* Left panel: key list                                              *)
(* ================================================================ *)

let render_list_panel ~state ~focus ~cols ~rows =
  let is_focused = focus && state.focus_panel = ListPanel in
  let lines =
    List.mapi
      (fun idx item ->
        let is_selected = idx = state.cursor in
        match item with
        | GroupHeader group ->
            let is_folded = StringSet.mem group.base_dir state.folded in
            render_group_header ~is_selected ~is_focused ~is_folded ~cols group
        | KeyItem (group, key) ->
            let multi_selected =
              state.multi_select && StringSet.mem key.pkh state.selected
            in
            render_key_row
              ~is_selected
              ~is_focused
              ~multi_selected
              ~cols
              ~base_dir:group.base_dir
              key)
      state.nav_items
  in
  (* Apply scroll offset *)
  let visible_lines =
    let len = List.length lines in
    if len <= rows then lines
    else
      let offset = state.scroll_offset in
      let take_from = max 0 (min offset (len - rows)) in
      List.filteri (fun i _ -> i >= take_from && i < take_from + rows) lines
  in
  (* Pad to fill height *)
  let padded =
    let n = List.length visible_lines in
    if n >= rows then visible_lines
    else visible_lines @ List.init (rows - n) (fun _ -> "")
  in
  String.concat "\n" padded

(* ================================================================ *)
(* Right panel: detail view                                          *)
(* ================================================================ *)

let render_dir_detail ~box_width group =
  let items =
    [("Path", group.base_dir)]
    @ (match group.sandbox_name with
      | None -> []
      | Some name -> [("Sandbox", Widgets.themed_accent name)])
    @ (if group.services <> [] then
         [("Services", String.concat ", " group.services)]
       else [])
    @ (if group.networks <> [] then
         [
           ( "Networks",
             String.concat ", " (List.map pretty_network_name group.networks) );
         ]
       else [])
    @ [("Keys", string_of_int (List.length group.keys))]
    @
    match group.error with
    | Some err -> [("Error", Widgets.red err)]
    | None -> []
  in
  let desc =
    Desc_list.create ~key_width:12 ~items ()
    |> Desc_list.render ~cols:(box_width - 4) ~wrap:true ~focus:false
  in
  Box.render ~title:"Directory" ~style:Rounded ~width:box_width desc

let render_key_detail ~box_width (group : enriched_group)
    (key : Keys_reader.key_metadata) =
  let kind_str =
    match key.key_kind with
    | Unencrypted -> "Unencrypted"
    | Encrypted -> "Encrypted"
    | Ledger path -> "Ledger: " ^ path
    | Remote endpoint -> "Remote: " ^ endpoint
  in
  let items =
    [
      ("Alias", display_alias ~base_dir:group.base_dir key);
      ("PKH", key.pkh);
      ("Key Type", kind_str);
      ( "Secret Key",
        if key.has_secret_key then Widgets.green "Yes"
        else Widgets.red "No (watch-only)" );
    ]
    @ (match key.public_key with Some pk -> [("Public Key", pk)] | None -> [])
    @ [("Base Dir", group.base_dir)]
  in
  let desc =
    Desc_list.create ~key_width:14 ~items ()
    |> Desc_list.render ~cols:(box_width - 4) ~wrap:true ~focus:false
  in
  let key_box =
    Box.render ~title:"Key Details" ~style:Rounded ~width:box_width desc
  in
  (* Balance section from scheduler cache.
     For sandbox wallets, restrict to the sandbox's own network so that
     balances from public nodes (mainnet, ghostnet, etc.) are not shown. *)
  let wallet_data =
    let all = Keys_scheduler.get_wallet_data ~pkh:key.pkh in
    match group.sandbox_name with
    | None -> all
    | Some _ ->
        List.filter
          (fun (wd : Keys_scheduler.wallet_data) ->
            List.exists (String.equal wd.network) group.networks)
          all
  in
  let balance_section =
    match wallet_data with
    | [] -> Widgets.themed_muted "\n  No balance data yet. Fetching..."
    | entries ->
        entries
        |> List.sort
             (fun
               (a : Keys_scheduler.wallet_data)
               (b : Keys_scheduler.wallet_data)
             -> String.compare a.network b.network)
        |> List.map (fun (wd : Keys_scheduler.wallet_data) ->
            let account_items =
              [
                ("Spendable", format_tez wd.spendable_balance);
                ("Staked", format_tez wd.staked_balance);
                ("Full Balance", format_tez wd.full_balance);
              ]
              @ (if wd.is_registered then
                   [("Status", Widgets.green "Registered delegate")]
                 else [])
              @
              match wd.active_consensus_key with
              | Some ck -> [("Consensus Key", ck)]
              | None -> []
            in
            let account_desc =
              Desc_list.create ~key_width:14 ~items:account_items ()
              |> Desc_list.render ~cols:(box_width - 4) ~wrap:true ~focus:false
            in
            let delegate_box =
              match wd.delegate with
              | Some d ->
                  let alias = Tzkt_aliases.find ~network:wd.network ~pkh:d in
                  let delegate_label =
                    match alias with
                    | Some a -> Printf.sprintf "%s (%s)" d a
                    | None -> d
                  in
                  let delegate_items =
                    [("Address", delegate_label)]
                    @ (match wd.delegate_staking_params with
                      | Some params ->
                          [
                            ( "Staking Limit",
                              Baker_wallet_data.format_staking_limit
                                params.limit_of_staking_over_baking );
                            ( "Baking Edge",
                              Baker_wallet_data.format_baking_edge
                                params.edge_of_baking_over_staking );
                          ]
                      | None -> [])
                    @
                    match wd.delegate_apy with
                    | Some apy -> [("Est. APY", Printf.sprintf "%.1f%%" apy)]
                    | None -> []
                  in
                  let inner_w = box_width - 4 in
                  let inner_desc =
                    Desc_list.create ~key_width:14 ~items:delegate_items ()
                    |> Desc_list.render ~cols:inner_w ~wrap:true ~focus:false
                  in
                  let bg =
                    let style =
                      Miaou_style.Style_context.background_secondary ()
                    in
                    match style.bg with
                    | Some (Miaou_style.Style.Fixed c) when c >= 0 -> Some c
                    | _ -> None
                  in
                  "\n"
                  ^ Box.render
                      ~title:"Delegate"
                      ~style:Single
                      ?bg
                      ~width:(box_width - 2)
                      inner_desc
              | None -> ""
            in
            let color =
              if String.equal wd.network "mainnet" then Some 208 else None
            in
            Box.render
              ~title:("Balance: " ^ pretty_network_name wd.network)
              ~style:Rounded
              ?color
              ~width:box_width
              (account_desc ^ delegate_box))
        |> String.concat "\n"
  in
  key_box ^ "\n" ^ balance_section

let render_detail_panel ~state ~box_width =
  match List.nth_opt state.nav_items state.cursor with
  | None -> Widgets.themed_muted "  No selection"
  | Some (GroupHeader group) -> render_dir_detail ~box_width group
  | Some (KeyItem (group, key)) -> render_key_detail ~box_width group key

(* ================================================================ *)
(* Page header                                                       *)
(* ================================================================ *)

let header s =
  let count_text =
    match s.total_keys with
    | 0 -> "No keys found"
    | 1 -> "1 key"
    | n -> Printf.sprintf "%d keys" n
  in
  let dir_count = List.length s.groups in
  let dir_text =
    match dir_count with
    | 0 -> ""
    | 1 -> " in 1 directory"
    | n -> Printf.sprintf " across %d directories" n
  in
  let status_parts =
    (if String.length s.search_query > 0 then
       [Printf.sprintf "filter: %s" s.search_query]
     else [])
    @ match s.sort_mode with SortAlias -> [] | m -> [sort_mode_label m]
  in
  let status_suffix =
    match status_parts with
    | [] -> ""
    | parts -> "  [" ^ String.concat " | " parts ^ "]"
  in
  [
    Widgets.themed_primary
      (Printf.sprintf " Wallets . %s%s%s" count_text dir_text status_suffix);
    Widgets.themed_muted
      "\xF0\x9F\x94\x91own \xF0\x9F\x93\x8Bwatch \xF0\x9F\x94\x92enc \
       \xF0\x9F\x94\x90hw \xE2\x9C\x92\xEF\xB8\x8Frmt \xF0\x9F\x8D\x9Ebaker | \
       j/k Tab / s +/n ?";
  ]

(* ================================================================ *)
(* Side-by-side layout                                               *)
(* ================================================================ *)

let side_by_side_min_width = 80

let render_side_by_side ~left ~right ~left_width ~total_width ~rows =
  let separator = Widgets.themed_muted " \xe2\x94\x82 " in
  let sep_column = String.concat "\n" (List.init rows (fun _ -> separator)) in
  let right_width = total_width - left_width - 3 in
  let grid =
    Grid.create
      ~rows:[Grid.Fr 1.]
      ~cols:[Grid.Px left_width; Grid.Px 3; Grid.Px right_width]
      [
        Grid.cell ~row:0 ~col:0 (fun ~size:_ -> left);
        Grid.cell ~row:0 ~col:1 (fun ~size:_ -> sep_column);
        Grid.cell ~row:0 ~col:2 (fun ~size:_ -> right);
      ]
  in
  Grid.render grid ~size:{LTerm_geom.rows; cols = total_width}

(* ================================================================ *)
(* Main view                                                         *)
(* ================================================================ *)

let default_client_base_dir = Base_dir_discovery.default_client_base_dir

let view s ~focus ~size =
  let body =
    if s.groups = [] && String.length s.search_query = 0 then
      [
        "";
        Widgets.themed_muted "  No keys found in any base directory.";
        "";
        Widgets.themed_muted "  Keys are stored in:";
        Widgets.themed_muted
          (Printf.sprintf "    . %s (default)" (default_client_base_dir ()));
        Widgets.themed_muted
          "    . Managed base directories from baker/accuser instances";
        "";
        Widgets.themed_muted "  To get started:";
        Widgets.themed_muted
          "    . Press + or n to create a new key or import an address";
        Widgets.themed_muted
          "    . Or install a baker/accuser to create a managed wallet";
      ]
      |> String.concat "\n"
    else if s.nav_items = [] && String.length s.search_query > 0 then
      [
        "";
        Widgets.themed_muted
          (Printf.sprintf "  No keys match filter: %s" s.search_query);
        "";
        Widgets.themed_muted "  Press Esc to clear the search filter.";
      ]
      |> String.concat "\n"
    else
      let cols = size.LTerm_geom.cols in
      let content_rows = size.LTerm_geom.rows - 5 in
      if cols >= side_by_side_min_width then
        let left_width = min 60 (cols * 2 / 5) in
        let right_width = cols - left_width - 3 in
        let left =
          render_list_panel ~state:s ~focus ~cols:left_width ~rows:content_rows
        in
        let right = render_detail_panel ~state:s ~box_width:right_width in
        render_side_by_side
          ~left
          ~right
          ~left_width
          ~total_width:cols
          ~rows:content_rows
      else render_list_panel ~state:s ~focus ~cols ~rows:content_rows
  in
  Page_layout.render_layout ~size ~header:(header s) ~footer:[] ~child:(fun _ ->
      body)
