(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module Widgets = Miaou_widgets_display.Widgets
module Grid = Miaou_widgets_layout.Grid_layout
module Box = Miaou_widgets_layout.Box_widget
module Desc_list = Miaou_widgets_display.Description_list
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation

let name = "keys"

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
}

(** Items in the flattened navigation list. *)
type nav_item =
  | GroupHeader of enriched_group
  | KeyItem of enriched_group * Keys_reader.key_metadata

type focus_panel = ListPanel | DetailPanel

type sort_mode = SortAlias | SortBalance | SortNetwork

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

let sort_mode_label = function
  | SortAlias -> "Alias A-Z"
  | SortBalance -> "Balance"
  | SortNetwork -> "Network"

type msg = unit

type pstate = state Navigation.t

(* ================================================================ *)
(* Data loading (I/O — called in init, not in view)                  *)
(* ================================================================ *)

let default_client_base_dir () =
  Filename.concat (Paths.home_dir ()) ".tezos-client"

let normalize_path path =
  let len = String.length path in
  if len > 1 && String.get path (len - 1) = '/' then String.sub path 0 (len - 1)
  else path

let get_all_base_dirs () =
  let default_dir = default_client_base_dir () in
  let managed_dirs =
    match Directory_registry.list ~dir_type:Client_base_dir () with
    | Ok entries ->
        List.map
          (fun (e : Directory_registry.directory_entry) -> e.path)
          entries
    | Error _ -> []
  in
  let all_dirs = default_dir :: managed_dirs in
  let normalized = List.map normalize_path all_dirs in
  List.sort_uniq String.compare normalized |> List.sort String.compare

(** Find registered services for a base directory path. *)
let services_for_dir base_dir =
  match Directory_registry.find_by_path base_dir with
  | Ok (Some entry) -> entry.registered_services
  | _ -> []

(** Find networks used by services associated with a base directory. *)
let networks_for_services service_names =
  let services = Data.load_service_states () in
  List.filter_map
    (fun (st : Data.Service_state.t) ->
      if List.exists (String.equal st.service.instance) service_names then
        Some (Network_name.normalize st.service.network)
      else None)
    services
  |> List.sort_uniq String.compare

let load_enriched_group base_dir =
  let services = services_for_dir base_dir in
  let networks = networks_for_services services in
  match Keys_reader.read_keys_full ~base_dir with
  | Ok keys -> {base_dir; keys; error = None; services; networks}
  | Error (`Msg msg) ->
      if Sys.file_exists base_dir then
        {base_dir; keys = []; error = Some msg; services; networks}
      else {base_dir; keys = []; error = None; services; networks}

(** Build the flat navigation list from groups and fold state. *)
let build_nav_items ~folded groups =
  List.concat_map
    (fun group ->
      let header = GroupHeader group in
      if StringSet.mem group.base_dir folded then [header]
      else header :: List.map (fun key -> KeyItem (group, key)) group.keys)
    groups

let count_keys groups =
  List.fold_left (fun acc g -> acc + List.length g.keys) 0 groups

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

(** Filter groups by search query (case-insensitive match on alias or PKH). *)
let filter_groups ~query groups =
  if String.length query = 0 then groups
  else
    let q = String.lowercase_ascii query in
    List.filter_map
      (fun (g : enriched_group) ->
        let matching_keys =
          List.filter
            (fun (k : Keys_reader.key_metadata) ->
              let alias_match =
                contains_substring
                  (String.lowercase_ascii
                     (display_alias ~base_dir:g.base_dir k))
                  q
              in
              let pkh_match =
                contains_substring (String.lowercase_ascii k.pkh) q
              in
              alias_match || pkh_match)
            g.keys
        in
        if matching_keys <> [] then Some {g with keys = matching_keys} else None)
      groups

(** Sort keys within groups by the given mode. *)
let sort_groups ~mode groups =
  List.map
    (fun (g : enriched_group) ->
      let keys =
        match mode with
        | SortAlias ->
            List.sort
              (fun (a : Keys_reader.key_metadata)
                   (b : Keys_reader.key_metadata)
                 ->
                String.compare
                  (display_alias ~base_dir:g.base_dir a)
                  (display_alias ~base_dir:g.base_dir b))
              g.keys
        | SortBalance ->
            List.sort
              (fun (a : Keys_reader.key_metadata)
                   (b : Keys_reader.key_metadata)
                 ->
                let bal_of k =
                  match
                    Keys_scheduler.get_wallet_data ~pkh:k.Keys_reader.pkh
                  with
                  | wd :: _ -> (
                      match
                        int_of_string_opt wd.Keys_scheduler.full_balance
                      with
                      | Some n -> n
                      | None -> 0)
                  | [] -> 0
                in
                Int.compare (bal_of b) (bal_of a))
              g.keys
        | SortNetwork ->
            List.sort
              (fun (a : Keys_reader.key_metadata)
                   (b : Keys_reader.key_metadata)
                 ->
                let net_of k =
                  match
                    Keys_scheduler.get_wallet_data ~pkh:k.Keys_reader.pkh
                  with
                  | wd :: _ -> wd.Keys_scheduler.network
                  | [] -> ""
                in
                String.compare (net_of a) (net_of b))
              g.keys
      in
      {g with keys})
    groups

(** Rebuild nav_items applying current search and sort, preserving cursor. *)
let rebuild_nav s =
  let filtered = filter_groups ~query:s.search_query s.groups in
  let sorted = sort_groups ~mode:s.sort_mode filtered in
  let nav_items = build_nav_items ~folded:s.folded sorted in
  let cursor = min s.cursor (max 0 (List.length nav_items - 1)) in
  {s with nav_items; cursor}

(** Backward-compatible: returns (pkh, alias, base_dir) tuples. *)
let get_all_keys () =
  let all_dirs = get_all_base_dirs () in
  all_dirs
  |> List.map (fun base_dir ->
      match Keys_reader.read_public_key_hashes ~base_dir with
      | Ok keys ->
          List.map
            (fun (k : Keys_reader.key_info) -> (k.value, k.name, base_dir))
            keys
      | Error _ -> [])
  |> List.flatten

let init () =
  let all_dirs = get_all_base_dirs () in
  let groups =
    all_dirs
    |> List.map load_enriched_group
    |> List.filter (fun g ->
        g.keys <> [] || g.error <> None || g.services <> [])
  in
  (* Register keys with the background scheduler *)
  let keys_by_dir =
    List.map
      (fun (g : enriched_group) ->
        ( g.base_dir,
          List.map (fun (k : Keys_reader.key_metadata) -> k.pkh) g.keys ))
      groups
  in
  Keys_scheduler.set_keys keys_by_dir ;
  Keys_scheduler.start () ;
  (* Load OM key alias overrides from disk *)
  Key_aliases.load () ;
  (* Load tzkt alias disk caches for known networks *)
  let all_networks =
    List.concat_map (fun (g : enriched_group) -> g.networks) groups
    |> List.sort_uniq String.compare
  in
  List.iter (fun network -> Tzkt_aliases.load ~network) all_networks ;
  let folded = StringSet.empty in
  let nav_items = build_nav_items ~folded groups in
  let total_keys = count_keys groups in
  Navigation.make
    {
      groups;
      nav_items;
      cursor = 0;
      folded;
      focus_panel = ListPanel;
      scroll_offset = 0;
      total_keys;
      search_query = "";
      sort_mode = SortAlias;
      multi_select = false;
      selected = StringSet.empty;
    }

let update ps _ = ps

let refresh ps = ps

let move ps _ = ps

let service_select ps _ = ps

(** Tracks the PKH the cursor has been resting on, and since when. *)
let focused_pkh : string option ref = ref None

let focused_since : float ref = ref 0.0

let focus_debounce = 2.0

let service_cycle ps _ =
  let ps =
    if Context.consume_keys_dirty () then (
      let all_dirs = get_all_base_dirs () in
      let groups =
        all_dirs
        |> List.map load_enriched_group
        |> List.filter (fun g ->
            g.keys <> [] || g.error <> None || g.services <> [])
      in
      let s = ps.Navigation.s in
      let nav_items = build_nav_items ~folded:s.folded groups in
      let total_keys = count_keys groups in
      let cursor = min s.cursor (max 0 (List.length nav_items - 1)) in
      (* Update scheduler with new key set *)
      let keys_by_dir =
        List.map
          (fun (g : enriched_group) ->
            ( g.base_dir,
              List.map (fun (k : Keys_reader.key_metadata) -> k.pkh) g.keys ))
          groups
      in
      Keys_scheduler.set_keys keys_by_dir ;
      {ps with s = {s with groups; nav_items; total_keys; cursor}})
    else ps
  in
  (* Auto-fetch for the focused key after debounce *)
  let s = ps.Navigation.s in
  let current_pkh =
    match List.nth_opt s.nav_items s.cursor with
    | Some (KeyItem (_, key)) -> Some key.pkh
    | _ -> None
  in
  let now = Unix.gettimeofday () in
  (match (current_pkh, !focused_pkh) with
  | Some pkh, Some prev when String.equal pkh prev ->
      if now -. !focused_since >= focus_debounce then
        Keys_scheduler.request_fetch ~pkh
  | Some pkh, _ ->
      focused_pkh := Some pkh ;
      focused_since := now
  | None, _ -> focused_pkh := None) ;
  ps

let back ps = Navigation.back ps

let handled_keys () = Miaou.Core.Keys.[Escape]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "Esc" "Back"; kb "?" "Help"]

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
  let fold_indicator = if is_folded then "▸ " else "▾ " in
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
                | Data.Service_state.Running -> Widgets.green "●"
                | Stopped -> Widgets.red "●"
                | Unknown _ -> Widgets.yellow "●")
            | None -> Widgets.themed_muted "○")
          group.services
      in
      " " ^ String.concat "" dots
  in
  let key_count =
    let n = List.length group.keys in
    Widgets.themed_muted
      (Printf.sprintf " (%d key%s)" n (if n = 1 then "" else "s"))
  in
  Printf.sprintf
    "%s%s%s%s%s"
    marker
    fold_indicator
    (Widgets.themed_primary dir_display)
    svc_dots
    key_count

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

(** Format mutez as tez with 6 decimal places. *)
let format_tez mutez_str =
  match int_of_string_opt mutez_str with
  | None -> mutez_str
  | Some mutez ->
      let tez = mutez / 1_000_000 in
      let frac = abs (mutez mod 1_000_000) in
      Printf.sprintf "%d.%06d ꜩ" tez frac

let render_dir_detail ~box_width group =
  let items =
    [("Path", group.base_dir)]
    @ (if group.services <> [] then
         [("Services", String.concat ", " group.services)]
       else [])
    @ (if group.networks <> [] then
         [("Networks", String.concat ", " group.networks)]
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
  (* Balance section from scheduler cache *)
  let wallet_data = Keys_scheduler.get_wallet_data ~pkh:key.pkh in
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
              ~title:("Balance: " ^ wd.network)
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
      (Printf.sprintf " Keys . %s%s%s" count_text dir_text status_suffix);
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
  let separator = Widgets.themed_muted " │ " in
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

let view ps ~focus ~size =
  let s = ps.Navigation.s in
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
  Themed_page.render_layout ~size ~header:(header s) ~footer:[] ~child:(fun _ ->
      body)

(* ================================================================ *)
(* Modal handling                                                    *)
(* ================================================================ *)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

(* ================================================================ *)
(* Navigation and keyboard handling                                  *)
(* ================================================================ *)

(* rebuild_nav defined earlier — applies search, sort, and fold. *)

(** Ensure cursor is visible by adjusting scroll_offset. *)
let ensure_visible ~rows state =
  let offset = state.scroll_offset in
  if state.cursor < offset then {state with scroll_offset = state.cursor}
  else if state.cursor >= offset + rows then
    {state with scroll_offset = state.cursor - rows + 1}
  else state

let move_cursor delta ~size ps =
  let rows = size.LTerm_geom.rows - 5 in
  Navigation.update
    (fun s ->
      let total = List.length s.nav_items in
      if total = 0 then s
      else
        let cursor = max 0 (min (total - 1) (s.cursor + delta)) in
        ensure_visible ~rows {s with cursor})
    ps

let jump_to_top ~size ps =
  let rows = size.LTerm_geom.rows - 5 in
  Navigation.update (fun s -> ensure_visible ~rows {s with cursor = 0}) ps

let jump_to_bottom ~size ps =
  let rows = size.LTerm_geom.rows - 5 in
  Navigation.update
    (fun s ->
      let total = List.length s.nav_items in
      ensure_visible ~rows {s with cursor = max 0 (total - 1)})
    ps

let toggle_fold ~size ps =
  let rows = size.LTerm_geom.rows - 5 in
  Navigation.update
    (fun s ->
      match List.nth_opt s.nav_items s.cursor with
      | Some (GroupHeader group) ->
          let folded =
            if StringSet.mem group.base_dir s.folded then
              StringSet.remove group.base_dir s.folded
            else StringSet.add group.base_dir s.folded
          in
          let s = {s with folded} in
          let s = rebuild_nav s in
          ensure_visible ~rows s
      | _ -> s)
    ps

let switch_panel ps =
  Navigation.update
    (fun s ->
      let focus_panel =
        match s.focus_panel with
        | ListPanel -> DetailPanel
        | DetailPanel -> ListPanel
      in
      {s with focus_panel})
    ps

(* ================================================================ *)
(* Key actions                                                       *)
(* ================================================================ *)

(** Find an octez-client binary for a base directory. Checks associated
    services first, then falls back to PATH. *)
let find_octez_client ~base_dir =
  let from_service =
    let svcs = services_for_dir base_dir in
    let states = Data.load_service_states () in
    List.find_map
      (fun svc_name ->
        match
          List.find_opt
            (fun (st : Data.Service_state.t) ->
              String.equal st.service.instance svc_name)
            states
        with
        | Some st ->
            let path = Filename.concat st.service.app_bin_dir "octez-client" in
            if Sys.file_exists path then Some path else None
        | None -> None)
      svcs
  in
  match from_service with
  | Some path -> Some path
  | None -> (
      (* 2. Try managed binary downloads (latest version first) *)
      let from_managed =
        match Binary_registry.list_managed_versions () with
        | Ok versions ->
            List.find_map
              (fun version ->
                let dir = Binary_registry.managed_version_path version in
                let path = Filename.concat dir "octez-client" in
                if Sys.file_exists path then Some path else None)
              versions
        | Error _ -> None
      in
      match from_managed with
      | Some path -> Some path
      | None -> (
          (* 3. Fall back to PATH *)
          match Cmd_runner.run_out_silent ["which"; "octez-client"] with
          | Ok path ->
              let p = String.trim path in
              if String.equal p "" then None else Some p
          | Error _ -> None))

(** Try to copy text to system clipboard using available tools.
    Returns Ok if successful, Error if no clipboard tool worked.
    This function runs in a background domain to avoid blocking the TUI. *)
let try_copy_to_system_clipboard text =
  (* Try xclip, xsel, wl-copy, pbcopy in order *)
  let tools =
    [
      ["xclip"; "-selection"; "clipboard"];
      ["xsel"; "--clipboard"; "--input"];
      ["wl-copy"];
      ["pbcopy"];
    ]
  in
  let success =
    List.exists
      (fun tool ->
        match
          Cmd_runner.run_out_silent
            (["sh"; "-c"]
            @ [
                Printf.sprintf
                  "echo -n %s | %s"
                  (Cmd_runner.sh_quote text)
                  (Cmd_runner.cmd_to_string tool);
              ])
        with
        | Ok _ -> true
        | Error _ -> false)
      tools
  in
  if success then Ok () else Error (`Msg "No clipboard tool available")

(** Copy PKH to clipboard using background job to avoid blocking the TUI.
    Shows a success toast if copied, or an info toast with the PKH if clipboard
    tools are not available. *)
let copy_to_clipboard pkh =
  Job_manager.submit
    ~timeout:(Some 2.0)
    ~description:"Copy to clipboard"
    (fun ~append_log:_ () -> try_copy_to_system_clipboard pkh)
    ~on_complete:(fun status ->
      match status with
      | Job_manager.Succeeded ->
          Context.toast_success (Printf.sprintf "Copied: %s" pkh)
      | Job_manager.Failed _ | Job_manager.Pending | Job_manager.Running ->
          Context.toast_info (Printf.sprintf "PKH: %s" pkh))

(** When octez-client is not found, offer to download the latest Octez
    version. Falls back to a plain error if no versions are available. *)
let offer_download_or_error ~action_label =
  match Versions_scheduler.get_cached () with
  | Some (latest :: _) ->
      Modal_helpers.confirm_modal
        ~title:"octez-client not found"
        ~message:
          (Printf.sprintf
             "Cannot %s without octez-client.\n\
              Download Octez v%s (includes octez-client)?"
             action_label
             latest.Binary_downloader.version)
        ~on_result:(fun confirmed ->
          if confirmed then Binaries_actions.download_octez_version latest)
        ()
  | _ ->
      Modal_helpers.show_error
        ~title:"Error"
        (Printf.sprintf
           "octez-client not found. Cannot %s.\n\
            Download Octez binaries from the Binaries page first."
           action_label)

(** Address entry for the pick-address modal. *)
type address_entry = {
  label : string;
  pkh : string;
  category : string;
  balance : string option;
  is_delegate : bool;
  delegate_alias : string option;
  is_mine : bool;
  last_used_at : float;
}

(** Look up the best wallet_data entry for a PKH (first network found). *)
let lookup_wallet_data ~pkh =
  match Keys_scheduler.get_wallet_data ~pkh with
  | wd :: _ -> Some wd
  | [] -> None

(** Build delegate display string from wallet data. *)
let delegate_alias_of_wd (wd : Keys_scheduler.wallet_data) =
  match wd.delegate with
  | Some d -> Tzkt_aliases.find ~network:wd.network ~pkh:d
  | None ->
      if wd.is_registered then Tzkt_aliases.find ~network:wd.network ~pkh:wd.pkh
      else None

(** Collect known addresses from wallet keys and MRU history.
    Excludes [exclude_pkh] (the source key) from the list. *)
let collect_addresses ~exclude_pkh ~include_mru =
  let all_dirs = get_all_base_dirs () in
  (* Build a set of all signable PKHs across all wallets *)
  let signable_pkhs = Hashtbl.create 32 in
  let mru_list = Transfer_mru.get () in
  let mru_by_pkh = Hashtbl.create 16 in
  List.iter
    (fun (e : Transfer_mru.entry) ->
      Hashtbl.replace mru_by_pkh e.pkh e.last_used_at)
    mru_list ;
  (* First pass: collect signable PKHs *)
  List.iter
    (fun dir ->
      match Keys_reader.read_keys_full ~base_dir:dir with
      | Ok keys ->
          List.iter
            (fun (k : Keys_reader.key_metadata) ->
              if k.has_secret_key then Hashtbl.replace signable_pkhs k.pkh true)
            keys
      | Error _ -> ())
    all_dirs ;
  let wallet_entries =
    List.concat_map
      (fun dir ->
        match Keys_reader.read_keys_full ~base_dir:dir with
        | Ok keys ->
            List.filter_map
              (fun (k : Keys_reader.key_metadata) ->
                if String.equal k.pkh exclude_pkh then None
                else
                  let wd = lookup_wallet_data ~pkh:k.pkh in
                  let balance =
                    Option.map
                      (fun (w : Keys_scheduler.wallet_data) ->
                        w.spendable_balance)
                      wd
                  in
                  let is_delegate =
                    match wd with Some w -> w.is_registered | None -> false
                  in
                  let delegate_alias =
                    match wd with
                    | Some w -> delegate_alias_of_wd w
                    | None -> None
                  in
                  let last_used_at =
                    match Hashtbl.find_opt mru_by_pkh k.pkh with
                    | Some t -> t
                    | None -> 0.0
                  in
                  Some
                    {
                      label = display_alias ~base_dir:dir k;
                      pkh = k.pkh;
                      category = "Wallet";
                      balance;
                      is_delegate;
                      delegate_alias;
                      is_mine = k.has_secret_key;
                      last_used_at;
                    })
              keys
        | Error _ -> [])
      all_dirs
  in
  (* Deduplicate by PKH — same key may appear in multiple base dirs *)
  let seen = Hashtbl.create 16 in
  let wallet_entries =
    List.filter
      (fun e ->
        if Hashtbl.mem seen e.pkh then false
        else (
          Hashtbl.replace seen e.pkh true ;
          true))
      wallet_entries
  in
  let mru_entries =
    if include_mru then
      mru_list
      |> List.filter_map (fun (e : Transfer_mru.entry) ->
          if String.equal e.pkh exclude_pkh || Hashtbl.mem seen e.pkh then None
          else
            let wd = lookup_wallet_data ~pkh:e.pkh in
            let balance =
              Option.map
                (fun (w : Keys_scheduler.wallet_data) -> w.spendable_balance)
                wd
            in
            let is_delegate =
              match wd with Some w -> w.is_registered | None -> false
            in
            let delegate_alias =
              match wd with Some w -> delegate_alias_of_wd w | None -> None
            in
            let label = match e.alias with Some a -> a | None -> "" in
            Some
              {
                label;
                pkh = e.pkh;
                category = "Recent";
                balance;
                is_delegate;
                delegate_alias;
                is_mine = Hashtbl.mem signable_pkhs e.pkh;
                last_used_at = e.last_used_at;
              })
    else []
  in
  (* Combine and sort: MRU entries first (by recency), then alphabetically *)
  let all = wallet_entries @ mru_entries in
  List.sort
    (fun a b ->
      match (a.last_used_at > 0.0, b.last_used_at > 0.0) with
      | true, true ->
          (* Both have MRU timestamps: most recent first *)
          Float.compare b.last_used_at a.last_used_at
      | true, false -> -1
      | false, true -> 1
      | false, false ->
          (* Both non-MRU: alphabetical by label *)
          String.compare a.label b.label)
    all

(** Format a short PKH for display in the picker: first 7 + last 4 chars. *)
let short_pkh pkh =
  let len = String.length pkh in
  if len <= 15 then pkh
  else String.sub pkh 0 7 ^ ".." ^ String.sub pkh (len - 4) 4

(** Render one address entry as a rich display string. *)
let address_entry_to_string e =
  let icon =
    if String.equal e.category "Recent" && not e.is_mine then
      "\xE2\x8F\xB1\xEF\xB8\x8F " (* ⏱️ *)
    else if e.is_mine then "\xF0\x9F\x94\x91 " (* 🔑 *)
    else "\xF0\x9F\x93\x8B "
    (* 📋 *)
  in
  let name_part =
    if String.length e.label > 0 then
      let max_len = 14 in
      let truncated =
        if String.length e.label > max_len then
          String.sub e.label 0 (max_len - 1) ^ "~"
        else e.label
      in
      Printf.sprintf "%-14s" truncated
    else Printf.sprintf "%-14s" (short_pkh e.pkh)
  in
  let bal_part =
    match e.balance with Some b -> " " ^ format_tez b | None -> ""
  in
  let delegate_part =
    if e.is_delegate then
      match e.delegate_alias with
      | Some a -> " \xF0\x9F\x8D\x9E " ^ a (* 🍞 *)
      | None -> " \xF0\x9F\x8D\x9E" (* 🍞 *)
    else
      match e.delegate_alias with
      | Some a -> " \xE2\x86\x92 " ^ a (* → *)
      | None -> ""
  in
  Printf.sprintf "%s%s%s%s" icon name_part bal_part delegate_part

(** Build describe lines for the hint panel. *)
let address_entry_describe = function
  | `Known e ->
      let lines = [Printf.sprintf "Address: %s" e.pkh] in
      let lines =
        match e.balance with
        | Some b -> lines @ [Printf.sprintf "Spendable: %s" (format_tez b)]
        | None -> lines @ ["Balance: unknown"]
      in
      let lines =
        if e.is_delegate then
          lines
          @ [
              Printf.sprintf
                "Registered delegate%s"
                (match e.delegate_alias with
                | Some a -> " (" ^ a ^ ")"
                | None -> "");
            ]
        else
          match e.delegate_alias with
          | Some a -> lines @ [Printf.sprintf "Delegating to: %s" a]
          | None -> lines
      in
      let lines =
        if e.is_mine then lines @ ["Signable (secret key available)"]
        else lines @ ["Read-only address"]
      in
      lines
  | `Custom -> ["Enter a public key hash manually"]

(** Open an address picker modal. Shows wallet keys and optionally MRU
    destinations, with a "Custom PKH" option at the end.
    @param title Modal title
    @param exclude_pkh PKH to exclude (the source key)
    @param include_mru Whether to include MRU transfer destinations
    @param on_select Called with the selected PKH *)
let pick_address ~title ~exclude_pkh ~include_mru ~on_select =
  let entries = collect_addresses ~exclude_pkh ~include_mru in
  let items = List.map (fun e -> `Known e) entries @ [`Custom] in
  Modal_helpers.open_choice_modal_with_hint
    ~title
    ~items
    ~to_string:(function
      | `Known e -> address_entry_to_string e | `Custom -> "Enter custom PKH...")
    ~hint:(fun _ -> ())
    ~describe:address_entry_describe
    ~on_select:(function
      | `Known e -> on_select e.pkh
      | `Custom ->
          Modal_helpers.prompt_text_modal
            ~title:(title ^ ": PKH")
            ~width:50
            ~initial:""
            ~placeholder:(Some "tz1...")
            ~on_submit:(fun pkh ->
              let pkh = String.trim pkh in
              match Pkh_validator.validate_format pkh with
              | Pkh_validator.Invalid reason ->
                  Modal_helpers.show_error
                    ~title:"Invalid PKH"
                    (Printf.sprintf "Invalid PKH: %s" reason)
              | Pkh_validator.Valid -> on_select pkh)
            ())
    ()

(** Build an alias validator for a given wallet directory.
    Reads keys once and returns a validator closure.

    @param exclude_pkh PKH to exclude from conflict checks (the key being
    renamed). *)
let make_alias_validator ~base_dir ?exclude_pkh () =
  let keys =
    match Keys_reader.read_keys_full ~base_dir with
    | Ok ks -> ks
    | Error _ -> []
  in
  let other_keys =
    match exclude_pkh with
    | Some pkh ->
        List.filter
          (fun (k : Keys_reader.key_metadata) -> not (String.equal k.pkh pkh))
          keys
    | None -> keys
  in
  fun alias ->
    let alias = String.trim alias in
    if String.length alias = 0 then Error "Alias cannot be empty"
    else
      let display_conflict =
        List.find_opt
          (fun (k : Keys_reader.key_metadata) ->
            String.equal (display_alias ~base_dir k) alias)
          other_keys
      in
      match display_conflict with
      | Some k ->
          Error
            (Printf.sprintf
               "Name '%s' is already used by key %s"
               alias
               (short_pkh k.pkh))
      | None -> (
          let hidden_conflict =
            List.find_opt
              (fun (k : Keys_reader.key_metadata) ->
                String.equal k.alias alias
                && not (String.equal (display_alias ~base_dir k) alias))
              other_keys
          in
          match hidden_conflict with
          | Some k ->
              Error
                (Printf.sprintf
                   "Name '%s' exists in wallet files (shown as '%s')"
                   alias
                   (display_alias ~base_dir k))
          | None -> Ok ())

(** Rename a key alias (OM-level alias override). *)
let action_rename ~base_dir (key : Keys_reader.key_metadata) =
  let current_display = display_alias ~base_dir key in
  let validator = make_alias_validator ~base_dir ~exclude_pkh:key.pkh () in
  Modal_helpers.prompt_validated_text_modal
    ~title:(Printf.sprintf "Rename '%s'" current_display)
    ~width:40
    ~initial:current_display
    ~placeholder:(Some "New alias")
    ~validator
    ~on_submit:(fun new_alias ->
      let new_alias = String.trim new_alias in
      if String.equal new_alias current_display then ()
      else (
        Key_aliases.set ~base_dir ~pkh:key.pkh ~alias:new_alias ;
        Context.toast_success
          (Printf.sprintf "Renamed '%s' to '%s'" current_display new_alias) ;
        Context.mark_keys_dirty ()))
    ()

(** Forget (remove) a key from the wallet. *)
let action_forget ~base_dir (key : Keys_reader.key_metadata) =
  Modal_helpers.confirm_modal
    ~title:(Printf.sprintf "Forget key '%s'?" (display_alias ~base_dir key))
    ~message:
      (Printf.sprintf
         "PKH: %s\nThis removes the key from this wallet only."
         key.pkh)
    ~on_result:(fun confirmed ->
      if confirmed then
        match find_octez_client ~base_dir with
        | None -> offer_download_or_error ~action_label:"forget key"
        | Some client ->
            let args =
              [
                client;
                "--base-dir";
                base_dir;
                "forget";
                "address";
                key.alias;
                "--force";
              ]
            in
            Job_manager.submit
              ~description:"Forget key"
              (fun ~append_log:_ () -> Cmd_runner.run_silent args)
              ~on_complete:(fun status ->
                match status with
                | Job_manager.Succeeded ->
                    Key_aliases.remove ~base_dir ~pkh:key.pkh ;
                    Context.toast_success
                      (Printf.sprintf
                         "Removed '%s' from wallet"
                         (display_alias ~base_dir key)) ;
                    Context.mark_keys_dirty ()
                | Job_manager.Failed msg ->
                    Context.toast_error (Printf.sprintf "Forget failed: %s" msg)
                | _ -> ()))
    ()

(** Execute an octez-client command with spinner and toast feedback.
    For local operations (gen keys, add address) that don't go on-chain. *)
let run_client_action ~base_dir ~description ~args ~on_success () =
  match find_octez_client ~base_dir with
  | None -> offer_download_or_error ~action_label:"perform operation"
  | Some client ->
      let full_args = client :: "--base-dir" :: base_dir :: args in
      Modal_helpers.show_spinner_modal
        ~title:description
        ~label:description
        ~work:(fun () -> Cmd_runner.run_silent full_args)
        ~on_complete:(function
          | `Succeeded ->
              on_success () ;
              Context.mark_keys_dirty ()
          | `Failed msg ->
              Context.toast_error
                (Printf.sprintf "%s failed: %s" description msg)
          | `Cancelled -> ())
        ()

(** Extract a human-readable error from Cmd_runner error messages.
    Cmd_runner returns ["Command failed: <full_path>\nOutput:\n<output>"].
    We look for ["Fatal error:"] or ["Error:"] lines and return from there. *)
let is_boilerplate line =
  let t = String.trim line in
  t = ""
  || contains_substring t "Command failed:"
  || contains_substring t "Output:"
  || contains_substring t "Node is bootstrapped"
  || contains_substring t "Estimated gas:"
  || contains_substring t "Estimated storage:"
  || contains_substring t "This simulation failed"
  || contains_substring t "Manager signed operations"
  || contains_substring t "From:"
  || contains_substring t "Fee to the"
  || contains_substring t "Expected counter:"
  || contains_substring t "Gas limit:"
  || contains_substring t "Storage limit:"
  || contains_substring t "Amount:"
  || contains_substring t "Stake:"
  || contains_substring t "Transfer:"
  || contains_substring t "Delegation:"

let extract_client_error raw_err =
  let lines = String.split_on_char '\n' raw_err in
  (* Find the index of the "Fatal error" line *)
  let fatal_idx =
    let rec find i = function
      | [] -> None
      | line :: rest ->
          if contains_substring (String.trim line) "Fatal error" then Some i
          else find (i + 1) rest
    in
    find 0 lines
  in
  match fatal_idx with
  | Some fi ->
      (* Scan backwards from Fatal error, collecting non-boilerplate
         lines that describe the actual error. *)
      let prev_desc =
        let rec scan i acc =
          if i < 0 then acc
          else
            let line = List.nth lines i in
            if is_boilerplate line then acc
            else scan (i - 1) (String.trim line :: acc)
        in
        scan (fi - 1) []
      in
      let fatal_and_rest =
        let rec drop n = function
          | [] -> []
          | _ :: rest when n > 0 -> drop (n - 1) rest
          | l -> l
        in
        drop fi lines |> List.map String.trim |> List.filter (fun s -> s <> "")
      in
      let all = prev_desc @ fatal_and_rest in
      if all <> [] then String.concat "\n" all else raw_err
  | None -> (
      (* Look for "Error:" lines *)
      let rec find_error_line = function
        | [] -> None
        | line :: rest ->
            let trimmed = String.trim line in
            if contains_substring trimmed "Error:" then
              let tail = trimmed :: List.map String.trim rest in
              Some (String.concat "\n" (List.filter (fun s -> s <> "") tail))
            else find_error_line rest
      in
      match find_error_line lines with
      | Some msg -> msg
      | None ->
          (* Fallback: strip boilerplate prefixes *)
          let useful =
            lines |> List.map String.trim
            |> List.filter (fun l -> not (is_boilerplate l))
          in
          if useful <> [] then String.concat "\n" useful else raw_err)

(** Execute an on-chain octez-client operation with tracking modal.
    Shows a real-time checklist (submitting → included → confirmed → finalized).
    Adds [--burn-cap 1] automatically.
    @param network Network name for explorer links in tracking modal.
    @param on_done Optional cleanup callback invoked regardless of outcome. *)
let run_onchain_operation ~base_dir ~description ~args ~network
    ?(on_done = fun () -> ()) ~on_success () =
  match find_octez_client ~base_dir with
  | None ->
      on_done () ;
      offer_download_or_error ~action_label:"perform operation"
  | Some client ->
      let full_args =
        (client :: "--base-dir" :: base_dir :: args) @ ["--burn-cap"; "1"]
      in
      let endpoint =
        let endpoints = Keys_scheduler.get_endpoints_for_network ~network in
        match endpoints with ep :: _ -> Some ep | [] -> None
      in
      let step_ref = Atomic.make Instances_wallet.Submitting in
      Instances_wallet.open_tracking_modal ~title:description ~network ~step_ref ;
      Job_manager.submit
        ~timeout:None
        ~description
        (fun ~append_log:_ () ->
          match Cmd_runner.run_out_with_timeout ~timeout:100.0 full_args with
          | Ok output -> (
              on_done () ;
              Context.mark_keys_dirty () ;
              match Baker_ops.extract_op_hash output with
              | Some op_hash ->
                  Atomic.set step_ref (Instances_wallet.Submitted {op_hash}) ;
                  Context.toast_success (description ^ ": operation submitted") ;
                  on_success () ;
                  (match endpoint with
                  | Some ep ->
                      Instances_wallet.poll_operation
                        ~endpoint:ep
                        ~op_hash
                        step_ref
                  | None -> ()) ;
                  Ok ()
              | None ->
                  Atomic.set
                    step_ref
                    (Instances_wallet.Failed "No operation hash returned") ;
                  on_success () ;
                  Ok ())
          | Error (`Msg err) ->
              on_done () ;
              let clean_err = extract_client_error err in
              Atomic.set step_ref (Instances_wallet.Failed clean_err) ;
              Context.toast_error
                (Printf.sprintf "%s failed: %s" description clean_err) ;
              Error (`Msg clean_err))
        ~on_complete:(fun _status -> ())

(** Show a confirmation modal, then execute on-chain with tracking.
    Matches the baker wallet flow: confirm → execute → track.
    No dry-run simulation to avoid blocking the node RPC worker. *)
let styled_network network =
  if String.equal network "mainnet" then Widgets.themed_warning network
  else network

let confirm_and_run ~base_dir ~title ~message ~args ~network
    ?(on_done = fun () -> ()) ~on_success () =
  let full_message =
    Printf.sprintf "%s\nNetwork: %s" message (styled_network network)
  in
  Modal_helpers.confirm_modal
    ~title
    ~message:full_message
    ~on_result:(fun confirmed ->
      if confirmed then
        run_onchain_operation
          ~base_dir
          ~description:title
          ~args
          ~network
          ~on_done
          ~on_success
          ()
      else on_done ())
    ()

(** If the key is encrypted, prompt for password and call [action] with
    extra [--password-filename] args. Otherwise call [action] directly.
    The temporary password file is cleaned up via [~on_done] in the action. *)
let with_password_if_needed ~base_dir (key : Keys_reader.key_metadata) ~action =
  match key.key_kind with
  | Encrypted ->
      Modal_helpers.prompt_password_modal
        ~title:(Printf.sprintf "Password for %s" (display_alias ~base_dir key))
        ~on_submit:(fun password ->
          let tmp = Filename.temp_file "octez-pw" "" in
          let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
          let oc = Unix.out_channel_of_descr fd in
          output_string oc password ;
          close_out oc ;
          let cleanup () = try Sys.remove tmp with _ -> () in
          action ~extra_args:["--password-filename"; tmp] ~cleanup)
        ()
  | _ -> action ~extra_args:[] ~cleanup:(fun () -> ())

(** Transfer action: pick destination from known addresses, then amount.
    Uses dry-run to estimate fees before executing. *)
let action_transfer ~base_dir ~network (key : Keys_reader.key_metadata) =
  pick_address
    ~title:"Transfer: Destination"
    ~exclude_pkh:key.pkh
    ~include_mru:true
    ~on_select:(fun dest_pkh ->
      Modal_helpers.prompt_text_modal
        ~title:"Transfer: Amount (tez)"
        ~width:30
        ~initial:""
        ~placeholder:(Some "e.g. 1.5")
        ~on_submit:(fun amount_str ->
          let amount_str = String.trim amount_str in
          match float_of_string_opt amount_str with
          | None ->
              Modal_helpers.show_error
                ~title:"Invalid amount"
                "Please enter a valid number."
          | Some _ ->
              with_password_if_needed
                ~base_dir
                key
                ~action:(fun ~extra_args ~cleanup ->
                  let description =
                    Printf.sprintf
                      "Transfer %s tez from %s"
                      amount_str
                      (display_alias ~base_dir key)
                  in
                  let endpoint_args =
                    let endpoints =
                      Keys_scheduler.get_endpoints_for_network ~network
                    in
                    match endpoints with
                    | ep :: _ -> ["--endpoint"; ep]
                    | [] -> []
                  in
                  let op_args =
                    extra_args @ endpoint_args
                    @ [
                        "transfer"; amount_str; "from"; key.alias; "to"; dest_pkh;
                      ]
                  in
                  confirm_and_run
                    ~base_dir
                    ~title:description
                    ~message:
                      (Printf.sprintf
                         "From: %s\nTo: %s\nAmount: %s tez"
                         (display_alias ~base_dir key)
                         (short_pkh dest_pkh)
                         amount_str)
                    ~args:op_args
                    ~network
                    ~on_done:cleanup
                    ~on_success:(fun () ->
                      Transfer_mru.add ~pkh:dest_pkh () ;
                      Keys_scheduler.force_refresh ~pkh:key.pkh)
                    ()))
        ())

(** Register as delegate action. *)
let action_register_delegate ~base_dir ~network (key : Keys_reader.key_metadata)
    =
  Modal_helpers.confirm_modal
    ~title:
      (Printf.sprintf
         "Register '%s' as delegate?"
         (display_alias ~base_dir key))
    ~message:
      (Printf.sprintf
         "This will register your key as a delegate.\nNetwork: %s"
         (styled_network network))
    ~on_result:(fun confirmed ->
      if confirmed then
        with_password_if_needed
          ~base_dir
          key
          ~action:(fun ~extra_args ~cleanup ->
            let description =
              Printf.sprintf
                "Register '%s' as delegate"
                (display_alias ~base_dir key)
            in
            let endpoint_args =
              let endpoints =
                Keys_scheduler.get_endpoints_for_network ~network
              in
              match endpoints with ep :: _ -> ["--endpoint"; ep] | [] -> []
            in
            run_onchain_operation
              ~base_dir
              ~description
              ~args:
                (extra_args @ endpoint_args
                @ ["register"; "key"; key.alias; "as"; "delegate"])
              ~network
              ~on_done:cleanup
              ~on_success:(fun () -> Keys_scheduler.force_refresh ~pkh:key.pkh)
              ()))
    ()

(** Delegate to another baker. *)
let action_delegate_to ~base_dir ~network (key : Keys_reader.key_metadata) =
  pick_address
    ~title:"Delegate to"
    ~exclude_pkh:key.pkh
    ~include_mru:false
    ~on_select:(fun baker_pkh ->
      with_password_if_needed ~base_dir key ~action:(fun ~extra_args ~cleanup ->
          let description =
            Printf.sprintf
              "Delegate '%s' to %s"
              (display_alias ~base_dir key)
              baker_pkh
          in
          let endpoint_args =
            let endpoints = Keys_scheduler.get_endpoints_for_network ~network in
            match endpoints with ep :: _ -> ["--endpoint"; ep] | [] -> []
          in
          run_onchain_operation
            ~base_dir
            ~description
            ~args:
              (extra_args @ endpoint_args
              @ ["set"; "delegate"; "for"; key.alias; "to"; baker_pkh])
            ~network
            ~on_done:cleanup
            ~on_success:(fun () -> Keys_scheduler.force_refresh ~pkh:key.pkh)
            ()))

(** Undelegate action. *)
let action_undelegate ~base_dir ~network (key : Keys_reader.key_metadata) =
  Modal_helpers.confirm_modal
    ~title:(Printf.sprintf "Undelegate '%s'?" (display_alias ~base_dir key))
    ~message:
      (Printf.sprintf
         "This will withdraw the delegation.\nNetwork: %s"
         (styled_network network))
    ~on_result:(fun confirmed ->
      if confirmed then
        with_password_if_needed
          ~base_dir
          key
          ~action:(fun ~extra_args ~cleanup ->
            let description =
              Printf.sprintf "Undelegate '%s'" (display_alias ~base_dir key)
            in
            let endpoint_args =
              let endpoints =
                Keys_scheduler.get_endpoints_for_network ~network
              in
              match endpoints with ep :: _ -> ["--endpoint"; ep] | [] -> []
            in
            run_onchain_operation
              ~base_dir
              ~description
              ~args:
                (extra_args @ endpoint_args
                @ ["withdraw"; "delegate"; "from"; key.alias])
              ~network
              ~on_done:cleanup
              ~on_success:(fun () -> Keys_scheduler.force_refresh ~pkh:key.pkh)
              ()))
    ()

(** Stake action: stake tez for a key with dry-run confirmation. *)
let action_stake ~base_dir ~network (key : Keys_reader.key_metadata) =
  Modal_helpers.prompt_text_modal
    ~title:"Stake: Amount (tez)"
    ~width:30
    ~initial:""
    ~placeholder:(Some "e.g. 1.5")
    ~on_submit:(fun amount_str ->
      let amount_str = String.trim amount_str in
      match float_of_string_opt amount_str with
      | None ->
          Modal_helpers.show_error
            ~title:"Invalid amount"
            "Please enter a valid number."
      | Some _ ->
          with_password_if_needed
            ~base_dir
            key
            ~action:(fun ~extra_args ~cleanup ->
              let description =
                Printf.sprintf
                  "Stake %s tez for %s"
                  amount_str
                  (display_alias ~base_dir key)
              in
              let endpoint_args =
                let endpoints =
                  Keys_scheduler.get_endpoints_for_network ~network
                in
                match endpoints with ep :: _ -> ["--endpoint"; ep] | [] -> []
              in
              let op_args =
                extra_args @ endpoint_args
                @ ["stake"; amount_str; "for"; key.alias]
              in
              confirm_and_run
                ~base_dir
                ~title:description
                ~message:
                  (Printf.sprintf
                     "Source: %s\nStake amount: %s tez"
                     (display_alias ~base_dir key)
                     amount_str)
                ~args:op_args
                ~network
                ~on_done:cleanup
                ~on_success:(fun () ->
                  Keys_scheduler.force_refresh ~pkh:key.pkh)
                ()))
    ()

(** Unstake action: unstake tez for a key with dry-run confirmation. *)
let action_unstake ~base_dir ~network (key : Keys_reader.key_metadata) =
  Modal_helpers.prompt_text_modal
    ~title:"Unstake: Amount (tez)"
    ~width:30
    ~initial:""
    ~placeholder:(Some "e.g. 1.5")
    ~on_submit:(fun amount_str ->
      let amount_str = String.trim amount_str in
      match float_of_string_opt amount_str with
      | None ->
          Modal_helpers.show_error
            ~title:"Invalid amount"
            "Please enter a valid number."
      | Some _ ->
          with_password_if_needed
            ~base_dir
            key
            ~action:(fun ~extra_args ~cleanup ->
              let description =
                Printf.sprintf
                  "Unstake %s tez for %s"
                  amount_str
                  (display_alias ~base_dir key)
              in
              let endpoint_args =
                let endpoints =
                  Keys_scheduler.get_endpoints_for_network ~network
                in
                match endpoints with ep :: _ -> ["--endpoint"; ep] | [] -> []
              in
              let op_args =
                extra_args @ endpoint_args
                @ ["unstake"; amount_str; "for"; key.alias]
              in
              confirm_and_run
                ~base_dir
                ~title:description
                ~message:
                  (Printf.sprintf
                     "Source: %s\nUnstake amount: %s tez"
                     (display_alias ~base_dir key)
                     amount_str)
                ~args:op_args
                ~network
                ~on_done:cleanup
                ~on_success:(fun () ->
                  Keys_scheduler.force_refresh ~pkh:key.pkh)
                ()))
    ()

(** Create a new key via octez-client gen keys. *)
let action_create_key ~base_dir =
  let validator = make_alias_validator ~base_dir () in
  Modal_helpers.prompt_validated_text_modal
    ~title:"Create Key: Alias"
    ~width:40
    ~initial:""
    ~placeholder:(Some "my_key")
    ~validator
    ~on_submit:(fun alias ->
      let alias = String.trim alias in
      let schemes =
        [
          ("Ed25519 (default)", "ed25519");
          ("Secp256k1", "secp256k1");
          ("P-256", "p256");
          ("BLS", "bls");
        ]
      in
      Modal_helpers.open_choice_modal
        ~title:"Crypto scheme"
        ~items:schemes
        ~to_string:fst
        ~on_select:(fun (_label, scheme) ->
          let description =
            Printf.sprintf "Generate '%s' key (%s)" alias scheme
          in
          run_client_action
            ~base_dir
            ~description
            ~args:["gen"; "keys"; alias; "--sig"; scheme]
            ~on_success:(fun () ->
              Context.toast_success
                (Printf.sprintf "Key '%s' created (%s)" alias scheme))
            ())
        ())
    ()

(** Import a public key hash as watch-only address. *)
let action_import_key ~base_dir =
  Modal_helpers.prompt_text_modal
    ~title:"Import Key: PKH"
    ~width:50
    ~initial:""
    ~placeholder:(Some "tz1...")
    ~on_submit:(fun pkh ->
      let pkh = String.trim pkh in
      match Pkh_validator.validate_format pkh with
      | Pkh_validator.Invalid reason ->
          Modal_helpers.show_error
            ~title:"Invalid PKH"
            (Printf.sprintf "Invalid PKH: %s" reason)
      | Pkh_validator.Valid ->
          let validator = make_alias_validator ~base_dir () in
          Modal_helpers.prompt_validated_text_modal
            ~title:"Import Key: Alias"
            ~width:40
            ~initial:""
            ~placeholder:(Some "my_contact")
            ~validator
            ~on_submit:(fun alias ->
              let alias = String.trim alias in
              let description = Printf.sprintf "Import '%s' (%s)" alias pkh in
              run_client_action
                ~base_dir
                ~description
                ~args:["add"; "address"; alias; pkh]
                ~on_success:(fun () ->
                  Context.toast_success
                    (Printf.sprintf "Imported '%s' as watch-only" alias))
                ())
            ())
    ()

(** Show receive info modal with PKH and explorer link. *)
let action_receive ~base_dir (key : Keys_reader.key_metadata) =
  let network =
    match Keys_scheduler.get_wallet_data ~pkh:key.pkh with
    | wd :: _ -> Some wd.Keys_scheduler.network
    | [] -> None
  in
  let explorer_url =
    match network with
    | Some net ->
        let subdomain = if String.equal net "mainnet" then "" else net ^ "." in
        Printf.sprintf "https://%stzkt.io/%s" subdomain key.pkh
    | None -> Printf.sprintf "https://tzkt.io/%s" key.pkh
  in
  let message =
    Printf.sprintf
      "Alias: %s\n\nPKH:\n%s\n\nExplorer:\n%s\n\nPress y/c to copy PKH."
      (display_alias ~base_dir key)
      key.pkh
      explorer_url
  in
  Modal_helpers.show_error ~title:"Receive" message

(** Show create/import modal for adding keys. *)
let open_create_import_modal ~base_dir =
  let items =
    [("Create new key", `Create); ("Import address (watch-only)", `Import)]
  in
  Modal_helpers.open_choice_modal
    ~title:"Add Key"
    ~items:(List.map snd items)
    ~to_string:(fun action ->
      match List.find_opt (fun (_, a) -> a = action) items with
      | Some (label, _) -> label
      | None -> "?")
    ~on_select:(function
      | `Create -> action_create_key ~base_dir
      | `Import -> action_import_key ~base_dir)
    ()

(** Prompt for network if a key has access to multiple networks.
    Lists all networks with available endpoints (local instances or public
    nodes), annotated with balance if known. Skips the picker if only one. *)
let with_network (key : Keys_reader.key_metadata) ~(group : enriched_group)
    ~action =
  let wallet_data = Keys_scheduler.get_wallet_data ~pkh:key.pkh in
  (* All networks with running local nodes *)
  let local_networks =
    Data.load_service_states ()
    |> List.filter (fun (st : Data.Service_state.t) ->
        String.equal st.service.role "node"
        && match st.status with Running -> true | _ -> false)
    |> List.map (fun (st : Data.Service_state.t) ->
        Network_name.normalize st.service.network)
    |> List.sort_uniq String.compare
  in
  (* All networks with public nodes *)
  let public_networks =
    Public_nodes_cache.get_nodes ()
    |> List.filter_map (fun (n : Public_nodes_cache.node_info) -> n.network)
    |> List.sort_uniq String.compare
  in
  let all_networks =
    List.sort_uniq String.compare (local_networks @ public_networks)
  in
  (* Fallback to group networks if nothing else *)
  let networks =
    if all_networks = [] then List.sort_uniq String.compare group.networks
    else all_networks
  in
  match networks with
  | [] -> action ~network:"mainnet"
  | [single] -> action ~network:single
  | multiple ->
      Modal_helpers.open_choice_modal
        ~title:"Select network"
        ~items:multiple
        ~to_string:(fun net ->
          let balance_str =
            match
              List.find_opt
                (fun (wd : Keys_scheduler.wallet_data) ->
                  String.equal wd.network net)
                wallet_data
            with
            | Some wd ->
                Printf.sprintf
                  "  %s tez"
                  (Baker_wallet_data.format_tez wd.spendable_balance)
            | None -> ""
          in
          let source =
            if List.exists (String.equal net) local_networks then "local"
            else "public"
          in
          let label = Printf.sprintf "%s  (%s)%s" net source balance_str in
          if String.equal net "mainnet" then Widgets.themed_warning label
          else label)
        ~on_select:(fun net -> action ~network:net)
        ()

(** Show the action modal for a selected key. *)
let open_key_action_modal ~(group : enriched_group)
    (key : Keys_reader.key_metadata) =
  let base_dir = group.base_dir in
  let wallet_data = Keys_scheduler.get_wallet_data ~pkh:key.pkh in
  let is_delegating =
    List.exists
      (fun (wd : Keys_scheduler.wallet_data) -> Option.is_some wd.delegate)
      wallet_data
  in
  let is_registered =
    List.exists
      (fun (wd : Keys_scheduler.wallet_data) -> wd.is_registered)
      wallet_data
  in
  let actions =
    [("Copy PKH", `Copy)]
    @ (if key.has_secret_key then [("Transfer", `Transfer)] else [])
    @ (if key.has_secret_key then [("Stake", `Stake)] else [])
    @ (if key.has_secret_key then [("Unstake", `Unstake)] else [])
    @ (if key.has_secret_key && not is_registered then
         [("Register as delegate", `Register)]
       else [])
    @ (if key.has_secret_key then [("Delegate to", `Delegate_to)] else [])
    @ (if key.has_secret_key && is_delegating then [("Undelegate", `Undelegate)]
       else [])
    @ [("Rename", `Rename)]
    @ [("Forget", `Forget)]
  in
  Modal_helpers.open_choice_modal
    ~title:(Printf.sprintf "Actions: %s" (display_alias ~base_dir key))
    ~items:(List.map snd actions)
    ~to_string:(fun action ->
      match List.find_opt (fun (_, a) -> a = action) actions with
      | Some (label, _) -> label
      | None -> "?")
    ~on_select:(function
      | `Copy -> copy_to_clipboard key.pkh
      | `Transfer ->
          with_network key ~group ~action:(fun ~network ->
              action_transfer ~base_dir ~network key)
      | `Stake ->
          with_network key ~group ~action:(fun ~network ->
              action_stake ~base_dir ~network key)
      | `Unstake ->
          with_network key ~group ~action:(fun ~network ->
              action_unstake ~base_dir ~network key)
      | `Register ->
          with_network key ~group ~action:(fun ~network ->
              action_register_delegate ~base_dir ~network key)
      | `Delegate_to ->
          with_network key ~group ~action:(fun ~network ->
              action_delegate_to ~base_dir ~network key)
      | `Undelegate ->
          with_network key ~group ~action:(fun ~network ->
              action_undelegate ~base_dir ~network key)
      | `Rename -> action_rename ~base_dir key
      | `Forget -> action_forget ~base_dir key)
    ()

(** Handle Enter key: dispatch action for selected item. *)
let action_on_selected ps =
  let s = ps.Navigation.s in
  (match List.nth_opt s.nav_items s.cursor with
  | Some (KeyItem (group, key)) -> open_key_action_modal ~group key
  | _ -> ()) ;
  ps

(** Open create/import modal for the currently selected group. *)
let create_import_selected ps =
  let s = ps.Navigation.s in
  let base_dir =
    match List.nth_opt s.nav_items s.cursor with
    | Some (GroupHeader g) -> g.base_dir
    | Some (KeyItem (g, _)) -> g.base_dir
    | None -> (
        match s.groups with
        | g :: _ -> g.base_dir
        | [] -> default_client_base_dir ())
  in
  open_create_import_modal ~base_dir ;
  ps

(** Cycle sort mode. *)
let cycle_sort ~size ps =
  let s = ps.Navigation.s in
  let next_mode =
    match s.sort_mode with
    | SortAlias -> SortBalance
    | SortBalance -> SortNetwork
    | SortNetwork -> SortAlias
  in
  let s = {s with sort_mode = next_mode} in
  let s = rebuild_nav s in
  Context.toast_info (Printf.sprintf "Sort: %s" (sort_mode_label next_mode)) ;
  let _ = size in
  {ps with s}

(** Pending search query set from modal callback. *)
let pending_search_query : string option ref = ref None

(** Start search: prompt for query. The query is stored in a mutable ref
    and applied on the next key event via [apply_pending_search]. *)
let start_search ps =
  Modal_helpers.prompt_text_modal
    ~title:"Search keys (empty to clear)"
    ~width:40
    ~initial:ps.Navigation.s.search_query
    ~placeholder:(Some "alias or PKH...")
    ~on_submit:(fun query ->
      let query = String.trim query in
      pending_search_query := Some query ;
      if String.length query = 0 then Context.toast_info "Search cleared"
      else Context.toast_info (Printf.sprintf "Filter: %s" query))
    () ;
  ps

(** Apply pending search query if one was set by modal callback. *)
let apply_pending_search ps =
  match !pending_search_query with
  | None -> ps
  | Some query ->
      pending_search_query := None ;
      let s = {ps.Navigation.s with search_query = query} in
      let s = rebuild_nav s in
      {ps with s}

(** Force refresh the currently selected key. *)
let force_refresh_keys ps =
  let s = ps.Navigation.s in
  (match List.nth_opt s.nav_items s.cursor with
  | Some (KeyItem (group, key)) ->
      Keys_scheduler.force_refresh ~pkh:key.pkh ;
      Context.toast_info
        (Printf.sprintf
           "Refreshing %s..."
           (display_alias ~base_dir:group.base_dir key))
  | _ -> Context.toast_info "Select a key to refresh") ;
  ps

(** Toggle visual multi-select mode. *)
let toggle_multi_select ps =
  let s = ps.Navigation.s in
  if s.multi_select then (
    Context.toast_info "Multi-select off" ;
    {ps with s = {s with multi_select = false; selected = StringSet.empty}})
  else (
    Context.toast_info "Multi-select: Space to toggle, Enter for batch action" ;
    {ps with s = {s with multi_select = true}})

(** Toggle selection of current key in multi-select mode. *)
let toggle_selection ps =
  let s = ps.Navigation.s in
  match List.nth_opt s.nav_items s.cursor with
  | Some (KeyItem (_, key)) ->
      let selected =
        if StringSet.mem key.pkh s.selected then
          StringSet.remove key.pkh s.selected
        else StringSet.add key.pkh s.selected
      in
      {ps with s = {s with selected}}
  | _ -> ps

(** Show receive/info modal for current key. *)
let show_receive ps =
  let s = ps.Navigation.s in
  (match List.nth_opt s.nav_items s.cursor with
  | Some (KeyItem (group, key)) -> action_receive ~base_dir:group.base_dir key
  | _ -> ()) ;
  ps

(** Collect selected keys for batch operations. *)
let get_selected_keys s =
  List.concat_map
    (fun (g : enriched_group) ->
      List.filter_map
        (fun (k : Keys_reader.key_metadata) ->
          if StringSet.mem k.pkh s.selected then Some (g, k) else None)
        g.keys)
    s.groups

(** Show batch operations modal for selected keys. *)
let open_batch_modal ps =
  let s = ps.Navigation.s in
  let selected_keys = get_selected_keys s in
  let count = List.length selected_keys in
  if count = 0 then (
    Context.toast_info "No keys selected" ;
    ps)
  else
    let items =
      [
        (Printf.sprintf "Register all %d as delegate" count, `Batch_register);
        (Printf.sprintf "Delegate all %d to baker" count, `Batch_delegate);
        (Printf.sprintf "Copy all %d PKHs" count, `Batch_copy);
      ]
    in
    Modal_helpers.open_choice_modal
      ~title:(Printf.sprintf "Batch: %d keys selected" count)
      ~items:(List.map snd items)
      ~to_string:(fun action ->
        match List.find_opt (fun (_, a) -> a = action) items with
        | Some (label, _) -> label
        | None -> "?")
      ~on_select:(function
        | `Batch_register ->
            List.iter
              (fun (group, key) ->
                let network =
                  match group.networks with n :: _ -> n | [] -> "mainnet"
                in
                action_register_delegate ~base_dir:group.base_dir ~network key)
              selected_keys
        | `Batch_delegate ->
            (* Pick a single baker address, then delegate all selected keys *)
            let exclude =
              List.map
                (fun (_, (k : Keys_reader.key_metadata)) -> k.pkh)
                selected_keys
            in
            let exclude_first =
              match exclude with pkh :: _ -> pkh | [] -> ""
            in
            pick_address
              ~title:"Delegate all to"
              ~exclude_pkh:exclude_first
              ~include_mru:false
              ~on_select:(fun baker_pkh ->
                List.iter
                  (fun ( (group : enriched_group),
                         (key : Keys_reader.key_metadata) )
                     ->
                    let network =
                      match group.networks with n :: _ -> n | [] -> "mainnet"
                    in
                    with_password_if_needed
                      ~base_dir:group.base_dir
                      key
                      ~action:(fun ~extra_args ~cleanup ->
                        let endpoint_args =
                          let endpoints =
                            Keys_scheduler.get_endpoints_for_network ~network
                          in
                          match endpoints with
                          | ep :: _ -> ["--endpoint"; ep]
                          | [] -> []
                        in
                        run_onchain_operation
                          ~base_dir:group.base_dir
                          ~description:
                            (Printf.sprintf
                               "Delegate '%s' to %s"
                               (display_alias ~base_dir:group.base_dir key)
                               baker_pkh)
                          ~args:
                            (extra_args @ endpoint_args
                            @ [
                                "set";
                                "delegate";
                                "for";
                                key.alias;
                                "to";
                                baker_pkh;
                              ])
                          ~network
                          ~on_done:cleanup
                          ~on_success:(fun () ->
                            Keys_scheduler.force_refresh ~pkh:key.pkh)
                          ()))
                  selected_keys)
        | `Batch_copy ->
            let all_pkhs =
              List.map
                (fun (_, (k : Keys_reader.key_metadata)) -> k.pkh)
                selected_keys
              |> String.concat "\n"
            in
            copy_to_clipboard all_pkhs ;
            Context.toast_success (Printf.sprintf "Copied %d PKHs" count))
      () ;
    ps

let show_help ps =
  Modal_helpers.show_error
    ~title:"Keys Page Help"
    "j/Down      Move down\n\
     k/Up        Move up\n\
     g           Jump to top\n\
     G           Jump to bottom\n\
     Space       Fold/unfold (or toggle select)\n\
     Tab         Switch panel\n\
     Enter       Key actions (or batch)\n\
     v           Multi-select mode\n\
     Q           Receive info\n\
     +/n         Create/import key\n\
     /           Search/filter\n\
     s           Cycle sort mode\n\
     r           Force refresh\n\
     y/c         Copy PKH\n\
     Esc/q       Back\n\
     ?           This help\n\n\
     \xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80 Icons \
     \xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\xE2\x94\x80\n\
     \xF0\x9F\x94\x91  Signable key         \xF0\x9F\x93\x8B  Watch-only [ro]\n\
     \xF0\x9F\x94\x92  Encrypted [enc]      \xF0\x9F\x94\x90  Hardware wallet \
     [hw]\n\
     \xE2\x9C\x92\xEF\xB8\x8F   Remote signer [rmt]  \xF0\x9F\x8D\x9E  Baker \
     (delegate)\n\
     \xE2\x8F\xB1\xEF\xB8\x8F   Recent target        \xE2\x86\x92   Delegating \
     to" ;
  ps

(** Copy PKH of currently selected key. *)
let copy_selected_pkh ps =
  let s = ps.Navigation.s in
  (match List.nth_opt s.nav_items s.cursor with
  | Some (KeyItem (_, key)) -> copy_to_clipboard key.pkh
  | _ -> ()) ;
  ps

let handle_key ps key ~size =
  let ps = apply_pending_search ps in
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some Keys.Escape | Some (Keys.Char "q") ->
        if ps.Navigation.s.multi_select then toggle_multi_select ps
        else if String.length ps.Navigation.s.search_query > 0 then (
          let s = {ps.Navigation.s with search_query = ""} in
          let s = rebuild_nav s in
          Context.toast_info "Search cleared" ;
          {ps with s})
        else Navigation.back ps
    | Some Keys.Up | Some (Keys.Char "k") -> move_cursor (-1) ~size ps
    | Some Keys.Down | Some (Keys.Char "j") -> move_cursor 1 ~size ps
    | Some (Keys.Char "g") -> jump_to_top ~size ps
    | Some (Keys.Char "G") -> jump_to_bottom ~size ps
    | Some (Keys.Char " ") ->
        if ps.Navigation.s.multi_select then toggle_selection ps
        else toggle_fold ~size ps
    | Some Keys.Tab -> switch_panel ps
    | Some Keys.Enter ->
        if ps.Navigation.s.multi_select then open_batch_modal ps
        else action_on_selected ps
    | Some (Keys.Char "v") -> toggle_multi_select ps
    | Some (Keys.Char "Q") -> show_receive ps
    | Some (Keys.Char "+") | Some (Keys.Char "n") -> create_import_selected ps
    | Some (Keys.Char "/") -> start_search ps
    | Some (Keys.Char "s") -> cycle_sort ~size ps
    | Some (Keys.Char "r") -> force_refresh_keys ps
    | Some (Keys.Char "y") | Some (Keys.Char "c") -> copy_selected_pkh ps
    | Some (Keys.Char "?") -> show_help ps
    | _ -> (
        if Miaou_helpers.Mouse.is_wheel_up key then
          move_cursor (-Miaou_helpers.Mouse.wheel_scroll_lines) ~size ps
        else if Miaou_helpers.Mouse.is_wheel_down key then
          move_cursor Miaou_helpers.Mouse.wheel_scroll_lines ~size ps
        else
          match Miaou_helpers.Mouse.parse_click key with
          | Some {row; col} ->
              let cols = size.LTerm_geom.cols in
              let left_width =
                if cols >= side_by_side_min_width then min 60 (cols * 2 / 5)
                else cols
              in
              if col < left_width then
                let idx = row - 3 + ps.Navigation.s.scroll_offset in
                if idx >= 0 && idx < List.length ps.Navigation.s.nav_items then
                  move_cursor (idx - ps.Navigation.s.cursor) ~size ps
                else ps
              else ps
          | None -> ps)

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

(* ================================================================ *)
(* Page registration                                                 *)
(* ================================================================ *)

module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let handled_keys = handled_keys

  let keymap = keymap

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let on_key ps key ~size =
    let ps' = handle_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let key_hints _ps =
    Miaou.Core.Tui_page.
      [
        {key = "j/k"; help = "Navigate"};
        {key = "Tab"; help = "Panel"};
        {key = "Space"; help = "Fold"};
        {key = "+/n"; help = "New key"};
        {key = "Esc"; help = "Back"};
        {key = "?"; help = "Help"};
      ]

  let has_modal = has_modal
end

(** Expose internals for testing *)
module Internal_for_tests = struct
  let default_client_base_dir = default_client_base_dir

  let get_all_base_dirs = get_all_base_dirs
end

let register () = Miaou.Core.Registry.register name (module Page_Impl)
