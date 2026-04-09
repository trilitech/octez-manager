(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC Node Selection page.

    Allows users to select a node to browse RPCs from:
    - PUBLIC NODES: fetched from Taquito's public node list
    - LOCAL INSTANCES: locally configured node instances *)

module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
open Octez_manager_lib

let name = "rpc_node_selection"

(** A selectable node item *)
type node_item = {
  label : string;
  rpc_addr : string;
  is_public : bool;  (** true for public nodes, false for local instances *)
  network : string option;
}

(** Page state *)
type state = {
  public_nodes : node_item list;
  local_instances : node_item list;
  cursor : int;
  loading : bool;
  error : string option;
  (* Flat list of all items for cursor navigation, with metadata *)
  display_items : display_item list;
}

and display_item =
  | SectionHeader of string (* "PUBLIC NODES" or "LOCAL INSTANCES" *)
  | NetworkHeader of string (* Network name like "Mainnet" *)
  | NodeItem of node_item

type msg = unit

type pstate = state Navigation.t

(** Parse Taquito JSON using Public_nodes_cache and convert to node_item.
    This function is kept for backward compatibility with tests. *)
let parse_taquito_json (txt : string) : node_item list =
  let nodes = Public_nodes_cache.parse_taquito_json txt in
  List.map
    (fun (info : Public_nodes_cache.node_info) ->
      {
        label = info.label;
        rpc_addr = info.rpc_addr;
        is_public = true;
        network = info.network;
      })
    nodes

(** Curated default public nodes.
    This is exposed for tests and converts from Public_nodes_cache. *)
let curated_defaults : node_item list =
  List.map
    (fun (info : Public_nodes_cache.node_info) ->
      {
        label = info.label;
        rpc_addr = info.rpc_addr;
        is_public = true;
        network = info.network;
      })
    Public_nodes_cache.curated_defaults

(** Fetch public nodes using Public_nodes_cache *)
let fetch_public_nodes () : node_item list * string option =
  let nodes = Public_nodes_cache.get_nodes () in
  (* Convert node_info to node_item by adding is_public field *)
  let node_items =
    List.map
      (fun (info : Public_nodes_cache.node_info) ->
        {
          label = info.label;
          rpc_addr = info.rpc_addr;
          is_public = true;
          network = info.network;
        })
      nodes
  in
  (node_items, None)

(** Extract simple network name from network URL or alias.
    E.g., "https://teztnets.com/shadownet" -> "shadownet", "mainnet" -> "mainnet" *)
let extract_network_name (network_str : string) : string =
  let lower = String.lowercase_ascii network_str in
  (* Check if it's already a simple alias *)
  if
    List.mem
      lower
      [
        "mainnet";
        "shadownet";
        "tallinnnet";
        "weeklynet";
        "dailynet";
        "mondaynet";
      ]
  then network_str
  else
    (* Try to extract from URL - look for network patterns *)
    let known_networks =
      [
        "mainnet";
        "shadownet";
        "tallinnnet";
        "weeklynet";
        "dailynet";
        "mondaynet";
      ]
    in
    match
      List.find_opt
        (fun net -> Str.string_match (Str.regexp (".*" ^ net)) lower 0)
        known_networks
    with
    | Some name -> name
    | None -> network_str

(** Load local node instances *)
let load_local_instances () : node_item list =
  let service_states = Data.load_service_states () in
  List.filter_map
    (fun (st : Data.Service_state.t) ->
      let svc = st.service in
      if
        svc.Service.role = "node"
        && Rpc_addr.to_string svc.Service.rpc_addr <> ""
      then
        Some
          {
            label = svc.Service.instance;
            rpc_addr = Rpc_addr.to_string svc.Service.rpc_addr;
            is_public = false;
            network = Some (extract_network_name svc.Service.network);
          }
      else None)
    service_states

(** Create a synthetic Service.t for a node item *)
let make_service_for_node (item : node_item) : Service.t =
  Service.make
    ~instance:item.label
    ~role:"node"
    ~network:(Option.value item.network ~default:"unknown")
    ~history_mode:History_mode.default
    ~data_dir:""
    ~rpc_addr:(Rpc_addr.of_string item.rpc_addr)
    ~net_addr:""
    ~service_user:""
    ~app_bin_dir:""
    ~logging_mode:Logging_mode.default
    ()

(** Group node items by network *)
let group_by_network (items : node_item list) : (string * node_item list) list =
  (* Build a map of network -> items *)
  let network_map =
    List.fold_left
      (fun acc item ->
        let network =
          match item.network with
          | Some n -> String.capitalize_ascii n
          | None -> "Unknown"
        in
        let existing = try List.assoc network acc with Not_found -> [] in
        (network, item :: existing) :: List.remove_assoc network acc)
      []
      items
  in
  (* Sort by network name and reverse item lists (they were consed) *)
  List.sort
    (fun (n1, _) (n2, _) -> String.compare n1 n2)
    (List.map (fun (net, its) -> (net, List.rev its)) network_map)

(** Build flat display list with section headers, network headers, and nodes *)
let build_display_items ~public_nodes ~local_instances : display_item list =
  let build_section_items section_header items =
    if items = [] then []
    else
      let grouped = group_by_network items in
      SectionHeader section_header
      :: List.concat_map
           (fun (network, nodes) ->
             NetworkHeader network :: List.map (fun item -> NodeItem item) nodes)
           grouped
  in
  build_section_items "LOCAL INSTANCES" local_instances
  @ build_section_items "PUBLIC NODES" public_nodes

let init () =
  let public_nodes, error = fetch_public_nodes () in
  let local_instances = load_local_instances () in
  let display_items = build_display_items ~public_nodes ~local_instances in
  Navigation.make
    {
      public_nodes;
      local_instances;
      cursor = 0;
      loading = false;
      error;
      display_items;
    }

let update ps _ = ps

let refresh ps =
  (* Check for pending navigation (e.g., from activate_selection) *)
  match Context.consume_navigation () with
  | Some (Context.Goto page) -> Navigation.goto page ps
  | Some Context.Back -> Navigation.back ps
  | Some Context.Quit -> Navigation.quit ps
  | None ->
      let public_nodes, error = fetch_public_nodes () in
      let local_instances = load_local_instances () in
      let display_items = build_display_items ~public_nodes ~local_instances in
      Navigation.update
        (fun s -> {s with public_nodes; local_instances; error; display_items})
        ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps = Navigation.back ps

let total_items s = List.length s.display_items

let get_item_at_cursor s =
  match List.nth_opt s.display_items s.cursor with
  | Some (SectionHeader _) -> `SectionHeader
  | Some (NetworkHeader _) -> `NetworkHeader
  | Some (NodeItem item) -> `Node item
  | None -> `None

let move_cursor delta s =
  let total = total_items s in
  if total = 0 then s
  else
    let new_cursor = s.cursor + delta in
    let new_cursor = max 0 (min (total - 1) new_cursor) in
    (* Skip headers when navigating with arrow keys *)
    let rec find_next_selectable cursor direction =
      if cursor < 0 || cursor >= total then
        (* Went out of bounds - find first/last selectable *)
        if direction > 0 then
          (* Find last selectable *)
          find_first_selectable_backward (total - 1)
        else
          (* Find first selectable *)
          find_first_selectable_forward 0
      else
        match List.nth_opt s.display_items cursor with
        | Some (NodeItem _) -> cursor
        | Some (SectionHeader _ | NetworkHeader _) ->
            find_next_selectable (cursor + direction) direction
        | None -> cursor
    and find_first_selectable_forward start =
      if start >= total then start
      else
        match List.nth_opt s.display_items start with
        | Some (NodeItem _) -> start
        | _ -> find_first_selectable_forward (start + 1)
    and find_first_selectable_backward start =
      if start < 0 then 0
      else
        match List.nth_opt s.display_items start with
        | Some (NodeItem _) -> start
        | _ -> find_first_selectable_backward (start - 1)
    in
    let new_cursor =
      if delta > 0 then find_next_selectable new_cursor 1
      else find_next_selectable new_cursor (-1)
    in
    let new_cursor = max 0 (min (total - 1) new_cursor) in
    {s with cursor = new_cursor}

let activate_selection s =
  match get_item_at_cursor s with
  | `Node item ->
      (* Create service and navigate to RPC browser *)
      let service = make_service_for_node item in
      Rpc_browser_state.set_selected_instance (Some service) ;
      Context.navigate Rpc_browser.name ;
      s
  | `SectionHeader | `NetworkHeader | `None -> s

let keymap _ps =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "Enter" "Select"; kb "↑/↓" "Navigate"; kb "r" "Refresh"; kb "Esc" "Back"]

let view ps ~focus:_ ~size =
  (* Register keymap for help modal (?) *)
  let keymap_pairs =
    List.map
      (fun (kb : state Miaou.Core.Tui_page.key_binding_desc) ->
        (kb.Miaou.Core.Tui_page.key, kb.help))
      (keymap ps)
  in
  Context.register_active_page_keymap (fun () -> keymap_pairs) ;

  let s = ps.Navigation.s in
  let cols = size.LTerm_geom.cols in
  let lines =
    (* Error/warning *)
    (match s.error with Some e -> [Widgets.themed_warning e; ""] | None -> [])
    @
    (* Render display items with proper indentation *)
    List.mapi
      (fun i item ->
        let is_selected = i = s.cursor in
        let prefix = if is_selected then "> " else "  " in
        match item with
        | SectionHeader title ->
            let styled_title =
              if String.contains title 'P' then Widgets.themed_info title
              else Widgets.themed_success title
            in
            if is_selected then Widgets.themed_emphasis prefix ^ styled_title
            else Widgets.themed_text "  " ^ styled_title
        | NetworkHeader network ->
            if is_selected then
              Widgets.themed_emphasis prefix
              ^ Widgets.themed_accent ("• " ^ network)
            else
              Widgets.themed_text "  " ^ Widgets.themed_accent ("• " ^ network)
        | NodeItem item ->
            if is_selected then
              Widgets.themed_emphasis prefix
              ^ Widgets.themed_text "    "
              ^ Widgets.themed_emphasis item.label
              ^ Widgets.themed_text "  "
              ^ Widgets.themed_muted item.rpc_addr
            else
              Widgets.themed_text "      "
              ^ Widgets.themed_text item.label
              ^ Widgets.themed_text "  "
              ^ Widgets.themed_muted item.rpc_addr)
      s.display_items
  in
  let hint =
    Widgets.themed_muted "↑/↓ navigate · Enter select · r refresh · Esc back"
  in
  let header =
    [Widgets.themed_primary " Browse RPCs - Select Node "; ""; hint; ""]
  in
  Themed_page.render_layout ~size ~header ~footer:[] ~child:(fun _ ->
      let truncate line =
        if Widgets.visible_chars_count line <= cols then line
        else
          let byte_idx = Widgets.visible_byte_index_of_pos line (cols - 3) in
          String.sub line 0 byte_idx ^ "..."
      in
      lines |> List.map truncate |> String.concat "\n")

let handle_key ps key ~size:_ =
  (* Try global shortcuts first (?, m, C-t, etc.) *)
  match Global_shortcuts.handle key with
  | Global_shortcuts.Handled -> ps
  | Global_shortcuts.NotGlobal -> (
      let s = ps.Navigation.s in
      match Keys.of_string key with
      | Some Keys.Escape -> back ps
      | Some Keys.Enter ->
          let new_state = activate_selection s in
          Navigation.update (fun _ -> new_state) ps
      | Some Keys.Up | Some (Keys.Char "k") ->
          Navigation.update (fun s -> move_cursor (-1) s) ps
      | Some Keys.Down | Some (Keys.Char "j") ->
          Navigation.update (fun s -> move_cursor 1 s) ps
      | Some (Keys.Char "r") -> refresh ps
      | _ -> ps)

let handled_keys () =
  Keys.[Escape; Enter; Up; Down; Char "j"; Char "k"; Char "r"]

let has_modal _ = false

let handle_modal_key ps _ ~size:_ = ps

module Page : Miaou.Core.Tui_page.PAGE_SIG = struct
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

  let keymap = keymap

  let handled_keys = handled_keys

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
        {key = "Enter"; help = "Select"};
        {key = "↑/↓"; help = "Navigate"};
        {key = "r"; help = "Refresh"};
        {key = "Esc"; help = "Back"};
      ]

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
