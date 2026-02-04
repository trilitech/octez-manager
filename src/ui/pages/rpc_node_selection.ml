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
}

type msg = unit

type pstate = state Navigation.t

(** Curated default public nodes used as fallback *)
let curated_defaults : node_item list =
  [
    {
      label = "Tezos Mainnet (ecadlabs)";
      rpc_addr = "https://mainnet.api.tez.ie";
      is_public = true;
      network = Some "mainnet";
    };
    {
      label = "Tezos Ghostnet";
      rpc_addr = "https://rpc.ghostnet.teztnets.com";
      is_public = true;
      network = Some "ghostnet";
    };
    {
      label = "Tezos Mainnet (SmartPy)";
      rpc_addr = "https://mainnet.smartpy.io";
      is_public = true;
      network = Some "mainnet";
    };
  ]

(** Parse Taquito JSON format to extract public nodes *)
let parse_taquito_json (txt : string) : node_item list =
  try
    let j = Yojson.Safe.from_string txt in
    let parse_assoc_list lst ~get_rpc ~get_label ~get_net =
      List.filter_map
        (function
          | `Assoc kv ->
              let rpc = get_rpc kv in
              if rpc = "" then None
              else
                Some
                  {
                    label = get_label kv rpc;
                    rpc_addr = rpc;
                    is_public = true;
                    network = get_net kv;
                  }
          | _ -> None)
        lst
    in
    match j with
    | `List lst ->
        (* Old format: list of objects with rpc/rpc_url/name fields *)
        parse_assoc_list
          lst
          ~get_rpc:(fun kv ->
            match List.assoc_opt "rpc" kv with
            | Some (`String s) -> s
            | _ -> (
                match List.assoc_opt "rpc_url" kv with
                | Some (`String s) -> s
                | _ -> ""))
          ~get_label:(fun kv rpc ->
            match List.assoc_opt "name" kv with
            | Some (`String s) when s <> "" -> s
            | _ -> rpc)
          ~get_net:(fun kv ->
            match List.assoc_opt "network" kv with
            | Some (`String s) -> Some s
            | _ -> None)
    | `Assoc kvs -> (
        (* Taquito format: providers map + rpc_endpoints list *)
        let provider_names =
          match List.assoc_opt "providers" kvs with
          | Some (`List provs) ->
              List.fold_left
                (fun acc p ->
                  match p with
                  | `Assoc pkv ->
                      let id =
                        match List.assoc_opt "id" pkv with
                        | Some (`String s) -> s
                        | _ -> ""
                      in
                      let name =
                        match List.assoc_opt "name" pkv with
                        | Some (`String s) -> s
                        | _ -> id
                      in
                      if id = "" then acc else (id, name) :: acc
                  | _ -> acc)
                []
                provs
          | _ -> []
        in
        let provider_of id =
          match List.assoc_opt id provider_names with Some n -> n | None -> id
        in
        match List.assoc_opt "rpc_endpoints" kvs with
        | Some (`List eps) ->
            List.filter_map
              (function
                | `Assoc ekv ->
                    let rpc =
                      match List.assoc_opt "url" ekv with
                      | Some (`String s) -> s
                      | _ -> ""
                    in
                    if rpc = "" then None
                    else
                      let provider =
                        match List.assoc_opt "provider" ekv with
                        | Some (`String s) -> provider_of s
                        | _ -> ""
                      in
                      let net =
                        match List.assoc_opt "net" ekv with
                        | Some (`String s) -> Some s
                        | _ -> None
                      in
                      let label =
                        match net with
                        | Some n when provider <> "" ->
                            Printf.sprintf "%s (%s)" provider n
                        | Some n -> n
                        | None when provider <> "" -> provider
                        | None -> rpc
                      in
                      Some
                        {label; rpc_addr = rpc; is_public = true; network = net}
                | _ -> None)
              eps
        | _ -> [])
    | _ -> []
  with _ -> []

(** Fetch public nodes from Taquito with fallback *)
let fetch_public_nodes () : node_item list * string option =
  let urls =
    [
      "https://taquito.io/docs/rpc_nodes.json";
      "https://taquito.io/rpc_nodes.json";
      "https://www.taquito.io/docs/rpc_nodes.json";
    ]
  in
  let rec try_urls = function
    | [] -> (curated_defaults, Some "Using fallback nodes (fetch failed)")
    | url :: rest -> (
        match Common.run_out ["curl"; "-fsSL"; "-m"; "5"; url] with
        | Ok body ->
            let nodes = parse_taquito_json body in
            if nodes = [] then try_urls rest else (nodes, None)
        | Error _ -> try_urls rest)
  in
  try_urls urls

(** Load local node instances *)
let load_local_instances () : node_item list =
  let service_states = Data.load_service_states () in
  List.filter_map
    (fun (st : Data.Service_state.t) ->
      let svc = st.service in
      if svc.Service.role = "node" && svc.Service.rpc_addr <> "" then
        Some
          {
            label = svc.Service.instance;
            rpc_addr = svc.Service.rpc_addr;
            is_public = false;
            network = Some svc.Service.network;
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
    ~rpc_addr:item.rpc_addr
    ~net_addr:""
    ~service_user:""
    ~app_bin_dir:""
    ~logging_mode:Logging_mode.default
    ()

let init () =
  let public_nodes, error = fetch_public_nodes () in
  let local_instances = load_local_instances () in
  Navigation.make
    {public_nodes; local_instances; cursor = 0; loading = false; error}

let update ps _ = ps

let refresh ps =
  (* Check for pending navigation (e.g., from activate_selection) *)
  match Context.consume_navigation () with
  | Some page -> Navigation.goto page ps
  | None ->
      let public_nodes, error = fetch_public_nodes () in
      let local_instances = load_local_instances () in
      Navigation.update
        (fun s -> {s with public_nodes; local_instances; error})
        ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = ps

let back ps = Navigation.back ps

let total_items s =
  (* PUBLIC NODES header + nodes + LOCAL INSTANCES header + instances *)
  let public_count =
    if s.public_nodes = [] then 0 else 1 + List.length s.public_nodes
  in
  let local_count =
    if s.local_instances = [] then 0 else 1 + List.length s.local_instances
  in
  public_count + local_count

let get_item_at_cursor s =
  let public_header_idx = 0 in
  let public_start = 1 in
  let public_end = public_start + List.length s.public_nodes in
  let local_header_idx = public_end in
  let local_start = local_header_idx + 1 in
  if s.cursor = public_header_idx then `PublicHeader
  else if s.cursor >= public_start && s.cursor < public_end then
    `PublicNode (List.nth s.public_nodes (s.cursor - public_start))
  else if s.cursor = local_header_idx then `LocalHeader
  else
    let local_idx = s.cursor - local_start in
    match List.nth_opt s.local_instances local_idx with
    | Some item -> `LocalNode item
    | None -> `None

let move_cursor delta s =
  let total = total_items s in
  if total = 0 then s
  else
    let new_cursor = s.cursor + delta in
    let new_cursor = max 0 (min (total - 1) new_cursor) in
    (* Skip headers when navigating *)
    let public_header_idx = 0 in
    let local_header_idx = 1 + List.length s.public_nodes in
    let new_cursor =
      if new_cursor = public_header_idx && delta > 0 then new_cursor + 1
      else if new_cursor = local_header_idx && delta > 0 then new_cursor + 1
      else if new_cursor = local_header_idx && delta < 0 then new_cursor - 1
      else new_cursor
    in
    let new_cursor = max 0 (min (total - 1) new_cursor) in
    {s with cursor = new_cursor}

let activate_selection s =
  match get_item_at_cursor s with
  | `PublicNode item | `LocalNode item ->
      (* Create service and navigate to RPC browser *)
      let service = make_service_for_node item in
      Rpc_browser_state.set_selected_instance (Some service) ;
      Context.navigate Rpc_browser.name ;
      s
  | `PublicHeader | `LocalHeader | `None -> s

let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let cols = size.LTerm_geom.cols in
  let public_header_idx = 0 in
  let local_header_idx = 1 + List.length s.public_nodes in
  let lines =
    (* Error/warning *)
    (match s.error with Some e -> [Widgets.yellow e; ""] | None -> [])
    (* PUBLIC NODES section *)
    @ [
        (if s.cursor = public_header_idx then
           Widgets.bold (Widgets.cyan "> PUBLIC NODES")
         else Widgets.bold (Widgets.cyan "  PUBLIC NODES"));
      ]
    @ List.mapi
        (fun i item ->
          let idx = 1 + i in
          let prefix = if s.cursor = idx then "> " else "  " in
          let network_str =
            match item.network with
            | Some n -> Printf.sprintf " [%s]" n
            | None -> ""
          in
          let line =
            Printf.sprintf
              "%s%s%s  %s"
              prefix
              item.label
              network_str
              (Widgets.dim item.rpc_addr)
          in
          if s.cursor = idx then Widgets.bold line else line)
        s.public_nodes
    @ [""]
    (* LOCAL INSTANCES section *)
    @ [
        (if s.cursor = local_header_idx then
           Widgets.bold (Widgets.green "> LOCAL INSTANCES")
         else Widgets.bold (Widgets.green "  LOCAL INSTANCES"));
      ]
    @
    if s.local_instances = [] then [Widgets.dim "  (no local nodes configured)"]
    else
      List.mapi
        (fun i item ->
          let idx = local_header_idx + 1 + i in
          let prefix = if s.cursor = idx then "> " else "  " in
          let network_str =
            match item.network with
            | Some n -> Printf.sprintf " [%s]" n
            | None -> ""
          in
          let line =
            Printf.sprintf
              "%s%s%s  %s"
              prefix
              item.label
              network_str
              (Widgets.dim item.rpc_addr)
          in
          if s.cursor = idx then Widgets.bold line else line)
        s.local_instances
  in
  let hint = Widgets.dim "↑/↓ navigate · Enter select · r refresh · Esc back" in
  let header =
    [Widgets.title_highlight " Browse RPCs - Select Node "; ""; hint; ""]
  in
  Vsection.render ~size ~header ~content_footer:[] ~child:(fun _ ->
      let truncate line =
        if Widgets.visible_chars_count line <= cols then line
        else
          let byte_idx = Widgets.visible_byte_index_of_pos line (cols - 3) in
          String.sub line 0 byte_idx ^ "..."
      in
      lines |> List.map truncate |> String.concat "\n")

let handle_key ps key ~size:_ =
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
  | _ -> ps

let keymap _ps =
  let kb key action help =
    {Miaou.Core.Tui_page.key; action; help; display_only = false}
  in
  let activate ps = Navigation.update activate_selection ps in
  [
    kb "Enter" activate "Select";
    kb "↑/↓" (fun ps -> ps) "Navigate";
    kb "r" refresh "Refresh";
    kb "Esc" back "Back";
  ]

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

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
