(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_ui

(* ============================================================ *)
(* parse_taquito_json Tests                                      *)
(* ============================================================ *)

let test_parse_old_format () =
  let json =
    {|[
      {"name": "Mainnet Node", "rpc": "https://mainnet.example.com", "network": "mainnet"},
      {"name": "Shadownet Node", "rpc_url": "https://shadownet.example.com", "network": "shadownet"}
    ]|}
  in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "two nodes" 2 (List.length nodes) ;
  let n0 = List.nth nodes 0 in
  Alcotest.(check string) "first label" "Mainnet Node" n0.label ;
  Alcotest.(check string) "first rpc" "https://mainnet.example.com" n0.rpc_addr ;
  Alcotest.(check bool) "first is_public" true n0.is_public ;
  Alcotest.(check (option string)) "first network" (Some "mainnet") n0.network ;
  let n1 = List.nth nodes 1 in
  Alcotest.(check string) "second label" "Shadownet Node" n1.label ;
  Alcotest.(check string)
    "second rpc (rpc_url)"
    "https://shadownet.example.com"
    n1.rpc_addr

let test_parse_old_format_no_name () =
  let json =
    {|[{"rpc": "https://unnamed.example.com", "network": "mainnet"}]|}
  in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "one node" 1 (List.length nodes) ;
  Alcotest.(check string)
    "label falls back to rpc"
    "https://unnamed.example.com"
    (List.nth nodes 0).label

let test_parse_old_format_empty_rpc () =
  let json = {|[{"name": "Node", "rpc": "", "network": "mainnet"}]|} in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "empty rpc filtered" 0 (List.length nodes)

let test_parse_old_format_no_network () =
  let json = {|[{"name": "Node", "rpc": "https://x.com"}]|} in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "one node" 1 (List.length nodes) ;
  Alcotest.(check (option string)) "no network" None (List.nth nodes 0).network

let test_parse_taquito_format () =
  let json =
    {|{
      "providers": [
        {"id": "ecad", "name": "ECAD Labs"},
        {"id": "smart", "name": "SmartPy"}
      ],
      "rpc_endpoints": [
        {"url": "https://mainnet.ecad.io", "provider": "ecad", "net": "mainnet"},
        {"url": "https://mainnet.smartpy.io", "provider": "smart", "net": "mainnet"},
        {"url": "https://shadownet.ecad.io", "provider": "ecad", "net": "shadownet"}
      ]
    }|}
  in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "three nodes" 3 (List.length nodes) ;
  let n0 = List.nth nodes 0 in
  (* Network is NOT included in label - it's stored separately to avoid duplication *)
  Alcotest.(check string) "label without network" "ECAD Labs" n0.label ;
  Alcotest.(check string) "rpc" "https://mainnet.ecad.io" n0.rpc_addr ;
  Alcotest.(check (option string)) "network" (Some "mainnet") n0.network

let test_parse_taquito_format_unknown_provider () =
  let json =
    {|{
      "providers": [],
      "rpc_endpoints": [
        {"url": "https://example.com/rpc", "provider": "unknown-prov", "net": "testnet"}
      ]
    }|}
  in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "one node" 1 (List.length nodes) ;
  (* provider_of falls back to the raw id when not in providers list *)
  (* Network is NOT included in label *)
  Alcotest.(check string)
    "label uses raw provider id"
    "unknown-prov"
    (List.nth nodes 0).label

let test_parse_taquito_format_no_provider () =
  let json =
    {|{
      "providers": [],
      "rpc_endpoints": [
        {"url": "https://example.com/rpc", "net": "testnet"}
      ]
    }|}
  in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "one node" 1 (List.length nodes) ;
  (* When no provider, label falls back to URL, not network *)
  Alcotest.(check string)
    "label falls back to URL"
    "https://example.com/rpc"
    (List.nth nodes 0).label

let test_parse_taquito_format_no_net_no_provider () =
  let json =
    {|{
      "providers": [],
      "rpc_endpoints": [
        {"url": "https://example.com/rpc"}
      ]
    }|}
  in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "one node" 1 (List.length nodes) ;
  Alcotest.(check string)
    "label falls back to URL"
    "https://example.com/rpc"
    (List.nth nodes 0).label

let test_parse_taquito_format_empty_url () =
  let json =
    {|{
      "providers": [],
      "rpc_endpoints": [{"url": "", "net": "mainnet"}]
    }|}
  in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "empty url filtered" 0 (List.length nodes)

let test_parse_taquito_format_no_endpoints () =
  let json = {|{"providers": [{"id": "foo", "name": "Foo"}]}|} in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "no endpoints" 0 (List.length nodes)

let test_parse_malformed_json () =
  let nodes = Rpc_node_selection.parse_taquito_json "not json at all" in
  Alcotest.(check int) "malformed returns empty" 0 (List.length nodes)

let test_parse_empty_string () =
  let nodes = Rpc_node_selection.parse_taquito_json "" in
  Alcotest.(check int) "empty returns empty" 0 (List.length nodes)

let test_parse_empty_list () =
  let nodes = Rpc_node_selection.parse_taquito_json "[]" in
  Alcotest.(check int) "empty list" 0 (List.length nodes)

let test_parse_unexpected_type () =
  let nodes = Rpc_node_selection.parse_taquito_json "42" in
  Alcotest.(check int) "number returns empty" 0 (List.length nodes) ;
  let nodes2 = Rpc_node_selection.parse_taquito_json "true" in
  Alcotest.(check int) "bool returns empty" 0 (List.length nodes2) ;
  let nodes3 = Rpc_node_selection.parse_taquito_json {|"just a string"|} in
  Alcotest.(check int) "string returns empty" 0 (List.length nodes3)

let test_parse_null () =
  let nodes = Rpc_node_selection.parse_taquito_json "null" in
  Alcotest.(check int) "null returns empty" 0 (List.length nodes)

let test_parse_mixed_valid_invalid () =
  let json =
    {|[
      {"name": "Good", "rpc": "https://good.com"},
      {"name": "Bad"},
      {"name": "Also Bad", "rpc": ""},
      {"name": "Good2", "rpc_url": "https://good2.com"}
    ]|}
  in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "only valid nodes" 2 (List.length nodes)

let test_parse_non_assoc_in_list () =
  let json = {|[42, "string", null, {"rpc": "https://valid.com"}]|} in
  let nodes = Rpc_node_selection.parse_taquito_json json in
  Alcotest.(check int) "only assoc items parsed" 1 (List.length nodes)

(* ============================================================ *)
(* total_items Tests                                             *)
(* ============================================================ *)

let make_state ?(public_nodes = []) ?(local_instances = []) ?(cursor = 0) () =
  let display_items =
    Rpc_node_selection.build_display_items ~public_nodes ~local_instances
  in
  Rpc_node_selection.
    {
      public_nodes;
      local_instances;
      cursor;
      loading = false;
      error = None;
      display_items;
    }

let make_item ?(label = "test") ?(rpc_addr = "http://localhost")
    ?(is_public = true) ?(network = None) () =
  Rpc_node_selection.{label; rpc_addr; is_public; network}

let test_total_items_empty () =
  let s = make_state () in
  Alcotest.(check int) "empty" 0 (Rpc_node_selection.total_items s)

let test_total_items_public_only () =
  let s = make_state ~public_nodes:[make_item (); make_item ~label:"b" ()] () in
  (* 1 section header + 1 network header + 2 nodes = 4 *)
  Alcotest.(check int) "public only" 4 (Rpc_node_selection.total_items s)

let test_total_items_local_only () =
  let s = make_state ~local_instances:[make_item ~is_public:false ()] () in
  (* 1 section header + 1 network header + 1 node = 3 *)
  Alcotest.(check int) "local only" 3 (Rpc_node_selection.total_items s)

let test_total_items_both () =
  let s =
    make_state
      ~public_nodes:[make_item ()]
      ~local_instances:[make_item ~is_public:false ()]
      ()
  in
  (* PUBLIC: 1 section header + 1 network header + 1 node = 3
     LOCAL: 1 section header + 1 network header + 1 node = 3
     Total = 6 *)
  Alcotest.(check int) "both" 6 (Rpc_node_selection.total_items s)

(* ============================================================ *)
(* get_item_at_cursor Tests                                      *)
(* ============================================================ *)

let test_get_item_public_header () =
  (* With local-first ordering: 0=LOCAL header, 1=network, 2=local node, 3=PUBLIC header *)
  let s =
    make_state
      ~public_nodes:[make_item ()]
      ~local_instances:[make_item ~is_public:false ()]
      ~cursor:3
      ()
  in
  match Rpc_node_selection.get_item_at_cursor s with
  | `SectionHeader -> ()
  | _ -> Alcotest.fail "expected SectionHeader"

let test_get_item_public_node () =
  let item = make_item ~label:"pub1" () in
  (* Public-only: 0=PUBLIC NODES header, 1=network header, 2=first node *)
  let s = make_state ~public_nodes:[item] ~cursor:2 () in
  match Rpc_node_selection.get_item_at_cursor s with
  | `Node n -> Alcotest.(check string) "correct item" "pub1" n.label
  | _ -> Alcotest.fail "expected Node"

let test_get_item_local_header () =
  (* Local-only: 0=LOCAL header, 1=network header, 2=first local node *)
  let s =
    make_state ~local_instances:[make_item ~is_public:false ()] ~cursor:0 ()
  in
  match Rpc_node_selection.get_item_at_cursor s with
  | `SectionHeader -> ()
  | _ -> Alcotest.fail "expected SectionHeader"

let test_get_item_local_node () =
  let local_item = make_item ~label:"local1" ~is_public:false () in
  let s =
    make_state
      ~public_nodes:[make_item ()]
      ~local_instances:[local_item]
      ~cursor:2
      ()
  in
  (* LOCAL-first ordering: 0=LOCAL header, 1=network, 2=local node, 3=PUBLIC header, 4=network, 5=pub node *)
  match Rpc_node_selection.get_item_at_cursor s with
  | `Node n -> Alcotest.(check string) "correct item" "local1" n.label
  | _ -> Alcotest.fail "expected Node"

let test_get_item_out_of_bounds () =
  let s = make_state ~public_nodes:[make_item ()] ~cursor:99 () in
  match Rpc_node_selection.get_item_at_cursor s with
  | `None -> ()
  | _ -> Alcotest.fail "expected None"

(* ============================================================ *)
(* move_cursor Tests                                             *)
(* ============================================================ *)

let test_move_cursor_empty () =
  let s = make_state () in
  let s' = Rpc_node_selection.move_cursor 1 s in
  Alcotest.(check int) "stays at 0" 0 s'.cursor

let test_move_cursor_down () =
  let s =
    make_state
      ~public_nodes:[make_item ~label:"a" (); make_item ~label:"b" ()]
      ~cursor:2
      ()
  in
  (* cursor=2 is first node (0=section header, 1=network header, 2=first node) *)
  let s' = Rpc_node_selection.move_cursor 1 s in
  (* Should move to second node *)
  Alcotest.(check int) "moves down" 3 s'.cursor

let test_move_cursor_up () =
  let s =
    make_state
      ~public_nodes:[make_item ~label:"a" (); make_item ~label:"b" ()]
      ~cursor:3
      ()
  in
  (* cursor=3 is second node *)
  let s' = Rpc_node_selection.move_cursor (-1) s in
  (* Should move to first node (skipping headers) *)
  Alcotest.(check int) "moves up" 2 s'.cursor

let test_move_cursor_bounds_max () =
  let s = make_state ~public_nodes:[make_item ()] ~cursor:2 () in
  (* total_items = 3 (section header + network header + 1 node), max selectable index = 2 *)
  let s' = Rpc_node_selection.move_cursor 1 s in
  (* Should stay at max selectable position *)
  Alcotest.(check int) "clamped at max" 2 s'.cursor

let test_move_cursor_bounds_min () =
  let s = make_state ~public_nodes:[make_item ()] ~cursor:2 () in
  let s' = Rpc_node_selection.move_cursor (-5) s in
  (* Cursor should go to first selectable item (index 2, the node) skipping headers *)
  Alcotest.(check int) "wraps to first selectable" 2 s'.cursor

let test_move_cursor_skips_header_down () =
  (* When moving down, header positions should be skipped.
     LOCAL-first ordering: 0=LOCAL header, 1=network, 2=local node, 3=PUBLIC header, 4=network, 5=pub node *)
  let s =
    make_state
      ~public_nodes:[make_item ()]
      ~local_instances:[make_item ~is_public:false ()]
      ~cursor:2
      ()
  in
  (* cursor at 2 (local node), moving down should skip PUBLIC section and network headers and land on pub node (idx 5) *)
  let s' = Rpc_node_selection.move_cursor 1 s in
  Alcotest.(check int) "skips headers to public node" 5 s'.cursor

let test_move_cursor_skips_header_up () =
  (* When moving up through headers, should skip to previous selectable node.
     LOCAL-first ordering: 0=LOCAL header, 1=network, 2=local node, 3=PUBLIC header, 4=network, 5=pub node *)
  let s =
    make_state
      ~public_nodes:[make_item ()]
      ~local_instances:[make_item ~is_public:false ()]
      ~cursor:5
      ()
  in
  (* cursor at 5 (public node), moving up should skip headers at 4,3 and land on local node at 2 *)
  let s' = Rpc_node_selection.move_cursor (-1) s in
  Alcotest.(check int) "skips headers up to local node" 2 s'.cursor

(* ============================================================ *)
(* curated_defaults Tests                                        *)
(* ============================================================ *)

let test_curated_defaults_not_empty () =
  Alcotest.(check bool)
    "has defaults"
    true
    (List.length Rpc_node_selection.curated_defaults > 0)

let test_curated_defaults_all_public () =
  List.iter
    (fun (n : Rpc_node_selection.node_item) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s is_public" n.label)
        true
        n.is_public)
    Rpc_node_selection.curated_defaults

let test_curated_defaults_all_have_rpc () =
  List.iter
    (fun (n : Rpc_node_selection.node_item) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s has rpc" n.label)
        true
        (String.length n.rpc_addr > 0))
    Rpc_node_selection.curated_defaults

(* ============================================================ *)
(* make_service_for_node Tests                                   *)
(* ============================================================ *)

let test_make_service_for_node () =
  let item =
    make_item
      ~label:"Test Node"
      ~rpc_addr:"https://rpc.example.com"
      ~network:(Some "mainnet")
      ()
  in
  let svc = Rpc_node_selection.make_service_for_node item in
  Alcotest.(check string)
    "instance"
    "Test Node"
    svc.Octez_manager_lib.Service.instance ;
  Alcotest.(check string) "role" "node" svc.Octez_manager_lib.Service.role ;
  Alcotest.(check string)
    "rpc_addr"
    "https://rpc.example.com"
    (Octez_manager_lib.Rpc_addr.to_string
       svc.Octez_manager_lib.Service.rpc_addr) ;
  Alcotest.(check string)
    "network"
    "mainnet"
    svc.Octez_manager_lib.Service.network

let test_make_service_for_node_no_network () =
  let item = make_item ~network:None () in
  let svc = Rpc_node_selection.make_service_for_node item in
  Alcotest.(check string)
    "defaults to unknown"
    "unknown"
    svc.Octez_manager_lib.Service.network

(* ============================================================ *)
(* PBT: parse_taquito_json no-crash on random strings            *)
(* ============================================================ *)

let test_parse_no_crash =
  QCheck.Test.make
    ~name:"parse_taquito_json never crashes"
    ~count:500
    QCheck.string
    (fun s ->
      let _ = Rpc_node_selection.parse_taquito_json s in
      true)

let test_parse_no_crash_json_like =
  let gen =
    QCheck.Gen.(
      oneof
        [
          map (fun s -> Printf.sprintf "[%s]" s) string;
          map (fun s -> Printf.sprintf "{%s}" s) string;
          string;
        ])
  in
  QCheck.Test.make
    ~name:"parse_taquito_json never crashes on JSON-like strings"
    ~count:500
    (QCheck.make gen)
    (fun s ->
      let _ = Rpc_node_selection.parse_taquito_json s in
      true)

(* ============================================================ *)
(* PBT: move_cursor invariants                                   *)
(* ============================================================ *)

let test_move_cursor_in_bounds =
  let gen =
    QCheck.Gen.(
      let* n_public = int_range 0 5 in
      let* n_local = int_range 0 5 in
      let total =
        (if n_public > 0 then 1 + n_public else 0)
        + if n_local > 0 then 1 + n_local else 0
      in
      let* cursor = if total = 0 then return 0 else int_range 0 (total - 1) in
      let* delta = int_range (-10) 10 in
      let public_nodes =
        List.init n_public (fun i ->
            make_item ~label:(Printf.sprintf "pub%d" i) ())
      in
      let local_instances =
        List.init n_local (fun i ->
            make_item ~label:(Printf.sprintf "loc%d" i) ~is_public:false ())
      in
      return (make_state ~public_nodes ~local_instances ~cursor (), delta))
  in
  QCheck.Test.make
    ~name:"move_cursor always produces valid cursor"
    ~count:500
    (QCheck.make gen)
    (fun (s, delta) ->
      let s' = Rpc_node_selection.move_cursor delta s in
      let total = Rpc_node_selection.total_items s in
      if total = 0 then s'.cursor = 0 else s'.cursor >= 0 && s'.cursor < total)

(* ============================================================ *)
(* Test Runner                                                   *)
(* ============================================================ *)

let () =
  Alcotest.run
    "Rpc_node_selection"
    [
      ( "parse_taquito_json",
        [
          Alcotest.test_case "old format" `Quick test_parse_old_format;
          Alcotest.test_case
            "old format no name"
            `Quick
            test_parse_old_format_no_name;
          Alcotest.test_case
            "old format empty rpc"
            `Quick
            test_parse_old_format_empty_rpc;
          Alcotest.test_case
            "old format no network"
            `Quick
            test_parse_old_format_no_network;
          Alcotest.test_case "taquito format" `Quick test_parse_taquito_format;
          Alcotest.test_case
            "taquito unknown provider"
            `Quick
            test_parse_taquito_format_unknown_provider;
          Alcotest.test_case
            "taquito no provider"
            `Quick
            test_parse_taquito_format_no_provider;
          Alcotest.test_case
            "taquito no net no provider"
            `Quick
            test_parse_taquito_format_no_net_no_provider;
          Alcotest.test_case
            "taquito empty url"
            `Quick
            test_parse_taquito_format_empty_url;
          Alcotest.test_case
            "taquito no endpoints"
            `Quick
            test_parse_taquito_format_no_endpoints;
          Alcotest.test_case "malformed" `Quick test_parse_malformed_json;
          Alcotest.test_case "empty string" `Quick test_parse_empty_string;
          Alcotest.test_case "empty list" `Quick test_parse_empty_list;
          Alcotest.test_case "unexpected type" `Quick test_parse_unexpected_type;
          Alcotest.test_case "null" `Quick test_parse_null;
          Alcotest.test_case
            "mixed valid/invalid"
            `Quick
            test_parse_mixed_valid_invalid;
          Alcotest.test_case
            "non-assoc in list"
            `Quick
            test_parse_non_assoc_in_list;
        ] );
      ( "total_items",
        [
          Alcotest.test_case "empty" `Quick test_total_items_empty;
          Alcotest.test_case "public only" `Quick test_total_items_public_only;
          Alcotest.test_case "local only" `Quick test_total_items_local_only;
          Alcotest.test_case "both" `Quick test_total_items_both;
        ] );
      ( "get_item_at_cursor",
        [
          Alcotest.test_case "public header" `Quick test_get_item_public_header;
          Alcotest.test_case "public node" `Quick test_get_item_public_node;
          Alcotest.test_case "local header" `Quick test_get_item_local_header;
          Alcotest.test_case "local node" `Quick test_get_item_local_node;
          Alcotest.test_case "out of bounds" `Quick test_get_item_out_of_bounds;
        ] );
      ( "move_cursor",
        [
          Alcotest.test_case "empty" `Quick test_move_cursor_empty;
          Alcotest.test_case "down" `Quick test_move_cursor_down;
          Alcotest.test_case "up" `Quick test_move_cursor_up;
          Alcotest.test_case "bounds max" `Quick test_move_cursor_bounds_max;
          Alcotest.test_case "bounds min" `Quick test_move_cursor_bounds_min;
          Alcotest.test_case
            "skips header down"
            `Quick
            test_move_cursor_skips_header_down;
          Alcotest.test_case
            "skips header up"
            `Quick
            test_move_cursor_skips_header_up;
        ] );
      ( "curated_defaults",
        [
          Alcotest.test_case "not empty" `Quick test_curated_defaults_not_empty;
          Alcotest.test_case
            "all public"
            `Quick
            test_curated_defaults_all_public;
          Alcotest.test_case
            "all have rpc"
            `Quick
            test_curated_defaults_all_have_rpc;
        ] );
      ( "make_service_for_node",
        [
          Alcotest.test_case "basic" `Quick test_make_service_for_node;
          Alcotest.test_case
            "no network"
            `Quick
            test_make_service_for_node_no_network;
        ] );
      ( "PBT",
        List.map
          QCheck_alcotest.to_alcotest
          [
            test_parse_no_crash;
            test_parse_no_crash_json_like;
            test_move_cursor_in_bounds;
          ] );
    ]
