(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module Help_parser = Octez_manager_lib.Help_parser
open Installer_types
module Binary_help_explorer = Octez_manager_ui.Binary_help_explorer
module Install_node_form_v3 = Octez_manager_ui.Install_node_form_v3
module Install_baker_form_v3 = Octez_manager_ui.Install_baker_form_v3
module Cache = Octez_manager_ui.Cache

let option_string = Alcotest.(option string)

let pair_string = Alcotest.(pair string string)

let list_pairs = Alcotest.(list pair_string)

let list_string = Alcotest.(list string)

let list_list_string = Alcotest.(list (list string))

let sort_pairs l = List.sort (fun (a, _) (b, _) -> compare a b) l

let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec loop idx =
    if idx + nlen > hlen then false
    else if String.sub haystack idx nlen = needle then true
    else loop (idx + 1)
  in
  if nlen = 0 then true else loop 0

type fake_xdg = {config : string; data : string}

let with_env overrides f =
  let set_var k = function
    | Some v -> Unix.putenv k v
    | None -> Unix.putenv k ""
  in
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) overrides in
  List.iter (fun (k, v) -> set_var k v) overrides ;
  Fun.protect ~finally:(fun () -> List.iter (fun (k, v) -> set_var k v) saved) f

let with_temp_dir f =
  let base = Filename.temp_file "octez_manager" "test" in
  Sys.remove base ;
  Unix.mkdir base 0o755 ;
  Fun.protect
    ~finally:(fun () ->
      let (_ : (unit, [> Rresult.R.msg]) result) = Common.remove_tree base in
      ())
    (fun () -> f base)

let with_fake_xdg f =
  with_temp_dir (fun base ->
      let mk_dir name =
        let dir = Filename.concat base name in
        Unix.mkdir dir 0o755 ;
        dir
      in
      let config = mk_dir "cfg" in
      let data = mk_dir "data" in
      with_env
        [("XDG_CONFIG_HOME", Some config); ("XDG_DATA_HOME", Some data)]
        (fun () -> f {config; data}))

let write_exec_file path body =
  let oc = open_out path in
  output_string oc body ;
  close_out oc ;
  Unix.chmod path 0o755

let with_systemctl_stub f =
  with_temp_dir (fun base ->
      let bin = Filename.concat base "bin" in
      Unix.mkdir bin 0o755 ;
      let script = Filename.concat bin "systemctl" in
      let body =
        "#!/bin/sh\n" ^ "if [ \"$1\" = \"--user\" ]; then shift; fi\n"
        ^ "cmd=\"$1\"\n" ^ "shift >/dev/null 2>&1 || true\n"
        ^ "case \"$cmd\" in\n" ^ "  cat)\n" ^ "    printf \"unit-stub\\n\"\n"
        ^ "    ;;\n" ^ "  status)\n" ^ "    printf \"status-stub\\n\"\n"
        ^ "    ;;\n" ^ "  is-enabled)\n" ^ "    printf \"enabled\\n\"\n"
        ^ "    ;;\n" ^ "  *)\n" ^ "    :\n" ^ "    ;;\n" ^ "esac\n" ^ "exit 0\n"
      in
      write_exec_file script body ;
      let previous = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
      let new_path = if previous = "" then bin else bin ^ ":" ^ previous in
      with_env [("PATH", Some new_path)] f)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let current_user_group () =
  match Common.current_user_group_names () with
  | "", "" ->
      let pw = Unix.getpwuid (Unix.geteuid ()) in
      let gr = Unix.getgrgid pw.Unix.pw_gid in
      (pw.Unix.pw_name, gr.Unix.gr_name)
  | pair -> pair

let logging_mode_equal a b =
  match (a, b) with Logging_mode.Journald, Logging_mode.Journald -> true

let service_equal a b =
  String.equal a.Service.instance b.Service.instance
  && String.equal a.role b.role
  && String.equal a.network b.network
  && a.history_mode = b.history_mode
  && String.equal a.data_dir b.data_dir
  && String.equal a.rpc_addr b.rpc_addr
  && String.equal a.net_addr b.net_addr
  && String.equal a.service_user b.service_user
  && String.equal a.app_bin_dir b.app_bin_dir
  && String.equal a.created_at b.created_at
  && logging_mode_equal a.logging_mode b.logging_mode
  && Bool.equal a.snapshot_auto b.snapshot_auto
  && Option.equal String.equal a.snapshot_uri b.snapshot_uri
  && Option.equal String.equal a.snapshot_network_slug b.snapshot_network_slug
  && Bool.equal a.snapshot_no_check b.snapshot_no_check
  && List.equal String.equal a.extra_args b.extra_args

let check_service expected actual =
  Alcotest.(check bool) "service equality" true (service_equal expected actual)

let sample_service ?(logging_mode = Logging_mode.Journald) () : Service.t =
  {
    Service.instance = "alpha";
    role = "node";
    network = "https://example/net.json";
    history_mode = History_mode.Full;
    data_dir = "/tmp/octez/alpha";
    rpc_addr = "127.0.0.1:8732";
    net_addr = "0.0.0.0:9732";
    service_user = "octez";
    app_bin_dir = "/opt/octez";
    created_at = "2024-01-01 00:00:00";
    logging_mode;
    snapshot_auto = false;
    snapshot_uri = None;
    snapshot_network_slug = None;
    snapshot_no_check = false;
    extra_args = [];
    depends_on = None;
    dependents = [];
  }

let sort_services =
  List.sort (fun a b -> compare a.Service.instance b.Service.instance)

let sample_node_request ?data_dir ?(bootstrap = Genesis)
    ?(history_mode = History_mode.Rolling)
    ?(logging_mode = Logging_mode.Journald) ?(extra_args = [])
    ?(auto_enable = false) ?(preserve_data = false) ?(snapshot_no_check = false)
    () : node_request =
  {
    instance = "alpha";
    network = "https://teztnets.com/seoulnet";
    history_mode;
    data_dir;
    rpc_addr = "127.0.0.1:8732";
    net_addr = "0.0.0.0:9732";
    service_user = "octez";
    app_bin_dir = "/opt/octez";
    logging_mode;
    extra_args;
    auto_enable;
    bootstrap;
    preserve_data;
    snapshot_no_check;
    tmp_dir = None;
    keep_snapshot = false;
  }

let service_make_populates_fields () =
  let service =
    Service.make
      ~instance:"beta"
      ~role:"node"
      ~network:"https://example/beta.json"
      ~history_mode:History_mode.Full
      ~data_dir:"/tmp/octez/beta"
      ~rpc_addr:"127.0.0.1:8732"
      ~net_addr:"0.0.0.0:9732"
      ~service_user:"octez"
      ~app_bin_dir:"/opt/octez"
      ~logging_mode:Logging_mode.Journald
      ~extra_args:["--foo"]
      ()
  in
  Alcotest.(check string) "instance" "beta" service.instance ;
  Alcotest.(check string) "network" "https://example/beta.json" service.network ;
  Alcotest.(check string) "role" "node" service.role ;
  Alcotest.(check bool) "history" true (service.history_mode = History_mode.Full) ;
  Alcotest.(check bool)
    "created_at"
    true
    (String.length service.created_at >= 19) ;
  Alcotest.(check bool)
    "logging"
    true
    (logging_mode_equal service.logging_mode Logging_mode.Journald) ;
  Alcotest.(check list_string) "extra_args" ["--foo"] service.extra_args

let service_of_yojson_invalid_history () =
  let json =
    `Assoc
      [
        ("instance", `String "beta");
        ("role", `String "node");
        ("network", `String "https://example/beta.json");
        ("history_mode", `String "unknown");
        ("data_dir", `String "/tmp/beta");
        ("rpc_addr", `String "127.0.0.1:8732");
        ("net_addr", `String "0.0.0.0:9732");
        ("service_user", `String "octez");
        ("app_bin_dir", `String "/opt/octez");
        ("created_at", `String "2024");
        ("logging_mode", `Assoc [("type", `String "journald")]);
      ]
  in
  match Service.of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "invalid history mode should fail"

let expect_ok = function
  | Ok v -> v
  | Error (`Msg msg) -> Alcotest.failf "unexpected error: %s" msg

let history_mode_roundtrip () =
  let open History_mode in
  [("rolling", Rolling); ("Full", Full); ("  ARCHIVE  ", Archive)]
  |> List.iter (fun (input, expected) ->
      match of_string input with
      | Ok parsed ->
          Alcotest.(check string)
            "history mode roundtrip"
            (to_string expected)
            (to_string parsed)
      | Error (`Msg msg) -> Alcotest.failf "unexpected error: %s" msg)

let history_mode_invalid () =
  match History_mode.of_string "unknown" with
  | Ok _ -> Alcotest.fail "invalid mode should not parse"
  | Error _ -> ()

let logging_mode_default_is_journald () =
  match Logging_mode.default with Logging_mode.Journald -> ()

let logging_mode_to_string_tests () =
  Alcotest.(check string)
    "journald"
    "journald"
    (Logging_mode.to_string Logging_mode.Journald)

(* Logging is via journald - no file setup needed *)
let installer_logging_journald_noop () =
  let owner, group = current_user_group () in
  expect_ok
    (Installer.For_tests.ensure_logging_base_directory
       ~owner
       ~group
       Logging_mode.Journald) ;
  expect_ok (Installer.For_tests.remove_logging_artifacts Logging_mode.Journald)

let installer_should_drop_service_user () =
  let mk_service ~instance ~user =
    {(sample_service ()) with Service.instance; service_user = user}
  in
  let services = [mk_service ~instance:"alpha" ~user:"octez"] in
  Alcotest.(check bool)
    "still referenced"
    false
    (Installer.For_tests.should_drop_service_user
       ~user:"octez"
       ~remaining_services:services) ;
  Alcotest.(check bool)
    "safe to drop"
    true
    (Installer.For_tests.should_drop_service_user
       ~user:"octez"
       ~remaining_services:[])

let installer_backup_missing_file () =
  with_temp_dir (fun base ->
      let path = Filename.concat base "missing.json" in
      match Installer.For_tests.backup_file_if_exists ~path with
      | Ok None -> ()
      | Ok (Some _) -> Alcotest.fail "expected no backup"
      | Error (`Msg msg) -> Alcotest.failf "backup missing: %s" msg)

let installer_backup_restore_roundtrip () =
  with_temp_dir (fun base ->
      let owner, group = current_user_group () in
      let data_dir = Filename.concat base "data" in
      Unix.mkdir data_dir 0o755 ;
      let identity_path = Filename.concat data_dir "identity.json" in
      let oc = open_out identity_path in
      output_string oc "{\"peer_id\":\"abc\"}" ;
      close_out oc ;
      let backup =
        match
          expect_ok
            (Installer.For_tests.backup_file_if_exists ~path:identity_path)
        with
        | Some b -> b
        | None -> Alcotest.fail "expected backup"
      in
      let (_ : (unit, [> Rresult.R.msg]) result) =
        Common.remove_tree data_dir
      in
      let () =
        expect_ok
          (Installer.For_tests.restore_backup ~owner ~group (Some backup))
      in
      let restored = read_file identity_path in
      Alcotest.(check string)
        "identity restored"
        "{\"peer_id\":\"abc\"}"
        restored)

let installer_normalize_data_dir_default () =
  with_fake_xdg (fun env ->
      let expected = Filename.concat env.data "octez/alpha" in
      let actual = Installer.For_tests.normalize_data_dir "alpha" None in
      Alcotest.(check string) "default data dir" expected actual)

let installer_endpoint_of_rpc_formats () =
  let trimmed = Installer.endpoint_of_rpc "  localhost:8732  " in
  Alcotest.(check string) "adds scheme" "http://localhost:8732" trimmed ;
  let passthrough = Installer.endpoint_of_rpc "https://node:8732" in
  Alcotest.(check string) "keeps scheme" "https://node:8732" passthrough

let installer_build_run_args_journald () =
  let logging_mode = Logging_mode.default in
  let args =
    Installer.For_tests.build_run_args
      ~network:"mainnet"
      ~history_mode:History_mode.Full
      ~rpc_addr:"127.0.0.1:8732"
      ~net_addr:"0.0.0.0:9732"
      ~extra_args:["--peer"; "2"]
      ~logging_mode
  in
  (* Journald logging: no --log-output flag *)
  Alcotest.(check bool)
    "no log-output flag"
    false
    (string_contains ~needle:"--log-output" args) ;
  (* Should still have other args *)
  Alcotest.(check bool)
    "contains rpc addr"
    true
    (string_contains ~needle:"--rpc-addr=127.0.0.1:8732" args)

let installer_snapshot_plan_direct_uri () =
  let request =
    sample_node_request
      ~bootstrap:(Snapshot {src = Some "file:///tmp/snapshot"})
      ()
  in
  match Installer.For_tests.snapshot_plan_of_request request with
  | Ok (Direct_snapshot {uri}) ->
      Alcotest.(check string) "direct uri" "file:///tmp/snapshot" uri
  | Ok _ -> Alcotest.fail "expected direct snapshot plan"
  | Error (`Msg msg) -> Alcotest.failf "plan error: %s" msg

let installer_snapshot_plan_tzinit () =
  let html =
    "<pre><b>History mode</b>: Rolling\n\
     <b>HTTPS</b>: https://example/rolling.snap</pre>"
  in
  let fetch url =
    if String.ends_with ~suffix:"/seoulnet/rolling.html" url then Ok (200, html)
    else Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      let request = sample_node_request ~bootstrap:(Snapshot {src = None}) () in
      match Installer.For_tests.snapshot_plan_of_request request with
      | Ok (Tzinit_snapshot res) ->
          Alcotest.(check string)
            "download url"
            "https://example/rolling.snap"
            res.download_url ;
          Alcotest.(check string) "kind" "rolling" res.kind_slug ;
          Alcotest.(check string) "network" "seoulnet" res.network_slug
      | Ok _ -> Alcotest.fail "expected tzinit plan"
      | Error (`Msg msg) -> Alcotest.failf "tzinit plan error: %s" msg)

let installer_snapshot_metadata_variants () =
  let no_meta =
    Installer.For_tests.snapshot_metadata_of_plan ~no_check:false No_snapshot
  in
  Alcotest.(check bool) "no auto" false no_meta.auto ;
  Alcotest.(check bool)
    "no_check propagates for No_snapshot"
    false
    no_meta.no_check ;
  let no_meta_with_check =
    Installer.For_tests.snapshot_metadata_of_plan ~no_check:true No_snapshot
  in
  Alcotest.(check bool)
    "no_check true for No_snapshot when set"
    true
    no_meta_with_check.no_check ;
  let direct_meta =
    Installer.For_tests.snapshot_metadata_of_plan
      ~no_check:false
      (Direct_snapshot {uri = "https://example"})
  in
  Alcotest.(check (option string))
    "uri stored"
    (Some "https://example")
    direct_meta.uri ;
  Alcotest.(check bool) "no_check false by default" false direct_meta.no_check ;
  let direct_meta_no_check =
    Installer.For_tests.snapshot_metadata_of_plan
      ~no_check:true
      (Direct_snapshot {uri = "https://example"})
  in
  Alcotest.(check bool)
    "no_check true when set"
    true
    direct_meta_no_check.no_check ;
  let tzinit_meta =
    Installer.For_tests.snapshot_metadata_of_plan
      ~no_check:false
      (Tzinit_snapshot
         {
           download_url = "https://example";
           network_slug = "seoulnet";
           kind_slug = "rolling";
         })
  in
  Alcotest.(check (option string))
    "network slug"
    (Some "seoulnet")
    tzinit_meta.network_slug ;
  Alcotest.(check bool) "no_check false for tzinit" false tzinit_meta.no_check ;
  let tzinit_meta_no_check =
    Installer.For_tests.snapshot_metadata_of_plan
      ~no_check:true
      (Tzinit_snapshot
         {
           download_url = "https://example";
           network_slug = "seoulnet";
           kind_slug = "rolling";
         })
  in
  Alcotest.(check bool)
    "no_check true for tzinit when set"
    true
    tzinit_meta_no_check.no_check

let snapshot_base = "https://snapshots.tzinit.org"

let snapshot_url network slug =
  Printf.sprintf "%s/%s/%s.html" snapshot_base network slug

let metadata_html ?(with_code = false) entries =
  let body =
    entries
    |> List.map (fun (k, v) -> Printf.sprintf "<b>%s</b>: %s" k v)
    |> String.concat "\n"
  in
  if with_code then Printf.sprintf "<pre><code>%s</code></pre>" body
  else Printf.sprintf "<pre>%s</pre>" body

let installer_strip_and_detect_uris () =
  Alcotest.(check bool)
    "http detection"
    true
    (Installer.For_tests.is_http_url "HTTPS://example") ;
  Alcotest.(check bool)
    "file detection"
    true
    (Installer.For_tests.is_file_uri "file:///tmp/foo") ;
  Alcotest.(check (option string))
    "strip"
    (Some "/tmp/foo")
    (Installer.For_tests.strip_file_uri "file:///tmp/foo")

let installer_history_mode_matches () =
  Alcotest.(check bool)
    "rolling matches"
    true
    (Installer.For_tests.history_mode_matches
       ~requested:History_mode.Rolling
       ~snapshot_mode:"rolling") ;
  Alcotest.(check bool)
    "full matches case-insensitive"
    true
    (Installer.For_tests.history_mode_matches
       ~requested:History_mode.Full
       ~snapshot_mode:"Full") ;
  Alcotest.(check bool)
    "mismatch detected"
    false
    (Installer.For_tests.history_mode_matches
       ~requested:History_mode.Rolling
       ~snapshot_mode:"full")

let installer_snapshot_history_mode_mismatch () =
  let html =
    metadata_html
      [("History mode", "full"); ("HTTPS", "https://example/full.snap")]
  in
  let fetch url =
    if String.ends_with ~suffix:"/mainnet/rolling.html" url then Ok (200, html)
    else Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match
        Installer.For_tests.resolve_snapshot_download
          ~network:"mainnet"
          ~history_mode:History_mode.Rolling
      with
      | Error (`Msg msg) ->
          Alcotest.(check bool)
            "error mentions mismatch"
            true
            (string_contains ~needle:"history mode" msg
            && string_contains ~needle:"rolling" msg
            && string_contains ~needle:"full" msg)
      | Ok _ -> Alcotest.fail "should reject history mode mismatch")

let installer_snapshot_history_mode_match () =
  let html =
    metadata_html
      [("History mode", "rolling"); ("HTTPS", "https://example/rolling.snap")]
  in
  let fetch url =
    if String.ends_with ~suffix:"/mainnet/rolling.html" url then Ok (200, html)
    else Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match
        Installer.For_tests.resolve_snapshot_download
          ~network:"mainnet"
          ~history_mode:History_mode.Rolling
      with
      | Ok res ->
          Alcotest.(check string)
            "download url"
            "https://example/rolling.snap"
            res.download_url
      | Error (`Msg msg) -> Alcotest.failf "should accept matching mode: %s" msg)

let install_node_form_v3_history_conflict () =
  let module F = Octez_manager_ui.Install_node_form_v3.For_tests in
  F.clear_snapshot_cache () ;
  let mk_entry ~slug ~history_mode =
    {
      Snapshots.network = "mainnet";
      slug;
      label = slug;
      download_url = None;
      history_mode = Some history_mode;
      metadata = [];
    }
  in
  F.set_snapshot_cache
    ~network:"mainnet"
    ~entries:[mk_entry ~slug:"full" ~history_mode:"full"] ;
  let snap_full =
    `Tzinit
      Install_node_form_v3.
        {network_slug = "mainnet"; kind_slug = "full"; label = "full"}
  in
  Alcotest.(check bool)
    "history conflict detected"
    true
    (F.history_snapshot_conflict
       ~history_mode:"rolling"
       ~snapshot:snap_full
       ~network:"mainnet") ;
  F.set_snapshot_cache
    ~network:"mainnet"
    ~entries:[mk_entry ~slug:"rolling" ~history_mode:"rolling"] ;
  let snap_roll =
    `Tzinit
      Install_node_form_v3.
        {network_slug = "mainnet"; kind_slug = "rolling"; label = "rolling"}
  in
  Alcotest.(check bool)
    "history conflict resolved"
    false
    (F.history_snapshot_conflict
       ~history_mode:"rolling"
       ~snapshot:snap_roll
       ~network:"mainnet")

let install_node_form_v3_snapshot_filtering () =
  let module F = Octez_manager_ui.Install_node_form_v3.For_tests in
  let mk_entry ~slug ~history_mode =
    {
      Snapshots.network = "mainnet";
      slug;
      label = slug;
      download_url = None;
      history_mode = Some history_mode;
      metadata = [];
    }
  in
  let rolling_entry = mk_entry ~slug:"rolling" ~history_mode:"rolling" in
  let full_entry = mk_entry ~slug:"full" ~history_mode:"full" in
  let archive_entry = mk_entry ~slug:"archive" ~history_mode:"archive" in
  (* Test rolling mode filters correctly *)
  Alcotest.(check bool)
    "rolling entry matches rolling mode"
    true
    (F.snapshot_entry_matches_history_mode rolling_entry ~history_mode:"rolling") ;
  Alcotest.(check bool)
    "full entry does not match rolling mode"
    false
    (F.snapshot_entry_matches_history_mode full_entry ~history_mode:"rolling") ;
  Alcotest.(check bool)
    "archive entry does not match rolling mode"
    false
    (F.snapshot_entry_matches_history_mode
       archive_entry
       ~history_mode:"rolling") ;
  (* Test full mode filters correctly *)
  Alcotest.(check bool)
    "full entry matches full mode"
    true
    (F.snapshot_entry_matches_history_mode full_entry ~history_mode:"full") ;
  Alcotest.(check bool)
    "rolling entry does not match full mode"
    false
    (F.snapshot_entry_matches_history_mode rolling_entry ~history_mode:"full") ;
  (* Test archive mode filters correctly *)
  Alcotest.(check bool)
    "archive entry matches archive mode"
    true
    (F.snapshot_entry_matches_history_mode
       archive_entry
       ~history_mode:"archive") ;
  Alcotest.(check bool)
    "rolling entry does not match archive mode"
    false
    (F.snapshot_entry_matches_history_mode
       rolling_entry
       ~history_mode:"archive") ;
  (* Test entry with no history mode matches all *)
  let no_mode_entry =
    {
      Snapshots.network = "mainnet";
      slug = "legacy";
      label = "legacy";
      download_url = None;
      history_mode = None;
      metadata = [];
    }
  in
  Alcotest.(check bool)
    "entry with no mode matches rolling"
    true
    (F.snapshot_entry_matches_history_mode no_mode_entry ~history_mode:"rolling") ;
  Alcotest.(check bool)
    "entry with no mode matches full"
    true
    (F.snapshot_entry_matches_history_mode no_mode_entry ~history_mode:"full")

let runtime_probe_writable_directory () =
  let dir = Filename.temp_file "octez_manager_probe" "" in
  Sys.remove dir ;
  Unix.mkdir dir 0o755 ;
  Fun.protect
    ~finally:(fun () ->
      try Unix.rmdir dir
      with _ ->
        () ;
        ())
    (fun () ->
      Octez_manager_ui.Runtime.initialize () ;
      let module Sys = Miaou_interfaces.System in
      let sys = Sys.require () in
      match sys.probe_writable ~path:dir with
      | Ok true -> ()
      | Ok false -> Alcotest.fail "Expected writable directory"
      | Error e -> Alcotest.failf "probe_writable failed: %s" e)

let octez_node_run_help_plain =
  {|
NAME
     octez-node-run - Run the Tezos node

SYNOPSIS
     octez-node run [OPTION]...

DESCRIPTION
     The run command is meant to run the Tezos node. Most of its command
     line arguments corresponds to config file entries, and will have
     priority over the latter if used.

P2P OPTIONS
     --advertised-net-port=PORT
       The alternative TCP port at which this instance can be reached.
       This instance does not actually binds to it. The port may be used
       by a NAT server to forward connections to the instance listenning
       port.

     --binary-chunks-size=NUM
       Size limit (in kB) of binary blocks that are sent to other peers.

     --bootstrap-threshold=NUM
       [DEPRECATED: use synchronisation-threshold instead] The number of
       peers to synchronize with before declaring the node bootstrapped.

     --connections=NUM
       Sets min_connections, expected_connections, max_connections to NUM
       / 2, NUM, (3 * NUM) / 2, respectively. Sets peer_table_size to 8 *
       NUM unless it is already defined on the command line. Sets
       synchronisation_threshold to max(NUM / 4, 2) unless it is already
       defined on the command line.

     --disable-mempool
       If set to [true], the node will not participate in the propagation
       of pending operations (mempool). Default value is [false]. It can
       be used to decrease the memory and computation footprints of the
       node.

     --disable-p2p-maintenance
       Disable the p2p maintenance. This option should be used for
       testing purposes only. The node will not try to establish or close
       connections by itself. It will accept incoming connections, and
       outgoing connection can be initiated by using the RPC 'POST
       /network/connections'.

     --disable-p2p-swap
       Disable p2p connection swaps. This option should be used for
       testing purposes only. The node will neither try to initiate a
       swap of connections with one of its neighbor nor answer to a swap
       request.

     --discovery-addr=ADDR:PORT
       The UDP address and port used for local peer discovery.

     --enable-testchain
       DEPRECATED. If set to [true], the node will spawn a testchain
       during the protocol's testing voting period. Default value is
       [false]. It will increase the node storage usage and computation
       by additionally validating the test network blocks.

     --expected-pow=FLOAT
       Expected level of proof-of-work for peers identity.

     --max-download-speed=NUM
       The maximum number of bytes read per second.

     --max-upload-speed=NUM
       The maximum number of bytes sent per second.

     --net-addr=ADDR:PORT
       The URL at which this instance can be reached.

     --no-bootstrap-peers
       Ignore the peers found in the config file (or the hard-coded
       bootstrap peers in the absence of config file).

     --peer=ADDR:PORT[#ID]
       A peer to bootstrap the network from. Can be used several times to
       add several peers. Optionally, the expected identity of the peer
       can be given using the b58 hash format of its public key.

     --peer-table-size=NUM
       Maximum size of internal peer tables, used to store metadata/logs
       about a peer or about a to-be-authenticated host:port couple.

     --private-mode
       Only open outgoing/accept incoming connections to/from peers
       listed in 'bootstrap-peers' or provided with '--peer' option.

     --sync-latency=NUM
       [latency] is the time interval (in seconds) used to determine if a
       peer is synchronized with a chain. For instance, a peer whose
       known head has a timestamp T is considered synchronized if T >=
       now - max_latency. This parameter's default value was set with the
       chain's current protocol's baking rate in mind (and some allowance
       for network latency).

     --synchronisation-threshold=NUM
       Set the number of peers with whom a chain synchronization must be
       completed to bootstrap the node

RPC OPTIONS
     --allow-all-rpc=ADDR:PORT
       Apply allow-all policy to a given RPC listening address rather
       than the safe default.

     --cors-header=HEADER
       Header reported by Access-Control-Allow-Headers reported during
       CORS preflighting; may be used multiple times

     --cors-origin=ORIGIN
       CORS origin allowed by the RPC server via
       Access-Control-Allow-Origin; may be used multiple times

     --disable-context-pruning
       Disables the storage maintenance of the context

     --enable-http-cache-headers
       Enables HTTP cache headers in the RPC response

     --external-rpc-addr=ADDR:PORT
       The URL at which this external RPC server instance can be reached.

     --max-active-rpc-connections=NUM (absent=100)
       Sets the maximum number of active connections per RPC server.

     --media-type=MEDIATYPE (absent=any)
       Set the media-types supported by the server.

     --rpc-addr=ADDR:PORT
       The URL at which this RPC server instance can be reached. Note
       that: as a local RPC server is handled by the node itself, calling
       computational intensive RPCs can affect the performances of the
       node.

     --rpc-tls=crt,key
       Enable TLS for this RPC server with the provided certificate and
       key.

     --storage-maintenance-delay=VAL
       Configures the storage maintenance delays

MISC OPTIONS
     --allow-yes-crypto
       Allow usage of yes cryptography. This is used conjointly with the
       `TEZOS_USE_YES_CRYPTO_I_KNOW_WHAT_I_AM_DOING` environment
       variable. To actually enable yes crypto this option must be used
       and the environment variable must be set to `true`. If only the
       environment variable is set, the node will refuse to start.

     --config-file=FILE
       The main configuration file.

     -d DIR, --data-dir=DIR (absent TEZOS_NODE_DIR env)
       The directory where the Tezos node will store all its data. Parent
       directories are created if necessary.

     --disable-config-validation
       Disable the node configuration validation.

     --force-history-mode-switch
       Forces the switch of history modes when a different history mode
       is found between the written configuration and the given history
       mode. Warning: this option will modify the storage irremediably.
       Please refer to the Tezos node documentation for more details.

     --history-mode=<mode>
       Set the mode for the chain's data history storage. Possible values
       are archive, full (default), full:N, rolling, rolling:N. Archive
       mode retains all data since the genesis block. Full mode only
       maintains block headers and operations allowing replaying the
       chain since the genesis if wanted. Rolling mode retains only the
       most recent data and deletes the rest. For both Full and Rolling
       modes, it is possible to adjust the number of cycles to preserve
       by using the :N annotation. The default number of preserved cycles
       is 1. The value experimental-rolling is deprecated but is
       equivalent to rolling which should be used instead.

     --log-coloring=VAL
       Enable or disable light coloring in default stdout logs. Coloring
       is enabled by default.

     --log-output=OUTPUT
       Log output. Either stdout, stderr, syslog:<facility> or a file
       path.

     --metadata-size-limit=<limit-in-bytes>
       Size limit (in bytes) for operation's metadata to be stored on
       disk. Default limit is 10000000 bytes. Use unlimited to disregard
       this limit.

     --metrics-addr=ADDR:PORT or :PORT (by default ADDR is localhost and
     PORT is 9932)
       Port on which to provide metrics over HTTP.

     --network=NETWORK
       Select which network to run. Possible values are: sandbox,
       mainnet, ghostnet. Default is mainnet. You can also specify custom
       networks by passing a path to a file containing the custom network
       configuration, or by passing a URL from which such a file can be
       downloaded. If you have a file named after a built-in network, you
       can prefix its name with './' so that the node treats it as a
       file. Otherwise it will be treated as a proper name of the
       built-in network. With commands other than 'config init',
       specifying this option causes the node to fail if the
       configuration implies another network.

     --sandbox=FILE.json
       Run the daemon in sandbox mode. P2P to non-localhost addresses are
       disabled, and constants of the economic protocol can be altered
       with a JSON file which overrides the genesis_parameters field of
       the network configuration (e.g. scripts/sandbox.json). IMPORTANT:
       Using sandbox mode affects the node state and subsequent runs of
       Tezos node must also use sandbox mode. In order to run the node in
       normal mode afterwards, a full reset must be performed (by
       removing the node's data directory).

     --singleprocess
       When enabled, it deactivates block validation using an external
       process. Thus, the validation procedure is done in the same
       process as the node and might not be responding when doing
       extensive I/Os.

     --target=<block_hash>,<level>
       When asked to take a block as a target, the daemon will only
       accept the chains that contains that block and those that might
       reach it.

     -v  Increase log level. Using -v is equivalent to using
       TEZOS_LOG='* -> info', and -vv is equivalent to using
       TEZOS_LOG='* -> debug'.

DEBUG
     The environment variable TEZOS_LOG is used to fine-tune what is going
     to be logged. The syntax is TEZOS_LOG='<section> -> <level> [ ;
     ...]' where section is one of block.validation
     brassaia.brassaia_pack.inode brassaia.brassaia_pack.unix.file_manager
     brassaia.brassaia_pack.unix.pack_store brassaia.object_graph
     brassaia.tree config crypto dal.cryptobox db_network_reader
     external_rpc-process-watchdog external_rpc-process-watchdog.process
     external_rpc-process-watchdog_hypervisor external_validation
     external_validator external_validator.process
     external_validator_hypervisor external_watchdog gc_setup
     internal_event_unix key_value_store lib_p2p.lib_p2p.p2p_maintenance
     lib_p2p.p2p.welcome lib_p2p.p2p_conn lib_p2p.p2p_discovery.answer
     lib_p2p.p2p_discovery.sender lib_p2p.p2p_io_scheduler.read
     lib_p2p.p2p_io_scheduler.write node node.config.validation
     node.context.disk node.context_brassaia.disk node.context_ops
     node.data_version node.distributed_db node.distributed_db.p2p_reader
     node.distributed_db.requester node.duo_context node.identity
     node.injection_directory node.main node.protocol node.protocol_store
     node.reconstruction node.requester.scheduler node.shutdown
     node.snapshots node.state node.storage_consistency node.store
     node.validator node.validator.bootstrap_pipeline p2p p2p-node p2p.conn
     p2p.connect_handler p2p.discovery p2p.fd p2p.io-scheduler
     p2p.maintenance p2p.pool p2p.protocol p2p.socket p2p.socket.reader
     p2p.socket.writer p2p.welcome prevalidator prevalidator_classification
     protocol_cache protocol_updater proxy.context rpc.process
     rpc_middleware rpc_server sequential_block_validator task validation
     validator.block validator.chain validator.peer and level is one of
     fatal, error, warn, notice, info or debug. A * can be used as a
     wildcard in sections, i.e. node* -> debug. The rules are matched
     left to right, therefore the leftmost rule is highest priority .

COMMON OPTIONS
     --help[=FMT] (default=auto)
       Show this help in format FMT. The value FMT must be one of auto,
       pager, groff or plain. With auto, the format is pager or plain
       whenever the TERM env var is dumb or undefined.

     --version
       Show version information.

EXIT STATUS
     octez-node run exits with:

     0   on success.

     123 on indiscriminate errors reported on standard error.

     124 on command line parsing errors.

     125 on unexpected internal errors (bugs).

ENVIRONMENT
     These environment variables affect the execution of octez-node run:

     TEZOS_NODE_DIR
       The directory where the Tezos node will store all its data. Parent
       directories are created if necessary.

EXAMPLES
     Run in sandbox mode listening to RPC commands at localhost port 8732
       octez-node run
       --sandbox=src/proto_alpha/parameters/sandbox-parameters.json
       --data-dir /custom/data/dir --rpc-addr localhost:8732

     Run a node that accepts network connections
       octez-node run

BUGS
     Check bug reports at https://gitlab.com/tezos/tezos/issues.

SEE ALSO
     octez-node(1)
|}

let binary_help_parses_options () =
  let open Help_parser in
  let opts : option_entry list =
    Binary_help_explorer.For_tests.parse_help octez_node_run_help_plain
  in
  (* Parser stops at ENVIRONMENT section, so we get 47 options instead of 49 *)
  Alcotest.(check int) "option count" 47 (List.length opts) ;
  let find name =
    match List.find_opt (fun o -> List.exists (( = ) name) o.names) opts with
    | Some o -> o
    | None -> Alcotest.failf "option %s not found" name
  in
  let advertised = find "--advertised-net-port" in
  Alcotest.(check (option string)) "advertised arg" (Some "PORT") advertised.arg ;
  Alcotest.(check bool)
    "advertised doc"
    true
    (string_contains ~needle:"instance can be reached" advertised.doc) ;
  Alcotest.(check bool)
    "advertised kind port"
    true
    (Binary_help_explorer.For_tests.arg_kind_to_string advertised.kind = "port") ;
  let rpc_addr = find "--rpc-addr" in
  Alcotest.(check bool)
    "rpc doc"
    true
    (string_contains
       ~needle:"The URL at which this RPC server instance can be reached"
       rpc_addr.doc) ;
  Alcotest.(check bool)
    "rpc kind addr:port"
    true
    (Binary_help_explorer.For_tests.arg_kind_to_string rpc_addr.kind
    = "addr_port") ;
  let data_dir = find "--data-dir" in
  Alcotest.(check bool)
    "data dir alias captured"
    true
    (List.exists (( = ) "-d") data_dir.names) ;
  Alcotest.(check bool)
    "data dir kind dir"
    true
    (Binary_help_explorer.For_tests.arg_kind_to_string data_dir.kind = "dir") ;
  let history_mode = find "--history-mode" in
  Alcotest.(check bool)
    "history mode doc"
    true
    (string_contains
       ~needle:"Set the mode for the chain's data history storage"
       history_mode.doc) ;
  Alcotest.(check bool)
    "history mode kind text"
    true
    (Binary_help_explorer.For_tests.arg_kind_to_string history_mode.kind
    = "text")

let baker_local_help =
  {|
Usage:
  octez-baker [global options] command [command options]

Global options (must come before the command):
  -d --base-dir <path>: client data directory (absent: TEZOS_CLIENT_DIR env)
  -E --endpoint <uri>: HTTP(S) endpoint of the node RPC interface; e.g. 'http://localhost:8732'
  --chain <hash|tag>: chain on which to apply contextual commands
  -R --remote-signer <uri>: URI of the remote signer

Commands related to the agnostic baker daemon.:

  run with local node <node_data_path> [<baker>...]
    Launch the baker daemon
    -P --pidfile <filename>: write process id in file
    --node-version-check-bypass: If node-version-check-bypass flag is set
    --node-version-allowed <<product>-[v]<major>.<minor>[.0]<info>[:<commit>]>
    --minimal-fees <amount>: exclude operations with fees lower than this threshold (in tez)
    --liquidity-baking-toggle-vote <vote>: Vote to continue or end the liquidity baking subsidy.
    --adaptive-issuance-vote <vote>: Vote to adopt or not the adaptive issuance feature.
    --operations-pool <file|uri>: When specified, the baker will try to fetch operations from this file (or uri)
    --dal-node <uri>: endpoint of the DAL node, e.g. 'http://localhost:8933'
|}

let baker_remote_help =
  {|
Usage:
  octez-baker [global options] command [command options]

Global options (must come before the command):
  -d --base-dir <path>: client data directory (absent: TEZOS_CLIENT_DIR env)
  -E --endpoint <uri>: HTTP(S) endpoint of the node RPC interface; e.g. 'http://localhost:8732'

Commands related to the agnostic baker daemon.:

  run remotely [<baker>...]
    Launch the baker daemon using RPCs only.
    --node-version-check-bypass: If node-version-check-bypass flag is set
    --node-version-allowed <<product>-[v]<major>.<minor>[.0]<info>[:<commit>]>
    --minimal-fees <amount>: exclude operations with fees lower than this threshold (in tez)
    --operations-pool <file|uri>: When specified, the baker will try to fetch operations from this file (or uri)
    --dal-node <uri>: endpoint of the DAL node, e.g. 'http://localhost:8933'
    --without-dal: If without-dal flag is set, the daemon will not try to connect to a DAL node.
|}

let baker_help_parses_local () =
  let open Help_parser in
  let opts : option_entry list =
    Binary_help_explorer.For_tests.parse_baker_help baker_local_help
  in
  Alcotest.(check bool) "has options" true (List.length opts > 5) ;
  let find name =
    List.find_opt (fun o -> List.exists (( = ) name) o.names) opts |> Option.get
  in
  let base_dir = find "--base-dir" in
  Alcotest.(check bool)
    "base-dir kind dir"
    true
    (Binary_help_explorer.For_tests.arg_kind_to_string base_dir.kind = "dir") ;
  let dal_node = find "--dal-node" in
  Alcotest.(check bool)
    "dal-node kind addr/text"
    true
    (List.mem
       (Binary_help_explorer.For_tests.arg_kind_to_string dal_node.kind)
       ["addr"; "addr_port"; "text"]) ;
  let ops_pool = find "--operations-pool" in
  Alcotest.(check bool)
    "operations-pool arg"
    true
    (ops_pool.arg = Some "<file|uri>") ;
  Alcotest.(check bool)
    "operations-pool doc"
    true
    (string_contains ~needle:"fetch operations from this file" ops_pool.doc)

let baker_help_parses_remote () =
  let open Help_parser in
  let opts : option_entry list =
    Binary_help_explorer.For_tests.parse_baker_help baker_remote_help
  in
  Alcotest.(check bool) "has options" true (List.length opts > 5) ;
  let has name = List.exists (fun o -> List.exists (( = ) name) o.names) opts in
  Alcotest.(check bool) "has base-dir" true (has "--base-dir") ;
  Alcotest.(check bool) "has endpoint" true (has "--endpoint") ;
  Alcotest.(check bool) "has without-dal" true (has "--without-dal")

let parse_help_skips_dividers () =
  let open Help_parser in
  let sample =
    {|
------
  -a --alpha <foo>: first
------
  -b --bravo: second
|}
  in
  let opts : option_entry list =
    Binary_help_explorer.For_tests.parse_help sample
  in
  Alcotest.(check int) "only options" 2 (List.length opts) ;
  let names = List.map (fun o -> List.hd o.names) opts in
  Alcotest.(check (list string)) "names" ["-a"; "-b"] names

let service_json_roundtrip () =
  let logging_mode = Logging_mode.default in
  let service = sample_service ~logging_mode () in
  match Service.to_yojson service |> Service.of_yojson with
  | Ok decoded -> check_service service decoded
  | Error (`Msg msg) -> Alcotest.failf "service json roundtrip: %s" msg

let service_json_invalid_logging () =
  let json =
    `Assoc
      [
        ("instance", `String "alpha");
        ("role", `String "node");
        ("network", `String "https://example");
        ("history_mode", `String "rolling");
        ("data_dir", `String "/tmp/data");
        ("rpc_addr", `String "127.0.0.1:8732");
        ("net_addr", `String "0.0.0.0:9732");
        ("service_user", `String "octez");
        ("app_bin_dir", `String "/opt/octez");
        ("created_at", `String "2024");
        ("logging_mode", `Assoc [("type", `String "unknown")]);
      ]
  in
  match Service.of_yojson json with
  | Ok _ -> Alcotest.fail "invalid logging mode should fail"
  | Error _ -> ()

let snapshots_slug_of_network () =
  let cases =
    [
      ("mainnet", Some "mainnet");
      ("  Ghostnet  ", Some "ghostnet");
      ("https://snapshots.tzinit.org/networks/Seoulnet.json", Some "seoulnet");
      ("", None);
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.check
        option_string
        "slug_of_network"
        expected
        (Snapshots.slug_of_network input))
    cases

let snapshots_sanitize_kind () =
  let cases =
    [
      (" full:50  ", Some "full50");
      ("ROLLING", Some "rolling");
      ("", None);
      ("   ", None);
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.check
        option_string
        "sanitize_kind"
        expected
        (Snapshots.sanitize_kind_input input))
    cases

let sh_quote_tests () =
  Alcotest.(check string)
    "needs quoting"
    "'hello world'"
    (Common.sh_quote "hello world") ;
  Alcotest.(check string) "no quoting" "simple" (Common.sh_quote "simple") ;
  Alcotest.(check string) "apostrophe" "'a'\"'\"'b'" (Common.sh_quote "a'b")

let cmd_to_string_tests () =
  let argv = ["echo"; "hello world"; "a'b"] in
  Alcotest.(check string)
    "cmd_to_string"
    "echo 'hello world' 'a'\"'\"'b'"
    (Common.cmd_to_string argv)

let shellwords_parser_tests () =
  let module FB = Octez_manager_ui.Form_builder_common in
  let test_case name input expected =
    Alcotest.(check list_string) name expected (FB.prepare_extra_args input)
  in
  (* Basic cases *)
  test_case "empty string" "" [] ;
  test_case "whitespace only" "   \t  \n  " [] ;
  test_case "simple words" "foo bar baz" ["foo"; "bar"; "baz"] ;
  test_case "multiple spaces" "foo  bar    baz" ["foo"; "bar"; "baz"] ;

  (* Single quotes *)
  test_case
    "single quotes preserve spaces"
    "foo 'bar baz' qux"
    ["foo"; "bar baz"; "qux"] ;
  test_case
    "single quotes preserve everything literally"
    "'hello world' 'foo\"bar'"
    ["hello world"; "foo\"bar"] ;

  (* Double quotes *)
  test_case
    "double quotes preserve spaces"
    "foo \"bar baz\" qux"
    ["foo"; "bar baz"; "qux"] ;
  test_case "double quotes allow escaping" "\"hello\\\"world\"" ["hello\"world"] ;

  (* Backslash escaping *)
  test_case "escape space outside quotes" "foo\\ bar" ["foo bar"] ;
  test_case "escape quote" "foo\\\"bar" ["foo\"bar"] ;
  test_case "backslash in single quotes is literal" "'foo\\bar'" ["foo\\bar"] ;

  (* Mixed quotes *)
  test_case
    "mixed single and double"
    "foo 'bar baz' \"qux quux\" end"
    ["foo"; "bar baz"; "qux quux"; "end"] ;

  (* Real-world examples *)
  test_case
    "typical CLI args"
    "--endpoint http://localhost:8732 --log-level info"
    ["--endpoint"; "http://localhost:8732"; "--log-level"; "info"] ;
  test_case
    "path with spaces"
    "--data-dir \"/home/user/my data/node\""
    ["--data-dir"; "/home/user/my data/node"] ;
  test_case
    "complex flag"
    "--option=\"value with spaces\" --flag"
    ["--option=value with spaces"; "--flag"] ;
  test_case
    "quoted URL"
    "--peer 'http://example.com:9732' --bootstrap-threshold 1"
    ["--peer"; "http://example.com:9732"; "--bootstrap-threshold"; "1"]

(** Form Builder Common Tests *)

let form_builder_is_nonempty_tests () =
  let module FB = Octez_manager_ui.Form_builder_common in
  Alcotest.(check bool) "empty string" false (FB.is_nonempty "") ;
  Alcotest.(check bool) "whitespace only" false (FB.is_nonempty "   \t  ") ;
  Alcotest.(check bool) "non-empty" true (FB.is_nonempty "hello") ;
  Alcotest.(check bool) "spaces around" true (FB.is_nonempty "  hello  ")

let form_builder_normalize_tests () =
  let module FB = Octez_manager_ui.Form_builder_common in
  Alcotest.(check string) "lowercase" "hello" (FB.normalize "HELLO") ;
  Alcotest.(check string) "trim" "hello" (FB.normalize "  hello  ") ;
  Alcotest.(check string) "both" "hello world" (FB.normalize "  HELLO World  ")

let form_builder_parse_host_port_tests () =
  let module FB = Octez_manager_ui.Form_builder_common in
  let option_pair = Alcotest.(option (pair string int)) in

  (* Valid cases *)
  Alcotest.(check option_pair)
    "valid host:port"
    (Some ("127.0.0.1", 8732))
    (FB.parse_host_port "127.0.0.1:8732") ;

  Alcotest.(check option_pair)
    "with spaces"
    (Some ("localhost", 9732))
    (FB.parse_host_port " localhost : 9732 ") ;

  Alcotest.(check option_pair)
    "hostname"
    (Some ("example.com", 443))
    (FB.parse_host_port "example.com:443") ;

  (* Invalid cases *)
  Alcotest.(check option_pair) "no port" None (FB.parse_host_port "localhost") ;

  Alcotest.(check option_pair) "no host" None (FB.parse_host_port ":8732") ;

  Alcotest.(check option_pair) "empty host" None (FB.parse_host_port "  :8732") ;

  Alcotest.(check option_pair)
    "invalid port"
    None
    (FB.parse_host_port "localhost:abc") ;

  Alcotest.(check option_pair)
    "port too high"
    None
    (FB.parse_host_port "localhost:99999") ;

  Alcotest.(check option_pair)
    "port zero"
    None
    (FB.parse_host_port "localhost:0") ;

  Alcotest.(check option_pair)
    "negative port"
    None
    (FB.parse_host_port "localhost:-1") ;

  Alcotest.(check option_pair)
    "multiple colons"
    None
    (FB.parse_host_port "host:123:456")

let form_builder_prepare_extra_args_integration () =
  let module FB = Octez_manager_ui.Form_builder_common in
  (* Test that prepare_extra_args uses shellwords parser correctly *)
  Alcotest.(check list_string)
    "uses shellwords parser"
    ["--option"; "value with spaces"]
    (FB.prepare_extra_args "--option \"value with spaces\"") ;

  Alcotest.(check list_string) "handles empty" [] (FB.prepare_extra_args "") ;

  Alcotest.(check list_string)
    "trims whitespace"
    ["--flag"]
    (FB.prepare_extra_args "  --flag  ")

let parse_initial_args_tests () =
  let module BH = Octez_manager_ui.Binary_help_explorer.For_tests in
  let flag_value_pair =
    Alcotest.testable
      (fun ppf (flag, value) ->
        match value with
        | None -> Format.fprintf ppf "(%s, None)" flag
        | Some v -> Format.fprintf ppf "(%s, Some %s)" flag v)
      ( = )
  in
  let list_pairs = Alcotest.list flag_value_pair in
  (* Single flag without value *)
  Alcotest.(check list_pairs)
    "single flag"
    [("--flag", None)]
    (BH.parse_initial_args "--flag") ;
  (* Flag with value using = *)
  Alcotest.(check list_pairs)
    "flag=value"
    [("--flag", Some "value")]
    (BH.parse_initial_args "--flag=value") ;
  (* Flag with value as next token *)
  Alcotest.(check list_pairs)
    "flag value"
    [("--flag", Some "value")]
    (BH.parse_initial_args "--flag value") ;
  (* Multiple flags *)
  Alcotest.(check list_pairs)
    "multiple flags"
    [("--flag1", None); ("--flag2", Some "val")]
    (BH.parse_initial_args "--flag1 --flag2=val") ;
  (* Empty string *)
  Alcotest.(check list_pairs) "empty" [] (BH.parse_initial_args "") ;
  (* Flag with quoted value *)
  Alcotest.(check list_pairs)
    "quoted value"
    [("--msg", Some "hello world")]
    (BH.parse_initial_args "--msg 'hello world'")

let home_dir_fallback () =
  let pw = Unix.getpwuid (Unix.geteuid ()) in
  with_env
    [("HOME", Some "")]
    (fun () ->
      Alcotest.(check string)
        "home_dir fallback"
        pw.Unix.pw_dir
        (Common.home_dir ()))

let xdg_config_custom () =
  with_temp_dir (fun base ->
      let cfg = Filename.concat base "cfg" in
      Unix.mkdir cfg 0o755 ;
      with_env
        [("XDG_CONFIG_HOME", Some cfg)]
        (fun () ->
          Alcotest.(check string) "xdg config" cfg (Common.xdg_config_home ()) ;
          let expected = Filename.concat cfg "octez/instances" in
          let actual = Common.env_instances_base_dir () in
          Alcotest.(check string) "instances dir" expected actual))

let default_data_dir_custom () =
  with_temp_dir (fun base ->
      let data = Filename.concat base "data" in
      Unix.mkdir data 0o755 ;
      with_env
        [("XDG_DATA_HOME", Some data)]
        (fun () ->
          let expected = Filename.concat data "octez/foo" in
          Alcotest.(check string)
            "default data dir"
            expected
            (Common.default_data_dir "foo")))

let default_role_dir_custom () =
  with_temp_dir (fun base ->
      let data = Filename.concat base "data" in
      Unix.mkdir data 0o755 ;
      with_env
        [("XDG_DATA_HOME", Some data)]
        (fun () ->
          let expected = Filename.concat data "octez/baker-alpha" in
          Alcotest.(check string)
            "default role dir"
            expected
            (Common.default_role_dir "Baker" "alpha")))

let default_role_dir_no_duplicate_prefix () =
  with_temp_dir (fun base ->
      let data = Filename.concat base "data" in
      Unix.mkdir data 0o755 ;
      with_env
        [("XDG_DATA_HOME", Some data)]
        (fun () ->
          (* Test node instance that already has "node-" prefix *)
          let expected_node = Filename.concat data "octez/node-shadownet" in
          Alcotest.(check string)
            "node with existing prefix"
            expected_node
            (Common.default_role_dir "node" "node-shadownet") ;
          (* Test baker instance that already has "baker-" prefix *)
          let expected_baker =
            Filename.concat data "octez/baker-node-shadownet"
          in
          Alcotest.(check string)
            "baker with existing prefix"
            expected_baker
            (Common.default_role_dir "baker" "baker-node-shadownet") ;
          (* Test accuser instance that already has "accuser-" prefix *)
          let expected_accuser =
            Filename.concat data "octez/accuser-node-mainnet"
          in
          Alcotest.(check string)
            "accuser with existing prefix"
            expected_accuser
            (Common.default_role_dir "accuser" "accuser-node-mainnet") ;
          (* Test dal-node with default name (no prefix duplication expected) *)
          let expected_dal = Filename.concat data "octez/dal-node-dal" in
          Alcotest.(check string)
            "dal-node with default name"
            expected_dal
            (Common.default_role_dir "dal-node" "dal")))

let ensure_dir_path_creates () =
  let owner, group = current_user_group () in
  with_temp_dir (fun base ->
      let nested = Filename.concat base "a/b/c" in
      match Common.ensure_dir_path ~owner ~group ~mode:0o700 nested with
      | Ok () ->
          Alcotest.(check bool) "dir exists" true (Sys.is_directory nested)
      | Error (`Msg msg) -> Alcotest.failf "ensure_dir_path error: %s" msg)

let ensure_dir_path_missing_owner () =
  with_temp_dir (fun base ->
      let nested = Filename.concat base "missing" in
      match
        Common.ensure_dir_path
          ~owner:"definitely_missing_user"
          ~group:"definitely_missing_group"
          ~mode:0o755
          nested
      with
      | Ok () ->
          Alcotest.(check bool) "dir exists" true (Sys.is_directory nested)
      | Error (`Msg msg) ->
          Alcotest.failf "ensure_dir_path missing owner: %s" msg)

let write_file_creates_contents () =
  let owner, group = current_user_group () in
  with_temp_dir (fun base ->
      let path = Filename.concat base "nested/file.txt" in
      let () =
        expect_ok (Common.write_file ~mode:0o600 ~owner ~group path "payload")
      in
      Alcotest.(check string) "file contents" "payload" (read_file path))

let remove_tree_file () =
  with_temp_dir (fun base ->
      let file = Filename.concat base "file.txt" in
      let oc = open_out file in
      output_string oc "data" ;
      close_out oc ;
      let () = expect_ok (Common.remove_tree file) in
      Alcotest.(check bool) "file removed" false (Sys.file_exists file))

let remove_tree_directory () =
  with_temp_dir (fun base ->
      let dir = Filename.concat base "dir" in
      Unix.mkdir dir 0o755 ;
      let nested = Filename.concat dir "inner" in
      Unix.mkdir nested 0o755 ;
      let file = Filename.concat nested "value" in
      let oc = open_out file in
      output_string oc "data" ;
      close_out oc ;
      let () = expect_ok (Common.remove_tree dir) in
      Alcotest.(check bool) "dir removed" false (Sys.file_exists dir))

let remove_tree_missing () =
  with_temp_dir (fun base ->
      let missing = Filename.concat base "missing" in
      let () = expect_ok (Common.remove_tree missing) in
      Alcotest.(check bool) "still missing" false (Sys.file_exists missing))

let common_run_helpers () =
  (match Common.run ["/bin/sh"; "-c"; "exit 0"] with
  | Ok () -> ()
  | Error (`Msg msg) -> Alcotest.failf "run success error: %s" msg) ;
  (match Common.run ["/bin/sh"; "-c"; "exit 1"] with
  | Ok () -> Alcotest.fail "run should have failed"
  | Error _ -> ()) ;
  match Common.run_out ["/bin/sh"; "-c"; "printf foo"] with
  | Ok out -> Alcotest.(check string) "run_out" "foo" out
  | Error (`Msg msg) -> Alcotest.failf "run_out error: %s" msg

let common_run_as_self () =
  let user, _ = current_user_group () in
  match Common.run_as ~user ["/bin/sh"; "-c"; "exit 0"] with
  | Ok () -> ()
  | Error (`Msg msg) -> Alcotest.failf "run_as error: %s" msg

let kill_active_download_noop () =
  (* Calling kill_active_download when no download is active should be safe *)
  Common.kill_active_download () ;
  (* Call twice to ensure idempotency *)
  Common.kill_active_download () ;
  ()

let snapshot_root = snapshot_base ^ "/"

let snapshots_fetch_entry_success () =
  let html =
    metadata_html
      [
        ("History mode", "Rolling");
        ("HTTPS", "https://example/rolling?x=1&amp;y=2");
        ("Extra", "value");
      ]
  in
  let fetch url =
    Alcotest.(check string) "entry url" (snapshot_url "seoulnet" "rolling") url ;
    Ok (200, html)
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match
        Snapshots.fetch_entry
          ~network_slug:"seoulnet"
          ~slug:"rolling"
          ~label:"  "
      with
      | Ok (Some entry) ->
          let {
            Snapshots.label;
            Snapshots.history_mode;
            Snapshots.download_url;
            Snapshots.metadata;
            _;
          } =
            entry
          in
          Alcotest.(check string) "label" "rolling" label ;
          Alcotest.(check option_string)
            "history mode"
            (Some "Rolling")
            history_mode ;
          Alcotest.(check option_string)
            "download url"
            (Some "https://example/rolling?x=1&y=2")
            download_url ;
          Alcotest.(check int) "metadata length" 3 (List.length metadata)
      | Ok None -> Alcotest.fail "expected snapshot entry"
      | Error (`Msg msg) -> Alcotest.failf "fetch_entry error: %s" msg)

let snapshots_fetch_entry_missing_metadata () =
  let fetch _ = Ok (200, "<html>No metadata</html>") in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match
        Snapshots.fetch_entry
          ~network_slug:"seoulnet"
          ~slug:"rolling"
          ~label:"rolling"
      with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "missing metadata should fail")

let snapshots_fetch_entry_not_found () =
  let fetch _ = Ok (404, "missing") in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match
        Snapshots.fetch_entry
          ~network_slug:"seoulnet"
          ~slug:"rolling"
          ~label:"rolling"
      with
      | Ok None -> ()
      | _ -> Alcotest.fail "404 should return None")

let snapshots_list_from_root () =
  let root_html =
    {|
    <div>
      <a href="/seoulnet/rolling.html">Rolling latest</a>
      <a href="/seoulnet/rolling.html">Duplicate</a>
      <a href="/seoulnet/full50.html">Full Fifty</a>
    </div>
    |}
  in
  let responses =
    [
      (snapshot_root, Ok (200, root_html));
      ( snapshot_url "seoulnet" "rolling",
        Ok
          ( 200,
            metadata_html
              [
                ("History mode", "Rolling");
                ("HTTPS", "https://example/rolling.snap");
              ] ) );
      ( snapshot_url "seoulnet" "full50",
        Ok
          ( 200,
            metadata_html
              [("History mode", "Full"); ("HTTPS", "https://example/full.snap")]
          ) );
    ]
  in
  let fetch url =
    match List.assoc_opt url responses with
    | Some v -> v
    | None -> Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match Snapshots.list ~network_slug:"seoulnet" with
      | Ok (entries : Snapshots.entry list) ->
          Alcotest.(check int) "two entries" 2 (List.length entries) ;
          let labels =
            List.map
              (fun (entry : Snapshots.entry) ->
                let {Snapshots.slug; Snapshots.label; _} = entry in
                (slug, label))
              entries
          in
          Alcotest.(check list_pairs)
            "labels preserved"
            [("rolling", "Rolling latest"); ("full50", "Full Fifty")]
            labels
      | Error (`Msg msg) -> Alcotest.failf "list error: %s" msg)

let snapshots_list_fallback () =
  let body =
    metadata_html
      ~with_code:true
      [("History mode", "Rolling"); ("HTTPS", "https://example/offline.snap")]
  in
  let fetch url =
    if String.equal url snapshot_root then Error (`Msg "offline")
    else if String.equal url (snapshot_url "seoulnet" "rolling") then
      Ok (200, body)
    else Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match Snapshots.list ~network_slug:"seoulnet" with
      | Ok ([entry] : Snapshots.entry list) ->
          let {Snapshots.slug; Snapshots.history_mode; _} = entry in
          Alcotest.(check string) "slug" "rolling" slug ;
          Alcotest.(check option_string)
            "history mode"
            (Some "Rolling")
            history_mode
      | Ok _ -> Alcotest.fail "expected single fallback entry"
      | Error (`Msg msg) -> Alcotest.failf "list fallback error: %s" msg)

let snapshots_list_missing_entries_error () =
  let html = "<a href=\"/ghostnet/rolling.html\">Rolling</a>" in
  let fetch url =
    if String.equal url snapshot_root then Ok (200, html)
    else Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match Snapshots.list ~network_slug:"ghostnet" with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "expected missing entry error")

let snapshots_list_disappeared_entry_error () =
  let root_html = "<a href=\"/seoulnet/rolling.html\">Rolling</a>" in
  let fetch url =
    if String.equal url snapshot_root then Ok (200, root_html)
    else Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match Snapshots.list ~network_slug:"seoulnet" with
      | Error (`Msg msg) ->
          if String.starts_with ~prefix:"Snapshot 'rolling' disappeared" msg
          then ()
          else Alcotest.failf "unexpected message: %s" msg
      | Ok _ -> Alcotest.fail "expected disappeared entry error")

let snapshots_list_no_advertised () =
  let fetch url =
    if String.equal url snapshot_root then Error (`Msg "offline root")
    else Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match Snapshots.list ~network_slug:"seoulnet" with
      | Error (`Msg msg) ->
          if
            String.starts_with
              ~prefix:"No snapshots advertised for network 'seoulnet'"
              msg
          then ()
          else Alcotest.failf "unexpected message: %s" msg
      | Ok _ -> Alcotest.fail "expected no snapshots error")

let snapshots_fetch_html_fallback_to_curl () =
  let eio_called = ref 0 in
  let curl_called = ref 0 in
  let result =
    Snapshots.For_tests.fetch_html_with
      ~try_eio:(fun () ->
        incr eio_called ;
        Error (`Msg "io_uring unavailable"))
      ~try_curl:(fun () ->
        incr curl_called ;
        Ok (200, "curl-body"))
  in
  Alcotest.(check int) "eio attempted" 1 !eio_called ;
  Alcotest.(check int) "curl used" 1 !curl_called ;
  match result with
  | Ok (200, body) -> Alcotest.(check string) "curl body" "curl-body" body
  | _ -> Alcotest.fail "expected curl fallback success"

let snapshots_fetch_html_prefers_eio () =
  let curl_called = ref 0 in
  let result =
    Snapshots.For_tests.fetch_html_with
      ~try_eio:(fun () -> Ok (200, "eio-body"))
      ~try_curl:(fun () ->
        incr curl_called ;
        Ok (503, "curl-body"))
  in
  Alcotest.(check int) "no curl when eio succeeds" 0 !curl_called ;
  match result with
  | Ok (200, body) -> Alcotest.(check string) "body" "eio-body" body
  | _ -> Alcotest.fail "expected eio result"

let teztnets_parse_pairs_basic () =
  let json =
    "{\n\
    \      \"nets\": [\n\
    \        {\"humanName\": \"Seoulnet\", \"networkJsonUrl\": \
     \"https://example/seo.json\"},\n\
    \        {\"slug\": \"custom\", \"networkURL\": \
     \"https://example/custom.json\"}\n\
    \      ]\n\
    \    }"
  in
  match Teztnets.parse_networks json with
  | Ok infos ->
      let pairs =
        List.map
          (fun (n : Teztnets.network_info) -> (n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs)
        "parsed pairs"
        [
          ("Seoulnet", "https://example/seo.json");
          ("unknown", "https://example/custom.json");
        ]
        pairs
  | Error (`Msg msg) -> Alcotest.failf "parse_networks error: %s" msg

let teztnets_parse_pairs_assoc_keys () =
  let json =
    "{\n\
    \  \"custom\": {\"human_name\": \"Custom\", \"config_url\": \
     \"https://example/custom.json\"},\n\
    \  \"ghost\": {\"humanName\": \"Ghostnet\"}\n\
     }"
  in
  match Teztnets.parse_networks json with
  | Ok infos ->
      let pairs =
        List.map
          (fun (n : Teztnets.network_info) -> (n.human_name, n.network_url))
          infos
      in
      let expected =
        [("Custom", "https://example/custom.json"); ("Ghostnet", "ghostnet")]
      in
      Alcotest.(check list_pairs)
        "assoc pairs"
        (sort_pairs expected)
        (sort_pairs pairs)
  | Error (`Msg msg) -> Alcotest.failf "parse_networks assoc error: %s" msg

let teztnets_parse_pairs_error () =
  match Teztnets.parse_networks "[]" with
  | Ok _ -> Alcotest.fail "expected failure on empty teztnets list"
  | Error _ -> ()

let teztnets_parse_pairs_mainnet_without_url () =
  let json = "{\n  \"nets\": [\n    {\"humanName\": \"Mainnet\"}\n  ]\n}" in
  match Teztnets.parse_networks json with
  | Ok infos ->
      let pairs =
        List.map
          (fun (n : Teztnets.network_info) -> (n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs)
        "mainnet fallback"
        [("Mainnet", "mainnet")]
        pairs
  | Error (`Msg msg) -> Alcotest.failf "parse mainnet fallback error: %s" msg

let teztnets_list_networks_custom_fetch () =
  let json =
    "{\n\
    \  \"nets\": [\n\
    \    {\"humanName\": \"Seoulnet\", \"networkJsonUrl\": \
     \"https://example/seoul.json\"},\n\
    \    {\"slug\": \"ghostnet\"}\n\
    \  ]\n\
     }"
  in
  match Teztnets.list_networks ~fetch:(fun () -> Ok json) () with
  | Ok infos ->
      let pairs =
        List.map
          (fun (n : Teztnets.network_info) -> (n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs)
        "list networks"
        [("Seoulnet", "https://example/seoul.json"); ("unknown", "ghostnet")]
        pairs
  | Error (`Msg msg) -> Alcotest.failf "list_networks error: %s" msg

let teztnets_list_networks_fallback_fetch () =
  match Teztnets.list_networks ~fetch:(fun () -> Error (`Msg "boom")) () with
  | Ok infos ->
      let pairs =
        List.map
          (fun (n : Teztnets.network_info) -> (n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs) "fallback" Teztnets.fallback_pairs pairs
  | Error (`Msg msg) -> Alcotest.failf "fallback error: %s" msg

let teztnets_list_networks_empty_json_fallback () =
  match Teztnets.list_networks ~fetch:(fun () -> Ok "{}") () with
  | Ok infos ->
      let pairs =
        List.map
          (fun (n : Teztnets.network_info) -> (n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs) "fallback" Teztnets.fallback_pairs pairs
  | Error (`Msg msg) -> Alcotest.failf "empty json fallback error: %s" msg

let teztnets_parse_top_level_list () =
  let json =
    "[\n\
     {\"name\": \"Alpha\", \"network_config_url\": \"https://example/a.json\", \
     \"description\": \"Desc\"},\n\
     {\"slug\": \"beta\", \"networkURL\": \"https://example/b.json\", \
     \"category\": \"Test\"}\n\
     ]"
  in
  match Teztnets.parse_networks json with
  | Ok infos ->
      let pairs =
        List.map
          (fun (n : Teztnets.network_info) -> (n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs)
        "top-level list"
        [
          ("Alpha", "https://example/a.json");
          ("unknown", "https://example/b.json");
        ]
        pairs
  | Error (`Msg msg) -> Alcotest.failf "parse top-level list error: %s" msg

let teztnets_fetch_json_fallback_to_curl () =
  let eio_called = ref 0 in
  let curl_called = ref 0 in
  let result =
    Teztnets.For_tests.fetch_json_with
      ~via_eio:(fun () ->
        incr eio_called ;
        Error (`Msg "eio failed"))
      ~via_curl:(fun () ->
        incr curl_called ;
        Ok "{\"nets\":[]}")
  in
  Alcotest.(check int) "eio attempt" 1 !eio_called ;
  Alcotest.(check int) "curl attempt" 1 !curl_called ;
  match result with
  | Ok body -> Alcotest.(check string) "body" "{\"nets\":[]}" body
  | Error (`Msg msg) -> Alcotest.failf "expected curl success: %s" msg

let teztnets_fetch_json_prefers_eio () =
  let curl_called = ref 0 in
  let result =
    Teztnets.For_tests.fetch_json_with
      ~via_eio:(fun () -> Ok "{\"nets\":[1]}")
      ~via_curl:(fun () ->
        incr curl_called ;
        Ok "{}")
  in
  Alcotest.(check int) "no curl when eio ok" 0 !curl_called ;
  match result with
  | Ok body -> Alcotest.(check string) "body" "{\"nets\":[1]}" body
  | Error (`Msg msg) -> Alcotest.failf "expected eio body: %s" msg

let teztnets_resolve_network_builtin () =
  let fetch () = Error (`Msg "should not fetch") in
  match Teztnets.resolve_network_for_octez_node ~fetch "mainnet" with
  | Ok net -> Alcotest.(check string) "mainnet builtin" "mainnet" net
  | Error (`Msg msg) -> Alcotest.failf "mainnet should be builtin: %s" msg

let teztnets_resolve_network_builtin_ghostnet () =
  let fetch () = Error (`Msg "should not fetch") in
  match Teztnets.resolve_network_for_octez_node ~fetch "ghostnet" with
  | Ok net -> Alcotest.(check string) "ghostnet builtin" "ghostnet" net
  | Error (`Msg msg) -> Alcotest.failf "ghostnet should be builtin: %s" msg

let teztnets_resolve_network_builtin_case_insensitive () =
  let fetch () = Error (`Msg "should not fetch") in
  match Teztnets.resolve_network_for_octez_node ~fetch "Mainnet" with
  | Ok net -> Alcotest.(check string) "Mainnet lowercased" "mainnet" net
  | Error (`Msg msg) -> Alcotest.failf "Mainnet should be lowercased: %s" msg

let teztnets_resolve_network_url_passthrough () =
  let fetch () = Error (`Msg "should not fetch") in
  let url = "https://teztnets.com/seoulnet" in
  match Teztnets.resolve_network_for_octez_node ~fetch url with
  | Ok net -> Alcotest.(check string) "URL passthrough" url net
  | Error (`Msg msg) -> Alcotest.failf "URL should pass through: %s" msg

let teztnets_resolve_network_alias_lowercase () =
  let fetch () =
    let json =
      "{\n\
      \  \"nets\": [\n\
      \    {\"humanName\": \"Seoulnet\", \"networkJsonUrl\": \
       \"https://teztnets.com/seoulnet\"},\n\
      \    {\"slug\": \"ghostnet\"}\n\
      \  ]\n\
       }"
    in
    Teztnets.list_networks ~fetch:(fun () -> Ok json) ()
  in
  match Teztnets.resolve_network_for_octez_node ~fetch "seoulnet" with
  | Ok net ->
      Alcotest.(check string)
        "seoulnet resolved"
        "https://teztnets.com/seoulnet"
        net
  | Error (`Msg msg) -> Alcotest.failf "seoulnet should resolve: %s" msg

let teztnets_resolve_network_alias_mixed_case () =
  let fetch () =
    let json =
      "{\n\
      \  \"nets\": [\n\
      \    {\"humanName\": \"Seoulnet\", \"networkJsonUrl\": \
       \"https://teztnets.com/seoulnet\"},\n\
      \    {\"slug\": \"ghostnet\"}\n\
      \  ]\n\
       }"
    in
    Teztnets.list_networks ~fetch:(fun () -> Ok json) ()
  in
  match Teztnets.resolve_network_for_octez_node ~fetch "Seoulnet" with
  | Ok net ->
      Alcotest.(check string)
        "Seoulnet resolved"
        "https://teztnets.com/seoulnet"
        net
  | Error (`Msg msg) -> Alcotest.failf "Seoulnet should resolve: %s" msg

let teztnets_resolve_network_alias_not_found () =
  let fetch () =
    let json = "{\"nets\": [{\"slug\": \"ghostnet\"}]}" in
    Teztnets.list_networks ~fetch:(fun () -> Ok json) ()
  in
  match Teztnets.resolve_network_for_octez_node ~fetch "unknownnet" with
  | Ok _net -> Alcotest.fail "unknownnet should not resolve"
  | Error (`Msg msg) ->
      Alcotest.(check bool)
        "error mentions unknownnet"
        true
        (string_contains ~needle:"unknownnet" msg)

let teztnets_resolve_network_empty_string () =
  let fetch () = Error (`Msg "should not fetch") in
  match Teztnets.resolve_network_for_octez_node ~fetch "" with
  | Ok _net -> Alcotest.fail "empty string should fail"
  | Error (`Msg msg) ->
      Alcotest.(check bool)
        "error mentions empty"
        true
        (string_contains ~needle:"empty" (String.lowercase_ascii msg))

let service_registry_list_empty () =
  with_fake_xdg (fun _env ->
      let services = expect_ok (Service_registry.list ()) in
      Alcotest.(check int) "empty registry" 0 (List.length services))

let service_registry_roundtrip () =
  with_fake_xdg (fun _env ->
      let service = sample_service () in
      let () = expect_ok (Service_registry.write service) in
      let listed = expect_ok (Service_registry.list ()) |> sort_services in
      Alcotest.(check int) "one entry" 1 (List.length listed) ;
      let found = expect_ok (Service_registry.find ~instance:"alpha") in
      (match found with
      | Some svc -> check_service service svc
      | None -> Alcotest.fail "service not found") ;
      let () = expect_ok (Service_registry.remove ~instance:"alpha") in
      let after = expect_ok (Service_registry.list ()) in
      Alcotest.(check int) "empty after removal" 0 (List.length after) ;
      match expect_ok (Service_registry.find ~instance:"alpha") with
      | None -> ()
      | Some _ -> Alcotest.fail "service should have been removed")

let service_registry_list_invalid_json () =
  with_fake_xdg (fun env ->
      let root = Filename.concat env.config "octez-manager" in
      let services = Filename.concat root "services" in
      let ensure_dir path =
        if not (Sys.file_exists path) then Unix.mkdir path 0o755
      in
      ensure_dir root ;
      ensure_dir services ;
      let broken = Filename.concat services "broken.json" in
      let oc = open_out broken in
      output_string oc "{ invalid" ;
      close_out oc ;
      match Service_registry.list () with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "broken registry entry should fail")

let service_registry_remove_missing () =
  with_fake_xdg (fun _env ->
      match Service_registry.remove ~instance:"ghost" with
      | Ok () -> ()
      | Error (`Msg msg) -> Alcotest.failf "remove missing registry: %s" msg)

let installer_instance_name_unique () =
  with_fake_xdg (fun _env ->
      (* Create a node service with instance "foo" *)
      let node_service = sample_service () in
      let node_service = {node_service with Service.instance = "foo"} in
      let () = expect_ok (Service_registry.write node_service) in
      (* Try to validate instance name "foo" - should fail *)
      match
        Installer.For_tests.validate_instance_name_unique ~instance:"foo"
      with
      | Ok () ->
          Alcotest.fail "Should have failed: instance name already in use"
      | Error (`Msg msg) -> (
          Alcotest.(check bool)
            "error message mentions instance in use"
            true
            (string_contains ~needle:"already in use" msg) ;
          Alcotest.(check bool)
            "error message mentions role"
            true
            (string_contains ~needle:"node" msg) ;
          (* Try to validate a different instance name - should succeed *)
          match
            Installer.For_tests.validate_instance_name_unique ~instance:"bar"
          with
          | Ok () -> ()
          | Error (`Msg msg) ->
              Alcotest.failf "Should have succeeded with different name: %s" msg
          ))

let installer_instance_name_chars () =
  with_fake_xdg (fun _env ->
      (* Test valid instance names with allowed characters *)
      let valid_names =
        [
          "simple";
          "with-hyphen";
          "with_underscore";
          "with.dot";
          "MixedCase123";
          "a1b2c3";
          "test-node_1.0";
        ]
      in
      List.iter
        (fun name ->
          match
            Installer.For_tests.validate_instance_name_chars ~instance:name
          with
          | Ok () -> ()
          | Error (`Msg msg) ->
              Alcotest.failf "Valid name '%s' was rejected: %s" name msg)
        valid_names ;
      (* Test invalid instance names with disallowed characters *)
      let invalid_names =
        [
          ("théo", "é");
          (* UTF-8 character *)
          ("test node", "space");
          (* space *)
          ("test@node", "@");
          (* at sign *)
          ("test#node", "#");
          (* hash *)
          ("test/node", "/");
          (* slash *)
          ("test\\node", "\\");
          (* backslash *)
          ("test|node", "|");
          (* pipe *)
          ("test&node", "&");
          (* ampersand *)
          ("test$node", "$");
          (* dollar *)
          ("test!node", "!");
          (* exclamation *)
          ("café", "é");
          (* another UTF-8 example *)
          ("tëst", "ë");
          (* UTF-8 character *)
          ("node*", "*");
          (* asterisk *)
          ("node?", "?");
          (* question mark *)
          ("node[1]", "[");
          (* bracket *)
          ("node(1)", "(");
          (* parenthesis *)
          ("node{1}", "{");
          (* brace *)
        ]
      in
      List.iter
        (fun (name, _desc) ->
          match
            Installer.For_tests.validate_instance_name_chars ~instance:name
          with
          | Ok () ->
              Alcotest.failf
                "Invalid name '%s' was accepted but should fail"
                name
          | Error (`Msg msg) ->
              Alcotest.(check bool)
                "error message mentions invalid characters"
                true
                (string_contains ~needle:"invalid characters" msg))
        invalid_names ;
      (* Test empty string *)
      match Installer.For_tests.validate_instance_name_chars ~instance:"" with
      | Ok () ->
          Alcotest.fail "Empty instance name was accepted but should fail"
      | Error (`Msg msg) ->
          Alcotest.(check bool)
            "error message mentions empty"
            true
            (string_contains ~needle:"cannot be empty" msg))

let installer_instance_name_full_validation () =
  with_fake_xdg (fun _env ->
      (* Create a node service with instance "foo" *)
      let node_service = sample_service () in
      let node_service = {node_service with Service.instance = "foo"} in
      let () = expect_ok (Service_registry.write node_service) in
      (* Test that validate_instance_name checks both chars and uniqueness *)
      (* 1. Invalid characters should fail before uniqueness check *)
      (match Installer.For_tests.validate_instance_name ~instance:"théo" () with
      | Ok () -> Alcotest.fail "Instance name with invalid chars was accepted"
      | Error (`Msg msg) ->
          Alcotest.(check bool)
            "error mentions invalid characters"
            true
            (string_contains ~needle:"invalid characters" msg)) ;
      (* 2. Duplicate valid name should fail with uniqueness error *)
      (match Installer.For_tests.validate_instance_name ~instance:"foo" () with
      | Ok () -> Alcotest.fail "Duplicate instance name was accepted"
      | Error (`Msg msg) ->
          Alcotest.(check bool)
            "error mentions already in use"
            true
            (string_contains ~needle:"already in use" msg)) ;
      (* 3. Valid unique name should succeed *)
      match Installer.For_tests.validate_instance_name ~instance:"bar" () with
      | Ok () -> ()
      | Error (`Msg msg) ->
          Alcotest.failf "Valid unique name was rejected: %s" msg)

let node_env_write_file () =
  with_fake_xdg (fun env ->
      let inst = "alpha" in
      let data_dir = Filename.concat env.data "octez/alpha" in
      let run_args = "--network https://example" in
      let () =
        expect_ok (Node_env.write ~inst ~data_dir ~run_args ~extra_env:[])
      in
      let env_file =
        Filename.concat env.config "octez/instances/alpha/node.env"
      in
      let contents = read_file env_file in
      let expected =
        Printf.sprintf
          "VERSION=v1\nOCTEZ_DATA_DIR=%s\nOCTEZ_NODE_ARGS=%s\n"
          data_dir
          run_args
      in
      Alcotest.(check string) "env contents" expected contents)

let node_env_overwrite_existing () =
  with_fake_xdg (fun env ->
      let inst = "beta" in
      let data_dir = Filename.concat env.data "octez/beta" in
      let first = "--network https://example" in
      let second = "--network https://example --history-mode archive" in
      let () =
        expect_ok (Node_env.write ~inst ~data_dir ~run_args:first ~extra_env:[])
      in
      let () =
        expect_ok
          (Node_env.write ~inst ~data_dir ~run_args:second ~extra_env:[])
      in
      let env_file =
        Filename.concat env.config "octez/instances/beta/node.env"
      in
      let contents = read_file env_file in
      Alcotest.(check bool)
        "updated args"
        true
        (string_contains ~needle:second contents) ;
      Alcotest.(check bool)
        "data dir preserved"
        true
        (string_contains ~needle:data_dir contents))

let node_env_write_pairs_custom_env () =
  with_fake_xdg (fun env ->
      let inst = "gamma" in
      let entries =
        [
          ("OCTEZ_DATA_DIR", "/tmp/data");
          ("CUSTOM_KEY", "custom-value");
          ("EMPTY", " ");
        ]
      in
      let () = expect_ok (Node_env.write_pairs ~inst entries) in
      let env_file =
        Filename.concat env.config "octez/instances/gamma/node.env"
      in
      let contents = read_file env_file in
      Alcotest.(check bool)
        "contains custom"
        true
        (string_contains ~needle:"CUSTOM_KEY=custom-value" contents) ;
      Alcotest.(check bool)
        "skips empty"
        false
        (string_contains ~needle:"EMPTY=" contents))

let systemd_helper_paths () =
  with_fake_xdg (fun env ->
      let open Systemd.For_tests in
      Alcotest.(check string)
        "unit name"
        "octez-node@alpha"
        (Systemd.unit_name "node" "alpha") ;
      Alcotest.(check string) "role binary" "octez-baker" (role_binary "dal") ;
      let unit_path_expected =
        Filename.concat env.config "systemd/user/octez-node@.service"
      in
      Alcotest.(check string) "unit path" unit_path_expected (unit_path "node") ;
      let dropin_expected =
        Filename.concat env.config "systemd/user/octez-node@alpha.service.d"
      in
      Alcotest.(check string)
        "dropin dir"
        dropin_expected
        (dropin_dir "node" "alpha") ;
      let dropin_path_expected =
        Filename.concat dropin_expected "override.conf"
      in
      Alcotest.(check string)
        "dropin path"
        dropin_path_expected
        (dropin_path "node" "alpha") ;
      let env_file = Filename.concat env.config "octez/instances/%i/node.env" in
      let template =
        unit_template ~role:"node" ~app_bin_dir:"/opt/octez" ~user:"octez" ()
      in
      Alcotest.(check bool)
        "env file referenced"
        true
        (string_contains ~needle:env_file template) ;
      Alcotest.(check bool)
        "default target"
        true
        (string_contains ~needle:"WantedBy=default.target" template))

let systemd_logging_lines () =
  let open Systemd.For_tests in
  (* Logging is always via journald *)
  Alcotest.(check list_string)
    "journald"
    ["StandardOutput=journal"; "StandardError=journal"]
    (render_logging_lines Logging_mode.default)

let systemd_unit_queries_use_stub () =
  with_fake_xdg (fun _env ->
      with_systemctl_stub (fun () ->
          let cat =
            expect_ok (Systemd.cat_unit ~role:"node" ~instance:"alpha")
          in
          Alcotest.(check string) "cat output" "unit-stub" cat ;
          let status =
            expect_ok (Systemd.status ~role:"node" ~instance:"alpha")
          in
          Alcotest.(check string) "status output" "status-stub" status ;
          let enabled =
            expect_ok (Systemd.is_enabled ~role:"node" ~instance:"alpha")
          in
          Alcotest.(check string) "enabled output" "enabled" enabled))

let systemd_install_dropin_and_service_commands () =
  with_fake_xdg (fun env ->
      with_systemctl_stub (fun () ->
          with_temp_dir (fun tmp ->
              let bin_dir = Filename.concat tmp "bin" in
              Unix.mkdir bin_dir 0o755 ;
              let node_bin = Filename.concat bin_dir "octez-node" in
              write_exec_file node_bin "#!/bin/sh\nexit 0\n" ;
              let owner, group = current_user_group () in
              let data_dir = Filename.concat env.data "octez/alpha" in
              let () =
                expect_ok
                  (Common.ensure_dir_path ~owner ~group ~mode:0o755 data_dir)
              in
              (* Logging is via journald *)
              let logging_mode = Logging_mode.default in
              let () =
                expect_ok
                  (Systemd.install_unit
                     ~role:"node"
                     ~app_bin_dir:bin_dir
                     ~user:owner
                     ())
              in
              let unit_path = Systemd.For_tests.unit_path "node" in
              let unit_body = read_file unit_path in
              Alcotest.(check bool)
                "includes exec"
                true
                (string_contains
                   ~needle:
                     "ExecStart=/bin/sh -lc 'exec \"${APP_BIN_DIR}/octez-node\""
                   unit_body) ;
              let () =
                expect_ok
                  (Systemd.write_dropin_node
                     ~inst:"alpha"
                     ~data_dir
                     ~logging_mode
                     ())
              in
              let dropin_path = Systemd.For_tests.dropin_path "node" "alpha" in
              let dropin_body = read_file dropin_path in
              Alcotest.(check bool)
                "data dir line"
                true
                (string_contains
                   ~needle:("Environment=OCTEZ_DATA_DIR=" ^ data_dir)
                   dropin_body) ;
              (* Journald logging - no log path lines *)
              Alcotest.(check bool)
                "journald output"
                true
                (string_contains ~needle:"StandardOutput=journal" dropin_body) ;
              let () =
                expect_ok
                  (Systemd.enable
                     ~role:"node"
                     ~instance:"alpha"
                     ~start_now:true
                     ())
              in
              let () =
                expect_ok
                  (Systemd.disable
                     ~role:"node"
                     ~instance:"alpha"
                     ~stop_now:true
                     ())
              in
              let () =
                expect_ok (Systemd.start ~role:"node" ~instance:"alpha" ())
              in
              let () =
                expect_ok (Systemd.stop ~role:"node" ~instance:"alpha" ())
              in
              let () =
                expect_ok (Systemd.restart ~role:"node" ~instance:"alpha" ())
              in
              Systemd.remove_dropin ~role:"node" ~instance:"alpha" ;
              Alcotest.(check bool)
                "dropin removed"
                false
                (Sys.file_exists dropin_path))))

let systemd_dropin_extra_paths () =
  with_fake_xdg (fun env ->
      with_systemctl_stub (fun () ->
          with_temp_dir (fun tmp ->
              let bin_dir = Filename.concat tmp "bin" in
              Unix.mkdir bin_dir 0o755 ;
              let baker_bin = Filename.concat bin_dir "octez-baker" in
              write_exec_file baker_bin "#!/bin/sh\nexit 0\n" ;
              let owner, group = current_user_group () in
              let node_dir = Filename.concat env.data "octez/node-alpha" in
              let () =
                expect_ok
                  (Common.ensure_dir_path ~owner ~group ~mode:0o755 node_dir)
              in
              let extra_path =
                Filename.concat env.data "octez/baker-alpha-base"
              in
              let () =
                expect_ok
                  (Common.ensure_dir_path ~owner ~group ~mode:0o755 extra_path)
              in
              let logging_mode = Logging_mode.default in
              let () =
                expect_ok
                  (Systemd.install_unit
                     ~role:"baker"
                     ~app_bin_dir:bin_dir
                     ~user:owner
                     ())
              in
              let () =
                expect_ok
                  (Systemd.write_dropin
                     ~role:"baker"
                     ~inst:"alpha"
                     ~data_dir:node_dir
                     ~logging_mode
                     ~extra_paths:[extra_path]
                     ())
              in
              let dropin_path = Systemd.For_tests.dropin_path "baker" "alpha" in
              let dropin_body = read_file dropin_path in
              Alcotest.(check bool)
                "includes extra path"
                true
                (string_contains
                   ~needle:("ReadWritePaths=" ^ extra_path)
                   dropin_body))))

let systemd_baker_exec_line_remote () =
  let exec = Systemd.For_tests.exec_line "baker" in
  Alcotest.(check bool)
    "contains 'run remotely' for remote mode"
    true
    (string_contains ~needle:"run remotely" exec) ;
  Alcotest.(check bool)
    "does not contain 'run with remote node'"
    false
    (string_contains ~needle:"run with remote node" exec)

let systemd_baker_exec_line_local () =
  let exec = Systemd.For_tests.exec_line "baker" in
  Alcotest.(check bool)
    "contains 'run with local node' for local mode"
    true
    (string_contains ~needle:"run with local node" exec)

let systemd_baker_exec_line_lb_vote () =
  let exec = Systemd.For_tests.exec_line "baker" in
  Alcotest.(check bool)
    "always includes liquidity-baking-toggle-vote flag"
    true
    (string_contains ~needle:"--liquidity-baking-toggle-vote" exec) ;
  Alcotest.(check bool)
    "references OCTEZ_BAKER_LB_VOTE env var"
    true
    (string_contains ~needle:"OCTEZ_BAKER_LB_VOTE" exec) ;
  Alcotest.(check bool)
    "flag is unconditional (no if statement around it)"
    false
    (string_contains ~needle:"if [ -n \"${OCTEZ_BAKER_LB_VOTE" exec)

let systemd_baker_exec_line_dal_config () =
  let exec = Systemd.For_tests.exec_line "baker" in
  Alcotest.(check bool)
    "references OCTEZ_DAL_CONFIG env var"
    true
    (string_contains ~needle:"OCTEZ_DAL_CONFIG" exec) ;
  Alcotest.(check bool)
    "includes --without-dal when disabled"
    true
    (string_contains ~needle:"--without-dal" exec) ;
  Alcotest.(check bool)
    "includes --dal-node when endpoint provided"
    true
    (string_contains ~needle:"--dal-node" exec) ;
  Alcotest.(check bool)
    "checks for 'disabled' value"
    true
    (string_contains ~needle:"= \"disabled\"" exec)

let system_user_validate_missing () =
  match
    System_user.validate_user_for_service ~user:"__missing_octez_user__"
  with
  | Ok () -> Alcotest.fail "missing user should be rejected"
  | Error _ -> ()

let system_user_service_account_non_root () =
  match System_user.ensure_service_account ~name:"octez-manager-test" () with
  | Ok () -> ()
  | Error (`Msg msg) -> Alcotest.failf "service account error: %s" msg

let system_user_system_directories_non_root () =
  let user, group = current_user_group () in
  match System_user.ensure_system_directories ~user ~group () with
  | Ok () -> ()
  | Error (`Msg msg) -> Alcotest.failf "system directories error: %s" msg

let system_user_service_account_root_path () =
  let commands = ref [] in
  let record ?quiet:_ ?on_log:_ argv =
    commands := !commands @ [argv] ;
    Ok ()
  in
  System_user.For_tests.with_overrides
    ~is_root:(fun () -> true)
    ~run:record
    ~user_exists:(fun _ -> false)
    ~group_exists:(fun _ -> false)
    (fun () ->
      expect_ok (System_user.ensure_service_account ~name:"octez-manager-ci" ())) ;
  let expected =
    [
      ["groupadd"; "--system"; "octez-manager-ci"];
      [
        "useradd";
        "--system";
        "--home-dir";
        "/var/lib/octez-manager-ci";
        "--shell";
        "/usr/sbin/nologin";
        "--comment";
        "Octez Manager service user";
        "--gid";
        "octez-manager-ci";
        "--no-create-home";
        "octez-manager-ci";
      ];
    ]
  in
  Alcotest.check list_list_string "commands" expected !commands

let system_user_remove_account_commands () =
  let commands = ref [] in
  let record ?quiet:_ ?on_log:_ argv =
    commands := !commands @ [argv] ;
    Ok ()
  in
  System_user.For_tests.with_overrides
    ~is_root:(fun () -> true)
    ~run:record
    ~user_exists:(fun _ -> true)
    ~group_exists:(fun _ -> true)
    (fun () ->
      expect_ok (System_user.remove_service_account ~name:"octez-test" ())) ;
  let expected =
    [["userdel"; "--remove"; "octez-test"]; ["groupdel"; "octez-test"]]
  in
  Alcotest.check list_list_string "remove commands" expected !commands

let settings_roundtrip () =
  let settings : Settings.t =
    {
      app_bin_dir = Some "/usr/local/bin";
      default_history_mode = Some History_mode.Archive;
      default_logging_mode = Some Logging_mode.Journald;
    }
  in
  with_fake_xdg (fun _env ->
      let () = expect_ok (Settings.save settings) in
      let loaded = expect_ok (Settings.load ()) in
      Alcotest.(check (option string))
        "app_bin_dir"
        settings.app_bin_dir
        loaded.app_bin_dir ;
      Alcotest.(check (option string))
        "history_mode"
        (Some "archive")
        (Option.map History_mode.to_string loaded.default_history_mode) ;
      Alcotest.(check (option string))
        "logging_mode"
        (Some "journald")
        (Option.map Logging_mode.to_string loaded.default_logging_mode))

let job_manager_submit_and_list () =
  let open Octez_manager_ui in
  Job_manager.clear_finished () ;
  let jobs = Job_manager.list () in
  Alcotest.(check int) "empty initially" 0 (List.length jobs) ;
  Job_manager.submit ~description:"test job" (fun ~append_log:_ () -> Ok ()) ;
  let jobs_after = Job_manager.list () in
  Alcotest.(check int) "one job" 1 (List.length jobs_after) ;
  let job = List.hd jobs_after in
  Alcotest.(check string) "description" "test job" job.description ;
  (* We can't easily test async completion in this sync test runner without Lwt_main.run *)
  (* But we verified submission works. *)
  ()

let background_runner_enqueue_sync () =
  let module BR = Octez_manager_ui.Background_runner in
  let did_run = ref false in
  BR.For_tests.with_synchronous_runner (fun () ->
      BR.enqueue (fun () -> did_run := true)) ;
  Alcotest.(check bool) "task ran" true !did_run

let background_runner_submit_on_complete () =
  let module BR = Octez_manager_ui.Background_runner in
  let ran = ref false in
  let completed = ref false in
  BR.For_tests.with_synchronous_runner (fun () ->
      BR.submit_blocking
        ~on_complete:(fun () -> completed := true)
        (fun () -> ran := true)) ;
  Alcotest.(check bool) "ran" true !ran ;
  Alcotest.(check bool) "on_complete" true !completed

(* RPC scheduler tests removed - now uses worker queue with dedup *)

let make_absolute_path_absolute_stays_same () =
  match Common.make_absolute_path "/usr/bin" with
  | Ok path -> Alcotest.(check string) "absolute unchanged" "/usr/bin" path
  | Error msg -> Alcotest.failf "unexpected error: %s" msg

let make_absolute_path_relative_converted () =
  let result = Common.make_absolute_path "bin" in
  match result with
  | Ok path ->
      Alcotest.(check bool)
        "path is absolute"
        true
        (not (Filename.is_relative path)) ;
      Alcotest.(check bool)
        "path ends with bin"
        true
        (String.ends_with ~suffix:"bin" path)
  | Error msg -> Alcotest.failf "unexpected error: %s" msg

let make_absolute_path_dotdot_relative () =
  let result = Common.make_absolute_path "../work/bin" in
  match result with
  | Ok path ->
      Alcotest.(check bool)
        "path is absolute"
        true
        (not (Filename.is_relative path)) ;
      Alcotest.(check bool)
        "path contains work/bin"
        true
        (String.ends_with ~suffix:"work/bin" path)
  | Error msg -> Alcotest.failf "unexpected error: %s" msg

let make_absolute_path_empty_error () =
  match Common.make_absolute_path "" with
  | Ok _ -> Alcotest.fail "empty path should fail"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions empty"
        true
        (String.contains msg ' ')

let make_absolute_path_whitespace_error () =
  match Common.make_absolute_path "   " with
  | Ok _ -> Alcotest.fail "whitespace-only path should fail"
  | Error _ -> ()

(* ========== Cache module tests ========== *)

let cache_ttl_basic () =
  let counter = ref 0 in
  let cache =
    Cache.create ~name:"test_ttl" ~ttl:0.1 (fun () ->
        incr counter ;
        !counter)
  in
  (* First get should fetch *)
  let v1 = Cache.get cache in
  Alcotest.(check int) "first fetch" 1 v1 ;
  (* Second get within TTL should return cached *)
  let v2 = Cache.get cache in
  Alcotest.(check int) "cached value" 1 v2 ;
  Alcotest.(check int) "fetch count unchanged" 1 !counter ;
  (* Wait for TTL to expire *)
  Unix.sleepf 0.15 ;
  (* Third get should re-fetch *)
  let v3 = Cache.get cache in
  Alcotest.(check int) "re-fetched value" 2 v3 ;
  Alcotest.(check int) "fetch count incremented" 2 !counter

let cache_invalidate () =
  let counter = ref 0 in
  let cache =
    Cache.create ~name:"test_invalidate" ~ttl:60.0 (fun () ->
        incr counter ;
        !counter)
  in
  let v1 = Cache.get cache in
  Alcotest.(check int) "first fetch" 1 v1 ;
  Cache.invalidate cache ;
  let v2 = Cache.get cache in
  Alcotest.(check int) "after invalidate" 2 v2

let cache_result_only_caches_success () =
  let counter = ref 0 in
  let cache =
    Cache.create_result ~name:"test_result" ~ttl:60.0 (fun () ->
        incr counter ;
        if !counter < 3 then Error "not yet" else Ok "success")
  in
  (* First two calls should fail and not cache *)
  (match Cache.get_result cache with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error") ;
  (match Cache.get_result cache with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error") ;
  (* Third call should succeed and cache *)
  (match Cache.get_result cache with
  | Ok v -> Alcotest.(check string) "success value" "success" v
  | Error _ -> Alcotest.fail "expected success") ;
  (* Fourth call should return cached success *)
  let count_before = !counter in
  (match Cache.get_result cache with
  | Ok v -> Alcotest.(check string) "cached success" "success" v
  | Error _ -> Alcotest.fail "expected cached success") ;
  Alcotest.(check int) "no re-fetch" count_before !counter

let cache_keyed_basic () =
  let counter = ref 0 in
  let cache =
    Cache.create_keyed ~name:"test_keyed" ~ttl:60.0 (fun key ->
        incr counter ;
        key ^ "_value")
  in
  let v1 = Cache.get_keyed cache "a" in
  Alcotest.(check string) "key a" "a_value" v1 ;
  let v2 = Cache.get_keyed cache "b" in
  Alcotest.(check string) "key b" "b_value" v2 ;
  Alcotest.(check int) "two fetches" 2 !counter ;
  (* Re-fetch cached key *)
  let v3 = Cache.get_keyed cache "a" in
  Alcotest.(check string) "key a cached" "a_value" v3 ;
  Alcotest.(check int) "no additional fetch" 2 !counter

let cache_safe_keyed_basic () =
  let cache = Cache.create_safe_keyed ~name:"test_safe" ~ttl:60.0 () in
  Cache.set_safe_keyed cache "key1" "value1" ;
  let v1 = Cache.get_safe_keyed_cached cache "key1" in
  Alcotest.(check (option string)) "cached value" (Some "value1") v1 ;
  let v2 = Cache.get_safe_keyed_cached cache "missing" in
  Alcotest.(check (option string)) "missing key" None v2

let cache_safe_keyed_fetch () =
  let counter = ref 0 in
  let cache = Cache.create_safe_keyed ~name:"test_safe_fetch" ~ttl:60.0 () in
  let v1 =
    Cache.get_safe_keyed cache "key1" ~fetch:(fun () ->
        incr counter ;
        "fetched")
  in
  Alcotest.(check string) "fetched value" "fetched" v1 ;
  Alcotest.(check int) "fetch called" 1 !counter ;
  (* Second get should use cache *)
  let v2 =
    Cache.get_safe_keyed cache "key1" ~fetch:(fun () ->
        incr counter ;
        "fetched again")
  in
  Alcotest.(check string) "cached value" "fetched" v2 ;
  Alcotest.(check int) "no re-fetch" 1 !counter

let cache_safe_keyed_per_key_ttl () =
  let cache = Cache.create_safe_keyed ~name:"test_per_key_ttl" ~ttl:0.1 () in
  Cache.set_safe_keyed cache "key1" "value1" ;
  (* Wait for TTL to expire *)
  Unix.sleepf 0.15 ;
  (* Fetch should be called since entry expired *)
  let counter = ref 0 in
  let v =
    Cache.get_safe_keyed cache "key1" ~fetch:(fun () ->
        incr counter ;
        "new_value")
  in
  Alcotest.(check string) "new value after expiry" "new_value" v ;
  Alcotest.(check int) "fetch called due to expiry" 1 !counter

let cache_registry_no_duplicates () =
  (* Create same-named cache twice, should not duplicate in registry *)
  let _c1 = Cache.create ~name:"duplicate_test" ~ttl:60.0 (fun () -> 1) in
  let _c2 = Cache.create ~name:"duplicate_test" ~ttl:60.0 (fun () -> 2) in
  let stats = Cache.get_stats () in
  let count =
    List.length
      (List.filter
         (fun (name, _, _, _, _, _, _) -> name = "duplicate_test")
         stats)
  in
  Alcotest.(check int) "only one registry entry" 1 count

let cache_stats_hit_miss () =
  let cache = Cache.create ~name:"test_stats" ~ttl:60.0 (fun () -> 42) in
  let _ = Cache.get cache in
  (* miss *)
  let _ = Cache.get cache in
  (* hit *)
  let _ = Cache.get cache in
  (* hit *)
  let stats = Cache.get_stats () in
  match
    List.find_opt (fun (name, _, _, _, _, _, _) -> name = "test_stats") stats
  with
  | None -> Alcotest.fail "cache not in stats"
  | Some (_, hits, misses, _, _, _, _) ->
      Alcotest.(check int) "hits" 2 hits ;
      Alcotest.(check int) "misses" 1 misses

(* ============================================================================
   Port validation tests
   ============================================================================ *)

let port_validation_parse_host_port_valid () =
  (* Standard host:port *)
  (match Port_validation.parse_host_port "127.0.0.1:8732" with
  | Some (host, port) ->
      Alcotest.(check string) "host" "127.0.0.1" host ;
      Alcotest.(check int) "port" 8732 port
  | None -> Alcotest.fail "failed to parse 127.0.0.1:8732") ;
  (* All interfaces *)
  (match Port_validation.parse_host_port "0.0.0.0:9732" with
  | Some (host, port) ->
      Alcotest.(check string) "host" "0.0.0.0" host ;
      Alcotest.(check int) "port" 9732 port
  | None -> Alcotest.fail "failed to parse 0.0.0.0:9732") ;
  (* Localhost *)
  match Port_validation.parse_host_port "localhost:8733" with
  | Some (host, port) ->
      Alcotest.(check string) "host localhost" "localhost" host ;
      Alcotest.(check int) "port" 8733 port
  | None -> Alcotest.fail "failed to parse localhost:8733"

let port_validation_parse_host_port_invalid () =
  (* No colon *)
  Alcotest.(check (option (pair string int)))
    "no colon"
    None
    (Port_validation.parse_host_port "invalid") ;
  (* Missing port *)
  Alcotest.(check (option (pair string int)))
    "missing port"
    None
    (Port_validation.parse_host_port "127.0.0.1:") ;
  (* Non-numeric port *)
  Alcotest.(check (option (pair string int)))
    "non-numeric"
    None
    (Port_validation.parse_host_port "127.0.0.1:abc") ;
  (* Negative port *)
  Alcotest.(check (option (pair string int)))
    "negative"
    None
    (Port_validation.parse_host_port "127.0.0.1:-1")

let port_validation_parse_port () =
  (* parse_port only extracts port from host:port format *)
  Alcotest.(check (option int))
    "host:port"
    (Some 9732)
    (Port_validation.parse_port "127.0.0.1:9732") ;
  Alcotest.(check (option int))
    "localhost:port"
    (Some 8732)
    (Port_validation.parse_port "localhost:8732") ;
  (* Just a number without colon returns None *)
  Alcotest.(check (option int))
    "no colon"
    None
    (Port_validation.parse_port "8732") ;
  Alcotest.(check (option int))
    "invalid"
    None
    (Port_validation.parse_port "abc") ;
  Alcotest.(check (option int)) "empty" None (Port_validation.parse_port "")

let port_validation_validate_addr_format () =
  (* Invalid format fails - this doesn't depend on system state *)
  (match
     Port_validation.validate_addr ~addr:"invalid" ~example:"127.0.0.1:8732" ()
   with
  | Error (Port_validation.Invalid_format _) -> ()
  | Ok () -> Alcotest.fail "expected Invalid_format error"
  | Error e ->
      Alcotest.failf "wrong error type: %s" (Port_validation.pp_error e)) ;
  (* Empty host fails *)
  match
    Port_validation.validate_addr ~addr:":8732" ~example:"127.0.0.1:8732" ()
  with
  | Error (Port_validation.Invalid_format _) -> ()
  | Ok () -> Alcotest.fail "expected Invalid_format error for empty host"
  | Error e ->
      Alcotest.failf "wrong error type: %s" (Port_validation.pp_error e)

let port_validation_out_of_range () =
  (* Port below 1024 - parse_host_port returns None for port 80 because p < 1024 check *)
  (* Actually the implementation validates port > 0 && port < 65536 in parse_host_port,
     so 80 would parse fine but validate_addr checks 1024-65535 range separately *)
  (match
     Port_validation.validate_addr
       ~addr:"127.0.0.1:80"
       ~example:"127.0.0.1:8732"
       ()
   with
  | Error Port_validation.Port_out_of_range -> ()
  | Ok () -> Alcotest.fail "expected Port_out_of_range error for port 80"
  | Error (Port_validation.Invalid_format _) ->
      (* This is also acceptable - implementation rejects low ports in parse *)
      ()
  | Error e -> Alcotest.failf "wrong error: %s" (Port_validation.pp_error e)) ;
  (* Port above 65535 - parse_host_port validates p < 65536 *)
  match
    Port_validation.validate_addr
      ~addr:"127.0.0.1:70000"
      ~example:"127.0.0.1:8732"
      ()
  with
  | Error Port_validation.Port_out_of_range -> ()
  | Error (Port_validation.Invalid_format _) ->
      (* Acceptable - parse_host_port rejects it *)
      ()
  | Ok () -> Alcotest.fail "expected error for port 70000"
  | Error e -> Alcotest.failf "wrong error: %s" (Port_validation.pp_error e)

let port_validation_pp_error () =
  let check_contains msg needle =
    if not (string_contains ~needle msg) then
      Alcotest.failf "expected '%s' in '%s'" needle msg
  in
  (* Invalid_format includes the example *)
  check_contains
    (Port_validation.pp_error (Port_validation.Invalid_format "127.0.0.1:8732"))
    "127.0.0.1:8732" ;
  (* Port_out_of_range mentions 1024 *)
  check_contains
    (Port_validation.pp_error Port_validation.Port_out_of_range)
    "1024" ;
  (* Used_by_other_instance mentions the port and instance name *)
  check_contains
    (Port_validation.pp_error
       (Port_validation.Used_by_other_instance (8732, "my-node")))
    "8732" ;
  check_contains
    (Port_validation.pp_error
       (Port_validation.Used_by_other_instance (8732, "my-node")))
    "my-node" ;
  (* Port_in_use mentions the port *)
  check_contains
    (Port_validation.pp_error (Port_validation.Port_in_use (9732, None)))
    "9732" ;
  (* Port_in_use with process name mentions both *)
  check_contains
    (Port_validation.pp_error
       (Port_validation.Port_in_use (9732, Some "nginx")))
    "9732" ;
  check_contains
    (Port_validation.pp_error
       (Port_validation.Port_in_use (9732, Some "nginx")))
    "nginx"

(* ============================================================================
   Service dependents field tests
   ============================================================================ *)

let service_roundtrip_with_dependents () =
  let service =
    {
      (sample_service ()) with
      Service.dependents = ["baker-1"; "accuser-1"; "dal-1"];
    }
  in
  match Service.to_yojson service |> Service.of_yojson with
  | Ok decoded ->
      Alcotest.(check (list string))
        "dependents preserved"
        ["baker-1"; "accuser-1"; "dal-1"]
        decoded.Service.dependents
  | Error (`Msg msg) -> Alcotest.failf "roundtrip failed: %s" msg

let service_roundtrip_with_depends_on () =
  let service =
    {(sample_service ()) with Service.depends_on = Some "my-node"}
  in
  match Service.to_yojson service |> Service.of_yojson with
  | Ok decoded ->
      Alcotest.(check (option string))
        "depends_on preserved"
        (Some "my-node")
        decoded.Service.depends_on
  | Error (`Msg msg) -> Alcotest.failf "roundtrip failed: %s" msg

let service_dependents_default_empty () =
  let service = sample_service () in
  Alcotest.(check (list string))
    "dependents default empty"
    []
    service.Service.dependents ;
  Alcotest.(check (option string))
    "depends_on default none"
    None
    service.Service.depends_on

(* ============================================================================
   Instance name validation tests (edit mode)
   ============================================================================ *)

let instance_name_valid_chars () =
  (* These should all be valid *)
  let valid_names = ["my-node"; "node_1"; "Node.Main"; "abc123"; "a-b_c.d"] in
  List.iter
    (fun name ->
      match Installer.For_tests.validate_instance_name_chars ~instance:name with
      | Ok () -> ()
      | Error (`Msg msg) ->
          Alcotest.failf "name '%s' should be valid but got: %s" name msg)
    valid_names

let instance_name_invalid_chars () =
  (* These should all be invalid *)
  let invalid_names =
    ["my node"; "node@host"; "node:1"; "node/path"; "node\\path"]
  in
  List.iter
    (fun name ->
      match Installer.For_tests.validate_instance_name_chars ~instance:name with
      | Ok () -> Alcotest.failf "name '%s' should be invalid" name
      | Error (`Msg _) -> ())
    invalid_names

(* ============================================================================
   Log viewer tests
   ============================================================================ *)

let assert_contains ~msg cmd needle =
  if not (string_contains ~needle cmd) then
    Alcotest.failf "%s: expected '%s' in command:\n%s" msg needle cmd

let log_viewer_journalctl_cmd_contains_cat () =
  (* Test that journalctl command uses -o cat for clean output *)
  match
    Log_viewer.get_log_cmd ~role:"node" ~instance:"test" ~source:Journald
  with
  | Ok cmd -> assert_contains ~msg:"-o cat" cmd "-o cat"
  | Error (`Msg msg) -> Alcotest.failf "get_log_cmd failed: %s" msg

let log_viewer_journalctl_cmd_contains_n_1000 () =
  (* Test that journalctl command shows 1000 lines of history *)
  match
    Log_viewer.get_log_cmd ~role:"node" ~instance:"test" ~source:Journald
  with
  | Ok cmd -> assert_contains ~msg:"-n 1000" cmd "-n 1000"
  | Error (`Msg msg) -> Alcotest.failf "get_log_cmd failed: %s" msg

let log_viewer_journalctl_cmd_colorizes_error () =
  (* Test that ERROR is colorized in red (ANSI 91) *)
  match
    Log_viewer.get_log_cmd ~role:"node" ~instance:"test" ~source:Journald
  with
  | Ok cmd ->
      assert_contains ~msg:"ERROR" cmd "ERROR" ;
      assert_contains ~msg:"91m (bright red)" cmd "91m"
  | Error (`Msg msg) -> Alcotest.failf "get_log_cmd failed: %s" msg

let log_viewer_journalctl_cmd_colorizes_warning () =
  (* Test that WARNING is colorized in magenta (ANSI 35) *)
  match
    Log_viewer.get_log_cmd ~role:"node" ~instance:"test" ~source:Journald
  with
  | Ok cmd ->
      assert_contains ~msg:"WARNING" cmd "WARNING" ;
      assert_contains ~msg:"35m (magenta)" cmd "35m"
  | Error (`Msg msg) -> Alcotest.failf "get_log_cmd failed: %s" msg

let log_viewer_journalctl_cmd_redirects_stderr () =
  (* Test that stderr is redirected to suppress warnings *)
  match
    Log_viewer.get_log_cmd ~role:"node" ~instance:"test" ~source:Journald
  with
  | Ok cmd -> assert_contains ~msg:"stderr redirect" cmd "2>/dev/null"
  | Error (`Msg msg) -> Alcotest.failf "get_log_cmd failed: %s" msg

let log_viewer_journalctl_unit_name () =
  (* Test that unit name is correctly formed *)
  match
    Log_viewer.get_log_cmd ~role:"node" ~instance:"my-node" ~source:Journald
  with
  | Ok cmd -> assert_contains ~msg:"unit name" cmd "octez-node@my-node"
  | Error (`Msg msg) -> Alcotest.failf "get_log_cmd failed: %s" msg

let log_viewer_journalctl_baker_unit_name () =
  (* Test baker unit name *)
  match
    Log_viewer.get_log_cmd ~role:"baker" ~instance:"my-baker" ~source:Journald
  with
  | Ok cmd -> assert_contains ~msg:"baker unit name" cmd "octez-baker@my-baker"
  | Error (`Msg msg) -> Alcotest.failf "get_log_cmd failed: %s" msg

(* Unit state parsing tests *)
let systemd_unit_state_active () =
  let output = "ActiveState=active\nSubState=running\nResult=success\n" in
  let state = Systemd.For_tests.parse_unit_state_output output in
  Alcotest.(check string) "active_state" "active" state.active_state ;
  Alcotest.(check string) "sub_state" "running" state.sub_state ;
  Alcotest.(check (option string)) "result" None state.result

let systemd_unit_state_inactive () =
  let output = "ActiveState=inactive\nSubState=dead\nResult=success\n" in
  let state = Systemd.For_tests.parse_unit_state_output output in
  Alcotest.(check string) "active_state" "inactive" state.active_state ;
  Alcotest.(check string) "sub_state" "dead" state.sub_state ;
  Alcotest.(check (option string)) "result" None state.result

let systemd_unit_state_failed_exit_code () =
  let output = "ActiveState=failed\nSubState=failed\nResult=exit-code\n" in
  let state = Systemd.For_tests.parse_unit_state_output output in
  Alcotest.(check string) "active_state" "failed" state.active_state ;
  Alcotest.(check string) "sub_state" "failed" state.sub_state ;
  Alcotest.(check (option string)) "result" (Some "exit-code") state.result

let systemd_unit_state_failed_signal () =
  let output = "ActiveState=failed\nSubState=failed\nResult=signal\n" in
  let state = Systemd.For_tests.parse_unit_state_output output in
  Alcotest.(check string) "active_state" "failed" state.active_state ;
  Alcotest.(check (option string)) "result" (Some "signal") state.result

let systemd_unit_state_success_result () =
  (* Result=success should be treated as no failure *)
  let output = "ActiveState=inactive\nSubState=dead\nResult=success\n" in
  let state = Systemd.For_tests.parse_unit_state_output output in
  Alcotest.(check (option string)) "result" None state.result

let () =
  Alcotest.run
    "octez-manager"
    [
      ( "history mode",
        [
          Alcotest.test_case "roundtrip" `Quick history_mode_roundtrip;
          Alcotest.test_case "invalid" `Quick history_mode_invalid;
        ] );
      ("settings", [Alcotest.test_case "roundtrip" `Quick settings_roundtrip]);
      ( "job_manager",
        [
          Alcotest.test_case "submit" `Quick job_manager_submit_and_list;
          Alcotest.test_case
            "enqueue sync"
            `Quick
            background_runner_enqueue_sync;
          Alcotest.test_case
            "submit on_complete"
            `Quick
            background_runner_submit_on_complete;
        ] );
      ( "common.env",
        [
          Alcotest.test_case "home_dir" `Quick home_dir_fallback;
          Alcotest.test_case "xdg config" `Quick xdg_config_custom;
          Alcotest.test_case "data dir" `Quick default_data_dir_custom;
          Alcotest.test_case "role dir" `Quick default_role_dir_custom;
          Alcotest.test_case
            "role dir no duplicate prefix"
            `Quick
            default_role_dir_no_duplicate_prefix;
        ] );
      ( "common.path",
        [
          Alcotest.test_case
            "absolute unchanged"
            `Quick
            make_absolute_path_absolute_stays_same;
          Alcotest.test_case
            "relative converted"
            `Quick
            make_absolute_path_relative_converted;
          Alcotest.test_case
            "dotdot relative"
            `Quick
            make_absolute_path_dotdot_relative;
          Alcotest.test_case "empty error" `Quick make_absolute_path_empty_error;
          Alcotest.test_case
            "whitespace error"
            `Quick
            make_absolute_path_whitespace_error;
        ] );
      ( "common.fs",
        [
          Alcotest.test_case "ensure dirs" `Quick ensure_dir_path_creates;
          Alcotest.test_case
            "ensure missing owner"
            `Quick
            ensure_dir_path_missing_owner;
          Alcotest.test_case "write file" `Quick write_file_creates_contents;
          Alcotest.test_case "remove file" `Quick remove_tree_file;
          Alcotest.test_case "remove directory" `Quick remove_tree_directory;
          Alcotest.test_case "remove missing" `Quick remove_tree_missing;
        ] );
      ( "common.cmd",
        [
          Alcotest.test_case "sh_quote" `Quick sh_quote_tests;
          Alcotest.test_case "cmd_to_string" `Quick cmd_to_string_tests;
          Alcotest.test_case "shellwords parser" `Quick shellwords_parser_tests;
          Alcotest.test_case "run helpers" `Quick common_run_helpers;
          Alcotest.test_case "run_as self" `Quick common_run_as_self;
        ] );
      ( "common.download",
        [
          Alcotest.test_case
            "kill_active_download noop"
            `Quick
            kill_active_download_noop;
        ] );
      ( "form_builder.common",
        [
          Alcotest.test_case "is_nonempty" `Quick form_builder_is_nonempty_tests;
          Alcotest.test_case "normalize" `Quick form_builder_normalize_tests;
          Alcotest.test_case
            "parse_host_port"
            `Quick
            form_builder_parse_host_port_tests;
          Alcotest.test_case
            "prepare_extra_args"
            `Quick
            form_builder_prepare_extra_args_integration;
        ] );
      ( "binary_help_explorer",
        [
          Alcotest.test_case "parse_initial_args" `Quick parse_initial_args_tests;
        ] );
      ( "snapshots.basic",
        [
          Alcotest.test_case "slug_of_network" `Quick snapshots_slug_of_network;
          Alcotest.test_case "sanitize" `Quick snapshots_sanitize_kind;
        ] );
      ( "snapshots.fetch",
        [
          Alcotest.test_case "entry success" `Quick snapshots_fetch_entry_success;
          Alcotest.test_case
            "entry missing metadata"
            `Quick
            snapshots_fetch_entry_missing_metadata;
          Alcotest.test_case "entry 404" `Quick snapshots_fetch_entry_not_found;
          Alcotest.test_case "list from root" `Quick snapshots_list_from_root;
          Alcotest.test_case "list fallback" `Quick snapshots_list_fallback;
          Alcotest.test_case
            "list missing entry error"
            `Quick
            snapshots_list_missing_entries_error;
          Alcotest.test_case
            "list disappeared entry"
            `Quick
            snapshots_list_disappeared_entry_error;
          Alcotest.test_case
            "list no advertised"
            `Quick
            snapshots_list_no_advertised;
          Alcotest.test_case
            "fetch_html fallback"
            `Quick
            snapshots_fetch_html_fallback_to_curl;
          Alcotest.test_case
            "fetch_html prefers eio"
            `Quick
            snapshots_fetch_html_prefers_eio;
        ] );
      ( "teztnets",
        [
          Alcotest.test_case "parse pairs" `Quick teztnets_parse_pairs_basic;
          Alcotest.test_case
            "parse assoc"
            `Quick
            teztnets_parse_pairs_assoc_keys;
          Alcotest.test_case "parse error" `Quick teztnets_parse_pairs_error;
          Alcotest.test_case
            "parse mainnet fallback"
            `Quick
            teztnets_parse_pairs_mainnet_without_url;
          Alcotest.test_case
            "list networks"
            `Quick
            teztnets_list_networks_custom_fetch;
          Alcotest.test_case
            "list fallback"
            `Quick
            teztnets_list_networks_fallback_fetch;
          Alcotest.test_case
            "list empty json fallback"
            `Quick
            teztnets_list_networks_empty_json_fallback;
          Alcotest.test_case
            "fetch_json fallback"
            `Quick
            teztnets_fetch_json_fallback_to_curl;
          Alcotest.test_case
            "fetch_json prefers eio"
            `Quick
            teztnets_fetch_json_prefers_eio;
          Alcotest.test_case
            "parse top-level list"
            `Quick
            teztnets_parse_top_level_list;
          Alcotest.test_case
            "resolve builtin mainnet"
            `Quick
            teztnets_resolve_network_builtin;
          Alcotest.test_case
            "resolve builtin ghostnet"
            `Quick
            teztnets_resolve_network_builtin_ghostnet;
          Alcotest.test_case
            "resolve builtin case insensitive"
            `Quick
            teztnets_resolve_network_builtin_case_insensitive;
          Alcotest.test_case
            "resolve URL passthrough"
            `Quick
            teztnets_resolve_network_url_passthrough;
          Alcotest.test_case
            "resolve alias lowercase"
            `Quick
            teztnets_resolve_network_alias_lowercase;
          Alcotest.test_case
            "resolve alias mixed case"
            `Quick
            teztnets_resolve_network_alias_mixed_case;
          Alcotest.test_case
            "resolve alias not found"
            `Quick
            teztnets_resolve_network_alias_not_found;
          Alcotest.test_case
            "resolve empty string"
            `Quick
            teztnets_resolve_network_empty_string;
        ] );
      ( "logging_mode",
        [
          Alcotest.test_case
            "default is journald"
            `Quick
            logging_mode_default_is_journald;
          Alcotest.test_case "to_string" `Quick logging_mode_to_string_tests;
        ] );
      ( "installer",
        [
          Alcotest.test_case
            "instance name unique"
            `Quick
            installer_instance_name_unique;
          Alcotest.test_case
            "instance name chars"
            `Quick
            installer_instance_name_chars;
          Alcotest.test_case
            "instance name full validation"
            `Quick
            installer_instance_name_full_validation;
          Alcotest.test_case
            "journald logging noop"
            `Quick
            installer_logging_journald_noop;
          Alcotest.test_case
            "drop user heuristic"
            `Quick
            installer_should_drop_service_user;
          Alcotest.test_case
            "backup missing"
            `Quick
            installer_backup_missing_file;
          Alcotest.test_case
            "backup restore"
            `Quick
            installer_backup_restore_roundtrip;
          Alcotest.test_case
            "normalize data dir"
            `Quick
            installer_normalize_data_dir_default;
          Alcotest.test_case
            "endpoint formatting"
            `Quick
            installer_endpoint_of_rpc_formats;
          Alcotest.test_case
            "build run args"
            `Quick
            installer_build_run_args_journald;
          Alcotest.test_case
            "plan direct uri"
            `Quick
            installer_snapshot_plan_direct_uri;
          Alcotest.test_case "plan tzinit" `Quick installer_snapshot_plan_tzinit;
          Alcotest.test_case
            "metadata variants"
            `Quick
            installer_snapshot_metadata_variants;
          Alcotest.test_case
            "strip + detect"
            `Quick
            installer_strip_and_detect_uris;
          Alcotest.test_case
            "history mode matches"
            `Quick
            installer_history_mode_matches;
          Alcotest.test_case
            "snapshot history mode mismatch"
            `Quick
            installer_snapshot_history_mode_mismatch;
          Alcotest.test_case
            "snapshot history mode match"
            `Quick
            installer_snapshot_history_mode_match;
          Alcotest.test_case
            "node form snapshot conflict validation"
            `Quick
            install_node_form_v3_history_conflict;
          Alcotest.test_case
            "node form snapshot filtering by history mode"
            `Quick
            install_node_form_v3_snapshot_filtering;
          Alcotest.test_case
            "runtime probe writable directory"
            `Quick
            runtime_probe_writable_directory;
          Alcotest.test_case "baker initial base dir default" `Quick (fun () ->
              Alcotest.(check bool)
                "base dir defaulted"
                true
                (String.trim
                   (Install_baker_form_v3.For_tests.initial_model ()).client
                     .base_dir
                <> ""));
          Alcotest.test_case
            "accuser initial base dir default"
            `Quick
            (fun () ->
              Alcotest.(check bool)
                "base dir defaulted"
                true
                (String.trim
                   Octez_manager_ui.Install_accuser_form_v3.For_tests
                   .initial_base_dir
                <> ""));
          Alcotest.test_case
            "binary help parses"
            `Quick
            binary_help_parses_options;
          Alcotest.test_case
            "help skips dividers"
            `Quick
            parse_help_skips_dividers;
          Alcotest.test_case
            "baker help parses local"
            `Quick
            baker_help_parses_local;
          Alcotest.test_case
            "baker help parses remote"
            `Quick
            baker_help_parses_remote;
        ] );
      ( "service.core",
        [
          Alcotest.test_case "make" `Quick service_make_populates_fields;
          Alcotest.test_case
            "invalid history"
            `Quick
            service_of_yojson_invalid_history;
        ] );
      ( "service.json",
        [
          Alcotest.test_case "roundtrip" `Quick service_json_roundtrip;
          Alcotest.test_case
            "invalid logging"
            `Quick
            service_json_invalid_logging;
        ] );
      ( "service.registry",
        [
          Alcotest.test_case "list empty" `Quick service_registry_list_empty;
          Alcotest.test_case "roundtrip" `Quick service_registry_roundtrip;
          Alcotest.test_case
            "invalid json"
            `Quick
            service_registry_list_invalid_json;
          Alcotest.test_case
            "remove missing"
            `Quick
            service_registry_remove_missing;
        ] );
      ( "node.env",
        [
          Alcotest.test_case "write file" `Quick node_env_write_file;
          Alcotest.test_case "overwrite" `Quick node_env_overwrite_existing;
          Alcotest.test_case
            "custom pairs"
            `Quick
            node_env_write_pairs_custom_env;
        ] );
      ( "systemd",
        [
          Alcotest.test_case "paths" `Quick systemd_helper_paths;
          Alcotest.test_case "logging" `Quick systemd_logging_lines;
          Alcotest.test_case "unit queries" `Quick systemd_unit_queries_use_stub;
          Alcotest.test_case
            "install + dropin"
            `Quick
            systemd_install_dropin_and_service_commands;
          Alcotest.test_case "extra paths" `Quick systemd_dropin_extra_paths;
          Alcotest.test_case
            "baker exec remote"
            `Quick
            systemd_baker_exec_line_remote;
          Alcotest.test_case
            "baker exec local"
            `Quick
            systemd_baker_exec_line_local;
          Alcotest.test_case
            "baker exec line lb vote"
            `Quick
            systemd_baker_exec_line_lb_vote;
          Alcotest.test_case
            "baker exec line dal config"
            `Quick
            systemd_baker_exec_line_dal_config;
        ] );
      ( "system_user",
        [
          Alcotest.test_case
            "validate missing"
            `Quick
            system_user_validate_missing;
          Alcotest.test_case
            "service account"
            `Quick
            system_user_service_account_non_root;
          Alcotest.test_case
            "system directories"
            `Quick
            system_user_system_directories_non_root;
          Alcotest.test_case
            "service account root"
            `Quick
            system_user_service_account_root_path;
          Alcotest.test_case
            "remove account root"
            `Quick
            system_user_remove_account_commands;
        ] );
      ( "cache",
        [
          Alcotest.test_case "ttl basic" `Quick cache_ttl_basic;
          Alcotest.test_case "invalidate" `Quick cache_invalidate;
          Alcotest.test_case
            "result only success"
            `Quick
            cache_result_only_caches_success;
          Alcotest.test_case "keyed basic" `Quick cache_keyed_basic;
          Alcotest.test_case "safe_keyed basic" `Quick cache_safe_keyed_basic;
          Alcotest.test_case "safe_keyed fetch" `Quick cache_safe_keyed_fetch;
          Alcotest.test_case
            "safe_keyed per-key ttl"
            `Quick
            cache_safe_keyed_per_key_ttl;
          Alcotest.test_case
            "registry no duplicates"
            `Quick
            cache_registry_no_duplicates;
          Alcotest.test_case "stats hit/miss" `Quick cache_stats_hit_miss;
        ] );
      ( "port_validation",
        [
          Alcotest.test_case
            "parse host:port valid"
            `Quick
            port_validation_parse_host_port_valid;
          Alcotest.test_case
            "parse host:port invalid"
            `Quick
            port_validation_parse_host_port_invalid;
          Alcotest.test_case "parse port" `Quick port_validation_parse_port;
          Alcotest.test_case
            "validate addr format"
            `Quick
            port_validation_validate_addr_format;
          Alcotest.test_case
            "port out of range"
            `Quick
            port_validation_out_of_range;
          Alcotest.test_case "pp_error" `Quick port_validation_pp_error;
        ] );
      ( "service.dependents",
        [
          Alcotest.test_case
            "roundtrip with dependents"
            `Quick
            service_roundtrip_with_dependents;
          Alcotest.test_case
            "roundtrip with depends_on"
            `Quick
            service_roundtrip_with_depends_on;
          Alcotest.test_case
            "defaults empty"
            `Quick
            service_dependents_default_empty;
        ] );
      ( "instance_name",
        [
          Alcotest.test_case "valid chars" `Quick instance_name_valid_chars;
          Alcotest.test_case "invalid chars" `Quick instance_name_invalid_chars;
        ] );
      ( "log_viewer",
        [
          Alcotest.test_case
            "journalctl -o cat"
            `Quick
            log_viewer_journalctl_cmd_contains_cat;
          Alcotest.test_case
            "journalctl -n 1000"
            `Quick
            log_viewer_journalctl_cmd_contains_n_1000;
          Alcotest.test_case
            "colorize ERROR"
            `Quick
            log_viewer_journalctl_cmd_colorizes_error;
          Alcotest.test_case
            "colorize WARNING"
            `Quick
            log_viewer_journalctl_cmd_colorizes_warning;
          Alcotest.test_case
            "stderr redirect"
            `Quick
            log_viewer_journalctl_cmd_redirects_stderr;
          Alcotest.test_case
            "node unit name"
            `Quick
            log_viewer_journalctl_unit_name;
          Alcotest.test_case
            "baker unit name"
            `Quick
            log_viewer_journalctl_baker_unit_name;
        ] );
      ( "systemd.unit_state",
        [
          Alcotest.test_case "parse active" `Quick systemd_unit_state_active;
          Alcotest.test_case "parse inactive" `Quick systemd_unit_state_inactive;
          Alcotest.test_case
            "parse failed exit-code"
            `Quick
            systemd_unit_state_failed_exit_code;
          Alcotest.test_case
            "parse failed signal"
            `Quick
            systemd_unit_state_failed_signal;
          Alcotest.test_case
            "parse success result"
            `Quick
            systemd_unit_state_success_result;
        ] );
    ]
