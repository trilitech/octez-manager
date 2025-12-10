open Octez_manager_lib
open Installer_types
module Binary_help_explorer = Octez_manager_ui.Binary_help_explorer

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

type fake_xdg = {config : string; data : string; state : string}

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
      let state = mk_dir "state" in
      with_env
        [
          ("XDG_CONFIG_HOME", Some config);
          ("XDG_DATA_HOME", Some data);
          ("XDG_STATE_HOME", Some state);
        ]
        (fun () -> f {config; data; state}))

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
  match (a, b) with
  | Logging_mode.Journald, Logging_mode.Journald -> true
  | ( Logging_mode.File {path = p1; rotate = r1},
      Logging_mode.File {path = p2; rotate = r2} ) ->
      String.equal p1 p2 && Bool.equal r1 r2
  | _ -> false

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
  && Option.equal String.equal a.snapshot_kind b.snapshot_kind
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
    snapshot_kind = None;
    snapshot_no_check = false;
    extra_args = [];
  }

let sort_services =
  List.sort (fun a b -> compare a.Service.instance b.Service.instance)

let sample_node_request ?data_dir ?(bootstrap = Genesis)
    ?(history_mode = History_mode.Rolling)
    ?(logging_mode = Logging_mode.Journald) ?(extra_args = [])
    ?(auto_enable = false) ?(preserve_data = false) () : node_request =
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

let logging_mode_default_paths () =
  with_fake_xdg (fun env ->
      match Logging_mode.default_for ~instance:"alpha" ~role:"node" with
      | Logging_mode.File {path; rotate} ->
          let expected =
            Filename.concat env.state "octez/logs/node-alpha.log"
          in
          Alcotest.(check string) "log path" expected path ;
          Alcotest.(check bool) "rotation enabled" true rotate
      | Logging_mode.Journald -> Alcotest.fail "expected file logging")

let logging_mode_to_string_tests () =
  let file_mode = Logging_mode.File {path = "/tmp/log"; rotate = true} in
  Alcotest.(check string)
    "journald"
    "journald"
    (Logging_mode.to_string Logging_mode.Journald) ;
  Alcotest.(check string) "file" "/tmp/log" (Logging_mode.to_string file_mode)

let logging_mode_baker_path_contains_role () =
  with_fake_xdg (fun env ->
      match Logging_mode.default_for ~instance:"beta" ~role:"baker" with
      | Logging_mode.File {path; _} ->
          let expected =
            Filename.concat env.state "octez/logs/baker-beta.log"
          in
          Alcotest.(check string) "baker log path" expected path
      | Logging_mode.Journald -> Alcotest.fail "expected file logging")

let installer_log_directory_creation () =
  with_fake_xdg (fun env ->
      let log_path = Filename.concat env.state "octez/logs/demo-alpha.log" in
      let log_dir = Filename.dirname log_path in
      let (_ : (unit, [> Rresult.R.msg]) result) = Common.remove_tree log_dir in
      let owner, group = current_user_group () in
      let logging_mode = Logging_mode.File {path = log_path; rotate = false} in
      let () =
        expect_ok
          (Installer.For_tests.ensure_logging_base_directory
             ~owner
             ~group
             logging_mode)
      in
      Alcotest.(check bool) "log dir exists" true (Sys.is_directory log_dir))

let installer_remove_logging_artifacts_file () =
  with_temp_dir (fun base ->
      let log_path = Filename.concat base "node-alpha.log" in
      let oc = open_out log_path in
      output_string oc "payload" ;
      close_out oc ;
      let logging_mode = Logging_mode.File {path = log_path; rotate = false} in
      let () =
        expect_ok (Installer.For_tests.remove_logging_artifacts logging_mode)
      in
      Alcotest.(check bool) "log removed" false (Sys.file_exists log_path))

let installer_remove_logging_artifacts_journald () =
  match Installer.For_tests.remove_logging_artifacts Logging_mode.Journald with
  | Ok () -> ()
  | Error (`Msg msg) -> Alcotest.failf "journald cleanup failed: %s" msg

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
  let trimmed = Installer.For_tests.endpoint_of_rpc "  localhost:8732  " in
  Alcotest.(check string) "adds scheme" "http://localhost:8732" trimmed ;
  let passthrough = Installer.For_tests.endpoint_of_rpc "https://node:8732" in
  Alcotest.(check string) "keeps scheme" "https://node:8732" passthrough

let installer_build_run_args_file_logging () =
  let logging_mode =
    Logging_mode.File {path = "/tmp/node-alpha.log"; rotate = false}
  in
  let args =
    Installer.For_tests.build_run_args
      ~network:"mainnet"
      ~history_mode:History_mode.Full
      ~rpc_addr:"127.0.0.1:8732"
      ~net_addr:"0.0.0.0:9732"
      ~extra_args:["--peer"; "2"]
      ~logging_mode
  in
  Alcotest.(check bool)
    "contains logging flag"
    true
    (string_contains ~needle:"--log-output=/tmp/node-alpha.log" args)

let installer_snapshot_plan_direct_uri () =
  let request =
    sample_node_request
      ~bootstrap:(Snapshot {src = Some "file:///tmp/snapshot"; kind = None})
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
      let request =
        sample_node_request
          ~bootstrap:(Snapshot {src = None; kind = Some "rolling"})
          ()
      in
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
  let no_meta = Installer.For_tests.snapshot_metadata_of_plan No_snapshot in
  Alcotest.(check bool) "no auto" false no_meta.auto ;
  let direct_meta =
    Installer.For_tests.snapshot_metadata_of_plan
      (Direct_snapshot {uri = "https://example"})
  in
  Alcotest.(check (option string))
    "uri stored"
    (Some "https://example")
    direct_meta.uri ;
  let tzinit_meta =
    Installer.For_tests.snapshot_metadata_of_plan
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
    tzinit_meta.network_slug

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
    if String.ends_with ~suffix:"/mainnet/rolling.html" url then
      Ok (200, html)
    else Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match
        Installer.For_tests.resolve_snapshot_download
          ~network:"mainnet"
          ~history_mode:History_mode.Rolling
          ~snapshot_kind:(Some "rolling")
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
    if String.ends_with ~suffix:"/mainnet/rolling.html" url then
      Ok (200, html)
    else Ok (404, "missing")
  in
  Snapshots.For_tests.with_fetch fetch (fun () ->
      match
        Installer.For_tests.resolve_snapshot_download
          ~network:"mainnet"
          ~history_mode:History_mode.Rolling
          ~snapshot_kind:(Some "rolling")
      with
      | Ok res ->
          Alcotest.(check string)
            "download url"
            "https://example/rolling.snap"
            res.download_url
      | Error (`Msg msg) -> Alcotest.failf "should accept matching mode: %s" msg)

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
  let opts =
    Binary_help_explorer.For_tests.parse_help octez_node_run_help_plain
  in
  (* Parser stops at ENVIRONMENT section, so we get 47 options instead of 49 *)
  Alcotest.(check int) "option count" 47 (List.length opts) ;
  let find name =
    match
      List.find_opt
        (fun o -> List.exists (( = ) name) o.Binary_help_explorer.names)
        opts
    with
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
    (List.exists (( = ) "-d") data_dir.Binary_help_explorer.names) ;
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
  let opts = Binary_help_explorer.For_tests.parse_baker_help baker_local_help in
  Alcotest.(check bool) "has options" true (List.length opts > 5) ;
  let find name =
    List.find_opt
      (fun o -> List.exists (( = ) name) o.Binary_help_explorer.names)
      opts
    |> Option.get
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
    (ops_pool.Binary_help_explorer.arg = Some "<file|uri>") ;
  Alcotest.(check bool)
    "operations-pool doc"
    true
    (string_contains
       ~needle:"fetch operations from this file"
       ops_pool.Binary_help_explorer.doc)

let baker_help_parses_remote () =
  let opts =
    Binary_help_explorer.For_tests.parse_baker_help baker_remote_help
  in
  Alcotest.(check bool) "has options" true (List.length opts > 5) ;
  let has name =
    List.exists
      (fun o -> List.exists (( = ) name) o.Binary_help_explorer.names)
      opts
  in
  Alcotest.(check bool) "has base-dir" true (has "--base-dir") ;
  Alcotest.(check bool) "has endpoint" true (has "--endpoint") ;
  Alcotest.(check bool) "has without-dal" true (has "--without-dal")

let parse_help_skips_dividers () =
  let sample =
    {|
------
  -a --alpha <foo>: first
------
  -b --bravo: second
|}
  in
  let opts = Binary_help_explorer.For_tests.parse_help sample in
  Alcotest.(check int) "only options" 2 (List.length opts) ;
  let names = List.map (fun o -> List.hd o.Binary_help_explorer.names) opts in
  Alcotest.(check (list string)) "names" ["-a"; "-b"] names

let service_json_roundtrip () =
  let logging_mode =
    Logging_mode.File {path = "/tmp/octez.log"; rotate = true}
  in
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
          (fun (n : Teztnets.network_info) ->
            (Option.value ~default:n.alias n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs)
        "parsed pairs"
        [
          ("Seoulnet", "https://example/seo.json");
          ("custom", "https://example/custom.json");
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
          (fun (n : Teztnets.network_info) ->
            (Option.value ~default:n.alias n.human_name, n.network_url))
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
          (fun (n : Teztnets.network_info) ->
            (Option.value ~default:n.alias n.human_name, n.network_url))
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
          (fun (n : Teztnets.network_info) ->
            (Option.value ~default:n.alias n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs)
        "list networks"
        [("Seoulnet", "https://example/seoul.json"); ("ghostnet", "ghostnet")]
        pairs
  | Error (`Msg msg) -> Alcotest.failf "list_networks error: %s" msg

let teztnets_list_networks_fallback_fetch () =
  match Teztnets.list_networks ~fetch:(fun () -> Error (`Msg "boom")) () with
  | Ok infos ->
      let pairs =
        List.map
          (fun (n : Teztnets.network_info) ->
            (Option.value ~default:n.alias n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs) "fallback" Teztnets.fallback_pairs pairs
  | Error (`Msg msg) -> Alcotest.failf "fallback error: %s" msg

let teztnets_list_networks_empty_json_fallback () =
  match Teztnets.list_networks ~fetch:(fun () -> Ok "{}") () with
  | Ok infos ->
      let pairs =
        List.map
          (fun (n : Teztnets.network_info) ->
            (Option.value ~default:n.alias n.human_name, n.network_url))
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
          (fun (n : Teztnets.network_info) ->
            (Option.value ~default:n.alias n.human_name, n.network_url))
          infos
      in
      Alcotest.(check list_pairs)
        "top-level list"
        [
          ("Alpha", "https://example/a.json"); ("beta", "https://example/b.json");
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
      Alcotest.(check string) "role binary" "octez-dal-node" (role_binary "dal") ;
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
  Alcotest.(check list_string)
    "journald explicit"
    ["StandardOutput=journal"; "StandardError=journal"]
    (render_logging_lines Logging_mode.Journald) ;
  let file_mode =
    Logging_mode.File {path = "/tmp/logs/node-alpha.log"; rotate = false}
  in
  Alcotest.(check list_string)
    "file entry"
    ["Environment=OCTEZ_LOG_PATH=/tmp/logs/node-alpha.log"]
    (render_logging_lines file_mode)

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
              let log_path =
                Filename.concat env.state "octez/logs/node-alpha.log"
              in
              let log_dir = Filename.dirname log_path in
              let () =
                expect_ok
                  (Common.ensure_dir_path ~owner ~group ~mode:0o755 log_dir)
              in
              let logging_mode =
                Logging_mode.File {path = log_path; rotate = false}
              in
              let () =
                expect_ok
                  (Systemd.install_unit
                     ~role:"node"
                     ~app_bin_dir:bin_dir
                     ~user:owner)
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
                     ~logging_mode)
              in
              let dropin_path = Systemd.For_tests.dropin_path "node" "alpha" in
              let dropin_body = read_file dropin_path in
              Alcotest.(check bool)
                "data dir line"
                true
                (string_contains
                   ~needle:("Environment=OCTEZ_DATA_DIR=" ^ data_dir)
                   dropin_body) ;
              Alcotest.(check bool)
                "log path line"
                true
                (string_contains
                   ~needle:("Environment=OCTEZ_LOG_PATH=" ^ log_path)
                   dropin_body) ;
              Alcotest.(check bool)
                "log rw path"
                true
                (string_contains
                   ~needle:("ReadWritePaths=" ^ log_dir)
                   dropin_body) ;
              let () =
                expect_ok
                  (Systemd.enable
                     ~role:"node"
                     ~instance:"alpha"
                     ~start_now:true)
              in
              let () =
                expect_ok
                  (Systemd.disable
                     ~role:"node"
                     ~instance:"alpha"
                     ~stop_now:true)
              in
              let () =
                expect_ok (Systemd.start ~role:"node" ~instance:"alpha")
              in
              let () =
                expect_ok (Systemd.stop ~role:"node" ~instance:"alpha")
              in
              let () =
                expect_ok (Systemd.restart ~role:"node" ~instance:"alpha")
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
              let logging_mode =
                Logging_mode.default_for ~instance:"alpha" ~role:"baker"
              in
              let () =
                expect_ok
                  (Systemd.install_unit
                     ~role:"baker"
                     ~app_bin_dir:bin_dir
                     ~user:owner)
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

let systemd_disable_nonexistent_logrotate_timer () =
  with_fake_xdg (fun _env ->
      with_systemctl_stub (fun () ->
          (* This test verifies that disabling logrotate units when they don't exist
             doesn't cause errors. This is the scenario when removing an unknown instance. *)
          Systemd.For_tests.disable_user_logrotate_timer () ;
          (* If we get here without exceptions or errors, the test passes *)
          ()))

let system_user_validate_missing () =
  match
    System_user.validate_user_for_service ~user:"__missing_octez_user__"
  with
  | Ok () -> Alcotest.fail "missing user should be rejected"
  | Error _ -> ()

let system_user_service_account_non_root () =
  match System_user.ensure_service_account ~name:"octez-manager-test" with
  | Ok () -> ()
  | Error (`Msg msg) -> Alcotest.failf "service account error: %s" msg

let system_user_system_directories_non_root () =
  let user, group = current_user_group () in
  match System_user.ensure_system_directories ~user ~group with
  | Ok () -> ()
  | Error (`Msg msg) -> Alcotest.failf "system directories error: %s" msg

let system_user_service_account_root_path () =
  let commands = ref [] in
  let record argv =
    commands := !commands @ [argv] ;
    Ok ()
  in
  System_user.For_tests.with_overrides
    ~is_root:(fun () -> true)
    ~run:record
    ~user_exists:(fun _ -> false)
    ~group_exists:(fun _ -> false)
    (fun () ->
      expect_ok (System_user.ensure_service_account ~name:"octez-manager-ci")) ;
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
  let record argv =
    commands := !commands @ [argv] ;
    Ok ()
  in
  System_user.For_tests.with_overrides
    ~is_root:(fun () -> true)
    ~run:record
    ~user_exists:(fun _ -> true)
    ~group_exists:(fun _ -> true)
    (fun () ->
      expect_ok (System_user.remove_service_account ~name:"octez-test")) ;
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
  Job_manager.submit ~description:"test job" (fun () -> Ok ()) ;
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

let rpc_scheduler_schedule_cap_and_spacing () =
  let module RS = Octez_manager_ui.Rpc_scheduler in
  let submits = ref 0 in
  let executed = ref [] in
  RS.For_tests.reset_state () ;
  RS.For_tests.set_bg_cap 1 ;
  RS.For_tests.set_last_global_rpc (-10.0) ;
  let submit_stub ?on_complete f =
    incr submits ;
    f () ;
    Option.iter (fun g -> g ()) on_complete
  in
  let now_stub () = float_of_int !submits in
  let svc = {(sample_service ()) with Service.instance = "node1"} in
  let poll_stub svc now =
    executed := (svc.Service.instance, now) :: !executed
  in
  RS.For_tests.with_submit_blocking submit_stub (fun () ->
      RS.For_tests.with_now now_stub (fun () ->
          RS.For_tests.with_poll_boot poll_stub (fun () ->
              RS.For_tests.dispatch [svc]))) ;
  Alcotest.(check int) "submitted once" 1 !submits ;
  Alcotest.(check int) "executed once" 1 (List.length !executed) ;
  match !executed with
  | [(inst, _now)] -> Alcotest.(check string) "instance" "node1" inst
  | _ -> Alcotest.fail "unexpected execution list"

let rpc_scheduler_respects_min_spacing () =
  let module RS = Octez_manager_ui.Rpc_scheduler in
  let submits = ref 0 in
  let executed = ref [] in
  RS.For_tests.reset_state () ;
  RS.For_tests.set_bg_cap 4 ;
  RS.For_tests.with_submit_blocking
    (fun ?on_complete f ->
      incr submits ;
      f () ;
      Option.iter (fun g -> g ()) on_complete)
    (fun () ->
      RS.For_tests.set_last_global_rpc (-10.0) ;
      let times = ref [0.0; 1.2; 1.8] in
      RS.For_tests.with_now
        (fun () ->
          match !times with
          | t :: rest ->
              times := rest ;
              t
          | [] -> 999.0)
        (fun () ->
          let mk instance = {(sample_service ()) with Service.instance} in
          let svc1 = mk "node1" in
          let svc2 = mk "node2" in
          let svc3 = mk "node3" in
          RS.For_tests.with_poll_boot
            (fun svc now ->
              executed := (svc.Service.instance, now) :: !executed)
            (fun () -> RS.For_tests.dispatch [svc1; svc2; svc3]))) ;
  (* min_spacing=1.0 so first at t=0.0, second at t=1.2, third skipped because 1.8 - 1.2 < 1.0 *)
  Alcotest.(check int) "submitted twice" 2 !submits ;
  let insts = List.map fst !executed |> List.rev in
  Alcotest.(check (list string)) "order" ["node1"; "node2"] insts

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
          Alcotest.test_case
            "rpc_scheduler cap"
            `Quick
            rpc_scheduler_schedule_cap_and_spacing;
          Alcotest.test_case
            "rpc_scheduler spacing"
            `Quick
            rpc_scheduler_respects_min_spacing;
        ] );
      ( "common.env",
        [
          Alcotest.test_case "home_dir" `Quick home_dir_fallback;
          Alcotest.test_case "xdg config" `Quick xdg_config_custom;
          Alcotest.test_case "data dir" `Quick default_data_dir_custom;
          Alcotest.test_case "role dir" `Quick default_role_dir_custom;
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
          Alcotest.test_case "run helpers" `Quick common_run_helpers;
          Alcotest.test_case "run_as self" `Quick common_run_as_self;
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
        ] );
      ( "logging_mode",
        [
          Alcotest.test_case "default paths" `Quick logging_mode_default_paths;
          Alcotest.test_case "to_string" `Quick logging_mode_to_string_tests;
          Alcotest.test_case
            "baker path"
            `Quick
            logging_mode_baker_path_contains_role;
        ] );
      ( "installer",
        [
          Alcotest.test_case
            "log dir base"
            `Quick
            installer_log_directory_creation;
          Alcotest.test_case
            "remove log artifacts"
            `Quick
            installer_remove_logging_artifacts_file;
          Alcotest.test_case
            "remove log journald"
            `Quick
            installer_remove_logging_artifacts_journald;
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
            installer_build_run_args_file_logging;
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
            "disable nonexistent logrotate"
            `Quick
            systemd_disable_nonexistent_logrotate_timer;
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
    ]
