(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Deterministic Mocks for UI Regression Testing
    
    Provides 100% reproducible mocks that eliminate all sources of
    flakiness and non-determinism:
    - Fixed timestamps
    - Seeded random number generation
    - Predictable file system operations
    - Consistent command outputs
    - Stable system metrics
    
    This ensures that identical UI states always produce identical renders.
*)

(* ============================================================ *)
(* Fixed Values *)
(* ============================================================ *)

(** Fixed timestamp: 2024-01-15 12:00:00 UTC (Monday) *)
let fixed_timestamp = 1705320000.0

(** Fixed test root directory *)
let test_root = "/tmp/octez-test-regression"

(** Seed for deterministic randomness *)
let random_seed = 42

(* ============================================================ *)
(* Time Mocking *)
(* ============================================================ *)

let mock_time_ref = ref None

let set_fixed_time timestamp = mock_time_ref := Some timestamp

let reset_time () = mock_time_ref := None

let get_time () =
  match !mock_time_ref with Some t -> t | None -> Unix.gettimeofday ()

(** Initialize with fixed time *)
let () = set_fixed_time fixed_timestamp

(* ============================================================ *)
(* Random Mocking *)
(* ============================================================ *)

(** Initialize random with fixed seed for reproducibility *)
let init_random () = Random.init random_seed

(* ============================================================ *)
(* Deterministic File System *)
(* ============================================================ *)

(** Predictable temp file names (no random components) *)
let mock_temp_file_counter = ref 0

let mock_temp_file prefix suffix =
  incr mock_temp_file_counter ;
  Printf.sprintf "%s/%s-%05d%s" test_root prefix !mock_temp_file_counter suffix

let reset_temp_counter () = mock_temp_file_counter := 0

(* ============================================================ *)
(* Deterministic System Capability *)
(* ============================================================ *)

(** Mock Miaou System capability with 100% deterministic behavior *)
let deterministic_miaou_system : Miaou_interfaces.System.t =
  {
    file_exists =
      (fun path ->
        (* Files in test root always exist *)
        String.starts_with ~prefix:test_root path);
    is_directory =
      (fun path ->
        (* Directories end with / or are known system paths *)
        String.ends_with ~suffix:"/" path
        || path = test_root
        || String.starts_with ~prefix:(test_root ^ "/.config") path
        || String.starts_with ~prefix:(test_root ^ "/.local") path);
    read_file =
      (fun path ->
        (* Return deterministic content based on path *)
        let content =
          if String.ends_with ~suffix:".json" path then
            "{\"version\": \"1.0\", \"data\": []}"
          else if String.ends_with ~suffix:".env" path then
            "VERSION=v1\nDATA_DIR=/data\n"
          else Printf.sprintf "# Mock content for %s\n" (Filename.basename path)
        in
        Ok content);
    write_file = (fun _path _content -> Ok ());
    mkdir = (fun _path -> Ok ());
    run_command =
      (fun ~argv ~cwd:_ ->
        (* Return deterministic output based on command *)
        let stdout =
          match argv with
          | "systemctl" :: "is-active" :: _ -> "active\n"
          | "systemctl" :: "is-enabled" :: _ -> "enabled\n"
          | "systemctl" :: "status" :: _ ->
              "● service.service - Test Service\n\
              \   Loaded: loaded\n\
              \   Active: active (running) since Mon 2024-01-15 12:00:00 UTC\n"
          | "df" :: "-B1" :: _ ->
              "Filesystem     1B-blocks      Used Available Use% Mounted on\n\
               /dev/sda1    1000000000000 500000000000 500000000000  50% /\n"
          | "ps" :: _ ->
              "  PID TTY          TIME CMD\n\
              \ 1234 ?        00:00:01 octez-node\n"
          | "curl" :: _ -> "{\"latest_version\": \"24.0\", \"networks\": []}"
          | "octez-node" :: "--version" :: _ -> "24.0\n"
          | "id" :: "-u" :: _ -> "1000\n"
          | "id" :: "-g" :: _ -> "1000\n"
          | "whoami" :: _ -> "testuser\n"
          | _ -> ""
        in
        Ok {Miaou_interfaces.System.exit_code = 0; stdout; stderr = ""});
    get_current_user_info = (fun () -> Ok ("testuser", "testgroup"));
    get_disk_usage =
      (fun ~path:_ ->
        (* Always return 1TB total *)
        Ok 1_000_000_000_000L);
    list_dir =
      (fun path ->
        (* Return predictable directory listings *)
        if path = test_root then Ok ["."; ".."; ".config"; ".local"; "data"]
        else if String.ends_with ~suffix:"/data" path then
          Ok ["."; ".."; "node1"; "node2"; "baker1"]
        else Ok ["."; ".."; "file1.txt"; "file2.json"]);
    probe_writable = (fun ~path:_ -> Ok true);
    get_env_var =
      (fun key ->
        (* Return fixed environment variables *)
        match key with
        | "HOME" -> Some (test_root ^ "/home")
        | "USER" -> Some "testuser"
        | "XDG_CONFIG_HOME" -> Some (test_root ^ "/.config")
        | "XDG_DATA_HOME" -> Some (test_root ^ "/.local/share")
        | "XDG_STATE_HOME" -> Some (test_root ^ "/.local/state")
        | "TERM" -> Some "xterm-256color"
        | "LANG" -> Some "en_US.UTF-8"
        | _ -> None);
  }

(* ============================================================ *)
(* Environment Setup *)
(* ============================================================ *)

(** Setup completely deterministic environment for regression tests *)
let setup_deterministic_env () =
  (* Reset all mutable state *)
  reset_time () ;
  set_fixed_time fixed_timestamp ;
  init_random () ;
  reset_temp_counter () ;

  (* Set fixed environment variables *)
  Unix.putenv "HOME" (test_root ^ "/home") ;
  Unix.putenv "USER" "testuser" ;
  Unix.putenv "XDG_CONFIG_HOME" (test_root ^ "/.config") ;
  Unix.putenv "XDG_DATA_HOME" (test_root ^ "/.local/share") ;
  Unix.putenv "XDG_STATE_HOME" (test_root ^ "/.local/state") ;
  Unix.putenv "TERM" "xterm-256color" ;
  Unix.putenv "LANG" "en_US.UTF-8" ;
  Unix.putenv "TZ" "UTC" ;

  (* Disable any randomness in octez-manager *)
  Unix.putenv "OCTEZ_MANAGER_TEST_MODE" "1" ;

  (* Override port-in-use checks so the form always gets clean default ports.
     Common.is_port_in_use is used by the System capability registered via
     Capabilities.register(); Port_validation has its own independent check.
     We set the ref-based overrides AND re-register the System capability
     with a mock is_port_in_use to be fully robust against linking issues. *)
  Octez_manager_lib.Common.set_port_in_use_override (fun _port -> false) ;
  Octez_manager_lib.Port_validation.set_port_in_use_override (fun _port ->
      false) ;
  Octez_manager_lib.Port_validation.clear_port_process_cache () ;
  (* Re-register System capability with is_port_in_use always returning false *)
  let module Mock_system : Octez_manager_lib.Manager_interfaces.System = struct
    include Octez_manager_lib.Common

    let is_port_in_use _port = false
  end in
  Miaou_interfaces.Capability.register
    Octez_manager_lib.Manager_interfaces.System_capability.key
    (module Mock_system : Octez_manager_lib.Manager_interfaces.System) ;

  (* Register deterministic Miaou system capability *)
  Miaou_interfaces.System.set deterministic_miaou_system ;

  (* Set headless driver to fixed size (80x24 is standard) *)
  Lib_miaou_internal.Headless_driver.set_size 24 80 ;

  (* Set iteration limits to prevent hangs *)
  Lib_miaou_internal.Headless_driver.set_limits
    ~iterations:1000
    ~seconds:30.0
    () ;

  (* Clear all state *)
  Lib_miaou_internal.Headless_driver.Key_queue.clear () ;
  Lib_miaou_internal.Headless_driver.Screen.clear () ;
  Miaou.Core.Modal_manager.clear () ;

  (* Create test root directory if needed *)
  let rec mkdir_p path =
    if not (Sys.file_exists path) then (
      let parent = Filename.dirname path in
      if parent <> path then mkdir_p parent ;
      try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  mkdir_p test_root ;
  mkdir_p (test_root ^ "/home") ;
  mkdir_p (test_root ^ "/.config") ;
  mkdir_p (test_root ^ "/.local/share") ;
  mkdir_p (test_root ^ "/.local/state")

(* ============================================================ *)
(* Test Cleanup *)
(* ============================================================ *)

(** Cleanup test environment (call after tests) *)
let cleanup_deterministic_env () =
  reset_time () ;
  reset_temp_counter ()

(* ============================================================ *)
(* Deterministic Data Generators *)
(* ============================================================ *)

(** Generate deterministic service data for testing *)
let mock_service_list () =
  [
    ("node-mainnet", "node", "active", "enabled", "mainnet");
    ("node-ghostnet", "node", "active", "disabled", "ghostnet");
    ("baker-mainnet", "baker", "inactive", "enabled", "mainnet");
    ("accuser-mainnet", "accuser", "active", "enabled", "mainnet");
    ("dal-mainnet", "dal-node", "failed", "disabled", "mainnet");
  ]

(** Generate deterministic binary versions *)
let mock_binary_versions () =
  [
    ("24.0", "/usr/local/bin", true);
    ("23.1", "/opt/octez/23.1", false);
    ("23.0", "/opt/octez/23.0", false);
  ]

(** Generate deterministic network list *)
let mock_networks () =
  [
    ("mainnet", "NetXdQprcVkpaWU", "Mainnet", "https://mainnet.teztnets.com");
    ("ghostnet", "NetXnHfVqm9iesp", "Ghostnet", "https://ghostnet.teztnets.com");
    ("parisnet", "NetXXQqwsmK8xHF", "Parisnet", "https://parisnet.teztnets.com");
  ]

(** Generate deterministic snapshot list *)
let mock_snapshots network =
  [
    ( network,
      "rolling",
      "Rolling",
      Some ("https://snapshots.tzinit.org/" ^ network ^ "/rolling") );
    ( network,
      "full",
      "Full",
      Some ("https://snapshots.tzinit.org/" ^ network ^ "/full") );
    ( network,
      "archive",
      "Archive",
      Some ("https://snapshots.tzinit.org/" ^ network ^ "/archive") );
  ]

(** System metrics record type *)
type system_metrics = {
  cpu_percent : float;
  memory_percent : float;
  disk_percent : float;
  network_rx_mbps : float;
  network_tx_mbps : float;
}

(** Generate deterministic system metrics *)
let mock_system_metrics () =
  {
    cpu_percent = 45.2;
    memory_percent = 62.8;
    disk_percent = 50.0;
    network_rx_mbps = 12.5;
    network_tx_mbps = 8.3;
  }

(* ============================================================ *)
(* Time Formatting for Display *)
(* ============================================================ *)

(** Format timestamp deterministically *)
let format_timestamp ?(relative = false) timestamp =
  if relative then
    (* For relative time, always show fixed duration from fixed_timestamp *)
    let diff = timestamp -. fixed_timestamp in
    if diff < 60.0 then "just now"
    else if diff < 3600.0 then
      Printf.sprintf "%d minutes ago" (int_of_float (diff /. 60.0))
    else if diff < 86400.0 then
      Printf.sprintf "%d hours ago" (int_of_float (diff /. 3600.0))
    else Printf.sprintf "%d days ago" (int_of_float (diff /. 86400.0))
  else
    (* Absolute time: always format as 2024-01-15 12:00:00 UTC *)
    "2024-01-15 12:00:00 UTC"

(* ============================================================ *)
(* Export for Testing *)
(* ============================================================ *)

module For_tests = struct
  let get_fixed_timestamp () = fixed_timestamp

  let get_test_root () = test_root

  let with_time timestamp f =
    let old = !mock_time_ref in
    set_fixed_time timestamp ;
    Fun.protect ~finally:(fun () -> mock_time_ref := old) f
end
