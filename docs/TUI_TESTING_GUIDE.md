# TUI Testing Guide for Octez Manager

Complete guide for writing TUI tests using the Miaou headless driver.

---

## Table of Contents

1. [Overview](#overview)
2. [Testing Architecture](#testing-architecture)
3. [Quick Start: Your First TUI Test](#quick-start-your-first-tui-test)
4. [Testing Patterns](#testing-patterns)
5. [Helper Functions Reference](#helper-functions-reference)
6. [Common Scenarios](#common-scenarios)
7. [Troubleshooting](#troubleshooting)

---

## Overview

### What is Headless TUI Testing?

The Miaou library provides a **headless driver** that allows testing TUI applications without a real terminal. It simulates:
- Terminal size and rendering
- Keyboard input
- Screen content capture
- Modal state management

### What We Test

- **Page navigation** - Moving between screens
- **Key handling** - Responding to user input
- **Modal interactions** - Forms, confirmations, wizards
- **State transitions** - Service lifecycle, data updates
- **Rendered output** - Verifying displayed content

### What We DON'T Test

- Real terminal rendering (colors, cursor positioning)
- Actual systemd operations (mocked)
- Real network I/O (stubbed)
- Performance/timing (tests are deterministic)

---

## Testing Architecture

### Test Structure

```
test/
├── unit_tests.ml           # Non-TUI unit tests
├── tui_flow_tests.ml       # TUI flow tests (minimal examples exist)
├── tui_test_helpers.ml     # Reusable helpers
└── mocks/                  # (to be created)
    ├── mock_systemd.ml     # Stub systemd operations
    └── mock_services.ml    # Test service fixtures
```

### Test Execution Flow

```ocaml
(* 1. Setup test environment *)
let tmp_dir = setup_test_env ()

(* 2. Initialize headless driver *)
HD.set_size 24 80  (* rows, cols *)
HD.Stateful.init (module My_Page)

(* 3. Feed simulated key presses *)
HD.feed_keys ["Down"; "Down"; "Enter"]

(* 4. Verify screen output *)
let screen = HD.get_screen_content ()
assert_contains screen "Expected text"

(* 5. Cleanup *)
cleanup tmp_dir
```

---

## Quick Start: Your First TUI Test

### Example: Test Instances Page Initialization

```ocaml
(* test/tui_flow_tests.ml *)

open Alcotest
module HD = Lib_miaou_internal.Headless_driver
module Instances = Octez_manager_ui.Instances

let test_instances_page_init () =
  (* Setup: Create temporary config directories *)
  let tmp_dir = Filename.temp_dir_name ^ "/octez-test-" ^ string_of_int (Unix.getpid ()) in
  Unix.mkdir tmp_dir 0o700;
  Unix.putenv "XDG_CONFIG_HOME" (tmp_dir ^ "/config");
  Unix.putenv "XDG_DATA_HOME" (tmp_dir ^ "/data");
  
  (* Configure headless driver *)
  HD.set_size 24 80;  (* 24 rows, 80 columns *)
  HD.set_limits ~iterations:100 ~seconds:5.0 ();
  HD.Key_queue.clear ();
  HD.Screen.clear ();
  
  (* Initialize the page *)
  HD.Stateful.init (module Instances.Page);
  
  (* Verify initial render succeeded *)
  let screen = HD.get_screen_content () in
  Alcotest.(check bool) "Page renders" true (String.length screen > 0);
  
  (* Check for expected header text *)
  let contains text =
    let screen = HD.get_screen_content () in
    String.contains_substring screen text
  in
  Alcotest.(check bool) "Shows instances header" true (contains "Instances");
  
  (* Cleanup *)
  let rec rm_rf path =
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
    ) else Sys.remove path
  in
  if Sys.file_exists tmp_dir then rm_rf tmp_dir
```

### Running the Test

```bash
# Run all TUI tests
dune runtest

# Run specific test
dune exec test/tui_flow_tests.exe -- test "instances_page_init"

# With verbose output
dune exec test/tui_flow_tests.exe -- test "instances_page_init" -v
```

---

## Testing Patterns

### Pattern 1: Testing Navigation

Test moving between list items with arrow keys:

```ocaml
let test_navigation () =
  HD.Stateful.init (module Instances.Page);
  
  (* Initially first item selected *)
  let screen1 = HD.get_screen_content () in
  Alcotest.(check bool) "First item highlighted" true 
    (contains_highlighted screen1 "node-main");
  
  (* Press Down to move to second item *)
  HD.feed_keys ["Down"];
  let screen2 = HD.get_screen_content () in
  Alcotest.(check bool) "Second item highlighted" true
    (contains_highlighted screen2 "baker-main");
```

**Helper needed:**
```ocaml
let contains_highlighted screen text =
  (* Strip ANSI codes and check for selection indicator *)
  let stripped = strip_ansi screen in
  String.contains_substring stripped ("> " ^ text)
```

### Pattern 2: Testing Modal Interaction

Test opening a confirmation modal and accepting:

```ocaml
let test_stop_service_confirmation () =
  setup_test_service ~instance:"test-node" ();
  
  HD.Stateful.init (module Instances.Page);
  
  (* Navigate to service *)
  navigate_to_service "test-node";
  
  (* Press 's' to stop *)
  HD.feed_keys ["s"];
  
  (* Wait for modal to open *)
  wait_until_modal_active ();
  
  (* Verify modal content *)
  let screen = HD.get_screen_content () in
  Alcotest.(check bool) "Shows confirmation" true
    (String.contains_substring screen "Stop service");
  
  (* Accept with Enter *)
  HD.feed_keys ["Enter"];
  
  (* Wait for modal to close *)
  wait_until_no_modal ();
  
  (* Verify service stopped (in mock) *)
  let status = Mock_systemd.get_status "node" "test-node" in
  Alcotest.(check string) "Service stopped" "inactive" status
```

### Pattern 3: Testing Form Wizards

Test multi-step form completion:

```ocaml
let test_install_node_wizard () =
  HD.Stateful.init (module Install_node_form.Page);
  
  (* Step 1: Instance name *)
  HD.feed_keys ["Enter"];  (* Open instance name modal *)
  wait_until_modal_active ();
  type_text "test-node";
  HD.feed_keys ["Enter"];  (* Confirm *)
  
  (* Step 2: Network selection *)
  HD.feed_keys ["Down"];   (* Move to next field *)
  HD.feed_keys ["Enter"];  (* Open network dropdown *)
  wait_until_modal_active ();
  HD.feed_keys ["Down"; "Down"];  (* Select shadownet *)
  HD.feed_keys ["Enter"];
  
  (* Step 3: RPC address *)
  HD.feed_keys ["Down"];
  HD.feed_keys ["Enter"];
  wait_until_modal_active ();
  type_text "127.0.0.1:8732";
  HD.feed_keys ["Enter"];
  
  (* ... continue for all steps ... *)
  
  (* Final step: Submit *)
  HD.feed_keys ["Down"; "Down"; "Down"];  (* Navigate to submit *)
  HD.feed_keys ["Enter"];
  
  (* Verify success *)
  wait_for_screen_content "Installation complete";
```

### Pattern 4: Testing Screen Snapshots

Compare entire screen output against expected snapshot:

```ocaml
let test_empty_instances_page () =
  (* Ensure no services registered *)
  Mock_services.clear_all ();
  
  HD.Stateful.init (module Instances.Page);
  
  let expected = {|
╔════════════════════════════════════════════════════════════════════════════╗
║ Octez Manager - Instances                                                 ║
╠════════════════════════════════════════════════════════════════════════════╣
║                                                                            ║
║  No managed instances found.                                               ║
║                                                                            ║
║  Press 'i' to install a new instance                                       ║
║                                                                            ║
╚════════════════════════════════════════════════════════════════════════════╝
|} in
  
  let actual = HD.get_screen_content () in
  assert_screen_snapshot expected actual
```

---

## Helper Functions Reference

### Test Environment Helpers

```ocaml
(** Setup isolated test environment with temp directories *)
val setup_test_env : unit -> string
(* Returns path to temp directory. Sets XDG env vars. *)

(** Cleanup test environment *)
val cleanup_test_env : string -> unit
(* Recursively deletes temp directory *)

(** Run test with automatic setup/cleanup *)
val with_test_env : (unit -> unit) -> unit
```

**Implementation:**
```ocaml
let setup_test_env () =
  let tmp = Filename.temp_file "octez-test-" "" in
  Unix.unlink tmp;
  Unix.mkdir tmp 0o700;
  Unix.putenv "XDG_CONFIG_HOME" (tmp ^ "/config");
  Unix.putenv "XDG_DATA_HOME" (tmp ^ "/data");
  Unix.putenv "XDG_STATE_HOME" (tmp ^ "/state");
  tmp

let cleanup_test_env tmp =
  let rec rm_rf path =
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name -> rm_rf (Filename.concat path name));
      Unix.rmdir path
    ) else Sys.remove path
  in
  if Sys.file_exists tmp then rm_rf tmp

let with_test_env f =
  let tmp = setup_test_env () in
  Fun.protect ~finally:(fun () -> cleanup_test_env tmp) f
```

### Screen Content Helpers

```ocaml
(** Strip ANSI escape codes from string *)
val strip_ansi : string -> string

(** Check if screen contains substring *)
val assert_contains : string -> string -> unit

(** Wait until specific text appears on screen *)
val wait_for_screen_content : string -> unit

(** Compare screen against snapshot *)
val assert_screen_snapshot : string -> string -> unit
```

**Implementation:**
```ocaml
let strip_ansi s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else if s.[i] = '\027' then
      (* Skip until letter (end of escape sequence) *)
      let rec skip j =
        if j >= len then j
        else
          let c = s.[j] in
          if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then j + 1
          else skip (j + 1)
      in
      loop (skip (i + 1))
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1)
    )
  in
  loop 0

let assert_contains screen needle =
  let stripped = strip_ansi screen in
  if not (String.contains_substring stripped needle) then
    Alcotest.fail (Printf.sprintf "Screen does not contain '%s'" needle)

let wait_for_screen_content text =
  let rec wait attempts =
    if attempts <= 0 then
      Alcotest.fail (Printf.sprintf "Timeout waiting for '%s'" text)
    else
      let screen = HD.get_screen_content () in
      if String.contains_substring (strip_ansi screen) text then ()
      else (
        Unix.sleepf 0.1;
        wait (attempts - 1)
      )
  in
  wait 50  (* 5 second timeout *)

let assert_screen_snapshot expected actual =
  let exp = String.trim (strip_ansi expected) in
  let act = String.trim (strip_ansi actual) in
  if exp <> act then
    Alcotest.fail (Printf.sprintf "Snapshot mismatch:\nExpected:\n%s\n\nActual:\n%s" exp act)
```

### Modal Helpers

```ocaml
(** Wait until a modal becomes active *)
val wait_until_modal_active : unit -> unit

(** Wait until no modal is active *)
val wait_until_no_modal : unit -> unit

(** Type text character by character *)
val type_text : string -> unit
```

**Implementation:**
```ocaml
let wait_until_modal_active () =
  let open Miaou.Core.Modal_manager in
  let rec wait attempts =
    if attempts <= 0 then
      Alcotest.fail "Timeout waiting for modal to open"
    else if has_active () then ()
    else (
      Unix.sleepf 0.1;
      wait (attempts - 1)
    )
  in
  wait 50

let wait_until_no_modal () =
  let open Miaou.Core.Modal_manager in
  let rec wait attempts =
    if attempts <= 0 then
      Alcotest.fail "Timeout waiting for modal to close"
    else if not (has_active ()) then ()
    else (
      Unix.sleepf 0.1;
      wait (attempts - 1)
    )
  in
  wait 50

let type_text str =
  String.iter (fun c ->
    HD.feed_keys [String.make 1 c]
  ) str
```

### Navigation Helpers

```ocaml
(** Navigate to specific service in list *)
val navigate_to_service : string -> unit

(** Navigate to action by label *)
val navigate_to_action : string -> unit

(** Press Down N times *)
val press_down : int -> unit
```

**Implementation:**
```ocaml
let navigate_to_service instance =
  (* Read current screen to find instance position *)
  let screen = HD.get_screen_content () in
  let lines = String.split_on_char '\n' screen in
  let rec find_index idx = function
    | [] -> Alcotest.fail (Printf.sprintf "Service '%s' not found" instance)
    | line :: rest ->
        if String.contains_substring line instance then idx
        else find_index (idx + 1) rest
  in
  let index = find_index 0 lines in
  (* Press Down to reach that index *)
  for _ = 1 to index do
    HD.feed_keys ["Down"]
  done

let press_down n =
  for _ = 1 to n do
    HD.feed_keys ["Down"]
  done

let navigate_to_action label =
  (* Similar to navigate_to_service but looks for action labels *)
  let rec try_navigate attempts =
    if attempts <= 0 then
      Alcotest.fail (Printf.sprintf "Action '%s' not found" label)
    else
      let screen = HD.get_screen_content () in
      if String.contains_substring screen ("> " ^ label) then ()
      else (
        HD.feed_keys ["Down"];
        try_navigate (attempts - 1)
      )
  in
  try_navigate 20
```

### Service Mock Helpers

```ocaml
(** Create a mock service for testing *)
val setup_test_service : 
  ?role:string ->
  ?network:string ->
  ?status:string ->
  instance:string ->
  unit ->
  unit

(** Clear all mock services *)
val clear_all_services : unit -> unit
```

**Implementation (create `test/mocks/mock_services.ml`):**
```ocaml
open Octez_manager_lib

let services : (string, Service.t) Hashtbl.t = Hashtbl.create 16

let setup_test_service 
    ?(role="node") 
    ?(network="mainnet") 
    ?(status="active")
    ~instance 
    () =
  let service = {
    Service.instance;
    role;
    network;
    data_dir = "/tmp/test-" ^ instance;
    rpc_addr = "127.0.0.1:8732";
    net_addr = "0.0.0.0:9732";
    history_mode = History_mode.Rolling;
    service_user = "tezos";
    app_bin_dir = "/usr/local/bin";
    logging_mode = Logging_mode.Stdout;
    extra_env = [];
    extra_args = [];
    depends_on = None;
    dependents = [];
    created_at = "2026-01-01T00:00:00Z";
  } in
  Hashtbl.replace services instance service;
  
  (* Also register with Service_registry if needed *)
  let _ = Service_registry.write service in
  ()

let clear_all_services () =
  Hashtbl.clear services;
  (* Clear actual registry too *)
  let _ = Service_registry.read_all () |> Result.map (fun svcs ->
    List.iter (fun svc -> Service_registry.remove ~instance:svc.Service.instance |> ignore) svcs
  ) in
  ()

let get_service instance =
  Hashtbl.find_opt services instance
```

---

## Common Scenarios

### Scenario 1: Test Service Start

```ocaml
let test_start_stopped_service () =
  with_test_env (fun () ->
    (* Setup: Service exists but is stopped *)
    setup_test_service ~instance:"test-node" ~status:"inactive" ();
    
    (* Open instances page *)
    HD.Stateful.init (module Instances.Page);
    
    (* Navigate to service *)
    navigate_to_service "test-node";
    
    (* Press 's' to start (or whatever key starts) *)
    HD.feed_keys ["s"];
    
    (* Confirm in modal if needed *)
    wait_until_modal_active ();
    HD.feed_keys ["Enter"];
    wait_until_no_modal ();
    
    (* Verify status changed *)
    wait_for_screen_content "running";
  )
```

### Scenario 2: Test Form Validation

```ocaml
let test_invalid_rpc_address () =
  with_test_env (fun () ->
    HD.Stateful.init (module Install_node_form.Page);
    
    (* Navigate to RPC field *)
    navigate_to_field "RPC Address";
    HD.feed_keys ["Enter"];
    wait_until_modal_active ();
    
    (* Enter invalid address *)
    type_text "invalid:address";
    HD.feed_keys ["Enter"];
    
    (* Modal should stay open with error *)
    let screen = HD.get_screen_content () in
    assert_contains screen "Invalid RPC address";
    
    (* Modal still active *)
    Alcotest.(check bool) "Modal still open" true
      (Miaou.Core.Modal_manager.has_active ());
  )
```

### Scenario 3: Test Multi-Step Wizard

```ocaml
let test_complete_node_install_wizard () =
  with_test_env (fun () ->
    HD.Stateful.init (module Install_node_form.Page);
    
    (* Define wizard steps *)
    let steps = [
      ("instance_name", fun () ->
        HD.feed_keys ["Enter"];
        wait_until_modal_active ();
        type_text "my-node";
        HD.feed_keys ["Enter"];
        wait_until_no_modal ();
      );
      
      ("network", fun () ->
        press_down 1;
        HD.feed_keys ["Enter"];
        wait_until_modal_active ();
        press_down 2;  (* Select shadownet *)
        HD.feed_keys ["Enter"];
        wait_until_no_modal ();
      );
      
      ("rpc_address", fun () ->
        press_down 1;
        HD.feed_keys ["Enter"];
        wait_until_modal_active ();
        (* Accept default *)
        HD.feed_keys ["Enter"];
        wait_until_no_modal ();
      );
      
      (* ... more steps ... *)
      
      ("submit", fun () ->
        press_down 5;  (* Navigate to submit button *)
        HD.feed_keys ["Enter"];
      );
    ] in
    
    (* Execute all steps *)
    List.iter (fun (name, action) ->
      Printf.printf "Executing step: %s\n" name;
      action ()
    ) steps;
    
    (* Verify completion *)
    wait_for_screen_content "Installation complete";
    
    (* Verify service created *)
    match Service_registry.find ~instance:"my-node" with
    | Ok (Some _) -> ()
    | _ -> Alcotest.fail "Service not created"
  )
```

---

## Troubleshooting

### Test Hangs or Times Out

**Problem:** Test waits forever for modal/content.

**Solutions:**
1. Check modal is actually triggered:
   ```ocaml
   Printf.printf "Has modal: %b\n" (Miaou.Core.Modal_manager.has_active ());
   ```

2. Print screen content to debug:
   ```ocaml
   let screen = HD.get_screen_content () in
   Printf.printf "Screen:\n%s\n" screen;
   ```

3. Increase timeout:
   ```ocaml
   HD.set_limits ~iterations:200 ~seconds:10.0 ();
   ```

### Screen Content Doesn't Match

**Problem:** `assert_contains` or snapshot fails.

**Solutions:**
1. Print actual content:
   ```ocaml
   let screen = HD.get_screen_content () in
   Printf.printf "Actual screen:\n===\n%s\n===\n" screen;
   ```

2. Strip ANSI codes:
   ```ocaml
   let stripped = strip_ansi screen in
   Printf.printf "Stripped:\n%s\n" stripped;
   ```

3. Check for partial matches:
   ```ocaml
   (* Instead of exact match, check substrings *)
   assert_contains screen "expected fragment";
   ```

### Services Not Found

**Problem:** `navigate_to_service` fails.

**Solutions:**
1. Verify service registry:
   ```ocaml
   let services = Service_registry.list () |> Result.get_ok in
   List.iter (fun s -> Printf.printf "Service: %s\n" s.Service.instance) services;
   ```

2. Check cache refresh:
   ```ocaml
   (* Force cache refresh before navigating *)
   Data.refresh_cache ();
   ```

### Key Presses Don't Work

**Problem:** Navigation keys don't move cursor.

**Solutions:**
1. Check page is initialized:
   ```ocaml
   HD.Stateful.init (module My_Page);
   (* Wait a tick for initialization *)
   Unix.sleepf 0.1;
   ```

2. Use correct key names:
   ```ocaml
   (* Correct *)
   HD.feed_keys ["Down"; "Up"; "Enter"; "Escape"]
   
   (* Incorrect *)
   HD.feed_keys ["down"; "enter"]  (* Wrong case *)
   ```

---

## Adding to `dune` File

Ensure your test executable includes TUI test files:

```lisp
(test
 (name tui_flow_tests)
 (libraries
   octez_manager_lib
   octez_manager_ui
   lib_miaou_internal
   miaou
   alcotest)
 (modules tui_flow_tests tui_test_helpers mock_services))
```

---

## Next Steps

1. **Start small:** Test page initialization
2. **Add helpers:** Build up your `tui_test_helpers.ml`
3. **Test workflows:** Full user journeys (install, start, stop)
4. **Add mocks:** Stub systemd, RPC, file I/O
5. **Snapshot tests:** Capture and compare full screens
6. **Iterate:** Refactor helpers as patterns emerge

Good luck! 🚀
