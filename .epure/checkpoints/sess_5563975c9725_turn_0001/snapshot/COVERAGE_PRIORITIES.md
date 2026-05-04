# Test Coverage Priorities for Octez Manager

**⚠️ CRITICAL: Reported Coverage is Misleading!**

**Bisect_ppx Report:** 56.99% (6688/11735 points) - *Only counts 53 executed files*  
**TRUE Coverage:** ~27% (6688/~25000 points) - *Includes all 103 source files*  
**Gap Analysis:** 50 files with 0% coverage = 18,770 lines untested (all TUI components)  
**Potential Impact:** Adding comprehensive TUI tests could push coverage from **27% → 70%+**

### Why the Discrepancy?

bisect_ppx only reports files that were **instrumented AND executed**. The 50 uncovered UI files are completely invisible in the default report, hiding ~13,000 coverage points (~53% of the codebase).

---

## Priority 1: TUI Golden Path Tests (CRITICAL - Est. +15-20% coverage)

**Impact:** These 50 uncovered files represent ~4000-5000 lines of untested code.

### Inspiration from octez-setup

The `octez_setup` repository has excellent patterns we should replicate:

- **`tui_smoke_tests.ml`** (32KB): Comprehensive flow tests using headless driver
- **`tui_test_helpers.ml`** (11KB): Reusable test infrastructure
- **Golden path testing**: Full user workflows end-to-end
- **Snapshot assertions**: Screen content validation
- **Modal interaction testing**: Form wizards, confirmation dialogs

### Files to Test (Highest Impact First)

#### 1. Form Wizards (Est. +5% coverage)
**Impact: HIGH** - These are core user workflows

- `src/ui/pages/install_node_form_v3.ml` (1217 lines) - **CRITICAL**
  - Test: Full node installation wizard flow
  - Snapshot config step, network selection, RPC address validation
  - Bootstrap options (genesis vs snapshot)
  - Completion and navigation to instances page

- `src/ui/pages/install_baker_form_v3.ml` (747 lines)
  - Test: Baker setup wizard with delegate selection
  - Base directory creation
  - Delegate key validation
  - Parent node association

- `src/ui/pages/install_accuser_form_v3.ml` (526 lines)
  - Test: Accuser installation with protocol selection

- `src/ui/pages/install_dal_node_form_v3.ml` (471 lines)
  - Test: DAL node setup wizard

**Testing Pattern (from octez-setup):**
```ocaml
let test_install_node_wizard () =
  TH.run_test_flow "install_node_complete" (fun () ->
    HD.Stateful.init (module Install_node_form_v3.Page);
    
    (* Step 1: Instance name *)
    HD.feed_keys ["Enter"];
    TH.wait_until_modal_active ();
    HD.feed_keys ["t"; "e"; "s"; "t"; "-"; "n"; "o"; "d"; "e"];
    HD.feed_keys ["Enter"];
    
    (* Step 2: Network selection *)
    HD.feed_keys ["Down"]; (* Move to shadownet *)
    HD.feed_keys ["Enter"];
    
    (* Step 3: RPC address *)
    TH.wait_until_modal_active ();
    HD.feed_keys ["Enter"]; (* Accept default *)
    
    (* ... continue through all steps *)
    
    (* Verify final screen shows success *)
    let screen = HD.get_screen_content () in
    TH.assert_contains screen "Installation complete";
  )
```

#### 2. Instance Management Page (Est. +4% coverage)
**Impact: HIGH** - Core functionality

- `src/ui/pages/instances/instances_actions.ml` (1328 lines) - **CRITICAL**
  - Test: Start/stop/restart service actions
  - Test: Enable/disable service toggles
  - Test: Remove service with confirmation modal
  - Test: Edit service configuration

- `src/ui/pages/instances/instances_render.ml` (843 lines)
  - Test: Service list rendering with different states
  - Test: Status indicators (running, stopped, failed)
  - Test: Sorting and filtering

- `src/ui/pages/instances.ml` (810 lines)
  - Test: Full instances page navigation
  - Test: Details panel display

**Testing Pattern:**
```ocaml
let test_service_start_stop () =
  TH.run_test_flow "service_lifecycle" (fun () ->
    let instance = TH.setup_test_instance () in
    
    HD.Stateful.init (module Instances.Page);
    
    (* Navigate to test instance *)
    TH.navigate_to_service ~instance:"node-main";
    
    (* Press 's' to stop *)
    HD.feed_keys ["s"];
    TH.wait_for_modal "Confirm stop";
    HD.feed_keys ["Enter"];
    
    (* Verify stopped state in UI *)
    TH.assert_screen_contains "stopped";
  )
```

#### 3. Binaries Page (Est. +2% coverage)

- `src/ui/pages/binaries/binaries_page.ml` (894 lines)
  - Test: Binary version display
  - Test: Download flow
  - Test: Binary validation

#### 4. Diagnostics/Metrics Pages (Est. +2% coverage)

- `src/ui/pages/diagnostics/diagnostics_page.ml` (629 lines)
  - Test: Chart rendering (CPU, memory, disk)
  - Test: RPC metrics display
  - Test: System health indicators

- `src/ui/pages/diagnostics/charts.ml` (394 lines)
  - Test: Time-series data visualization
  - Test: Chart legend and labels

#### 5. Form Builder Infrastructure (Est. +2% coverage)

- `src/ui/form_builder.ml` (714 lines) - **Foundation for all wizards**
  - Test: Field validation
  - Test: Form state management
  - Test: Step navigation (forward/backward)
  - Test: Conditional field display

- `src/ui/form_builder_bundles.ml` (626 lines)
  - Test: Pre-configured field bundles
  - Test: Network selection bundle
  - Test: Address validation bundle

---

## Priority 2: Background Schedulers (MEDIUM - Est. +3-4% coverage)

**Impact:** Critical for TUI but easier to test than UI

### Files with Business Logic (No UI Dependency)

#### System Metrics Scheduler
- `src/ui/system_metrics_scheduler.ml` (568 lines)
  - **Testable:** CPU/memory/disk metrics collection
  - **Pattern:** Mock `ps`, `df`, `free` command outputs
  - **Test:** Verify metric parsing and caching

```ocaml
let test_cpu_metrics () =
  (* Stub Common.run_out to return fake ps output *)
  let stub_ps = "  PID  %CPU  %MEM COMMAND\n 1234  45.2   3.1 octez-node" in
  
  (* Call refresh and verify metrics *)
  System_metrics_scheduler.refresh ~instance:"test-node";
  
  let cpu = System_metrics_scheduler.get_cpu ~instance:"test-node" in
  Alcotest.(check (float 0.1)) "CPU usage" 45.2 cpu;
```

#### RPC Scheduler
- `src/ui/rpc_scheduler.ml` (~400 lines estimated)
  - **Testable:** RPC response parsing
  - **Pattern:** Mock RPC endpoints with canned JSON
  - **Test:** Bootstrap status, head level, protocol parsing

#### Delegate Scheduler  
- `src/ui/delegate_scheduler.ml` (~300 lines estimated)
  - **Testable:** Baker config parsing
  - **Pattern:** Mock env file reads
  - **Test:** Delegate detection, highwatermark parsing

---

## Priority 3: Data Layer & Utilities (EASY - Est. +2-3% coverage)

### Pure Functions (No I/O, Easy to Test)

#### 1. Registry Modules (0% coverage currently)

- `src/directory_registry.ml` (294 lines) - **EASY WINS**
  - **Testable:** JSON serialization/deserialization
  - **Testable:** Directory entry management (add/remove/find)
  - **Testable:** Migration from old format
  - **Testable:** Entry limiting (max 10 per type)

```ocaml
let test_directory_registry_add_and_find () =
  (* Use temp directory for registry *)
  let tmp = temp_dir () in
  set_env "XDG_CONFIG_HOME" tmp;
  
  let result = Directory_registry.add 
    ~path:"/data/node1" 
    ~dir_type:Node_data_dir 
    ~linked_services:["node-main"] in
  
  Alcotest.(check (result unit msg)) "Add succeeds" (Ok ()) result;
  
  let found = Directory_registry.find_by_path "/data/node1" in
  Alcotest.(check bool) "Entry found" true (Result.is_ok found);
```

- `src/keys_reader.ml` (43 lines) - **TRIVIAL**
  - Test JSON parsing of public key hashes
  - Create sample `public_key_hashs` file
  - Verify key info extraction

- `src/cli/cli_progress.ml` (198 lines) - **PURE LOGIC**
  - Test progress bar rendering (ASCII vs Unicode)
  - Test file size formatting
  - Test multi-file progress state updates

#### 2. Low-Coverage Core Modules

- `src/binary_downloader.ml` (24.17% → Target: 70%+)
  - Add tests for checksum verification
  - Add tests for multi-file download progress
  - Mock HTTP requests with test fixtures

- `src/installer/removal.ml` (23.00% → Target: 60%+)
  - Test service removal with dependents
  - Test data directory deletion
  - Test cascade stop of dependent services

---

## Priority 4: CLI Command Coverage (LOW - Est. +1-2% coverage)

**Note:** These are already tested via integration tests, but library coverage isn't captured.

- `src/cli/cmd_instance.ml` (13.71% → Target: 40%+)
  - Extract core logic into testable functions
  - Test argument parsing and validation
  - Test error message generation

---

## Implementation Strategy

### Phase 1: Quick Wins (1-2 days) - **+6% TRUE coverage** (+3% reported)

1. **Registry modules** (directory_registry, keys_reader, cli_progress)
   - Pure functions, no I/O mocking needed
   - JSON serialization tests
   - Simple unit tests with Alcotest

2. **System metrics parsing** (system_metrics_scheduler)
   - Mock command outputs
   - Test metric extraction logic

### Phase 2: Form Wizards (3-5 days) - **+17% TRUE coverage** (+10% reported) - **HIGHEST IMPACT**

1. **Create `test/tui_golden_tests.ml`** (modeled on octez-setup)
   - Reuse existing `test/tui_test_helpers.ml` infrastructure
   - Add helper functions from octez-setup patterns

2. **Install Node Wizard** (install_node_form_v3.ml)
   - Full happy path: genesis bootstrap
   - Full happy path: snapshot bootstrap
   - Error cases: invalid RPC address
   - Error cases: duplicate instance name

3. **Install Baker Wizard** (install_baker_form_v3.ml)
   - Full happy path with single delegate
   - Multiple delegates
   - Base directory selection

4. **Install Accuser & DAL** (lower priority)

### Phase 3: Instances Page (2-3 days) - **+12% TRUE coverage** (+8% reported)

1. **Instances actions** (instances_actions.ml)
   - Service lifecycle: start/stop/restart
   - Service enable/disable
   - Service removal with confirmation
   - Edit service flow

2. **Instances rendering** (instances_render.ml)
   - Status display for various states
   - Service list with mixed states
   - Empty state

### Phase 4: Remaining TUI (2-4 days) - **+8% TRUE coverage** (+5% reported)

1. **Binaries page** - version display, download flow
2. **Diagnostics page** - metrics charts
3. **Import wizard** - external service detection

---

## Testing Infrastructure Needs

### Expand `test/tui_test_helpers.ml`

Add these helpers from octez-setup:

```ocaml
(** Wait for specific screen content to appear *)
val wait_for_screen_content : substring:string -> unit

(** Navigate to specific action by label *)
val navigate_to_action_by_label : label:string -> unit

(** Setup mock service for testing *)
val setup_mock_service : 
  role:string -> 
  instance:string -> 
  Service.t

(** Drive form wizard through all steps *)
val drive_wizard :
  steps:(string * (unit -> unit)) list ->
  unit
```

### Mock Infrastructure

Create `test/mocks/`:
- `mock_systemd.ml` - Stub systemd operations
- `mock_rpc_responses.ml` - Canned RPC JSON responses  
- `mock_command_outputs.ml` - Fake process outputs (ps, df, etc.)

---

## Expected Coverage After Full Implementation

| Category | Current | After Phase 1 | After Phase 2 | After Phase 3 | After Phase 4 |
|----------|---------|---------------|---------------|---------------|---------------|
| **Registries** | 0% | 75% | 75% | 75% | 75% |
| **CLI Progress** | 0% | 80% | 80% | 80% | 80% |
| **Schedulers** | 0% | 60% | 60% | 60% | 60% |
| **Form Wizards** | 0% | 0% | 70% | 70% | 70% |
| **Instances Page** | 0% | 0% | 0% | 65% | 65% |
| **Other TUI** | 0% | 0% | 0% | 0% | 40% |
| **Overall (TRUE)** | **~27%** | **~33%** | **~50%** | **~62%** | **~70%** |
| **Overall (REPORTED)** | **57%** | **62%** | **72%** | **77%** | **80%** |

*Note: REPORTED coverage only includes executed files. TRUE coverage includes all 103 source files.*

---

## Why This Approach Works

1. **Modeled on proven patterns** - octez-setup has 32KB of working TUI tests
2. **Headless driver already works** - We have `lib_miaou_internal.Headless_driver`
3. **Infrastructure exists** - `test/tui_test_helpers.ml` and `test/tui_flow_tests.ml`
4. **High impact** - 50 uncovered files = biggest coverage gap
5. **Validates real workflows** - Tests actual user paths, not just code coverage

---

## Next Steps

1. **Start with Phase 1** (quick wins, 1-2 days)
2. **Validate with CI** after each phase
3. **Iterate on test helpers** as patterns emerge
4. **Add snapshot testing** for UI regressions
5. **Document testing patterns** for future contributors

---

## Appendix: Coverage Calculation Methodology

### The bisect_ppx Limitation

bisect_ppx only reports files that appear in `.coverage` files (i.e., were instrumented and executed during tests). This creates a misleading picture when large parts of the codebase are untested.

### Actual Numbers

```
Source files breakdown:
- Total .ml files: 103
- Files in bisect_ppx report: 53
- Missing from report: 50 (all UI components)

Line count:
- Covered files: 16,557 lines
- Uncovered files: 18,770 lines  
- Total: 35,327 lines

Coverage points (bisect_ppx):
- Reported: 6,688 / 11,735 = 56.99%
- True: 6,688 / ~25,038 = ~26.71%
```

### Why Our Uncovered File Listing Helps

The coverage workflow now explicitly lists all 50 missing files in the report, making the gap visible to developers. This prevents the false sense of security from seeing "57%" when the reality is closer to "27%".

### What Gets Tested Now

✅ **Well-covered (>70%):**
- Service management core (81-94%)
- Binary registry (85%)
- Unit tests themselves (83%)
- Configuration parsing (74-83%)

❌ **Completely untested (0%):**
- All TUI pages (50 files)
- All background schedulers
- All form builders
- All UI utilities

🟡 **Partially tested (<50%):**
- CLI commands (14-47%) - tested via integration tests, not library tests
- Common utilities (42%)
- Installers (23-55%)
- Systemd wrapper (50%)

