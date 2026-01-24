# Testing Roadmap

Current status and next steps for test coverage improvement.

## 📊 Current State (Jan 24, 2026)

**Coverage: ~38-39%** (from 27% initially)
**Target: 70%** (per #425)

### ✅ Completed Today (15 PRs)

**Infrastructure Fixes:**
- Fixed Debian coverage workflow (critical CI fix)
- Established cache management strategy
- Fixed opam initialization issues

**Test PRs:**
- Phase 2: Basic TUI tests for all install forms (node, baker, accuser, DAL)
- Phase 3: Modal helpers + instances page tests
- Validation: Added validation/error tests to all forms
- Mock System capability for file browser testing

### 📝 Test Files Added

```
test/
├── test_install_node_form.ml       (493 lines) ✅
├── test_install_baker_form.ml      (434 lines) ✅
├── test_install_accuser_form.ml    (406 lines) ✅
├── test_install_dal_node_form.ml   (495 lines) ✅
├── test_instances_page.ml          (260 lines) ✅
├── test_modal_interactions.ml      (274 lines) ✅
└── tui_test_helpers.ml             (9.8KB) ✅ w/ Mock System
```

---

## 🎯 Active Work - Two Parallel Tracks

### Track 1: Quality (Bug Finding) - Issue #457

**Goal:** Improve test quality, find bugs

**Tasks:**
- [ ] Add required field validation tests to all forms
- [ ] Add invalid input handling tests (bad ports, paths, names)
- [ ] Add edge case tests (long names, special chars, duplicates)
- [ ] Add error condition tests (binary not found, permissions)
- [ ] Add full workflow tests (install → start → verify)

**Success Criteria:**
- Each form has ≥5 validation tests
- Each form has ≥3 edge case tests
- ≥2 full workflow tests
- Find ≥1 real bug

**Effort:** 2-3 days  
**Coverage Impact:** +1-2%  
**Quality Impact:** HIGH

### Track 2: Infrastructure - Issue #458

**Goal:** Enable testing of service operations

**Tasks:**
- [ ] Create `test/mocks/mock_systemd.ml` infrastructure
- [ ] Mock service state registry (running, stopped, failed)
- [ ] Mock systemctl commands (start, stop, restart, enable)
- [ ] Failure injection (permission denied, timeouts, etc.)
- [ ] Integration with test helpers
- [ ] Document in TUI_TESTING_GUIDE.md

**What This Unlocks:**
- Testing instances_actions.ml (644 points, 0% → 60%)
- Testing cmd_instance.ml (453 points, 0% → 40%)
- Service lifecycle tests
- Error scenario tests

**Effort:** 3-5 days  
**Coverage Impact:** +5% (~1,000 points)  
**Quality Impact:** VERY HIGH (tests critical operations)

---

## 📈 Path to 70% Coverage

**Current:** 38% (7,988 / 20,935 points)  
**Target:** 70% (14,655 / 20,935 points)  
**Gap:** 6,667 points

**Realistic Roadmap:**

### Phase 4: Quality + Infrastructure (Current)
- #457: Validation tests → +100 points, HIGH quality
- #458: Systemd mocks → +1,000 points, unlock service testing
- **Result: ~43% coverage**

### Phase 5: High-Value Targets
- #443: instances_render.ml tests → +500 points (+2.4%)
- #444: form_builder tests → +530 points (+3.6%)
- Complete modal helpers → +432 points
- **Result: ~52% coverage**

### Phase 6: CLI & Integration
- CLI command tests → +500 points
- Additional TUI workflows → +500 points
- Service lifecycle tests → +500 points
- **Result: ~58% coverage**

### Phase 7: Comprehensive Coverage
- Deeper integration tests
- Error path coverage
- Edge case coverage
- **Result: 65-70% coverage**

**Timeline:**
- **50% coverage:** 2-3 weeks
- **60% coverage:** 4-6 weeks
- **70% coverage:** 8-10 weeks

---

## 🎪 Known Coverage Gaps

### High Priority (0-20% coverage)
1. **Service operations** (instances_actions.ml) - Blocked on #458
2. **CLI commands** (cmd_*.ml) - Need integration tests
3. **Form infrastructure** (form_builder.ml) - Need unit tests
4. **Instance rendering** (instances_render.ml) - Need TUI tests

### Medium Priority (20-50% coverage)
5. Installation flows (installer/*.ml)
6. Binary management
7. Configuration parsing

### Low Priority (<50% or type definitions)
8. Navigation flows
9. Type-only modules
10. Unused features (snapshots page)

---

## 🔧 Available Infrastructure

### Test Helpers
- `tui_test_helpers.ml`: Mock System, setup/cleanup, assertions
- `HD` (Headless_driver): Simulated terminal for TUI testing
- Mock capabilities for file/directory operations

### Documentation
- `docs/TUI_TESTING_GUIDE.md`: Complete TUI testing guide
- `docs/COVERAGE.md`: Coverage infrastructure
- Integration test examples in `test/integration/`

### Mocks Available
✅ Mock System (file browser, file ops)  
❌ Mock systemd (Issue #458)  
❌ Mock binaries (could add)  
❌ Mock network (could add)

---

## 🚀 Getting Started

### Working on Issue #457 (Validation Tests)

1. Pick a form to enhance (e.g., `test_install_node_form.ml`)
2. Add validation test cases:
   ```ocaml
   let test_invalid_rpc_port () =
     setup_test_env ();
     HD.Stateful.init (module Install_node_form_v3);
     (* Navigate to RPC port field *)
     HD.feed_keys ["Down"; "Down"; "Down"];
     (* Enter invalid port *)
     HD.feed_string "99999";
     HD.feed_keys ["Enter"];
     (* Verify error shown *)
     let screen = HD.get_screen_content () in
     assert_contains screen "Invalid port";
     cleanup ()
   ```
3. Run: `dune runtest`
4. Document bugs found

### Working on Issue #458 (Systemd Mocks)

1. Create `test/mocks/mock_systemd.ml`:
   ```ocaml
   type service_state = Stopped | Running | Failed of string
   
   let services : (string, service_state) Hashtbl.t = Hashtbl.create 17
   
   let register_service name ~state =
     Hashtbl.replace services name state
   
   let start_service name =
     match Hashtbl.find_opt services name with
     | None -> Error "Service not found"
     | Some Stopped -> 
         Hashtbl.replace services name Running;
         Ok ()
     | Some Running -> Error "Already running"
     | Some (Failed _) -> Error "Service in failed state"
   ```
2. Add hook in `src/common.ml` to intercept systemctl calls in test mode
3. Create `test/test_instance_actions.ml` with tests
4. Document usage

---

## 📊 Success Metrics

### Quality Metrics
- [ ] Tests find ≥3 real bugs
- [ ] All critical operations have test coverage
- [ ] Error paths tested
- [ ] Edge cases covered

### Coverage Metrics
- [ ] 50% overall coverage by Feb 14
- [ ] 60% overall coverage by Feb 28
- [ ] 70% overall coverage by Mar 31

### Development Metrics
- [ ] Test suite runs in <30 seconds
- [ ] CI completes in <15 minutes
- [ ] Coverage reports on all PRs
- [ ] No flaky tests

---

## 🔗 Related Issues

- #425: [META] Test Coverage Improvement: 27% → 70%
- #457: Improve form validation and error path coverage (NEW)
- #458: Build systemd mock infrastructure (NEW)
- #443: instances_render.ml tests (TO CREATE)
- #444: form_builder infrastructure tests (TO CREATE)

---

**Last Updated:** Jan 24, 2026  
**Next Review:** When #457 or #458 complete
