# Test Coverage Roadmap: 47% → 75%

**Created:** January 25, 2026  
**Current Coverage:** 47.12% (7,974/16,921 points)  
**Target Coverage:** 75% (12,691/16,921 points)  
**Estimated Effort:** 3-4 weeks  

## Tracking Issues

- **Master Issue:** [#465](https://github.com/trilitech/octez-manager/issues/465) - Overall roadmap tracking
- **Phase 1:** [#466](https://github.com/trilitech/octez-manager/issues/466) - Unit tests (Week 1)
- **Phase 2:** [#467](https://github.com/trilitech/octez-manager/issues/467) - UI tests (Week 2)
- **Phase 3:** [#468](https://github.com/trilitech/octez-manager/issues/468) - System mocking (Weeks 3-4)

## Overview

This roadmap outlines a comprehensive plan to increase test coverage from 47.12% to 75% through three sequential phases:

1. **Phase 1 (Week 1):** Unit tests for core modules → 56% coverage
2. **Phase 2 (Week 2):** UI interaction tests → 68% coverage
3. **Phase 3 (Weeks 3-4):** System mocking & installer tests → 75% coverage

## Phase Breakdown

### Phase 1: Unit Tests for Core Modules (Days 1-4)

**Coverage Gain:** +9% (1,500 points)  
**Tests Added:** ~190 tests  

**Target Modules:**
- `src/common.ml` - String utilities, shell quoting, path operations
- `src/binary_downloader.ml` - Download logic, URL parsing, atomic operations
- `src/systemd.ml` - Unit name generation, service paths, dropin files
- `src/external_service_detector.ml` - Service detection, unit file parsing
- `src/help_parser.ml` - Help output parsing, option extraction
- Plus extensions to existing tests

**Why This First:**
- Quick wins with straightforward unit tests
- Builds momentum for harder phases
- Establishes testing patterns
- No complex mocking required

---

### Phase 2: UI Interaction Tests (Days 5-9)

**Coverage Gain:** +12% (2,000 points)  
**Tests Added:** ~200 tests  

**Target Modules:**
- `src/ui/modal_helpers.ml` - All modal types and interactions
- `src/ui/pages/binaries/binaries_page.ml` - Complete page coverage
- `src/ui/pages/install_*_form_v3.ml` - Advanced form interactions
- `src/ui/metrics.ml` - Metric collection and rendering
- `src/ui/context.ml` - Context management
- `src/ui/binary_help_explorer.ml` - Help navigation

**Key Tests:**
- Modal interactions (confirmation, error, input, file browser)
- Page navigation and state management
- Form validation error states
- Help text and tooltips
- Complex user interaction flows

**Builds On:**
- Existing UI regression framework
- Deterministic mocks from previous work
- Proven patterns from initial 54 regression tests

---

### Phase 3: System Mocking & Installer Tests (Days 10-18)

**Coverage Gain:** +7% (1,217 points)  
**Tests Added:** ~145 tests  

**Part A - System Mocking Framework (Days 10-12):**

Build comprehensive mocking infrastructure:
- **Systemd Mock:** Service operations, daemon-reload, status tracking
- **User/Group Mock:** useradd, groupadd, id commands
- **File System Mock:** mkdir, chown, chmod, file tree management
- **Capabilities Integration:** Hook mocks into octez-manager capabilities

**Part B - Installer Module Tests (Days 13-18):**

Test all installer modules (currently 0% coverage):
- `src/installer/node.ml` - Node installation flow
- `src/installer/baker.ml` - Baker installation
- `src/installer/accuser.ml` - Accuser installation
- `src/installer/dal_node.ml` - DAL node installation
- `src/installer/lifecycle.ml` - Start/stop/restart operations
- `src/installer/removal.ml` - Service removal and cleanup
- `src/installer/config.ml` - Config generation
- `src/installer/snapshot.ml` - Snapshot resolution and import
- `src/installer/import.ml` - Service import (partial)
- `src/installer/import_cascade.ml` - Dependency graphs (partial)

**Why This Last:**
- Most complex phase requiring extensive mocking
- Builds on patterns from Phases 1 & 2
- Unlocks testing of previously untestable code
- Provides infrastructure for future development

---

## Coverage Progression

| Milestone | Coverage | Points | Tests | Cumulative Tests |
|-----------|----------|--------|-------|------------------|
| **Baseline** | 47.12% | 7,974 | 265 | 265 |
| **Phase 1 Complete** | 56% | 9,474 | +190 | 455 |
| **Phase 2 Complete** | 68% | 11,474 | +200 | 655 |
| **Phase 3 Complete** | 75% | 12,691 | +145 | 800 |

## Timeline

```
Week 1: Phase 1 - Unit Tests
├─ Day 1-2: Core utilities (common.ml, binary_downloader.ml)
├─ Day 3: System modules (systemd.ml, external_service_detector.ml)
└─ Day 4: Parsers and consolidation

Week 2: Phase 2 - UI Tests
├─ Day 5-6: Modal and page tests
├─ Day 7-8: Advanced form interactions
└─ Day 9: UI helper modules

Week 3: Phase 3a - System Mocking
├─ Day 10-11: Build mocking framework
└─ Day 12: Test mocks, integrate with capabilities

Week 4: Phase 3b - Installer Tests
├─ Day 13-14: Core installer modules (node, baker, accuser, dal)
├─ Day 15-16: Lifecycle and removal tests
└─ Day 17-18: Import tests and final verification
```

**Total Duration:** 18 working days (~3.5-4 weeks)

## Success Criteria

### Phase 1
- [ ] Coverage reaches 56% or higher
- [ ] ~190 new tests added
- [ ] All tests pass in CI
- [ ] Zero test flakiness

### Phase 2
- [ ] Coverage reaches 68% or higher
- [ ] ~200 new tests added
- [ ] All UI states have regression baselines
- [ ] Modal interactions fully tested

### Phase 3
- [ ] Coverage reaches 75% or higher
- [ ] System mocking framework complete
- [ ] All installer modules >40% covered (from 0%)
- [ ] ~145 new tests added
- [ ] Complete documentation of mocks

### Overall
- [ ] Total coverage at 75%
- [ ] ~535 new tests added
- [ ] All tests deterministic (0% flake rate)
- [ ] CI passes all tests
- [ ] Documentation updated

## Technical Approach

### Testing Patterns

**Unit Tests (Phase 1):**
```ocaml
let test_function_behavior () =
  let input = "test input" in
  let expected = "expected output" in
  let actual = Module.function input in
  check string "function works" expected actual
```

**UI Regression Tests (Phase 2):**
```ocaml
let test_ui_state () =
  TH.with_test_env (fun () ->
    DM.setup_deterministic_env () ;
    HD.Stateful.init (module MyPage) ;
    (* Interact with UI *)
    TH.type_string "input" ;
    ignore (TH.send_key_and_wait "Enter") ;
    (* Verify UI state *)
    URF.assert_ui_regression "state_name")
```

**System Mock Tests (Phase 3):**
```ocaml
let test_installer_operation () =
  System_mocks.setup_mocked_capabilities () ;
  DM.setup_deterministic_env () ;
  
  let result = Installer.operation ~params in
  
  check (result_ok) "succeeded" true (Result.is_ok result) ;
  check bool "service created" true
    (System_mocks.Systemd_mock.service_exists "name")
```

### Mocking Strategy

**Phase 1:** Lightweight mocks
- HTTP responses: Fixed strings
- File system: In-memory hashtables
- Commands: Predefined outputs

**Phase 2:** Deterministic UI mocks
- File browser: Predictable directory listings
- Modals: State-based rendering
- Forms: Validation without I/O

**Phase 3:** Comprehensive system mocks
- Systemd: Full service lifecycle
- Users/Groups: Complete management
- File system: Permissions and ownership

## Risk Assessment

### High Risk Areas
- **System mocking complexity** - Phase 3 may take longer than estimated
- **Installer test interactions** - Complex call chains may be hard to mock

### Mitigation Strategies
1. Start with easiest phase first (Phase 1)
2. Build mocks incrementally, test each component
3. Add coverage checkpoints after each phase
4. Budget buffer time in Phase 3

### Contingency Plans
- If Phase 3 takes too long, target 70% instead of 75%
- Partial coverage of installer modules (40%) is still valuable
- Can defer import.ml and import_cascade.ml to future work

## Dependencies

### Required
- Existing UI regression framework (already built)
- Deterministic mocks module (already built)
- Current test infrastructure

### New Infrastructure
- System mocking framework (Phase 3)
- Extended test helpers (Phase 2)

## Post-Completion Benefits

### Immediate Benefits
- 75% test coverage provides confidence for refactoring
- Installer modules become safely modifiable
- System operations are testable
- CI catches more bugs automatically

### Long-term Benefits
- Infrastructure for testing future installer features
- Patterns for mocking complex system interactions
- Foundation for integration testing
- Developer confidence for changes

## Maintenance

### After Reaching 75%
- Run coverage reports in CI
- Fail builds if coverage drops below 70%
- Add tests for all new features
- Update mocks when system interfaces change

### Coverage Reporting
```bash
# Generate coverage report
dune clean
dune runtest --instrument-with bisect_ppx
bisect-ppx-report summary
bisect-ppx-report html -o _coverage
```

## References

- [UI Regression Testing Guide](./UI_REGRESSION_TESTING.md)
- [Issue #465](https://github.com/trilitech/octez-manager/issues/465) - Master tracking
- [Issue #466](https://github.com/trilitech/octez-manager/issues/466) - Phase 1 details
- [Issue #467](https://github.com/trilitech/octez-manager/issues/467) - Phase 2 details
- [Issue #468](https://github.com/trilitech/octez-manager/issues/468) - Phase 3 details

---

**Status:** Ready to begin Phase 1  
**Next Steps:** Start implementing tests for `src/common.ml` and `src/binary_downloader.ml`
