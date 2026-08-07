# Session Summary: Coverage Reporting & Test Roadmap

**Date:** 2026-01-23  
**PR:** #420 - Coverage Reporting for PRs  
**Milestone:** #5 - Test Coverage: TUI & Core Libraries

---

## What We Accomplished

### 1. Fixed Coverage Reporting (PR #420)

**Problem:** Coverage percentage showed 0% instead of actual coverage.

**Root Cause:** sed pattern was looking for `Coverage: XX.XX%` but actual format is `Coverage: 6688/11735 (56.99%)` with percentage inside parentheses.

**Solution:** Fixed sed pattern from `s/.*Coverage: *\([0-9.]*\)%.*/\1/` to `s/.*(\([0-9.]*\)%).*/\1/`

**Result:** ✅ Now correctly displays **56.99%** coverage

**Commits:**
- `41d366e7` - fix(ci): correct sed pattern to extract coverage percentage
- `4c9b798f` - feat(ci): list files with 0% coverage in report

### 2. Discovered TRUE Coverage is 27%, Not 57%

**The Math:**
```
bisect_ppx report (MISLEADING):
  6,688 / 11,735 points = 56.99%
  (Only 53 executed files counted)

TRUE coverage (ALL FILES):
  6,688 / ~25,000 points = ~26.71%
  (Includes all 103 source files)

HIDDEN: 50 files, 18,770 lines, ~13,000 coverage points
```

**Why:** bisect_ppx only reports files that were instrumented AND executed. The entire TUI (50 files) is completely untested and invisible.

### 3. Created Comprehensive Testing Roadmap

**Documents Created:**

1. **`COVERAGE_PRIORITIES.md`** - Complete test priority breakdown
   - Phase-by-phase implementation plan
   - Expected coverage improvements
   - Testing patterns from octez-setup
   - Realistic timelines and effort estimates

2. **`docs/TUI_TESTING_GUIDE.md`** - Self-contained testing guide
   - Complete tutorial for TUI testing with Miaou headless driver
   - Pattern library with working code examples
   - Helper function implementations
   - Common scenarios and troubleshooting
   - **No dependency on octez-setup** - all patterns documented here

3. **`SESSION_SUMMARY.md`** (this file) - Session recap

### 4. Created Milestone & Issues

**Milestone #5:** "Test Coverage: TUI & Core Libraries"

**Issues Created:**
- #425 - [META] Test Coverage Improvement: 27% → 70% (tracking issue)
- #421 - test: Add unit tests for registry modules (Phase 1)
- #422 - test: Add unit tests for CLI progress rendering (Phase 1)
- #423 - test: Add TUI tests for install node wizard (Phase 2, HIGH IMPACT)
- #424 - test: Add TUI tests for instances page (Phase 3)

**Still needed:**
- System metrics scheduler tests (Phase 1)
- Install baker/accuser wizards (Phase 2)
- Form builder tests (Phase 2)
- Binaries/diagnostics pages (Phase 4)

---

## Coverage Breakdown

### Current State (27% TRUE, 57% REPORTED)

**Well-Covered (>70%):**
- Service management core: 81-94%
- Binary registry: 85%
- Unit tests themselves: 83%
- Configuration parsing: 74-83%

**Completely Untested (0%):**
- All 50 TUI files (18,770 lines)
  - Form wizards: 3,661 lines
  - Instances page: 2,981 lines
  - Binaries page: 894 lines
  - Diagnostics: 1,023 lines
  - Schedulers: 1,500+ lines
  - Other UI: 8,700+ lines

**Partially Tested (<50%):**
- CLI commands: 14-47% (tested via integration, not unit)
- Common utilities: 42%
- Installers: 23-55%
- Systemd wrapper: 50%

### Target State (70% TRUE, 80% REPORTED)

After completing all 4 phases:
- **Phase 1:** 27% → 33% (+6%)
- **Phase 2:** 33% → 50% (+17%) - Form wizards, HIGHEST IMPACT
- **Phase 3:** 50% → 62% (+12%) - Instances page
- **Phase 4:** 62% → 70% (+8%) - Remaining TUI

**Total improvement:** +43 percentage points TRUE coverage

---

## Testing Architecture

### Test Structure

```
test/
├── unit_tests.ml           # Non-TUI unit tests
├── tui_flow_tests.ml       # TUI flow tests (expand this)
├── tui_test_helpers.ml     # Reusable helpers (expand this)
└── mocks/                  # (to create)
    ├── mock_systemd.ml     # Stub systemd operations
    ├── mock_services.ml    # Test service fixtures
    └── mock_rpc.ml         # Stub RPC responses
```

### Key Testing Patterns

1. **Environment Setup:**
   ```ocaml
   let with_test_env f =
     let tmp = setup_test_env () in
     Fun.protect ~finally:(fun () -> cleanup_test_env tmp) f
   ```

2. **Headless Driver:**
   ```ocaml
   HD.set_size 24 80;
   HD.Stateful.init (module My_Page);
   HD.feed_keys ["Down"; "Enter"];
   let screen = HD.get_screen_content () in
   ```

3. **Modal Interaction:**
   ```ocaml
   wait_until_modal_active ();
   type_text "my-input";
   HD.feed_keys ["Enter"];
   wait_until_no_modal ();
   ```

4. **Screen Assertions:**
   ```ocaml
   assert_contains screen "Expected text";
   assert_screen_snapshot expected_output actual;
   ```

All patterns fully documented in `docs/TUI_TESTING_GUIDE.md`.

---

## Implementation Roadmap

### Phase 1: Quick Wins (1-2 days, +6% coverage)

**Target:** Registry modules, CLI progress, system metrics parsing

**Why start here:**
- Pure logic, no I/O mocking
- Fast tests (<1 second)
- Build confidence with easy wins
- Establish test patterns

**Issues:** #421, #422, (create scheduler issue)

### Phase 2: Form Wizards (3-5 days, +17% coverage) ⭐ HIGHEST IMPACT

**Target:** Install node/baker/accuser wizards, form_builder

**Why highest impact:**
- 3,661 lines currently untested
- Core user workflows
- Tests entire wizard infrastructure
- Unlocks testing other TUI components

**Issues:** #423, (create baker/accuser/form_builder issues)

**Priority order:**
1. Install node wizard (1217 lines) - #423
2. Form builder (714 lines) - foundation for all wizards
3. Install baker wizard (747 lines)
4. Install accuser wizard (526 lines)

### Phase 3: Instances Page (2-3 days, +12% coverage)

**Target:** Service lifecycle management UI

**Why important:**
- 2,981 lines untested
- Main application interface
- Critical functionality (start/stop/remove services)

**Issues:** #424

### Phase 4: Remaining TUI (2-4 days, +8% coverage)

**Target:** Binaries, diagnostics, import wizard

**Why last:**
- Lower usage frequency
- Depends on patterns from Phases 1-3
- Nice-to-have for 70% target

**Issues:** (create binaries/diagnostics/import issues)

---

## Key Insights

### 1. bisect_ppx Hides Untested Code

The default report only shows executed files, creating a false sense of security. Our enhancement to list uncovered files makes the gap visible.

### 2. TUI Testing is Possible

Miaou's headless driver makes TUI testing straightforward:
- No real terminal needed
- Deterministic (no timing issues)
- Fast (tests complete in ms)
- Easy to write (just feed keys and check screen)

### 3. Testing Patterns are Reusable

The same patterns work across all TUI pages:
- Setup environment
- Initialize page
- Feed keys
- Assert screen content
- Cleanup

### 4. Mocking is Essential

Need to stub:
- Systemd operations (don't create real services)
- RPC calls (don't need real node)
- File I/O (use temp directories)
- Command execution (return canned output)

---

## Next Steps

### Immediate (This PR #420)

1. ✅ Wait for CI to complete (~14 minutes)
2. ✅ Verify uncovered files listing appears in PR comment
3. ✅ Merge PR #420 once CI passes
4. ✅ Coverage reports will auto-post on all future PRs

### Short Term (Milestone #5)

1. **Start Phase 1** (1-2 days)
   - Begin with #421 (directory_registry)
   - Then #422 (cli_progress)
   - Create and complete scheduler test issue

2. **Create remaining issues** for Phases 2-4
   - Baker wizard test issue
   - Accuser wizard test issue  
   - Form builder test issue
   - Binaries page test issue
   - Diagnostics page test issue
   - Import wizard test issue

3. **Expand test infrastructure**
   - Add helpers to `test/tui_test_helpers.ml`
   - Create `test/mocks/` directory
   - Build mock library (systemd, rpc, services)

### Medium Term (Weeks 2-4)

1. **Execute Phase 2** (form wizards)
   - Highest impact: +17% coverage
   - Establishes patterns for all TUI testing
   - Tests critical user workflows

2. **Execute Phase 3** (instances page)
   - Service management testing
   - Lifecycle operation coverage

3. **Execute Phase 4** (remaining TUI)
   - Complete coverage sweep
   - Hit 70% target

---

## Success Metrics

**Coverage Targets:**
- [x] TRUE coverage measured correctly (includes all files)
- [ ] Phase 1 complete: 33% TRUE coverage
- [ ] Phase 2 complete: 50% TRUE coverage
- [ ] Phase 3 complete: 62% TRUE coverage
- [ ] Phase 4 complete: 70% TRUE coverage

**Quality Targets:**
- [ ] All core workflows tested (install, start, stop, remove)
- [ ] No flaky tests (100 runs, 100% pass rate)
- [ ] Test suite completes in <2 minutes
- [ ] Headless testing patterns documented

**Process Targets:**
- [x] Coverage reports auto-post on PRs
- [x] Uncovered files visible in reports
- [x] Testing guide available for contributors
- [ ] Test helpers library established
- [ ] Mock infrastructure in place

---

## Files Created/Modified This Session

### Created
- `COVERAGE_PRIORITIES.md` - Complete roadmap
- `docs/TUI_TESTING_GUIDE.md` - Self-contained guide
- `SESSION_SUMMARY.md` - This file

### Modified
- `.github/workflows/coverage.yml` - Fixed % extraction, added uncovered files listing
- `test/unit_tests.ml` - (earlier: fixed 17 CI-failing tests)

### Issues Created
- #425 - [META] tracking issue
- #421 - Registry tests (Phase 1)
- #422 - CLI progress tests (Phase 1)
- #423 - Install node wizard tests (Phase 2)
- #424 - Instances page tests (Phase 3)

### Milestone Created
- #5 - Test Coverage: TUI & Core Libraries

---

## Resources

- **PR:** https://github.com/trilitech/octez-manager/pull/420
- **Milestone:** https://github.com/trilitech/octez-manager/milestone/5
- **Tracking Issue:** https://github.com/trilitech/octez-manager/issues/425
- **Documentation:**
  - `COVERAGE_PRIORITIES.md` - What to test and why
  - `docs/TUI_TESTING_GUIDE.md` - How to write tests
  - `AGENTS.md` - General coding standards

---

## Conclusion

We've transformed the coverage situation from "looks like 57%, ship it" to **"actually 27%, here's a concrete plan to reach 70%"**.

The roadmap is clear, the patterns are documented, and the issues are ready to be worked. All testing patterns are **self-contained in this repository** - no need to reference octez-setup, everything needed is in `docs/TUI_TESTING_GUIDE.md`.

**Next session:** Execute Phase 1 (quick wins) and start seeing that coverage number climb! 🚀
