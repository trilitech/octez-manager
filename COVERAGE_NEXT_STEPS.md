## Top Coverage Improvement Opportunities

### 🎯 Highest Impact Targets (500+ uncovered points each)

1. **instances_actions.ml** - 644 points, 0% coverage
   - Service start/stop/restart actions
   - **Blocker:** Requires systemd mocking
   - **Effort:** 3-5 days to build mock infrastructure
   - **Impact:** +3% coverage

2. **instances_render.ml** - 500 points, 0% coverage  
   - Service list rendering logic
   - **Easy to test:** Just needs more TUI tests with mock data
   - **Effort:** 1 day
   - **Impact:** +2.4% coverage

3. **binary_help_explorer.ml** - 459 uncovered, 4.97% coverage
   - Binary help text parsing/display
   - **Medium difficulty:** Needs mock help output
   - **Effort:** 1-2 days
   - **Impact:** +2.2% coverage

4. **cmd_instance.ml** - 453 uncovered, 13.71% coverage
   - CLI instance management commands
   - **Blocker:** Requires systemd mocking
   - **Effort:** 2-3 days
   - **Impact:** +2.2% coverage

5. **modal_helpers.ml** - 447 points, 0% coverage
   - ✅ **Started:** PR #440 has 3% coverage
   - **Needs:** More comprehensive modal interaction tests
   - **Effort:** 2-3 days more
   - **Impact:** +2% more coverage

### 🟡 Medium Impact Targets (300-450 uncovered points)

6. **import.ml** - 438 uncovered, 45.52% coverage
   - Import functionality (already decent coverage!)
   - **Opportunity:** Test edge cases, error paths
   - **Effort:** 1-2 days
   - **Impact:** +2% coverage

7. **form_builder.ml** - 403 uncovered, 0.98% coverage
   - Core form infrastructure
   - **Easy to test:** More form interaction tests
   - **Effort:** 2-3 days
   - **Impact:** +1.9% coverage

8. **form_builder_bundles.ml** - 351 uncovered, 0.28% coverage
   - Form field bundles/presets
   - **Easy to test:** Unit tests for field combinations
   - **Effort:** 1 day
   - **Impact:** +1.7% coverage

9. **common.ml** - 334 uncovered, 42.21% coverage
   - Utility functions (already decent!)
   - **Opportunity:** Test uncovered helpers
   - **Effort:** 1 day
   - **Impact:** +1.6% coverage

10. **cli_helpers.ml** - 329 uncovered, 15.42% coverage
    - CLI utility functions
    - **Medium difficulty:** Some I/O operations
    - **Effort:** 1-2 days
    - **Impact:** +1.6% coverage

### 📊 Quick Calculation to Reach 70%

Current: **38.16%** (7,988 / 20,935 points)
Target: **70%** (14,655 / 20,935 points)  
**Gap: 6,667 points needed**

**Realistic path to 70%:**
1. instances_render.ml - +500 points
2. binary_help_explorer.ml - +459 points  
3. form_builder.ml - +403 points
4. form_builder_bundles.ml - +351 points
5. modal_helpers.ml (complete) - +432 points
6. import.ml (improve to 80%) - +280 points
7. common.ml (improve to 70%) - +160 points
8. cli_helpers.ml (improve to 50%) - +135 points
9. More form interaction tests - +500 points
10. Removal flows - +164 points
11. Snapshot tests - +78 points
12. Misc small files - +400 points

**Total from above: ~3,860 points (would reach ~56%)**

**To actually reach 70%, need one of:**
- **Systemd mocking** for instances_actions + cmd_instance: +1,100 points
- **Extensive TUI interaction tests** (deeper modal/form workflows): +1,500 points
- **CLI command tests** (with mocked I/O): +1,000 points

### 🚫 Why We Haven't Caught Bugs

**Good question!** Here's why:

1. **Testing "happy paths"** - Our tests verify things work when used correctly
   - We test that forms load, render, navigate
   - We DON'T test: invalid input, edge cases, error conditions

2. **No assertions on actual behavior** - Many tests just verify "doesn't crash"
   - Example: `test_can_open_edit_modal` just checks screen has content
   - Doesn't verify the modal shows correct data or validation works

3. **Testing well-exercised code** - The forms are used daily in real usage
   - Bugs would have been found by users already
   - Tests confirm existing behavior, don't explore edge cases

4. **Missing integration scenarios** - We test components in isolation
   - Don't test: install → start → configure → restart workflows
   - Don't test: multiple instances, conflicts, race conditions

5. **No error injection** - We don't test failure modes
   - What if systemd is down?
   - What if disk is full?
   - What if network selection returns empty?

### 🎯 How to Actually Find Bugs

**Test error paths and edge cases:**

```ocaml
(* Instead of this *)
let test_form_loads () =
  init_form ();
  assert (screen_has_content ())

(* Do this *)  
let test_form_validates_required_fields () =
  init_form ();
  (* Try to submit without filling required field *)
  submit_form ();
  (* Should show validation error *)
  assert (screen_contains "Instance name is required")

let test_form_rejects_invalid_port () =
  init_form ();
  enter_field "RPC Port" "99999" ;  (* Invalid port *)
  assert (screen_contains "Port must be between 1-65535")

let test_form_handles_duplicate_instance_name () =
  (* Create instance "test-node" *)
  create_instance "test-node" ;
  (* Try to create another with same name *)
  init_form ();
  enter_field "Instance Name" "test-node" ;
  submit_form ();
  (* Should reject *)
  assert (screen_contains "Instance name already exists")
```

**Property-based testing:**
```ocaml
(* Test that form state is always recoverable *)
let test_form_state_invariants () =
  QCheck.Test.make 
    ~count:100
    (Gen.list (Gen.oneof [gen_key_press; gen_navigation]))
    (fun actions ->
      init_form ();
      List.iter send_action actions ;
      (* After ANY sequence of inputs, form should still be valid *)
      screen_is_valid () && form_can_be_closed ())
```

### 📋 Recommended Next Steps

**For immediate coverage gains (to ~50%):**
1. **instances_render.ml** - Add tests with mock service data (1 day) → +2.4%
2. **form_builder tests** - Test field validation, submission (2 days) → +3.6%
3. **modal_helpers complete** - All modal types tested (2 days) → +2%

**For finding actual bugs:**
1. **Add validation tests** to existing forms (1 day)
2. **Test error conditions** - network failures, invalid input (1-2 days)  
3. **Property tests** for state invariants (2-3 days)

**For reaching 70% (longer term):**
1. **Build systemd mock** infrastructure (3-5 days) → +5%
2. **Comprehensive CLI tests** with I/O mocking (3-5 days) → +4%
3. **Deep TUI workflow tests** (3-5 days) → +7%

**Total effort to 70%: 15-25 days of focused work**
