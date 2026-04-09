# Navigation Keymap Bug - Root Cause Analysis & Fix

## Problem Statement

**Critical UX Bug**: When navigating from Binaries page to RPC pages, pressing `?` showed Binaries shortcuts instead of RPC page shortcuts. The keymap wasn't updating even after waiting.

## Reproduction Steps

1. User on Binaries page → press `?` → shows Binaries shortcuts ✅
2. Navigate to RPC Node Selection page
3. Press `?` → shows BINARIES shortcuts ❌ (WRONG!)
4. Wait → still shows wrong shortcuts ❌

## Root Cause

**RPC Node Selection page doesn't register its keymap** (`src/ui/pages/rpc_node_selection.ml`)

### Why This Happened

Pages that implement `PAGE_SIG` directly (without `Themed_page.Make` wrapper) must **manually** call `Context.register_active_page_keymap` in their `view` function. RPC Node Selection was missing this call.

### Affected Pages

| Page | Wrapper | Registration | Status |
|------|---------|--------------|--------|
| **RPC Node Selection** | None (direct PAGE_SIG) | ❌ Missing | **BROKEN** |
| RPC Browser | `Themed_page.Make` | ✅ Automatic | Working |
| Binaries | `Themed_page.Make` | ✅ Automatic | Working |
| Instances | `Themed_page.Make` | ✅ Automatic | Working |
| Wallets | Manual | ✅ Present | Working |
| Diagnostics | Manual | ✅ Present | Working |

### Why RPC Browser Wasn't Affected

RPC Browser uses `Themed_page.Make`, which automatically registers the keymap in its view function wrapper. Only pages implementing PAGE_SIG directly need manual registration.

## The Fix

### 1. Add Keymap Registration in View Function

```ocaml
(* src/ui/pages/rpc_node_selection.ml:301 *)
let view ps ~focus:_ ~size =
  (* Register keymap for help modal (?) *)
  let keymap_pairs =
    List.map
      (fun (kb : state Miaou.Core.Tui_page.key_binding_desc) ->
        (kb.Miaou.Core.Tui_page.key, kb.help))
      (keymap ps)
  in
  Context.register_active_page_keymap (fun () -> keymap_pairs) ;
  
  let s = ps.Navigation.s in
  (* ... rest of view function *)
```

**Why in view?** The view function is called on every render. This ensures the keymap is always fresh when switching pages.

### 2. Add Global Shortcuts Delegation

Also fixed issue #848 - RPC Node Selection wasn't handling global shortcuts (`?`, `Esc`, etc.):

```ocaml
(* src/ui/pages/rpc_node_selection.ml:372 *)
let handle_key ps key ~size:_ =
  (* Try global shortcuts first (?, m, C-t, etc.) *)
  match Global_shortcuts.handle key with
  | Global_shortcuts.Handled -> ps
  | Global_shortcuts.NotGlobal ->
      (* Page-specific key handling *)
      let s = ps.Navigation.s in
      match Keys.of_string key with
      | Some Keys.Escape -> back ps
      | Some Keys.Enter -> ...
```

### 3. Code Reorganization

Moved `keymap` function definition before `view` (OCaml requires forward declarations).

## Test Coverage

### New Regression Test: `test_help_modal_navigation.ml`

Tests that help modal shows correct shortcuts after navigation:

```ocaml
(** Test: RPC Node Selection page shows correct shortcuts *)
let test_rpc_node_selection_shortcuts () =
  let module Rpc_node_selection = Octez_manager_ui.Rpc_node_selection in
  TH.with_test_env (fun () ->
      HD.Stateful.init (module Rpc_node_selection.Page) ;
      ignore (HD.Stateful.idle_wait ~iterations:2 ~sleep:0.001 ()) ;

      verify_help_modal_shortcuts
        ~page_name:"RPC Node Selection"
        ~expected_shortcuts:["Select"; "Navigate"]
        ~forbidden_shortcuts:["Download"; "Register directory"; "Prune"]
        ())
```

**Test Strategy:**
- Initialize RPC Node Selection page
- Open help modal (`?`)
- Verify expected shortcuts appear (Select, Navigate)
- Verify forbidden shortcuts DON'T appear (Download, Prune - from Binaries)

**Result:** ✅ PASS (1 test)

### Why RPC Browser Test Was Skipped

RPC Browser has complex initialization that fails without RPC connection (tracked in issue #849). Since it uses `Themed_page.Make`, the keymap registration works automatically once initialization is fixed.

## Before/After

**BEFORE (bug):**
```
1. On Binaries page → press ? → shows:
   Page shortcuts:
     d       - Download latest
     l       - Register directory
     p       - Prune unused

2. Navigate to RPC Node Selection
   
3. Press ? → shows (WRONG!):
   Page shortcuts:
     d       - Download latest    ← Wrong page!
     l       - Register directory ← Wrong page!
     p       - Prune unused       ← Wrong page!
```

**AFTER (fixed):**
```
1. On Binaries page → press ? → shows:
   Page shortcuts:
     d       - Download latest
     l       - Register directory
     p       - Prune unused

2. Navigate to RPC Node Selection
   
3. Press ? → shows (CORRECT!):
   Page shortcuts:
     Enter   - Select             ← Correct!
     ↑/↓     - Navigate           ← Correct!
     r       - Refresh            ← Correct!
```

## Files Modified

1. **src/ui/pages/rpc_node_selection.ml**
   - Added keymap registration in `view` function (line 301)
   - Added Global_shortcuts delegation in `handle_key` (line 372)
   - Moved `keymap` function before `view` for proper ordering

2. **test/test_help_modal_navigation.ml** (NEW)
   - Regression test for navigation keymap updates
   - 1 test for RPC Node Selection
   - Helper function to verify shortcuts

3. **test/dune**
   - Added test executable definition

## Impact

- ✅ **RPC Node Selection**: Help modal now works correctly
- ✅ **Navigation**: Keymap updates when switching pages
- ✅ **Global shortcuts**: `?` key now works on RPC Node Selection (fixes #848)
- ✅ **No regressions**: All 269 existing tests still pass

## Related Issues

- Fixes navigation keymap bug (reported by user)
- Fixes #848 - RPC Node Selection help modal didn't work
- Related to #849 - RPC Browser initialization issues (separate issue)

## Test Results

```
Help Modal - Navigation Updates: 1/1 tests PASS ✅
All existing tests: 269/269 PASS ✅
Total: 270 tests PASS
```

## Key Takeaway for Future Development

**Pages implementing PAGE_SIG directly MUST:**
1. Call `Context.register_active_page_keymap` in `view` function
2. Call `Global_shortcuts.handle` in `handle_key` function

**Or use `Themed_page.Make` wrapper** which does both automatically.
