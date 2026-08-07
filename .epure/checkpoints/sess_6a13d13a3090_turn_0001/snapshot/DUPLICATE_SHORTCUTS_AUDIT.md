# Duplicate Shortcuts Audit & Fix Plan

## Problem Statement

Pages are defining global shortcuts (`?`, `Esc`) in their keymaps, causing them to appear TWICE in the help modal:
- Once in the "Global shortcuts" section
- Once in the "Page shortcuts" section

## Root Cause

Global shortcuts are handled by `Global_shortcuts.handle` (which checks `?`, `Esc`, `q`, `C-t`), but many pages also include these in their `keymap` function with `display_only = true`. This was likely done for documentation purposes before the global shortcuts infrastructure existed.

## Pages with Duplicate `Esc - Back`

1. **wallets_page.ml** - `[kb "Esc" "Back"; kb "?" "Help"]`
2. **instance_details.ml** - `[kb "Esc" "Back"; kb "?" "Help"]`
3. **snapshots.ml** - `[kb "Esc" "Back"; kb "?" "Help"]`
4. **topology_page.ml** - `[kb "Esc" "Back"; kb "?" "Help"]`
5. **binaries/binaries_page.ml** - `kb "Esc" "Back"`
6. **diagnostics/diagnostics_page.ml** - `kb "Esc" "Back"`
7. **import_wizard.ml** - `kb "Esc" "Back / Previous"`
8. **sandbox_page.ml** - `kb "Esc" "Back"`
9. **sandbox_key_alloc_page.ml** - `kb "Esc" "Back"`
10. **rpc_browser/rpc_browser.ml** - `kb "Esc" "Back"`
11. **rpc_node_selection.ml** - `kb "Esc" "Back"`
12. **rewards/rewards_page.ml** - `kb "Esc" "Back"`

## Pages with Duplicate `? - Help`

1. **wallets_page.ml** - `[kb "Esc" "Back"; kb "?" "Help"]`
2. **instance_details.ml** - `[kb "Esc" "Back"; kb "?" "Help"]`
3. **snapshots.ml** - `[kb "Esc" "Back"; kb "?" "Help"]`
4. **topology_page.ml** - `[kb "Esc" "Back"; kb "?" "Help"]`
5. **binaries/binaries_page.ml** - `kb "?" "Help"`
6. **diagnostics/diagnostics_page.ml** - `kb "?" "Help"`
7. **instances.ml** - `kb "?" "Help"`
8. **rpc_browser/rpc_browser.ml** - `kb "?" "Help"`

## Fix Strategy: Defense in Depth

We'll implement BOTH filtering AND source fixes:

### 1. Filter Duplicates in `modal_helpers.ml` (Immediate Protection)

**Why:** Defensive programming - prevents duplicates even if a page mistakenly adds global shortcuts.

**How:** When rendering the help modal, filter out any page shortcuts that match global shortcut keys.

```ocaml
let render_help_modal () =
  let global_keys = Global_shortcuts.reserved_keys in
  let page_shortcuts = 
    Context.get_active_page_keymap ()
    |> List.filter (fun (key, _help) -> 
        not (List.mem key global_keys))
  in
  (* render with filtered page_shortcuts *)
```

### 2. Remove Global Shortcuts from Page Keymaps (Source Fix)

**Why:** Pages shouldn't define what they don't handle. Clean separation of concerns.

**How:** Remove `"Esc"`, `"?"`, and other global shortcuts from all page keymaps.

### 3. Add Regression Tests (Prevention)

**Why:** Catch future violations automatically.

**How:** Test helper that verifies no duplicates appear in help modal output.

## Implementation Order

1. ✅ **Add filter in `modal_helpers.ml`** - Immediate fix, works for all pages
2. ✅ **Add regression test** - Verify filtering works
3. ✅ **Clean up page keymaps** - Remove all global shortcuts from pages
4. ✅ **Update existing tests** - Fix any tests expecting duplicates
5. ✅ **Format and commit**

## Expected Outcome

After the fix, pressing `?` on any page shows:

```
Help
─────────────────────────────

Global shortcuts:
  ?       - Help
  m       - Menu
  C-t     - Theme picker
  Esc/q   - Close modals / Back

Page shortcuts:
  Enter   - Open               ← Only page-specific shortcuts
  g       - Toggle view
  r       - Refresh
```

No duplicates between sections.
