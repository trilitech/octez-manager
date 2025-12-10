# Key Handling Strategy: Global Shortcuts vs Input Focus

**Date:** 2025-12-10
**Context:** Ensuring global shortcuts (like `s` for Settings) don't interfere with text input

---

## The Problem

We want:
- ✅ `s` key to open Settings (global shortcut)
- ✅ BUT still be able to type `s` in textboxes
- ✅ AND use `s` in any modal/dialog

**Challenge:** How do we know when `s` means "Settings" vs "type the letter s"?

---

## The Solution: Focus Hierarchy

Miaou already implements a **key handling priority system**:

### **Priority Order (Highest to Lowest)**

```
1. Modal/Dialog (if open)
   ├─ Modal handles ALL keys
   └─ Widgets (textbox, select, etc.) get keys first
       ↓
2. Page-level handlers (if no modal)
   ├─ Check for global shortcuts
   └─ Handle page-specific keys
```

### **How It Works**

**From `instances.ml:796-826`:**
```ocaml
let handle_key s key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    (* Modal is open → send ALL keys to modal *)
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation s
  )
  else
    (* No modal → handle page-level shortcuts *)
    match key with
    | "Up" -> move_selection s (-1)
    | "Down" -> move_selection s 1
    | "s" -> open_settings s      (* Global shortcut works here *)
    | "c" -> create_instance s
    | "Enter" -> handle_enter s
    | _ -> s
```

**The pattern:**
1. **Check first:** `Modal_manager.has_active()`
2. **If true:** Modal owns the key (textboxes work!)
3. **If false:** Page owns the key (global shortcuts work!)

---

## Example Flow: Typing 's' in a Textbox

### Scenario: User opens instance name input and types "test-server"

**Step 1: Open textbox modal**
```ocaml
(* User presses 'c' to create instance *)
open_text_input_modal
  ~title:"Instance Name"
  ~on_select:(fun name -> ...)
```

**Step 2: Modal is now active**
- `Modal_manager.has_active()` returns `true`

**Step 3: User types 't', 'e', 's', 't'**
```ocaml
(* Each keypress goes through handle_key *)
handle_key state "t" ~size
  → Modal_manager.has_active() = true
  → Modal_manager.handle_key "t"
      → Textbox_widget.handle_key "t"
          → Text buffer now: "t"

handle_key state "e" ~size
  → Modal owns key → Text buffer: "te"

handle_key state "s" ~size
  → Modal owns key → Text buffer: "tes"  ← 's' typed, NOT settings!

handle_key state "t" ~size
  → Modal owns key → Text buffer: "test"
```

**Step 4: User presses Enter**
```ocaml
handle_key state "Enter" ~size
  → Modal configured with commit_on:["Enter"]
  → Modal closes, calls on_select("test")
  → Modal_manager.has_active() = false again
```

**Step 5: Now global shortcuts work again**
```ocaml
handle_key state "s" ~size
  → Modal_manager.has_active() = false
  → Match "s" → open_settings()  ← Global shortcut works!
```

---

## Implementation Pattern

### **Every Page Must Follow This Pattern**

```ocaml
(* Standard key handling pattern for all pages *)

let handle_key (state : state) (key : string) ~size =
  (* 1. Modal takes priority *)
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    check_navigation state  (* Handle navigation signals *)
  )
  else
    (* 2. Handle page-level keys *)
    match key with
    (* Global shortcuts *)
    | "s" ->
        Context.navigate "settings";
        state
    | "?" | "h" ->
        show_help_modal ();
        state

    (* Page-specific shortcuts *)
    | "Up" | "k" -> move_selection state (-1)
    | "Down" | "j" -> move_selection state 1
    | "Enter" -> handle_enter state
    | "Tab" -> toggle_view_mode state
    | "c" -> create_instance state
    | "f" -> cycle_filter state

    (* Navigation *)
    | "Esc" | "q" -> {state with next_page = Some "__BACK__"}

    (* Unhandled *)
    | _ -> state
```

---

## Global Shortcut Registry

### **Recommended Global Shortcuts**

These should work on **every page** (when no modal is active):

| Key | Action | Rationale |
|-----|--------|-----------|
| `s` | Settings | Common convention (`s` for settings) |
| `?` or `h` | Help | Universal help key |
| `m` | Menu | Global menu access |
| `Esc` | Back/Cancel | Universal escape |
| `q` | Quit | Common quit key (with confirmation) |
| `Ctrl+C` | Force quit | Emergency exit |

### **Page-Specific Keys**

These can vary per page:

| Key | Action | Page |
|-----|--------|------|
| `Tab` | Toggle view mode | Instances |
| `c` | Create instance | Instances |
| `f` | Filter | Instances |
| `r` | Refresh | Any |
| `Enter` | Context action | Any |
| `Space` | Expand/Select | Lists |
| Arrow keys | Navigate | Any |

---

## Reserved Keys: What to Avoid

### **Don't Use These for Page-Specific Actions**

❌ **Bad:**
```ocaml
(* DON'T: Use 's' for page-specific action *)
let handle_key state key ~size =
  if not (Modal_manager.has_active ()) then
    match key with
    | "s" -> start_service state  (* Conflicts with global Settings! *)
    | ...
```

✅ **Good:**
```ocaml
(* DO: Use different key for page-specific action *)
let handle_key state key ~size =
  if not (Modal_manager.has_active ()) then
    match key with
    | "S" -> start_service state   (* Capital S, or... *)
    | "t" -> start_service state   (* Different key, or... *)
    | "Enter" -> action_menu state (* Context menu for actions *)
    | ...
```

### **Guidelines:**

**Reserve globally:**
- `s` - Settings
- `?`, `h` - Help
- `m` - Menu
- `Esc` - Back
- `q` - Quit
- `Ctrl+C` - Force quit

**Safe for page-specific use:**
- `c`, `d`, `e`, `f`, `i`, `l`, `n`, `o`, `p`, `r`, `t`, `u`, `v`, `w`, `x`, `y`, `z`
- Capital letters: `A-Z` (shift+letter)
- Numbers: `0-9`
- Function keys: `F1-F12`
- Special: `Space`, `Enter`, `Tab`, Arrow keys

---

## Helper Module: Global Shortcuts

### **Create `src/ui/global_shortcuts.ml`**

```ocaml
(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Global keyboard shortcuts that work across all pages *)

(** List of reserved keys for global shortcuts. Pages should avoid using these
    for page-specific actions. *)
let reserved_keys = ["s"; "?"; "h"; "m"; "Esc"; "q"]

(** Check if a key is reserved for global use *)
let is_reserved key = List.mem key reserved_keys

(** Handle global shortcuts. Returns Some new_page if navigation occurred,
    None if key was not a global shortcut. *)
let handle_global_key key =
  match key with
  | "s" ->
      Context.navigate "settings";
      Some "settings"
  | "?" | "h" ->
      Modal_helpers.show_help_modal ();
      None
  | "m" ->
      Modal_helpers.show_menu_modal ();
      None
  | _ -> None

(** Standard key handler wrapper for pages. Use this pattern:

    {[
      let handle_key state key ~size =
        if Modal_manager.has_active () then
          handle_modal_key state key ~size
        else
          match Global_shortcuts.handle_global_key key with
          | Some _page -> state  (* Navigation handled *)
          | None ->
              (* Handle page-specific keys *)
              match key with
              | "c" -> create_instance state
              | "f" -> filter state
              | ...
    ]}
*)
```

---

## Usage in Pages

### **Pattern 1: Manual Check (Current)**

```ocaml
let handle_key state key ~size =
  if Modal_manager.has_active () then (
    Modal_manager.handle_key key;
    state
  ) else
    match key with
    | "s" -> Context.navigate "settings"; state
    | "?" | "h" -> show_help (); state
    | "c" -> create_instance state
    | ...
```

### **Pattern 2: With Helper Module (Recommended)**

```ocaml
let handle_key state key ~size =
  if Modal_manager.has_active () then (
    Modal_manager.handle_key key;
    state
  ) else
    match Global_shortcuts.handle_global_key key with
    | Some _page -> state  (* Global shortcut handled *)
    | None ->
        (* Page-specific keys *)
        match key with
        | "c" -> create_instance state
        | "Tab" -> toggle_view_mode state
        | "f" -> cycle_filter state
        | ...
```

**Benefits:**
- ✅ Global shortcuts defined in one place
- ✅ Easier to maintain consistency
- ✅ Pages can focus on their specific keys
- ✅ Can warn if page uses reserved key

---

## Edge Cases

### **Case 1: Modal Opens Another Modal**

**Scenario:** Settings modal opens a file browser

```
Instances page
  → Press 's' → Settings page
                  → Press 'Enter' on data_dir → File browser modal
                                                    → Type 's' in path
```

**Handling:**
```ocaml
(* Settings page *)
let handle_key state key ~size =
  if Modal_manager.has_active () then
    (* File browser is open, it handles 's' *)
    Modal_manager.handle_key key; state
  else
    (* Settings page handles its own keys *)
    match key with
    | "Enter" -> edit_selected_setting state
    | ...
```

**Result:** ✅ Typing 's' in file browser path works correctly

---

### **Case 2: Textbox in Main Page (not modal)**

**Scenario:** Inline search/filter textbox

```ocaml
type state = {
  services : Service_state.t list;
  filter_input : Textbox_widget.t option;  (* Inline textbox *)
  ...
}

let handle_key state key ~size =
  (* Check modal first *)
  if Modal_manager.has_active () then
    Modal_manager.handle_key key; state
  (* Check if inline textbox has focus *)
  else match state.filter_input with
  | Some textbox when Textbox_widget.has_focus textbox ->
      (* Textbox handles key *)
      let textbox' = Textbox_widget.handle_key textbox key in
      {state with filter_input = Some textbox'}
  | _ ->
      (* No modal, no textbox focus → global shortcuts work *)
      match key with
      | "s" -> open_settings state
      | ...
```

**Pattern:**
1. Check modal
2. Check widget focus
3. Handle global shortcuts
4. Handle page keys

---

### **Case 3: Vim-style Normal vs Insert Mode**

**Alternative approach:** Explicit modes (like vim)

```ocaml
type mode = Normal | Insert of Textbox_widget.t

type state = {
  services : Service_state.t list;
  mode : mode;
  ...
}

let handle_key state key ~size =
  if Modal_manager.has_active () then
    Modal_manager.handle_key key; state
  else
    match state.mode with
    | Insert textbox ->
        (* In insert mode, most keys go to textbox *)
        (match key with
        | "Esc" -> {state with mode = Normal}  (* Exit insert mode *)
        | _ ->
            let textbox' = Textbox_widget.handle_key textbox key in
            {state with mode = Insert textbox'})
    | Normal ->
        (* In normal mode, all shortcuts work *)
        match key with
        | "s" -> open_settings state
        | "/" -> enter_search_mode state  (* Enter insert mode *)
        | ...
```

**Not recommended for octez-manager** (over-engineered), but shows the concept.

---

## Testing Strategy

### **Unit Tests for Key Handling**

```ocaml
(* test/test_key_handling.ml *)

let test_global_shortcut_when_no_modal () =
  let state = Instances.init All in
  (* No modal active *)
  assert (not (Modal_manager.has_active ()));
  (* Press 's' → should navigate to settings *)
  let _state' = Instances.handle_key state "s" ~size:default_size in
  let nav = Context.consume_navigation () in
  assert (nav = Some "settings")

let test_global_shortcut_blocked_by_modal () =
  let state = Instances.init All in
  (* Open modal with textbox *)
  Modal_helpers.open_text_input_modal
    ~title:"Test"
    ~on_select:(fun _ -> ());
  (* Modal is active *)
  assert (Modal_manager.has_active ());
  (* Press 's' → should NOT navigate to settings *)
  let _state' = Instances.handle_key state "s" ~size:default_size in
  let nav = Context.consume_navigation () in
  assert (nav = None);  (* No navigation occurred *)
  (* Textbox should have 's' in it instead *)
  ...

let test_reserved_key_detection () =
  assert (Global_shortcuts.is_reserved "s");
  assert (Global_shortcuts.is_reserved "?");
  assert (not (Global_shortcuts.is_reserved "c"));
  assert (not (Global_shortcuts.is_reserved "f"))
```

---

## Documentation for Developers

### **Comment Template for Pages**

```ocaml
(** Key handling priority:
    1. Modal (if active) - handles ALL keys including global shortcuts
    2. Global shortcuts - 's' (settings), '?' (help), 'm' (menu)
    3. Page-specific keys - see table below

    Page-specific shortcuts:
    - Tab: Toggle view mode (instances ↔ resources)
    - c: Create new instance
    - f: Cycle filter
    - Enter: Actions menu
    - Up/Down: Navigate list
    - Esc: Back to previous page
*)
let handle_key state key ~size =
  if Modal_manager.has_active () then
    (* Modal owns all keys *)
    Modal_manager.handle_key key;
    check_navigation state
  else
    (* Handle global + page-specific keys *)
    ...
```

---

## Summary

### **How It Works**

1. ✅ **Modal_manager provides isolation**
   - When modal is open → modal handles ALL keys
   - When modal is closed → page handles keys

2. ✅ **No special handling needed for textboxes**
   - Textboxes are always in modals
   - Modal system already protects them

3. ✅ **Global shortcuts are safe**
   - `s` opens Settings when no modal
   - `s` types 's' when textbox is focused
   - Automatic, based on modal state

### **Best Practices**

**DO:**
- ✅ Always check `Modal_manager.has_active()` first
- ✅ Use helper module for global shortcuts
- ✅ Document reserved keys in code
- ✅ Test modal interactions

**DON'T:**
- ❌ Use reserved keys for page-specific actions
- ❌ Handle keys differently per page (be consistent)
- ❌ Forget to check modal state

### **Reserved Keys**

| Key | Use | Status |
|-----|-----|--------|
| `s` | Settings | RESERVED - global only |
| `?`, `h` | Help | RESERVED - global only |
| `m` | Menu | RESERVED - global only |
| `Esc` | Back | RESERVED - global only |
| `q` | Quit | RESERVED - global only |
| All others | Available | Use for page-specific |

---

**Document Version:** 1.0
**Last Updated:** 2025-12-10
**Status:** Implementation Guide
