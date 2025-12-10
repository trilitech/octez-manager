# UI Layout Options: Cards, Tables, Navigation

**Date:** 2025-12-10
**Context:** Evaluating display patterns and navigation for octez-manager TUI

## Current State

### Current Layout (Table-based)

```
┌────────────────────────────────────────────────────────────────────────┐
│  octez-manager    ● USER    Hint: ↑/↓ move · Enter open · Esc back    │
│  Instances: 3 | Filter: all                                            │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ ➤ ● [ Install new instance ]                                           │
│                                                                         │
│   ● mainnet-node   node      rolling    mainnet      [enabled]        │
│                    ✓ synced · L5847234 · proto:PsQueb · chain:NetXdQp │
│                                                                         │
│   ○ test-baker     baker     (inherited) (inherited) [disabled]        │
│                    RPC not available for bakers; use logs.             │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ Arrows: move  Enter: actions  c: create  f: filter  b: bulk  Esc: back│
└────────────────────────────────────────────────────────────────────────┘
```

**Characteristics:**
- Fixed-width columns
- Two lines per instance (summary + details)
- Dense information display
- Selection marker (➤)
- ~80 chars wide minimum

---

## Option 1: Cards vs Tables

### A. Current: Table/List View ⭐⭐⭐⭐⭐

**Pros:**
- ✅ **Information density** - See 10+ instances on one screen
- ✅ **Easy scanning** - Aligned columns make comparison trivial
- ✅ **Terminal-friendly** - Works perfectly in 80-col terminals
- ✅ **Keyboard navigation** - Natural up/down movement
- ✅ **Sorting potential** - Can sort by column
- ✅ **Compact** - Minimal wasted space

**Cons:**
- ❌ Column width limitations (truncation)
- ❌ Less visual separation between instances
- ❌ Can feel cramped with many columns

**Best for:**
- Dashboards with many items
- Quick status overview
- Power users who want information density

---

### B. Card Layout

#### Card Layout Example:
```
┌────────────────────────────────────────────────────────────────────────┐
│  octez-manager    ● USER    Hint: ↑/↓ move · Enter open · Esc back    │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │ ➤ mainnet-node                                         ● Running │  │
│  │                                                                   │  │
│  │   Role: node           Network: mainnet                          │  │
│  │   History: rolling     Status: ✓ Synced                          │  │
│  │   Level: L5847234      Proto: PsQueb... Chain: NetXdQp...        │  │
│  │   Enabled: yes         Last seen: 4s ago                         │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                         │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │   test-baker                                           ○ Stopped │  │
│  │                                                                   │  │
│  │   Role: baker          Parent: mainnet-node                      │  │
│  │   Delegate: tz1...     Status: Stopped                           │  │
│  │   Enabled: no          Last seen: 2h ago                         │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ ↑/↓: navigate  Enter: open  c: create  f: filter  Esc: back           │
└────────────────────────────────────────────────────────────────────────┘
```

**Pros:**
- ✅ **Visual separation** - Clear boundaries between instances
- ✅ **Flexible layout** - No column alignment constraints
- ✅ **More breathing room** - Less cramped feeling
- ✅ **Better for complex data** - Can show nested/hierarchical info
- ✅ **Modern aesthetic** - Feels more polished

**Cons:**
- ❌ **Fewer items visible** - Maybe 3-4 cards on screen vs 10+ rows
- ❌ **More scrolling** - Increased vertical space per item
- ❌ **Wastes horizontal space** - Cards typically don't use full width
- ❌ **Harder to compare** - Can't easily align values across cards
- ❌ **Complex to render** - Box drawing, spacing, alignment

**Best for:**
- Detailed item views
- Few items (< 5-6 on screen)
- Rich metadata display
- Visual learners

---

### Hybrid: Table with Expandable Details

```
┌────────────────────────────────────────────────────────────────────────┐
│  octez-manager    ● USER                                               │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ ➤ ● mainnet-node   node      rolling    mainnet      [enabled]        │
│   ┌────────────────────────────────────────────────────────────────┐  │
│   │ ✓ Synced · L5847234 · proto:PsQueb · chain:NetXdQp            │  │
│   │ CPU: 35% ████████░░  Memory: 4.2GB  Disk: 89GB                │  │
│   │ Last block: 4s ago   Chain health: ✓ Stable                   │  │
│   └────────────────────────────────────────────────────────────────┘  │
│                                                                         │
│   ○ test-baker     baker     inherited   inherited    [disabled]       │
│     RPC not available; use logs                                        │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ Space: expand  ↑/↓: navigate  Enter: actions  f: filter  Esc: back    │
└────────────────────────────────────────────────────────────────────────┘
```

**Pros:**
- ✅ Best of both worlds - compact list + expandable details
- ✅ Progressive disclosure - show details on demand
- ✅ Keyboard-friendly - Space to toggle
- ✅ Scalable - Works with many instances

**Cons:**
- ❌ More complex state management
- ❌ Can be confusing which items are expanded

---

### **Recommendation: Stick with Table, Add Optional Details Panel** ⭐⭐⭐⭐⭐

**Rationale:**
1. **Octez-manager manages multiple services** - Need to see 5-10+ instances at once
2. **Comparison is key** - Users want to see which nodes are synced, which are stuck
3. **Terminal constraints** - Most TUI users work in 80-120 col terminals
4. **Power user tool** - Operators prefer information density over visual polish

**Enhancement:** Use **Pane_layout** for side-by-side details when item is selected:

```
┌─────────────────────────────────────┬──────────────────────────────────┐
│  Instances                          │  mainnet-node                    │
│                                     │                                  │
│ ➤ ● mainnet-node   node   running  │  Status: ✓ Synced               │
│   ○ test-baker     baker  stopped  │  Level: L5,847,234              │
│   ● archive-node   node   running  │  Last block: 4s ago             │
│                                     │  Chain: Stable ═══              │
│                                     │                                  │
│                                     │  Resources:                      │
│                                     │  CPU:    35% ████████░░         │
│                                     │  Memory: 4.2GB ████████░░       │
│                                     │  Disk:   89GB ████████████░     │
│                                     │                                  │
│                                     │  RPC: 127.0.0.1:8732            │
│                                     │  P2P: 0.0.0.0:9732              │
│                                     │  Data: ~/.tezos-node            │
│                                     │                                  │
│                                     │  [Enter] Actions                │
├─────────────────────────────────────┴──────────────────────────────────┤
│ ↑/↓: move  d: toggle details  Enter: actions  Esc: back               │
└────────────────────────────────────────────────────────────────────────┘
```

Press `d` to toggle detail panel on/off.

---

## Option 2: Navigation Patterns

### Current: Stack-based (No breadcrumbs/tabs)

```
Instances page
    ↓ [Enter on instance]
Instance details page
    ← [Esc]
Back to instances
```

**Works but:** No indication of "where am I?" or "how did I get here?"

---

### A. Breadcrumbs ⭐⭐⭐⭐

#### Visual Example:
```
┌────────────────────────────────────────────────────────────────────────┐
│  octez-manager > Instances > mainnet-node                              │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  Instance: mainnet-node                                                │
│  Role: node                                                            │
│  Network: mainnet                                                      │
│  ...                                                                   │
├────────────────────────────────────────────────────────────────────────┤
│ Esc: back                                                              │
└────────────────────────────────────────────────────────────────────────┘
```

**Pros:**
- ✅ **Shows current location** - "I'm in Instances > mainnet-node"
- ✅ **Shows navigation path** - How you got here
- ✅ **Clickable** (if using mouse) - Jump to parent levels
- ✅ **Minimal space** - Single line in header
- ✅ **Standard pattern** - Familiar from web/GUIs

**Cons:**
- ❌ **Not keyboard-friendly** - Can't click in pure terminal
- ❌ **Long paths truncate** - "Home > Instances > Install > Node > Network..."
- ❌ **Redundant with title** - Title often shows same info
- ❌ **Limited value in shallow hierarchies** - Most pages are 1-2 levels deep

**Best for:**
- Deep navigation hierarchies (4+ levels)
- Mouse-enabled terminals
- Complex multi-step workflows

---

### B. Tabs ⭐⭐⭐⭐⭐

#### Visual Example:
```
┌────────────────────────────────────────────────────────────────────────┐
│ [Instances] [Resources] [Snapshots] [Settings]           ● USER        │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ ➤ ● mainnet-node   node      rolling    mainnet      [enabled]        │
│                    ✓ synced · L5847234 · proto:PsQueb                 │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ Tab/1-4: switch view  ↑/↓: navigate  Enter: actions  Esc: back        │
└────────────────────────────────────────────────────────────────────────┘
```

**With keyboard shortcuts:**
- `1` or `Tab` → Instances
- `2` or `Shift+Tab` → Resources
- `3` → Snapshots
- `4` → Settings
- `m` → Menu (existing)

**Pros:**
- ✅ **Fast switching** - Jump between main views instantly
- ✅ **Clear context** - Always know which section you're in
- ✅ **Keyboard-friendly** - Number keys or Tab cycling
- ✅ **Scalable** - Add new sections easily
- ✅ **Standard pattern** - Familiar from tmux, screen, browsers

**Cons:**
- ❌ **Horizontal space** - Takes up ~40-50 chars
- ❌ **Flat hierarchy only** - Can't show nested views
- ❌ **Overload risk** - Too many tabs = cluttered

**Best for:**
- Main app sections (Instances, Resources, Config)
- Peer-level views (not parent-child)
- Keyboard-first interfaces

---

### C. Hybrid: Tabs + Breadcrumbs

#### Visual Example:
```
┌────────────────────────────────────────────────────────────────────────┐
│ [Instances] [Resources] [Snapshots] [Settings]           ● USER        │
│ Home > Instances > mainnet-node                                        │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  Instance: mainnet-node                                                │
│  ...                                                                   │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ 1-4: tabs  Esc: back                                                   │
└────────────────────────────────────────────────────────────────────────┘
```

**Pros:**
- ✅ Best of both - tabs for sections, breadcrumbs for depth
- ✅ Clear context at all levels

**Cons:**
- ❌ Uses 2 lines of header (expensive in TUI)
- ❌ Overkill for shallow hierarchies

---

### D. Status Bar Navigation (Lightweight Alternative)

```
┌────────────────────────────────────────────────────────────────────────┐
│  octez-manager    ● USER    Section: Instances (3 services)            │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ ➤ ● mainnet-node   node      rolling    mainnet      [enabled]        │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ m: menu  r: resources  s: snapshots  ↑/↓: move  Enter: actions        │
└────────────────────────────────────────────────────────────────────────┘
```

**Pros:**
- ✅ No visual overhead - uses existing header line
- ✅ Keyboard shortcuts visible in footer
- ✅ Minimal complexity

**Cons:**
- ❌ No visual indication of available sections
- ❌ Relies on memory of shortcuts

---

## Recommendations

### For Display Layout: **Keep Table View** ⭐⭐⭐⭐⭐

**Action items:**
1. ✅ Keep current table-based list
2. ✅ Add optional detail panel with `Pane_layout` (toggle with `d` key)
3. ✅ Enhance table with resource bars (CPU/Memory sparklines)
4. ❌ Skip cards - wrong pattern for this use case

**Rationale:**
- Managing multiple services requires seeing many at once
- Comparison is essential (which nodes are synced?)
- Terminal width is precious
- Power users prefer density

---

### For Navigation: **Tabs for Main Sections** ⭐⭐⭐⭐⭐

**Action items:**
1. ✅ Add tabs for main sections: `[Instances] [Resources] [Snapshots] [Settings]`
2. ✅ Use number keys 1-4 for quick switching
3. ✅ Highlight active tab
4. ❌ Skip breadcrumbs - hierarchy is too shallow (2 levels max)
5. ✅ Keep stack-based navigation within sections (Esc to go back)

**Rationale:**
- Only 3-4 main sections → tabs are perfect
- Keyboard-first navigation (number keys)
- Clear context without consuming vertical space
- Standard pattern in TUI apps (like htop's F-keys)

---

## Implementation Sketch

### Tab Widget (new)

```ocaml
(* src/ui/widgets/tab_bar.ml *)

type tab = {
  label : string;
  key : string;  (* shortcut key: "1", "2", etc. *)
  page : string; (* page name to navigate to *)
}

type t = {
  tabs : tab list;
  active : int;
}

let create tabs = { tabs; active = 0 }

let set_active t page_name =
  let active =
    List.find_index (fun tab -> tab.page = page_name) t.tabs
    |> Option.value ~default:0
  in
  { t with active }

let render t =
  let render_tab idx tab =
    let is_active = idx = t.active in
    let style =
      if is_active then
        fun s -> Widgets.bold (Widgets.bg 240 (Widgets.fg 255 s))
      else
        fun s -> Widgets.dim s
    in
    Printf.sprintf "[%s]" (style tab.label)
  in
  let tabs_str =
    t.tabs
    |> List.mapi render_tab
    |> String.concat " "
  in
  tabs_str

let handle_key t key =
  (* Check if key matches a tab shortcut *)
  match List.find_index (fun tab -> tab.key = key) t.tabs with
  | Some idx when idx <> t.active ->
      Some t.tabs.(idx).page
  | _ -> None
```

### Enhanced Header

```ocaml
(* In instances.ml *)
let header s =
  let privilege =
    if Common.is_root () then Widgets.red "● SYSTEM"
    else Widgets.green "● USER"
  in
  let tabs = Tab_bar.render s.tabs in
  let summary = summary_line s in
  [
    Printf.sprintf "%s    %s" tabs privilege;
    Widgets.dim summary;
  ]
```

### Keyboard Handling

```ocaml
let handle_key s key ~size =
  (* Check tab navigation first *)
  match Tab_bar.handle_key s.tabs key with
  | Some page_name ->
      Context.navigate page_name;
      s
  | None ->
      (* Existing key handling... *)
      match key with
      | "Up" | "k" -> move_selection s (-1)
      | "Down" | "j" -> move_selection s 1
      | ...
```

---

## Visual Mockups

### Recommended: Tabs + Table + Optional Detail Panel

#### Compact View (default):
```
┌────────────────────────────────────────────────────────────────────────┐
│ [Instances] [Resources] [Snapshots] [Settings]            ● USER       │
│ 3 instances | Filter: all | Last refresh: 2s ago                       │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ ➤ ● mainnet-node   node      rolling    mainnet      [enabled]        │
│                    ✓ synced · L5847234 · proto:PsQueb · 4s ago        │
│                                                                         │
│   ● archive-node   node      archive    mainnet      [enabled]        │
│                    ⚠ catching up · L4523122 · proto:PsQueb · 2s ago   │
│                                                                         │
│   ○ test-baker     baker     inherited   inherited    [disabled]       │
│                    RPC not available; use logs                         │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ 1-4: tabs  d: details  ↑/↓: move  Enter: actions  f: filter  Esc: quit│
└────────────────────────────────────────────────────────────────────────┘
```

#### With Detail Panel (press `d`):
```
┌────────────────────────────────────┬───────────────────────────────────┐
│ [Instances] [Resources] [Snapshots] [Settings]          ● USER        │
│ 3 instances | Filter: all                                             │
├────────────────────────────────────┼───────────────────────────────────┤
│                                    │ mainnet-node                      │
│ ➤ ● mainnet-node   node   rolling  │ ───────────────────────────────  │
│                    ✓ synced        │ Status: ✓ Synced                 │
│                                    │ Level: L5,847,234                │
│   ● archive-node   node   archive  │ Last block: 4s ago               │
│                    ⚠ catching up   │ Chain: Stable ═══════════        │
│                                    │                                   │
│   ○ test-baker     baker           │ Resources:                        │
│                                    │  CPU:  35% ████████░░░░░         │
│                                    │  Mem:  4.2G ████████░░░░░        │
│                                    │  Disk: 89G ████████████░░        │
│                                    │                                   │
│                                    │ Network:                          │
│                                    │  RPC: 127.0.0.1:8732             │
│                                    │  P2P: 0.0.0.0:9732               │
│                                    │  Peers: 47/50                    │
│                                    │                                   │
├────────────────────────────────────┴───────────────────────────────────┤
│ 1-4: tabs  d: hide details  ↑/↓: move  Enter: actions  Esc: back     │
└────────────────────────────────────────────────────────────────────────┘
```

---

## Summary Table

| Pattern | Value | Complexity | Best For | Recommend? |
|---------|-------|------------|----------|------------|
| **Display:** ||||
| Table/List | ⭐⭐⭐⭐⭐ | Low | Many items, comparison | ✅ YES |
| Cards | ⭐⭐⭐ | Medium | Few items, rich detail | ❌ NO |
| Hybrid (expandable) | ⭐⭐⭐⭐ | Medium | Progressive disclosure | ✅ YES |
| **Navigation:** ||||
| Breadcrumbs | ⭐⭐⭐ | Low | Deep hierarchies (4+ levels) | ❌ NO |
| Tabs | ⭐⭐⭐⭐⭐ | Low | Flat sections (3-5) | ✅ YES |
| Status bar | ⭐⭐⭐ | Low | Minimal overhead | 🤔 MAYBE |
| Tabs + Breadcrumbs | ⭐⭐⭐⭐ | Medium | Mixed hierarchy | ❌ OVERKILL |

---

## Implementation Priority

### Phase 1: Tab Navigation (2-3 days)
1. Create `Tab_bar` widget (or use simple string formatting)
2. Add tabs to all main pages: Instances, Resources, Snapshots, Settings
3. Wire up number key shortcuts (1-4)
4. Update footer hints

### Phase 2: Detail Panel Toggle (2-3 days)
5. Add `detail_panel_visible` to instances page state
6. Use `Pane_layout` for split view when `d` is pressed
7. Render selected instance details in right panel
8. Show resource bars and metrics

### Phase 3: Polish (1-2 days)
9. Add visual tab highlighting
10. Smooth transitions
11. Keyboard shortcut hints

**Total effort:** ~5-8 days

---

## Conclusion

**Final Recommendations:**

✅ **Display:** Stick with **table/list view** + optional **detail panel** (Pane_layout)
✅ **Navigation:** Add **tabs** for main sections (Instances, Resources, Snapshots, Settings)
❌ **Skip:** Cards (wrong pattern), Breadcrumbs (unnecessary)

**Why:**
- Octez-manager is a **power user tool** for managing multiple services
- **Information density** > visual polish in terminal UIs
- **Tabs** provide clear context without consuming vertical space
- **Detail panel** gives best of both worlds - compact list + rich details on demand

**Next steps:**
1. Implement basic tab navigation
2. Add detail panel toggle
3. Test with real usage patterns
4. Iterate based on feedback
