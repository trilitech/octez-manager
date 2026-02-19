# TUI Development Guide

Guidelines for working on the TUI layer (`src/ui/`). For general project rules, see the root [AGENTS.md](../../AGENTS.md).

## TUI Architecture (CRITICAL)

### The Render Loop

The TUI render loop runs frequently (on every keypress, timer tick, and data update). Understanding this is critical to avoid performance issues.

```
┌─────────────────────────────────────────────────────────────┐
│                      RENDER LOOP                            │
│                                                             │
│  User Input ──► handle_key ──► update state ──► view()     │
│       ▲                                            │        │
│       │                                            ▼        │
│       └─────────────── render to terminal ◄────────┘        │
└─────────────────────────────────────────────────────────────┘
```

### CRITICAL RULE: No I/O During Rendering

**View functions must NEVER perform:**
- File I/O (`Node_env.read`, `open_in`, `Sys.file_exists`, `Sys.readdir`)
- Network I/O (RPC calls, HTTP requests)
- Shell commands (`Common.run`, `Common.run_out`)
- Any blocking operations

**Why:** The render loop runs many times per second. Even small I/O delays compound into noticeable lag.

### Background Schedulers

Data is fetched by background schedulers running in separate OCaml domains:

| Scheduler | Tick Rate | Data Provided |
|-----------|-----------|---------------|
| `Rpc_scheduler` | 1s | Node bootstrap status, head level, chain ID, protocol |
| `System_metrics_scheduler` | 0.5s | CPU, memory, disk usage, binary versions |
| `Delegate_scheduler` | 60s | Baker config, delegate participation, highwatermarks |
| `Data.refresh_cache` | 5s TTL | Service states from systemd |

Each scheduler populates in-memory caches that view functions read from.

### How to Access Data in View Functions

**CORRECT - Read from cache:**
```ocaml
(* Good: reads from in-memory cache *)
let has_dal = Delegate_scheduler.baker_has_dal ~instance in
let delegates = Delegate_scheduler.get_baker_delegates ~instance in
let rpc_metrics = Rpc_metrics.get ~instance in
let cpu_chart = System_metrics_scheduler.render_cpu_chart ~role ~instance in
```

**WRONG - Direct I/O in render path:**
```ocaml
(* BAD: reads file from disk on every render! *)
let has_dal = match Node_env.read ~inst:instance with
  | Ok pairs -> List.assoc_opt "OCTEZ_DAL_CONFIG" pairs |> Option.is_some
  | Error _ -> false
```

### Adding New Data to the Render Loop

If you need new data during rendering:

1. **DO NOT** add I/O calls directly in view functions
2. **DO** add the data fetch to the appropriate scheduler:
   - `Delegate_scheduler` for baker/delegate config (reads env files)
   - `Rpc_scheduler` for node RPC data
   - `System_metrics_scheduler` for system/process metrics
3. **DO** add a cached accessor function that reads from the scheduler's cache
4. **DO** use the cached accessor in view functions

**Example - Adding a new cached value:**

```ocaml
(* In the scheduler module *)
let my_cache : (string, my_data) Hashtbl.t = Hashtbl.create 17
let cache_lock = Mutex.create ()

(* Called by scheduler tick - does I/O *)
let refresh ~instance =
  let data = read_from_disk ~instance in  (* I/O happens here *)
  Mutex.protect cache_lock (fun () ->
    Hashtbl.replace my_cache instance data)

(* Called by view functions - no I/O *)
let get ~instance =
  Mutex.protect cache_lock (fun () ->
    Hashtbl.find_opt my_cache instance)
```

### Data Flow Summary

```
┌──────────────────┐     ┌─────────────────┐     ┌──────────────┐
│  Background      │     │   In-Memory     │     │    View      │
│  Schedulers      │────►│   Caches        │────►│  Functions   │
│  (do I/O)        │     │   (fast reads)  │     │  (no I/O!)   │
└──────────────────┘     └─────────────────┘     └──────────────┘
     Domains 2-6              Hashtables            Main thread
```

### Testing TUI Form Changes (MANDATORY)

**When adding, removing, or reordering form fields, you MUST run the golden path tests and fix them if necessary.**

#### Why This Matters

The golden path test (`test/test_golden_path_tui_v2.ml`) validates the complete end-to-end flow of creating services through the TUI. It uses declarative keypresses to navigate forms and expects specific field counts to reach the "Confirm & Install" button.

**The Problem:**
- The golden path test is **intentionally skipped during local `dune runtest`** (requires systemd in Docker)
- This means `dune build && dune runtest` will pass locally even if the test is broken
- The test only runs in CI, where failures block the PR

#### Affected Forms

Forms with field-count dependencies in the golden path test:
- `install_node_form_v3` - Node installation form
- `install_dal_node_form_v3` - DAL node installation form
- `install_baker_form_v3` - Baker installation form
- `install_accuser_form_v3` - Accuser installation form

#### Required Steps When Modifying Forms

1. **Check if the form is tested in the golden path:**
   ```bash
   grep -n "install_.*_form" test/test_golden_path_tui_v2.ml
   ```

2. **Count the field change:**
   - Adding a field: increment Down key count
   - Removing a field: decrement Down key count
   - Reordering fields: verify navigation logic still works

3. **Update the test in the SAME commit:**
   ```ocaml
   (* Before: Baker form has 13 fields *)
   (* Baker form: 13 fields + confirm. Cursor on field 0, need 13 Downs *)
   @ submit_form ~downs:13
   
   (* After: Added "Remote Signer" field, now 14 fields *)
   (* Baker form: 14 fields + confirm. Cursor on field 0, need 14 Downs *)
   @ submit_form ~downs:14
   ```

4. **Document the change in the test comment** - explain what field was added/removed

5. **Verify in CI** - the test will run automatically, but check the logs if it fails

#### Why The Test Doesn't Run Locally

The golden path test creates real systemd services. To protect developer machines from service pollution, the test detects non-CI environments and skips:

```ocaml
(** SAFETY: This test ONLY runs in CI (Docker containers with systemd).
    It will skip when run locally to avoid creating services on your system. *)
```

This safety feature means **you cannot catch these failures locally with `dune runtest`**.

#### Detection Pattern

When you see CI failures like:
```
WaitFor timeout after 500 iterations
Condition: ScreenContains("Hint: c create")
```

This usually means the form submission failed (cursor didn't reach Confirm button) due to incorrect field count.

---

## Working with Miaou

The Miaou TUI library lives in its own repository and is pinned via opam.

### Important Rules

- **Do NOT vendor Miaou.** If temporary vendoring is needed for debugging, revert it before committing.
- **Changes needed in Miaou** should be documented and addressed in the Miaou repo, not worked around in octez-manager.
- **Check the Miaou changelog** when updating the pin - API changes may require updates.

### Page Structure

Pages implement the `PAGE_SIG` interface:

```ocaml
module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG = struct
  type state = { ... }
  type msg = ...
  type pstate = state Navigation.t
  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  let init () = ...
  let view ps ~focus ~size = ...      (* NO I/O HERE *)
  let handle_key ps key ~size = ...
  (* ... other functions *)
end
```

### Keymap Format

Keymaps use records, not tuples:

```ocaml
let keymap _ =
  let kb key action help =
    {Miaou.Core.Tui_page.key; action; help; display_only = false}
  in
  [
    kb "Enter" do_action "Perform action";
    kb "Esc" back "Back";
    {Miaou.Core.Tui_page.key = "?"; action = noop; help = "Help"; display_only = true};
  ]
```

### Direct_page: Simplified Page Development

For simple pages, prefer `Direct_page` over the full `PAGE_SIG`. It requires only 3 functions instead of 13:

```ocaml
include Miaou.Core.Direct_page.Make (struct
  include Miaou.Core.Direct_page.With_defaults (struct
    type state = { items : string list; cursor : int }

    let init () = { items = []; cursor = 0 }

    let view s ~focus ~size =
      (* Render your page *)
      render_items s.items s.cursor

    let on_key s key ~size =
      match key with
      | "q" -> Miaou.Core.Direct_page.quit () ; s
      | "Esc" -> Miaou.Core.Direct_page.go_back () ; s
      | "Enter" -> Miaou.Core.Direct_page.navigate "details" ; s
      | "j" -> { s with cursor = s.cursor + 1 }
      | _ -> s
  end)
end)
```

**When to use Direct_page:**
- Simple pages with straightforward navigation
- Pages without complex modal handling
- New pages where you want minimal boilerplate

**When to use full PAGE_SIG:**
- Pages with custom modal key handling (`handle_modal_key`)
- Pages that need fine-grained control over all lifecycle functions
- Existing pages that already use PAGE_SIG

### Recommended Miaou Widgets

**Layout widgets** (`Miaou_widgets_layout`):

| Widget | Use Case |
|--------|----------|
| `Box_widget` | Bordered containers with 5 styles (Single, Double, Rounded, Heavy, Ascii) |
| `Flex_layout` | Row/column layouts with gap, padding, basis sizing (like CSS flexbox) |
| `Grid_layout` | CSS-grid-like layouts with Fr/Px/Auto track sizing |
| `Pane` | Split views (horizontal/vertical) |

**Focus management** (`Miaou_internals`):

| Widget | Use Case |
|--------|----------|
| `Focus_ring` | Named-slot focus for forms/toolbars with Tab/Shift-Tab navigation |
| `Focus_container` | Type-safe heterogeneous widget containers (GADT-based) |

**Display widgets** (`Miaou_widgets_display`):

| Widget | Use Case |
|--------|----------|
| `Pager_widget` | Scrollable text with search (`/`), wrap toggle (`w`) |
| `Sparkline_widget` | Inline charts for metrics |
| `Description_list` | Key-value displays |

**Example: Using Box_widget for status panels**

```ocaml
let render_status_box ~title ~content =
  Miaou_widgets_layout.Box_widget.render
    ~title
    ~style:Single
    ~width:40
    content
```

**Example: Using Focus_ring for forms**

```ocaml
let focus = Focus_ring.create ["name"; "network"; "confirm"] in
let focus, result = Focus_ring.handle_key focus key in
match result with
| `Handled -> (* key was Tab/Shift-Tab *) ...
| `Bubble -> (* pass to focused widget *) ...
```

