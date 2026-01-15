# AGENTS.md for octez-manager

Guidelines for AI agents and contributors working on the octez-manager repository.

## Project Overview

octez-manager is a TUI application for managing Octez blockchain services (nodes, bakers, accusers, DAL nodes). It uses the Miaou TUI library and is built with OCaml 5 / Dune.

### Repository Layout

```
src/                      # Main library (octez_manager_lib)
src/ui/                   # TUI components
src/ui/pages/             # Individual page implementations
src/ui/form_builder.ml    # Form system for install/edit wizards
src/ui/*_scheduler.ml     # Background data polling
test/                     # Unit tests
test/integration/         # Integration tests
docs/                     # Documentation site (Astro)
```

## Build & Verification

Before any commit:

```bash
dune build          # Verify compilation
dune runtest        # Run tests
dune fmt            # Format code (MUST pass before commit)
```

**Critical:** Every commit must be properly formatted. Do not create separate "formatting" commits.

## OCaml Coding Standards

### General Rules
- Interface-first: provide `.mli` before `.ml` for public modules
- Documentation in `.mli` using `(** ... *)` with `@param`, `@return` where helpful
- Prefer immutability and functional style
- Error handling: use `Result` and `Option`, avoid exceptions for control flow

### Forbidden
- `Obj.magic`
- Mutable globals (use proper state management)
- Incomplete pattern matches

### Discouraged
- `List.hd`, `Option.get` (use pattern matching or `_opt` variants)
- Stringly-typed code (use variants/records)
- Partial functions

### Code Duplication Prevention

**Search First Policy:** Before writing any new function, especially helpers or utilities:
1. Search the existing codebase for similar functionality
2. Check `src/common.ml` for general utilities
3. Check scheduler modules for cached data accessors
4. Refactor existing code to be more generic rather than duplicating

---

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

---

## Commit Messages

Use conventional commit format:

```
type(scope): description

[optional body]

Co-Authored-By: Claude <noreply@anthropic.com>
```

**Types:** `feat`, `fix`, `refactor`, `docs`, `test`, `chore`, `ci`

Keep the first line under 72 characters.

## Git Hygiene

- **Always use pull requests** - never push directly to main
- Keep diffs minimal and focused on the task
- Don't opportunistically fix unrelated issues
- Never commit secrets or credentials
- Use `git mv` for renames to preserve history

## Questions or Uncertainty

When unsure about:
- Architectural decisions
- API design choices
- Whether to add I/O to a render path
- Breaking changes

Ask for confirmation before proceeding.
