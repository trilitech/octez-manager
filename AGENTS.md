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

### Recommended: Install Git Hooks

**Install once** to automatically run checks before every commit:

```bash
./scripts/install-git-hooks.sh
```

This installs a pre-commit hook that automatically:
- Formats code (`dune fmt`)
- Checks/fixes copyright headers
- Verifies the build passes
- Runs quick unit tests

### Manual Verification

If not using hooks, run these before every commit:

```bash
dune build                      # Verify compilation
dune runtest                    # Run tests
dune fmt                        # Format code (MUST pass before commit)
./scripts/check-copyright.sh    # Verify copyright headers (MUST pass before commit)
```

**Critical:**
- **Every commit must compile independently.** You should be able to run `git rebase --exec 'dune build' main` and have every commit pass. Broken intermediate commits destroy `git bisect`.
- Every commit must be properly formatted. Do not create separate "formatting" commits.
- Every commit must have correct copyright headers. Run `./scripts/check-copyright.sh --fix` to automatically update headers if needed.
- Shell completions must be up to date. Run `make completions` after CLI changes.
- **Never weaken checks to pass CI.** Fix the code instead. Don't skip hooks, disable lints, or relax thresholds to make a build green.
- To bypass hooks temporarily: `git commit --no-verify` (use sparingly!)

### Verification Check Pattern

When adding new verification checks (like format-check, completions-check), follow this **non-destructive pattern**:

```makefile
# CORRECT: Non-destructive check (matches CI)
check-something:
	@mkdir -p /tmp/octez-something-check
	@generate-something --out-dir /tmp/octez-something-check
	@if ! diff -q expected/file /tmp/octez-something-check/file >/dev/null 2>&1; then \
		echo "ERROR: Something is out of date."; \
		echo "Run: make generate-something"; \
		diff -u expected/file /tmp/octez-something-check/file || true; \
		rm -rf /tmp/octez-something-check; \
		exit 1; \
	fi
	@rm -rf /tmp/octez-something-check
	@echo "Something is up to date."
```

**Why this pattern?**
- ✅ **Non-destructive**: Doesn't modify the working directory
- ✅ **Consistent with CI**: Same logic runs locally and in CI
- ✅ **Helpful**: Shows actual diff when check fails
- ✅ **Clean**: Cleans up temp directory after check

**Anti-pattern (DO NOT use):**
```makefile
# WRONG: Destructive check
check-something: generate-something
	@git diff --exit-code something/ || exit 1
```

This modifies the working directory, leaving uncommitted changes if the check fails.

## OCaml LSP Server

AI coding agents can use the OCaml LSP server (`ocamllsp`) for code intelligence features like go-to-definition, find-references, hover documentation, and workspace symbol search.

### Setup by Tool

#### OpenCode

OpenCode has **built-in OCaml LSP support**. The project includes an `opencode.json` config that routes through `opam exec` to find `ocamllsp` in the project's local opam switch:

```json
{
  "$schema": "https://opencode.ai/config.json",
  "lsp": {
    "ocaml-lsp": {
      "command": ["opam", "exec", "--", "ocamllsp"]
    }
  }
}
```

This file is committed to the repo -- no manual setup needed. Just start OpenCode in the project directory. The LSP server starts automatically when `.ml`/`.mli` files are opened.

**Note:** If you add or change the `opencode.json` config, you must restart the OpenCode session for changes to take effect.

#### Claude Code

The OCaml LSP plugin is available via the [claude-code-lsps](https://github.com/Piebald-AI/claude-code-lsps) marketplace:

```bash
# Add the marketplace (one-time)
claude
/plugin marketplace add Piebald-AI/claude-code-lsps

# Install the OCaml LSP plugin
/plugins  # Navigate to Marketplaces > claude-code-lsps > Browse plugins
# Select ocaml-lsp with spacebar, press "i" to install
# Restart Claude Code
```

#### Other Tools

Any tool that supports LSP can use `ocamllsp`. Ensure the binary is reachable:

```bash
# The binary lives in the project's local opam switch
opam exec -- which ocamllsp
# → /home/<user>/dev/octez-manager/_opam/bin/ocamllsp

# If your tool doesn't go through opam exec, add _opam/bin to PATH:
eval $(opam env)
```

### Building the Index for Project-Wide References

By default, `findReferences` only searches the current file. To enable **project-wide** find references, you must build the ocaml-index:

```bash
opam exec -- dune build @ocaml-index
```

This creates an index in `_build/default/.ocaml-index` that the LSP uses for cross-file reference lookups.

### Keeping the Index Up to Date

**The index must be rebuilt when code changes.** Options:

1. **Manual rebuild** after significant changes:
   ```bash
   opam exec -- dune build @ocaml-index
   ```

2. **Continuous rebuild** during development:
   ```bash
   opam exec -- dune build @ocaml-index --watch
   ```

Note: Unlike `dune build @check`, the `@ocaml-index` target builds the entire project including tests.

### Available LSP Operations

| Operation | Status | Description |
|-----------|--------|-------------|
| `hover` | ✅ | Type signature and documentation |
| `goToDefinition` | ✅ | Jump to symbol definition |
| `findReferences` | ✅ | Find all usages (requires index for cross-file) |
| `documentSymbol` | ✅ | List symbols in current file |
| `workspaceSymbol` | ✅ | Search symbols across project |
| `goToImplementation` | ❌ | Not supported by ocaml-lsp |
| `incomingCalls` | ❌ | Not supported by ocaml-lsp |
| `outgoingCalls` | ❌ | Not supported by ocaml-lsp |

### Requirements

Project-wide references require:
- OCaml 5.2+ (we use 5.3.0)
- Dune 3.16+ (we use 3.20.2)
- ocaml-lsp-server 1.18+ (we use 1.23.1)
- Merlin 5.1-502+ (we use 5.6-504)

All requirements are satisfied by the project's opam switch.

## OpenCode Configuration

The project includes an `opencode.json` config that is committed to the repo. It provides:

### Auto-Formatting

OCaml files (`.ml`, `.mli`) are automatically formatted via `ocamlformat` when written or edited. The formatter runs through `opam exec` to use the project's local switch.

### Custom Commands

The following commands are available in the OpenCode TUI (type `/` to see them):

| Command | Description |
|---------|-------------|
| `/build` | Run `dune build` and fix compilation errors |
| `/test` | Run `dune runtest` and fix test failures |
| `/fmt` | Format code with `dune fmt` |
| `/copyright` | Fix and verify copyright headers |
| `/pre-commit` | Full pre-commit sequence (fmt + copyright + build + test) |
| `/index` | Rebuild OCaml LSP index for project-wide references |
| `/archdb <query>` | Query the architecture database |

### Pre-Allowed Commands

To reduce permission prompts, the following are auto-allowed:

- `opam exec -- dune *` and `opam exec -- ocaml*` (build/test/format)
- Read-only git commands (`status`, `diff`, `log`, `branch`, `show`, `fetch`)
- `gh pr` and `gh issue` (GitHub CLI)
- `sqlite3 docs/architecture.db` (architecture queries)
- `make *` (Makefile targets)

Destructive operations (`gh api`, `git push`, `git rebase`, etc.) still require confirmation.

### Instructions

AGENTS.md is loaded as an instruction file, so its contents are available to the agent as context.

---

## Integration Tests

Integration tests live in `test/integration/cli-tester/tests/` and are run in parallel during CI using time-based sharding.

### Test Independence (CRITICAL)

**Every integration test MUST be completely independent and self-contained.**

Tests are distributed across parallel shards in CI, which means:
- Tests may run in **any order**
- Tests run **simultaneously** in different containers
- Tests **cannot depend** on other tests running first

### Writing Independent Tests

**Required pattern for all integration tests:**

```bash
#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Description"

# 1. Use a unique instance name for this test
TEST_INSTANCE="test-unique-name"

# 2. Cleanup any previous state
cleanup_instance "$TEST_INSTANCE" || true

# 3. Create your own test instances
om install-node \
    --instance "$TEST_INSTANCE" \
    --network shadownet \
    --snapshot \
    --snapshot-no-check \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "127.0.0.1:UNIQUE_PORT" \
    --service-user tezos \
    --no-enable 2>&1 || true

# 4. Run your test assertions
# ...

# 5. Cleanup at the end
cleanup_instance "$TEST_INSTANCE" || true

echo "Test passed"
```

### DO NOT Do This

```bash
# ❌ BAD: Assumes another test created an instance
if ! instance_exists "$TEST_INSTANCE"; then
    echo "ERROR: Run test 01-install first"
    exit 1
fi

# ❌ BAD: Uses hardcoded instance name shared with other tests
TEST_INSTANCE="test-node"  # Conflicts with other tests!

# ❌ BAD: No cleanup - leaves instances for other tests
om install-node --instance "$TEST_INSTANCE"
# ... test code ...
# exit (no cleanup!)
```

### Port Allocation

When tests need RPC endpoints, use unique ports:
- Test 01: `127.0.0.1:18731`
- Test 02: `127.0.0.1:18732`
- Test 03: `127.0.0.1:18733`
- etc.

Avoid the default `127.0.0.1:8732` which may conflict with other tests running in parallel.

### Verifying Test Independence

Before committing a new test, verify it can run standalone:

```bash
# Run just your test
cd test/integration/cli-tester
./run-tests.sh node/XX-your-test.sh

# Run it multiple times
for i in {1..3}; do ./run-tests.sh node/XX-your-test.sh; done

# Run it alongside other tests (simulates parallel execution)
./run-tests.sh node/01-install.sh & \
./run-tests.sh node/XX-your-test.sh & \
wait
```

If any run fails, the test has dependencies or conflicts.

---

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
- `exit` in library code (only allowed in binary entry points)
- Catching `Stack_overflow` or `Out_of_memory` without compelling justification

### Discouraged
- `List.hd`, `Option.get` (use pattern matching or `_opt` variants)
- Stringly-typed code (use variants/records)
- Partial functions
- Polymorphic equality `(=)` on structured types — use typed comparators (e.g. `String.equal`, `Int.equal`)
- `Stdlib.compare` on structured types — use typed comparators
- `Hashtbl` in public APIs — prefer `Map` for determinism; `Hashtbl` is fine for internal caches

### TODO/FIXME Comments

TODO and FIXME comments must reference a GitHub issue so they remain trackable:

```ocaml
(* TODO: https://github.com/trilitech/octez-manager/issues/123
   Handle the case where the node is unreachable *)

(* FIXME: #456 — Race condition when two schedulers update simultaneously *)
```

Untracked TODOs rot. If a TODO doesn't have an issue yet, create one.

### Exposing Internals for Tests

When you need to expose internal functions for testing, use an explicit internal module:

```ocaml
(* In the .ml file *)
module Internal_for_tests = struct
  let parse_version_string = parse_version_string
  let validate_port = validate_port
end
```

```ocaml
(* In the .mli file — exclude from public docs *)
(**/**)
module Internal_for_tests : sig
  val parse_version_string : string -> (int * int * int) option
  val validate_port : int -> bool
end
(**/**)
```

The `(**/**)` stop comment excludes the module from odoc-generated documentation.

### Logging Guidelines

Use appropriate log levels consistently:

| Level | Use For | Example |
|-------|---------|---------|
| Debug | Execution flow details for developers | "Parsing RPC response for instance X" |
| Info | Useful context during normal operation | "Connected to node at localhost:8732" |
| Warning | Actionable items requiring attention | "Disk usage above 90% for instance X" |
| Error | Requires intervention, include context | "Failed to reach node RPC: connection refused" |

- Never log sensitive data (keys, passwords, tokens)
- Include enough context to diagnose the issue (instance name, port, file path)
- Prefer structured data over string interpolation where possible

### Code Duplication Prevention (MANDATORY)

AI agents tend to duplicate code at 2-3x the rate of human developers. This is because agents optimize for the immediate task without long-term memory of what already exists elsewhere in the codebase. The architecture database compensates for this -- **use it.**

**Before writing any new function, you MUST complete this checklist:**

1. **Search the architecture database** (mandatory, not optional):
   ```bash
   dune exec tools/arch_query.exe -- search "what your function does"
   ```
   If a similar function exists, **use it or extend it** instead of writing a new one.

2. **Search the actual codebase** (the DB may lag behind uncommitted changes):
   ```bash
   grep -rn "your_keyword" src/
   ```

3. **Check common locations:**
   - `src/common.ml` for general utilities
   - Scheduler modules for cached data accessors
   - The module you're about to duplicate from -- can it be parameterized instead?

4. **If you find a near-duplicate:** refactor the existing code to be more generic rather than creating a copy. Extract shared logic into a helper, functor, or shared module.

**Skipping this checklist is not acceptable.** If you write a function that duplicates existing code because you didn't search first, the CI metrics gate will catch it and the PR will fail.

### Module Inclusion: `open` vs `include`

**Prefer `open` over `include` for internal modules.**

When extracting code into submodules, use `open` to bring functions into scope without re-exporting them:

```ocaml
(* PREFERRED: Use 'open' *)
open Rresult
open Installer_types
open Helpers  (* Functions available locally, not re-exported *)

let my_function () =
  backup_file_if_exists path  (* From Helpers, but not part of public API *)
```

**Avoid `include` unless explicitly needed for API design:**

```ocaml
(* DISCOURAGED: Using 'include' *)
include Helpers  (* Re-exports ALL functions from Helpers *)
(* Now all Helpers functions are part of this module's public API *)
```

**Why prefer `open`?**
- **Explicit API boundaries**: Only intentionally exposed functions appear in `.mli`
- **Clearer dependencies**: Obvious which modules provide functionality
- **Easier refactoring**: Moving functions between modules doesn't change public API
- **Better IDE support**: "Go to definition" can identify source modules

**When `include` is appropriate:**
- Delegation pattern: thin wrapper modules that intentionally re-export everything
- Type sharing: when you need to expose types from another module as if they were local

**For explicit module references:**

When extracting multiple related submodules, consider using explicit module aliases for even greater clarity:

```ocaml
(* MOST EXPLICIT: Module aliases *)
module State = My_module_state
module Layout = My_module_layout

let view state = Layout.render_view state
let init = State.create ()
```

This makes the origin of every function crystal clear, though it's more verbose.

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

## Pull Request Requirements

All pull requests must include:

1. **Changelog entry** - Add an entry to `CHANGELOG.md` under the `[Unreleased]` section
   - Use the appropriate category: `Added`, `Changed`, `Fixed`, `Deprecated`, `Removed`, or `Security`
   - Write user-facing descriptions (what changed, not how it was implemented)
   - Reference the issue number if applicable
   - Example: `- Node sync status now displays bootstrap phase (fixes #123)`
   - Small internal refactorings or documentation-only changes may skip this if they don't affect users

2. **Tests** (for bug fixes) - See "Bug Fix PRs" section below

## Git Hygiene

- **Always use pull requests** - never push directly to main
- Keep diffs minimal and focused on the task
- Never commit secrets or credentials
- Use `git mv` for renames to preserve history
- **Ask for confirmation before force pushing** - force push operations rewrite history and should only be done with explicit user approval
- **Never delete untracked files without confirmation** - user scripts, test data, and work-in-progress files must be preserved unless explicitly requested

### Atomic Commits

- **Separate refactoring from functional changes.** A commit that renames a function should not also change its behavior. This makes each commit reviewable and revertable in isolation.
- **No "penelop" commits.** Don't undo work from an earlier commit in a later one within the same PR. If you realize an earlier approach was wrong, amend or squash — don't add a "revert part of commit X" commit.
- **Squash fixups before merge.** Typo fixes, formatting corrections, and "oops forgot this file" commits should be squashed into the commit they fix. Preserve meaningful atomic commits that tell the story of the change.
- **Don't touch unrelated code.** A commit that fixes a bug in the installer should not also rename variables in the DAL scheduler. If you notice something unrelated, fix it in a separate commit or create a gardening issue.

### Opportunistic Code Quality Improvements

Agents lack the long-term memory that lets human developers notice and fix code smells over time. To compensate, **small code quality improvements are encouraged inline** when you encounter them during normal work. Larger refactorings must be separate.

**DO fix inline** (same PR, same commit or dedicated commit):
- Extracting a duplicated helper (< 20 lines) to a shared module
- Adding a missing doc comment to a function you're already modifying
- Renaming a misleading variable or parameter in code you're touching
- Replacing a stringly-typed parameter with a variant in code you're editing
- Adding a missing `.mli` for a module you're already changing

**DO NOT fix inline** (create a gardening issue instead):
- Splitting a large file into multiple modules
- Refactoring multiple pages/modules to share a base abstraction
- Rewriting a function's core algorithm
- Any change that touches files unrelated to your current task
- Any improvement that would make the diff significantly harder to review

**Rule of thumb:** If the improvement touches only files you're already modifying and adds fewer than ~30 lines to the diff, do it inline. Otherwise, create an issue:
```bash
gh issue create --label gardening --title "gardening: [category] description"
```

## Parallel Work with Worktrees

Multiple agents (or an agent and a human) can work on the repository simultaneously using **git worktrees**. Each worktree is an independent working directory sharing the same `.git` history.

### Setting Up a Worktree

```bash
# Create a worktree for a new branch (from the main repo directory)
git worktree add ../octez-manager-feat-xyz -b feat/xyz

# Or for an existing branch
git worktree add ../octez-manager-fix-123 fix/issue-123

# List active worktrees
git worktree list

# Remove a worktree when done
git worktree remove ../octez-manager-feat-xyz
```

### Worktree Rules

- **Each worktree must be on a different branch** — git enforces this
- **Build artifacts are per-worktree** — each has its own `_build/` directory
- **opam switch is shared** — no need to reinstall dependencies
- **Never delete a worktree directory manually** — always use `git worktree remove`

## Issue Tracking for Parallel Work

When multiple agents may work concurrently, proper issue tracking prevents conflicts and duplicated effort.

### Starting Work on an Issue

1. **Assign the issue to yourself** before starting:
   ```bash
   gh issue edit <NUMBER> --add-assignee @me
   ```
2. **Create a branch** (in a worktree if working in parallel):
   ```bash
   git worktree add ../octez-manager-issue-<NUMBER> -b feat/issue-<NUMBER>
   ```

### Ending a Session

If the issue is **fully resolved**: create the PR and let the PR reference close it (`fixes #NUMBER`).

If work is **incomplete** (session ending, context limit, etc.):
1. **Commit and push** all progress so far
2. **Add a comment to the issue** summarizing:
   - What was done
   - What remains to be done
   - Any blockers or decisions needed
   - The branch name with the in-progress work
3. **Unassign yourself** so another agent can pick it up:
   ```bash
   gh issue edit <NUMBER> --remove-assignee @me
   ```

### Example Issue Comment (Incomplete Work)

```markdown
### Progress update

**Branch:** `feat/issue-42`

**Done:**
- Implemented the new RPC endpoint parser
- Added unit tests for happy path

**Remaining:**
- Error handling for malformed JSON responses
- Integration test

**Notes:**
- The parser needs to handle both v1 and v2 response formats (see `src/rpc_client.ml:180`)
```

## Interacting with GitHub Copilot Reviews

When Copilot reviews a PR, follow these rules to avoid noise:

### DO NOT reply individually to each Copilot comment

Replying to individual Copilot review comments with `@copilot` triggers it to create a **separate PR for each reply**. This creates significant noise (we observed 11 spurious PRs from 10 individual replies).

### DO use a single PR-level comment

After fixing all Copilot feedback, post **one PR-level comment** summarizing all changes, then re-request review:

```bash
# Post a single summary comment
gh pr comment <NUMBER> --body "## Copilot feedback addressed

1. **file.ml:42** — Fixed X
2. **file.ml:99** — Fixed Y
...

@copilot please re-review this PR."

# Re-request copilot as reviewer
gh pr edit <NUMBER> --add-reviewer "copilot-pull-request-reviewer[bot]"
```

### Resolving Copilot threads

Copilot does **not** resolve its own threads, even after re-review. Resolve them via the GraphQL API:

```bash
# Get thread IDs
gh api graphql -f query='{
  repository(owner: "trilitech", name: "octez-manager") {
    pullRequest(number: <NUMBER>) {
      reviewThreads(first: 50) {
        nodes { id isResolved }
      }
    }
  }
}' --jq '.data.repository.pullRequest.reviewThreads.nodes[] | select(.isResolved == false) | .id'

# Resolve a thread
gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "<THREAD_ID>"}) { thread { isResolved } } }'
```

### Closing spurious Copilot PRs

If Copilot creates unwanted sub-PRs, close them and delete their branches:

```bash
gh pr close <NUMBER> --comment "Closing: auto-created by copilot. Feedback addressed in #<ORIGINAL_PR>." --delete-branch
```

## Bug Fix PRs

**Every bug fix PR MUST include a test** that reproduces the bug and validates the fix. This applies to both unit tests and integration tests as appropriate.

- The test should **fail without the fix** and **pass with the fix**
- If the bug is in rendering or UI logic, add a headless TUI test (see `test/test_instances_page.ml` for examples)
- If the bug is in CLI behavior, add an integration test in `test/integration/cli-tester/tests/`
- If the bug is in core logic, add a unit test in `test/unit_tests.ml` or a dedicated test file

**If a test is truly impossible**, the PR description must explain why with detailed arguments (e.g., the bug only manifests with real hardware, requires network conditions that cannot be simulated, etc.). "It's hard to test" is not a valid reason to skip the test.

## Questions or Uncertainty

When unsure about:
- Architectural decisions
- API design choices
- Whether to add I/O to a render path
- Breaking changes

Ask for confirmation before proceeding.

---

## Plan-First Workflow

For non-trivial changes, write a plan before writing code. Plans serve as the primary review unit — it's cheaper to catch a wrong approach in a plan than in a 500-line diff.

### When a Plan Is Required

- New abstractions or module boundaries
- Changes touching 3+ files (non-local refactoring)
- New TUI pages or major page rewrites
- Changes to scheduler architecture or data flow
- Anything that affects the public CLI interface

### Plan Format

Plans live in `plans/<short-name>.md` and must include:

```markdown
# Plan: <short description>

## Goal
What user-visible problem does this solve?

## Affected Modules
Which files/modules will be created, modified, or deleted?

## Approach
How will you implement this? Include key design decisions.

## Risks
What could go wrong? What assumptions are you making?

## Verification
How will you verify the change works? (tests, manual steps, CI checks)
```

### Workflow

1. **Write the plan** and get approval before writing code
2. **Implement** following the approved plan
3. **Validate** through CI pipeline
4. **Review** the diff against the plan

Small changes (bug fixes, single-file edits, documentation) don't need plans — use good judgment.

---

## Gardening & Architecture Index

The project uses a "gardening" approach for ongoing code maintenance. See `GARDENING.md` for the full guide.

### Architecture Database & Query Tools

An SQLite database at `docs/architecture.db` indexes the entire codebase: modules, functions (with type signatures and doc comments), types (with record fields and variant constructors). It is gitignored and regenerated from `.cmt`/`.cmti` files produced by `dune build`.

#### Generating the Database

```bash
# Build the project first (produces .cmt/.cmti files)
dune build

# Populate the database (~500ms, scans all .cmt/.cmti files)
make arch-index
# or: dune exec -- tools/arch_index.exe
```

The indexer extracts:
- **Modules**: path, line count, `.mli` presence
- **Functions**: name, type signature, line range, exposed in `.mli`, doc comment, mutable pattern usage
- **Types**: name, kind (record/variant/abstract/alias), fields, constructors, doc comment

**Note:** Function and type line counts **exclude doc comments** to avoid penalizing documentation. The count starts from the `let`/`type` keyword, not from any preceding `(** ... *)` comment.

Doc comments from `.mli` files are preferred; `.ml` implementation comments are used as fallback. Hand-written intent fields set via `sqlite3 UPDATE` are preserved across re-indexing.

#### Querying with `arch-query`

The `arch-query` CLI provides canned queries and fuzzy search without writing SQL:

```bash
# Fuzzy search by intent, name, or signature
dune exec tools/arch_query.exe -- search "network download"
dune exec tools/arch_query.exe -- search -t 0.7 "snapshot bootstrap"    # 70% threshold
dune exec tools/arch_query.exe -- search -k functions "port validation"  # functions only
dune exec tools/arch_query.exe -- search -k types "binary source"        # types only

# Find types by their shape (field names and/or field types)
dune exec tools/arch_query.exe -- type-search -f instance -T string -T bool
dune exec tools/arch_query.exe -- type-search -T string -T int

# Code health queries
dune exec tools/arch_query.exe -- duplicates        # duplicate functions across modules
dune exec tools/arch_query.exe -- large-files        # files > 500 lines (--min N)
dune exec tools/arch_query.exe -- large-functions    # functions > 50 lines (--min N)
dune exec tools/arch_query.exe -- missing-docs       # exposed functions without docs
dune exec tools/arch_query.exe -- missing-mli        # modules without .mli
dune exec tools/arch_query.exe -- god-modules        # modules with 30+ functions (--min N)
dune exec tools/arch_query.exe -- unsafe-strings     # string fields appearing 3+ times
dune exec tools/arch_query.exe -- mutables           # mutable pattern usage (ref, :=, !, mutable fields)

# Summary and raw SQL
dune exec tools/arch_query.exe -- stats
dune exec tools/arch_query.exe -- sql "SELECT ..."

# Rebuild the database
dune exec tools/arch_query.exe -- refresh

# Machine-readable metrics (for CI)
dune exec tools/arch_query.exe -- metrics -o metrics.json

# Compare against baseline (exits 1 on regression)
dune exec tools/arch_query.exe -- compare baseline.json current.json
```

#### CI Integration

The CI pipeline runs `arch-query metrics` on every build and compares against the main branch baseline. **PRs that increase duplicates, large files/functions, missing docs, or other tracked metrics will fail CI.**

Tracked metrics (regressions block merge):
- `duplicate_groups` -- must not increase
- `large_files` (>500 lines) -- must not increase
- `large_functions` (>50 lines) -- must not increase
- `missing_docs` (exposed without docs) -- must not increase
- `missing_mli` -- must not increase
- `god_modules` (>30 functions) -- must not increase
- `unsafe_string_fields` -- must not increase
- `mutable_fields` -- must not increase
- `functions_with_mutables` -- must not increase
- `doc_coverage_pct` -- must not decrease

#### When Creating New Functions

Before writing a new function:

1. **Search for existing implementations:**
   ```bash
   dune exec tools/arch_query.exe -- search "what your function does"
   dune exec tools/arch_query.exe -- duplicates
   ```
2. **Also search the actual codebase** (the DB may lag behind uncommitted changes):
   ```bash
   grep -rn "your_keyword" src/
   ```
3. If the function is a utility that others might need, add it to `src/common.ml`

### Gardening Tasks

When you notice code health issues during development:
- Large files (>500 lines)
- Large functions (>50 lines)
- String parameters that should be typed
- Missing .mli files
- Duplicated code

For small fixes in files you're already touching, fix them inline (see "Opportunistic Code Quality Improvements" above). For everything else, create a gardening issue:
```bash
gh issue create --label gardening --title "gardening: [category] description"
```

---

## Refactoring: Moving Code Between Files

When splitting large files or moving code between modules, **never use Read+Write** to copy code. AI agents can accidentally drop lines, subtly modify code, or hallucinate changes when passing large code blocks through their context window.

### CRITICAL RULE: Use Shell Commands for Code Movement

**DO use shell commands for extracting/moving code:**

```bash
# Extract lines 100-300 to a new file
sed -n '100,300p' src/large_file.ml > src/new_module.ml

# Extract from a pattern to another pattern
sed -n '/^let prompt_input/,/^let logging_mode_term/p' src/main.ml > src/cli_prompts.ml

# Split file at specific patterns
csplit src/main.ml '/^let install_node_cmd/' '/^let instance_term/'
```

**DO use Edit tool only for small surgical changes:**
- Adding license headers to new files
- Adding `open` or `include` statements
- Updating `dune` files
- Removing the moved section from the original file (after verifying the extraction)

**DO NOT use Read+Write to "copy" code:**
```
❌ Read src/main.ml → Write src/new_file.ml with "copied" content
```

### Verification Steps

After each code movement:

```bash
# 1. Verify line counts make sense
wc -l src/original.ml src/new_module.ml

# 2. Verify compilation
dune build

# 3. Run tests
dune runtest

# 4. Check formatting
dune fmt

# 5. Optionally verify exact content with checksums
sed -n '100,300p' src/original_backup.ml | md5sum
cat src/new_module.ml | tail -n +7 | md5sum  # skip header lines
```

### Refactoring Workflow

1. **Create a branch** for the refactoring work
2. **Identify extraction boundaries** - find exact line numbers or patterns
3. **Extract with `sed`** - guaranteed exact copy
4. **Add necessary scaffolding** with Edit:
   - License header
   - Module imports (`open`, `include`)
   - Interface file (`.mli`)
5. **Update `dune`** to include the new module
6. **Remove extracted code** from original with Edit
7. **Update original** to use the new module
8. **Verify** with `dune build && dune runtest && dune fmt`
9. **Commit** with clear message describing what was moved

---

## Code Review Guidelines

When reviewing PRs:

### Focus on Issues Only

- **Do:** Point out bugs, architectural problems, performance issues
- **Don't:** Praise what works well - assume good code is expected
- **Don't:** State that tests pass - CI already validates this

### Be Concise

- Use bullet points
- One issue per bullet
- Include line numbers for specific problems
- Provide fix suggestions, not explanations of the problem

### Review Format

```markdown
## Review

### BLOCKER 🔴
- Issue description (line X)
- **Fix:** Concrete solution

### Issues
- Problem 1 (line Y)
- Problem 2 (lines Z-W)

### Questions
- Clarification needed on X
```

### What to Skip

- ❌ "What's great" sections
- ❌ Testing reports (CI handles this)
- ❌ Praise or encouragement
- ❌ Long explanations of why something is wrong
- ❌ Multiple comments - use one comment with bullets

### What to Include

- ✅ Specific line numbers
- ✅ Concrete fix suggestions
- ✅ Links to correct patterns in codebase
- ✅ Severity indicators (BLOCKER, issue, question)

---

## Common Mistakes

Consolidated list of mistakes agents repeatedly make. Check this before submitting a PR.

1. **I/O in view functions.** The render loop runs many times per second. File reads, RPC calls, or shell commands in `view` functions cause visible lag. Use scheduler caches instead. (See: TUI Architecture)

2. **Duplicating existing code.** Search `arch_query` and `grep` before writing new functions. The CI metrics gate catches duplicates and blocks the PR. (See: Code Duplication Prevention)

3. **Commits that don't compile.** Every commit must build independently. A commit that adds a function call before the commit that defines it breaks `git bisect`. (See: Build & Verification)

4. **Modifying golden path test counts.** Adding or removing form fields without updating `submit_form ~downs:N` in the golden path test. This test doesn't run locally — it only fails in CI. (See: Testing TUI Form Changes)

5. **Stale shell completions.** Adding CLI subcommands without running `make completions`. The completions check in CI will catch this.

6. **Missing copyright headers.** Creating new files without running `./scripts/check-copyright.sh --fix`. The copyright check in CI will reject the PR.

7. **TODO without issue reference.** Writing `(* TODO: fix this later *)` without a GitHub issue link. These are untrackable and rot. (See: TODO/FIXME Comments)

8. **Weakening CI to pass.** Disabling checks, skipping hooks (`--no-verify`), or relaxing thresholds instead of fixing the underlying issue.

9. **Mixing refactoring with functional changes.** A single commit that renames variables AND changes behavior is impossible to review or revert cleanly. Separate them. (See: Atomic Commits)

10. **Using `include` instead of `open`.** Re-exporting an entire module's API when you only need local access. This pollutes the public interface. (See: Module Inclusion)

11. **Large Read+Write for code movement.** Copying code through the agent's context window drops lines and introduces subtle errors. Use `sed` for extraction. (See: Refactoring)

12. **Polymorphic equality on structured types.** Using `(=)` instead of typed comparators like `String.equal`. Polymorphic equality can produce wrong results on abstract types.
