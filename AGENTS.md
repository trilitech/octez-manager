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
- Every commit must be properly formatted. Do not create separate "formatting" commits.
- Every commit must have correct copyright headers. Run `./scripts/check-copyright.sh --fix` to automatically update headers if needed.
- Shell completions must be up to date. Run `make completions` after CLI changes.
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

Claude Code can use the OCaml LSP server for code intelligence features like go-to-definition, find-references, hover documentation, and workspace symbol search.

### Installation (Claude Code)

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

### Discouraged
- `List.hd`, `Option.get` (use pattern matching or `_opt` variants)
- Stringly-typed code (use variants/records)
- Partial functions

### Code Duplication Prevention

**Search First Policy:** Before writing any new function, especially helpers or utilities:

1. **Query the architecture database** (quick first check):
   ```bash
   sqlite3 docs/architecture.db "SELECT m.path, f.name, f.intent FROM functions f JOIN modules m ON f.module_id = m.id WHERE f.name LIKE '%your_keyword%'"
   ```

2. **Search the actual codebase** (database may be incomplete):
   ```bash
   grep -rn "your_keyword" src/
   ```

3. Check `src/common.ml` for general utilities
4. Check scheduler modules for cached data accessors
5. Refactor existing code to be more generic rather than duplicating

**Important:** The architecture database is a helpful tool but is NOT complete. It may be missing functions, have outdated information, or lack intent descriptions. Always verify by searching the actual code.

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
- Don't opportunistically fix unrelated issues
- Never commit secrets or credentials
- Use `git mv` for renames to preserve history
- **Ask for confirmation before force pushing** - force push operations rewrite history and should only be done with explicit user approval
- **Never delete untracked files without confirmation** - user scripts, test data, and work-in-progress files must be preserved unless explicitly requested

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

## Gardening & Architecture Index

The project uses a "gardening" approach for ongoing code maintenance. See `GARDENING.md` for the full guide.

### Architecture Database

An SQLite database at `docs/architecture.db` can index the codebase. The database is gitignored and must be generated locally from the schema:

```bash
# Generate the database (one-time setup)
sqlite3 docs/architecture.db < docs/architecture-schema.sql
```

> **Note:** Full tooling to populate this database (#274) is not yet implemented.
> Until then, the database serves as a schema reference and manual queries work
> if you populate it yourself.

Once populated, you can query it:

```bash
# Check for similar functions before creating new ones
sqlite3 docs/architecture.db "SELECT m.path, f.name, f.signature, f.intent
  FROM functions f JOIN modules m ON f.module_id = m.id
  WHERE f.name LIKE '%install%'"

# Find large files that may need splitting
sqlite3 docs/architecture.db "SELECT * FROM v_large_files"

# Find functions without documentation
sqlite3 docs/architecture.db "SELECT * FROM v_undocumented"
```

### CRITICAL: Database Limitations

**The database is NOT complete and may never be.** It is a helpful tool but does NOT replace searching the actual codebase.

When looking for existing functionality:
1. Query the database first (fast initial check)
2. **Always also search the actual code** with grep/ripgrep
3. Read relevant files to understand context

The database may be:
- Missing recently added functions
- Missing functions from modules not yet indexed
- Lacking intent descriptions for many functions
- Out of date with current signatures

### When Creating New Functions

If you create a new function, especially in a public module:

1. **Check it doesn't already exist** (both database AND grep)
2. Add it to the database if the module is already indexed:
   ```bash
   sqlite3 docs/architecture.db "INSERT INTO functions (module_id, name, signature, line_start, line_end, exposed, intent)
     SELECT id, 'my_new_function', '?quiet:bool -> string -> unit', 42, 55, 1, 'Brief description of purpose'
     FROM modules WHERE path = 'src/mymodule.ml'"
   ```
3. If the function is a utility that others might need, consider adding it to `src/common.ml`

### Gardening Tasks

When you notice code health issues during development:
- Large files (>500 lines)
- Large functions (>50 lines)
- String parameters that should be typed
- Missing .mli files
- Duplicated code

Create a gardening issue:
```bash
gh issue create --label gardening --title "gardening: [category] description"
```

Don't fix unrelated issues opportunistically in the same PR - create an issue for later.

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
