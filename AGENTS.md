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

## Common Mistakes

Consolidated list of mistakes agents repeatedly make. Check this before submitting a PR.

1. **I/O in view functions.** The render loop runs many times per second. File reads, RPC calls, or shell commands in `view` functions cause visible lag. Use scheduler caches instead. (See: [src/ui/AGENTS.md](src/ui/AGENTS.md))

2. **Duplicating existing code.** Search `arch_query` and `grep` before writing new functions. The CI metrics gate catches duplicates and blocks the PR. (See: [tools/AGENTS.md](tools/AGENTS.md))

3. **Commits that don't compile.** Every commit must build independently. A commit that adds a function call before the commit that defines it breaks `git bisect`. (See: Build & Verification)

4. **Modifying golden path test counts.** Adding or removing form fields without updating `submit_form ~downs:N` in the golden path test. This test doesn't run locally — it only fails in CI. (See: [src/ui/AGENTS.md](src/ui/AGENTS.md))

5. **Stale shell completions.** Adding CLI subcommands without running `make completions`. The completions check in CI will catch this.

6. **Missing copyright headers.** Creating new files without running `./scripts/check-copyright.sh --fix`. The copyright check in CI will reject the PR.

7. **TODO without issue reference.** Writing `(* TODO: fix this later *)` without a GitHub issue link. These are untrackable and rot. (See: TODO/FIXME Comments)

8. **Weakening CI to pass.** Disabling checks, skipping hooks (`--no-verify`), or relaxing thresholds instead of fixing the underlying issue.

9. **Mixing refactoring with functional changes.** A single commit that renames variables AND changes behavior is impossible to review or revert cleanly. Separate them. (See: Atomic Commits)

10. **Using `include` instead of `open`.** Re-exporting an entire module's API when you only need local access. This pollutes the public interface. (See: Module Inclusion)

11. **Large Read+Write for code movement.** Copying code through the agent's context window drops lines and introduces subtle errors. Use `sed` for extraction. (See: [docs/agents/refactoring.md](docs/agents/refactoring.md))

12. **Polymorphic equality on structured types.** Using `(=)` instead of typed comparators like `String.equal`. Polymorphic equality can produce wrong results on abstract types.

13. **Manual string layouts instead of Miaou widgets.** Using `Printf.sprintf` width specifiers or `String.make n ' '` to align columns. Use `Flex_layout`, `Grid_layout`, or `Box_widget` — they handle terminal resizing and overflow automatically. (See: [src/ui/AGENTS.md](src/ui/AGENTS.md))

---

## Subdirectory Guides

These AGENTS.md files are auto-loaded when working in the corresponding directories:

| Directory | Guide | Topics |
|-----------|-------|--------|
| `src/ui/` | [AGENTS.md](src/ui/AGENTS.md) | TUI render loop, no-I/O rule, schedulers, Miaou pages & widgets, golden path tests |
| `test/integration/` | [AGENTS.md](test/integration/AGENTS.md) | Test independence, required patterns, port allocation |
| `tools/` | [AGENTS.md](tools/AGENTS.md) | Architecture DB, arch-query commands, CI metrics, code duplication prevention |
| `.github/` | [AGENTS.md](.github/AGENTS.md) | Verification check patterns, Copilot interaction rules |

## Reference Docs

Deep-dive guides in `docs/agents/` — consult when needed:

- [LSP & Tooling Setup](docs/agents/lsp-and-tooling.md) — OCaml LSP setup for OpenCode, Claude Code, and other editors
- [Refactoring](docs/agents/refactoring.md) — Safe code movement between modules using shell commands
- [Code Review](docs/agents/code-review.md) — PR review format and guidelines
- [Parallel Work](docs/agents/parallel-work.md) — Git worktrees, issue tracking, session handoff
- [Plan-First Workflow](docs/agents/plan-workflow.md) — When and how to write implementation plans
