# Gardening Guide

Gardening is the ongoing practice of maintaining and improving code health in octez-manager. It's never "done" - there's always something to tend to.

**Anyone can garden.** Preparing tasks for gardening is itself gardening.

## Quick Start

```bash
# Find existing gardening tasks
gh issue list --label gardening

# Start a discovery session (analyze, create issues)
# See "Discovery Sessions" below

# Start an execution session (fix something)
gh issue list --label gardening --assignee @me
```

## Session Types

### Discovery Sessions

**Goal:** Analyze the codebase, identify issues, create GitHub issues. Don't fix anything.

**Approach:** Free-form. Pick what bothers you, or use the checklist below.

**Output:** New GitHub issues with the `gardening` label.

### Execution Sessions

**Goal:** Pick an existing gardening issue and fix it.

**Approach:**
1. `gh issue list --label gardening`
2. Pick one that interests you
3. Fix it, submit PR
4. Update the gardening log (see "Tracking Progress")

---

## Gardening Categories

### 1. Split Large Files

**Threshold:** Files over 500 lines should be evaluated for splitting.

**Detection:**
```bash
find src -name "*.ml" | xargs wc -l | sort -rn | head -20
```

**Current targets:**
- `src/main.ml` (3,053 lines) → #271
- `src/ui/pages/instances.ml` (2,246 lines) → #272
- `src/installer.ml` (1,634 lines) → #273

---

### 2. Split Large Functions

**Threshold:** Functions over 50 lines should be evaluated.

**Detection:**
```bash
# Find function definitions and their lengths
grep -n "^let " src/*.ml | head -50
# Manual review needed for function end boundaries
```

**When splitting:**
- Extract helper functions with clear names
- Keep the public interface stable
- Add tests for extracted functions

---

### 3. Type Safety

**Goal:** No raw strings in exposed function signatures. Use phantom types.

**Target types to create:**

| Current | Target Type | Validates |
|---------|-------------|-----------|
| `instance: string` | `Instance_name.t` | Charset, uniqueness |
| `role: string` | `Role.t` | Known roles only |
| `network: string` | `Network.t` | Valid network names |
| `path: string` | `Path.t` or specific types | Existence, permissions |
| `port: int` | `Port.t` | 1-65535 range |

**Pattern for phantom types:**
```ocaml
module Instance_name : sig
  type t
  val of_string : string -> (t, [> `Msg of string]) result
  val of_string_exn : string -> t  (* for tests/internal use *)
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end = struct
  type t = string
  let of_string s =
    if valid_instance_name s then Ok s
    else Error (`Msg "invalid instance name")
  let of_string_exn s = match of_string s with Ok t -> t | Error _ -> failwith "invalid"
  let to_string t = t
  let pp fmt t = Format.pp_print_string fmt t
end
```

**Detection:**
```bash
# Find exposed functions with string parameters
grep -n ": string ->" src/*.mli
grep -n "~instance:string" src/*.mli
grep -n "~path:string" src/*.mli
```

---

### 4. Test Coverage

**Targets:**
- Minimum: 50% (never drop below)
- Goal: 75%
- Critical paths: 90%+

**Critical paths (must have high coverage):**
- Installation flows: `install_node`, `install_baker`, `install_accuser`, `install_dal_node`
- Service lifecycle: `start_service`, `stop_service`, `restart_service`, `stop_service_cascade`
- Data integrity: `Service_registry.save`, `Service_registry.load`, snapshot import

**Check coverage:**
```bash
dune test --instrument-with bisect_ppx
bisect-ppx-report summary
bisect-ppx-report html  # generates _coverage/index.html
```

**Focus areas for new tests:**
1. Error paths (what happens when things fail?)
2. Edge cases (empty lists, missing files, invalid input)
3. Integration points (systemd interaction, file I/O)

---

### 5. Interface Files (.mli)

**Goal:** Every public module should have an `.mli` file with documentation.

**Detection:**
```bash
# Find .ml files without corresponding .mli
for f in src/*.ml; do
  mli="${f%.ml}.mli"
  [ ! -f "$mli" ] && echo "Missing: $mli"
done
```

**When creating .mli:**
- Only expose what's needed (hide internal helpers)
- Add doc comments with `(** ... *)` syntax
- Group related functions with `(** {2 Section Name} *)` headers
- Include examples in doc comments where helpful

---

### 6. Documentation

**Areas to review:**
- [ ] README.md - accurate, up-to-date?
- [ ] docs/ folder - matches current behavior?
- [ ] Module headers - explain purpose?
- [ ] Function docs - explain non-obvious behavior?
- [ ] Examples - do they still work?

**Detection:**
```bash
# Find modules without header documentation
for f in src/*.ml; do
  head -20 "$f" | grep -q '(\*\*' || echo "No module doc: $f"
done
```

---

### 7. Dependencies

**Regular checks:**
```bash
# Check for outdated packages
opam update
opam outdated

# Check for security issues (if available)
opam audit  # requires opam-audit plugin
```

**Evaluation criteria for replacements:**
- Is the current dep maintained?
- Does an alternative have better performance?
- Does an alternative have fewer transitive deps?
- Is the alternative more idiomatic for our codebase?

**Current dependencies to evaluate:**
| Package | Purpose | Evaluate? |
|---------|---------|-----------|
| `rresult` | Result types | Keep (standard) |
| `bos` | OS interaction | Keep (standard) |
| `yojson` | JSON | Keep (standard) |
| `cohttp-eio` | HTTP | Evaluate alternatives? |
| `cmdliner` | CLI | Keep (standard) |
| `linenoise` | REPL | Keep (works well) |

---

### 8. Architecture Index (Mindmap)

**Location:** `docs/architecture.db` (SQLite, gitignored, generated locally)

**Purpose:** Queryable index of all modules, functions, their intents, and relationships.

> **Note:** The gardening CLI tool (#274) is required to generate and maintain this database.
> Until that tool exists, the database schema (`docs/architecture-schema.sql`) serves as a
> reference. You can manually create the database with:
> ```bash
> sqlite3 docs/architecture.db < docs/architecture-schema.sql
> ```

**Schema overview:**
- `modules` - all .ml files with line counts and intent
- `functions` - all functions with signatures, exposed status, intent
- `calls` - which functions call which
- `unsafe_params` - parameters that need type hardening
- `coverage` - per-function coverage data
- `gardening_tasks` - links to GitHub issues

**Data population strategy:**

| Data | Source | Update Method |
|------|--------|---------------|
| Function names, signatures, line counts | AST/odoc parsing | Automated by tooling |
| Exposed status | .mli file presence | Automated by tooling |
| Call relationships | AST analysis | Automated by tooling |
| Coverage data | bisect_ppx reports | Automated by tooling |
| Module intents | `(** ... *)` in .mli headers | Semi-automated extraction |
| Function intents | `(** ... *)` doc comments | Semi-automated extraction |
| Intents for undocumented code | Human input | Manual during gardening |
| Target types for unsafe params | Human decision | Manual during gardening |

**Querying (once database exists):**
```bash
# CLI
sqlite3 docs/architecture.db "SELECT path, lines FROM modules ORDER BY lines DESC LIMIT 10"

# Or use the gardening tool (requires #274)
dune exec -- gardening query "functions without intent"
dune exec -- gardening query "unsafe params"
dune exec -- gardening query "low coverage"
```

**Updating (requires #274):**
```bash
# Regenerate from source (keeps human-written intents)
dune exec -- gardening refresh-index

# Add intent to a function
dune exec -- gardening set-intent --module src/installer.ml --function install_node \
  --intent "Installs octez-node systemd service with optional snapshot bootstrap"
```

---

## Discovery Checklist

When doing a discovery session, pick any of these:

```
[ ] Run `wc -l` on src/*.ml - any new files over 500 lines?
[ ] Check coverage report - any critical paths below 90%?
[ ] Grep for `string ->` in .mli files - any new unsafe types?
[ ] Run `opam outdated` - any updates available?
[ ] Read a random module - does it make sense? Is it documented?
[ ] Check the architecture index - any functions without intents?
[ ] Look at recent PRs - did they introduce technical debt?
[ ] Run the test suite - any flaky tests?
```

---

## Tracking Progress

### Gardening Log

Update this section when completing gardening work:

| Date | Who | Category | Description | PR/Issue |
|------|-----|----------|-------------|----------|
| 2025-01-15 | - | setup | Created gardening framework | - |
| | | | | |

### Stats

<!-- Updated periodically during gardening sessions -->
```
Files over 500 lines: 14 (as of 2025-01-15)
Functions without intent: TBD (requires #274)
Unsafe string params: TBD (requires #274)
Test coverage: TBD
```

---

## Creating Gardening Issues

When you find something to fix, create an issue:

```bash
gh issue create \
  --title "gardening: [category] description" \
  --label "gardening" \
  --body "## Problem
...

## Proposed Solution
...

## Files Affected
- src/foo.ml

## Acceptance Criteria
- [ ] ..."
```

Categories for titles:
- `gardening: split` - file/function splitting
- `gardening: types` - type safety improvements
- `gardening: coverage` - test coverage
- `gardening: docs` - documentation
- `gardening: deps` - dependency updates
- `gardening: mli` - interface files
- `gardening: index` - architecture index updates
