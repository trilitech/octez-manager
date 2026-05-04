# Architecture Database & Gardening Guide

Guidelines for working with the architecture database and code health tools in `tools/`. For general project rules, see the root [AGENTS.md](../AGENTS.md).

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

For small fixes in files you're already touching, fix them inline (see "Opportunistic Code Quality Improvements" in the root [AGENTS.md](../AGENTS.md)). For everything else, create a gardening issue:
```bash
gh issue create --label gardening --title "gardening: [category] description"
```
