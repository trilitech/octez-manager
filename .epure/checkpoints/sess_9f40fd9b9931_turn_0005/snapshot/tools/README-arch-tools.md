# Architecture Index Tools

Tools for indexing and querying the octez-manager codebase structure. Used for code health metrics, duplicate detection, and enforcing quality gates in CI.

## Overview

The architecture tools consist of two components:

1. **`arch_index.exe`** - Scans compiled `.cmt`/`.cmti` files and populates an SQLite database
2. **`arch_query.exe`** - CLI for querying the database and generating metrics

The database (`docs/architecture.db`) is gitignored and regenerated on each build.

## Quick Start

```bash
# Build the project (produces .cmt/.cmti files)
dune build

# Generate/refresh the database
make arch-index
# or: dune exec tools/arch_index.exe

# Query the database
dune exec tools/arch_query.exe -- stats
dune exec tools/arch_query.exe -- search "your query"
```

## Database Schema

### Tables

| Table | Description |
|-------|-------------|
| `modules` | OCaml modules with path, line count, `.mli` presence |
| `functions` | Functions with signatures, line ranges, doc comments |
| `types` | Type definitions (record/variant/abstract/alias) |
| `type_fields` | Record fields with types and mutability |
| `type_constructors` | Variant constructors with arguments |
| `mutable_usages` | Tracks ref/atomic/mutable field usage per function |

### What Gets Indexed

- **Modules**: File path, total lines, whether `.mli` exists
- **Functions**: Name, type signature, line range, exposed in `.mli`, doc comment
- **Types**: Name, kind, fields/constructors, doc comment
- **Mutable patterns**: `ref`, `:=`, `!`, `Atomic.*`, mutable field assignments

## Line Counting

### Doc Comments Are Excluded

**Important**: Function and type line counts **exclude doc comments** to avoid penalizing documentation.

```ocaml
(** This is a 5-line doc comment
    explaining what the function does.
    It includes examples and notes.
    All of this is excluded from the count.
*)
let my_function x =    (* Line count starts HERE *)
  let y = x + 1 in
  y * 2
(* Line count ends here - total: 3 lines, not 8 *)
```

The indexer uses the pattern/name location (where `let`/`type` keyword appears) rather than the binding location (which includes preceding doc comments).

### Thresholds

| Metric | Threshold | Rationale |
|--------|-----------|-----------|
| Large files | >500 lines | Files this large are hard to navigate |
| Large functions | >50 lines | Functions this long are hard to understand |
| God modules | >30 functions | Too many responsibilities in one module |

## arch_query Commands

### Search Commands

```bash
# Fuzzy search across functions, types, and modules
arch_query search "network download"
arch_query search -t 0.7 "snapshot"      # 70% similarity threshold
arch_query search -k functions "port"     # Search only functions
arch_query search -k types "config"       # Search only types

# Find types by shape (field names and types)
arch_query type-search -f instance -f network    # Has these fields
arch_query type-search -T string -T int          # Has these field types
arch_query type-search -f name -T string         # Combined
```

### Code Health Queries

```bash
# Duplicate detection
arch_query duplicates              # Functions with same name in multiple modules

# Size checks
arch_query large-files             # Files >500 lines
arch_query large-files --min 300   # Custom threshold
arch_query large-functions         # Functions >50 lines
arch_query large-functions --min 30

# Documentation coverage
arch_query missing-docs            # Exposed functions without doc comments
arch_query missing-mli             # Modules without .mli files

# Complexity indicators
arch_query god-modules             # Modules with >30 functions
arch_query god-modules --min 20    # Custom threshold

# Type safety
arch_query unsafe-strings          # String fields appearing 3+ times (should be typed)

# Mutable state tracking
arch_query mutables                # All mutable pattern usage
```

### Mutable Pattern Detection

The `mutables` command tracks usage of mutable state:

| Kind | Pattern | Example |
|------|---------|---------|
| `ref` | Creating a ref | `let x = ref 0` |
| `ref_assign` | Assigning to ref | `x := 5` |
| `ref_deref` | Dereferencing ref | `!x` |
| `atomic_make` | Creating atomic | `Atomic.make 0` |
| `atomic_get` | Reading atomic | `Atomic.get x` |
| `atomic_set` | Writing atomic | `Atomic.set x 5` |
| `atomic_other` | Other atomic ops | `Atomic.incr x` |
| `mutable_field` | Mutable field assignment | `record.field <- value` |

### Metrics and CI

```bash
# Generate machine-readable metrics
arch_query metrics                           # Print to stdout
arch_query metrics -o metrics.json           # Write to file

# Compare against baseline (exits 1 on regression)
arch_query compare baseline.json current.json

# Refresh the database
arch_query refresh

# Summary statistics
arch_query stats

# Raw SQL (for custom queries)
arch_query sql "SELECT * FROM functions WHERE line_count > 100"
```

## CI Integration

### Tracked Metrics

The CI pipeline compares these metrics against the main branch baseline:

| Metric | Rule | Description |
|--------|------|-------------|
| `duplicate_groups` | Must not increase | Functions duplicated across modules |
| `large_files` | Must not increase | Files >500 lines |
| `large_functions` | Must not increase | Functions >50 lines |
| `missing_docs` | Must not increase | Exposed functions without docs |
| `missing_mli` | Must not increase | Modules without interface files |
| `god_modules` | Must not increase | Modules with >30 functions |
| `unsafe_string_fields` | Must not increase | Repeated string fields |
| `mutable_fields` | Must not increase | Mutable record fields |
| `functions_with_mutables` | Must not increase | Functions using ref/atomic |
| `doc_coverage_pct` | Must not decrease | Percentage of documented functions |

### PR Workflow

1. CI builds the project and generates `architecture-metrics.json`
2. CI downloads the baseline from the latest successful main branch build
3. `arch_query compare` checks for regressions
4. PR fails if any metric regresses

### Fixing Regressions

If CI fails due to metrics regression:

```bash
# See what regressed
dune exec tools/arch_query.exe -- compare baseline.json current.json

# For large_functions regression:
dune exec tools/arch_query.exe -- large-functions

# For duplicates regression:
dune exec tools/arch_query.exe -- duplicates

# For missing_docs regression:
dune exec tools/arch_query.exe -- missing-docs
```

## Doc Comment Extraction

### Priority Order

1. `.mli` doc comments (preferred - these are the public API docs)
2. `.ml` doc comments (fallback for unexposed functions)

### Preserved Intent Fields

Hand-written `intent` fields (set via direct SQL UPDATE) are preserved across re-indexing:

```bash
sqlite3 docs/architecture.db "UPDATE functions SET intent = 'Validates port format' WHERE name = 'parse_port'"
```

These survive `arch_index` rebuilds.

## Examples

### Finding Code to Reuse

Before writing a new function:

```bash
# Search by description
dune exec tools/arch_query.exe -- search "download file"

# Search by type signature pattern
dune exec tools/arch_query.exe -- search "string -> result"

# Check for duplicates with similar names
dune exec tools/arch_query.exe -- duplicates | grep -i download
```

### Investigating Large Functions

```bash
# List all large functions
dune exec tools/arch_query.exe -- large-functions

# Find the worst offenders
dune exec tools/arch_query.exe -- sql \
  "SELECT m.path, f.name, f.line_count 
   FROM functions f JOIN modules m ON f.module_id = m.id 
   ORDER BY f.line_count DESC LIMIT 10"
```

### Tracking Mutable State

```bash
# Overall mutable usage stats
dune exec tools/arch_query.exe -- mutables

# Find functions with most ref usage
dune exec tools/arch_query.exe -- sql \
  "SELECT m.path, f.name, COUNT(*) as refs
   FROM mutable_usages mu
   JOIN functions f ON mu.function_id = f.id
   JOIN modules m ON f.module_id = m.id
   WHERE mu.kind IN ('ref', 'ref_assign', 'ref_deref')
   GROUP BY f.id ORDER BY refs DESC LIMIT 20"
```

## Troubleshooting

### Database Out of Date

If queries return stale results:

```bash
dune build && dune exec tools/arch_query.exe -- refresh
```

### Missing Functions/Types

The indexer only processes successfully compiled code. If a module has errors:

```bash
dune build 2>&1 | grep Error
```

### Performance

The indexer typically completes in <1 second. If slow:

1. Check that `docs/architecture.db` isn't on a network mount
2. Ensure `_build` exists (avoid full recompilation)
