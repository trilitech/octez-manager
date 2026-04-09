---
name: code-quality
description: Universal code quality limits aligned with octez-manager CI metrics gates.
scope: global
category: common
version: 1.0.0
---

# Code Quality Limits

These limits are aligned with the octez-manager CI metrics gate. Violations will block PRs.

## Size Limits

- **Max file length:** 500 lines. If a file exceeds this, split it by responsibility. (Enforced by `arch_query large-files`.)
- **Max function length:** 50 lines. If a function exceeds this, extract sub-functions. (Enforced by `arch_query large-functions`.)
- **Max nesting depth:** 4 levels. Use early returns, guard clauses, or extracted functions to flatten.
- **Max module size:** 30 public functions. Beyond this, split into focused submodules. (Enforced by `arch_query god-modules`.)

## Documentation

- All public functions exposed in `.mli` must have doc comments (`(** ... *)`).
- Doc coverage must not decrease. (Enforced by `arch_query missing-docs` and `doc_coverage_pct` metric.)
- Every public module must have an `.mli` file. (Enforced by `arch_query missing-mli`.)

## Duplication

- No duplicate functions across modules. (Enforced by `arch_query duplicates`.)
- Before writing a new function, search the architecture database:
  ```bash
  dune exec tools/arch_query.exe -- search "what your function does"
  ```
- If a similar function exists, extend it or extract shared logic.

## Dead Code

- No dead code. If code is commented out, delete it. Version control preserves history.
- No TODO/FIXME comments without a GitHub issue reference. Untracked TODOs rot.
- No unreachable code paths.

## Naming

- Names must be descriptive and reveal intent.
- No single-letter variables except loop counters and well-known conventions (`x`, `y`, `acc`, `env`).
- Use typed comparators (`String.equal`, `Int.equal`), not polymorphic equality.

## Structure

- Prefer `open` over `include` for internal modules. `include` re-exports everything — use it only for deliberate API delegation.
- Prefer composition and functors over deep module hierarchies.
- Functions should do one thing. If a function name requires "and", it does too much.
