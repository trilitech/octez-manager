# Code Context

## Project Context
Languages: ocaml (100%), shell (100%), python (65%), sql (55%), typescript (55%)


---

# Design Context

The following SPEC and interface definitions were produced during the SPECIFY phase. Your implementation MUST conform to these types and interfaces.

## SPEC Document (story #13 extract)

# SPEC: OCaml Lint Hook for Épure Validation Pipeline

## 1. Overview

*This spec describes a narrow, well-scoped feature: wiring an OCaml-specific lint hook into Épure's existing build validation system.*

Épure is a build and validation framework with a hook system that lets language-specific checks run at defined points in the build lifecycle. Currently no lint hook exists for OCaml projects. This feature registers one: when Épure runs its pre-build phase on a project containing OCaml files, it will invoke `dune fmt --check` (formatting verification) and `dune build` (compilation check). Any failure in either command blocks the build from proceeding.

Once built, the hook is intended to be contributed back to Épure's hook registry for use by other OCaml projects.

---

## 2. User Roles

*One role is identified across all stories.*

| Role | Description |
|------|-------------|
| **Developer** | An engineer working on an OCaml codebase who uses Épure as their build and validation tool. They need automated lint and format checks to catch issues before a build completes. |

---

## 3. Functional Requirements
### Epic: Build lint tool for OCaml

> No lint tool is registered for OCaml. This epic tracks the work to build one that integrates with Épure's hook system. Once built the hook should be contributed back to the registry.

#### Story #13 — Register OCaml lint hook for Épure validation pipeline
**Priority:** Must

**Informal summary:** When Épure runs a build on an OCaml project, it should automatically check formatting and compilation before proceeding. Broken code or misformatted files must stop the build.

**Acceptance Criteria:**

| # | Given | When | Then |
|---|-------|------|------|
| AC-1 | The project contains OCaml files | Épure's pre-build lint hook executes | `dune fmt --check` is invoked |
| AC-2 | The project contains OCaml files | Épure's pre-build lint hook executes | `dune build` is invoked |
| AC-3 | `dune fmt --check` exits with a non-zero status | The pre-build lint hook runs | The build is blocked (does not proceed to the build phase) |
| AC-4 | `dune build` exits with a non-zero status | The pre-build lint hook runs | The build is blocked (does not proceed to the build phase) |

**Out of scope for this story:** contributing the hook to a shared registry (that is the follow-up described in the epic description but not present in the accepted stories).

---

## 4. Non-Functional Requirements

*These constraints apply to the implementation of the hook and are derived from the project's ratified laws.*

### Reliability

- **NFR-R1 — Build integrity:** Every commit that introduces or modifies the hook must itself compile (`dune build` must pass on that commit). Broken intermediate commits are not permitted (Law: *Every commit must compile independently*).
- **NFR-R2 — No check weakening:** The hook must not be altered to skip, soften, or bypass the lint checks in order to make a build pass. Failures must be fixed in the source, not in the hook configuration (Law: *Never weaken checks to pass CI*).
- **NFR-R3 — Formatting on commit:** The hook implementation files must be properly formatted at commit time; no separate "formatting" commits are allowed (Law: *Every commit must be properly formatted*).

### Usability

- **NFR-U1 — Fail fast with clear feedback:** When either `dune fmt --check` or `dune build` fails, the output from the failing command must be visible to the developer so they can identify and fix the problem without additional investigation steps.

### Security

- **NFR-S1 — No sensitive data in logs:** The hook must not emit environment variables, credentials, tokens, or other sensitive data to stdout/stderr (Law: *Never log sensitive data*).

### Code quality

- **NFR-Q1 — Functional style:** Implementation must prefer immutability and functional style; `Result` and `Option` types for error handling rather than exceptions (Laws: *Prefer immutability and functional style*; *Error handling: use Result and Option*).
- **NFR-Q2 — Structured data:** Where the hook produces structured output (e.g., a result record), prefer structured types over raw string interpolation (Law: *Prefer structured data over string interpolation*).

---

## 5. Quality Gates

*Each gate defines a measurable criterion that must be satisfied before the story is considered done.*

| Gate | What is measured | Target | How to verify |
|------|-----------------|--------|---------------|
| **QG-1 Format check runs** | `dune fmt --check` is called by the hook | Must be called on every pre-build hook invocation for an OCaml project | Run the hook in a test project; inspect hook execution log for the command invocation |
| **QG-2 Build check runs** | `dune build` is called by the hook | Must be called on every pre-build hook invocation for an OCaml project | Run the hook in a test project; inspect hook execution log for the command invocation |
| **QG-3 Format failure blocks** | A mis-formatted OCaml file causes the build to be blocked | Exit code of hook is non-zero when `dune fmt --check` fails | Introduce a formatting violation; run `epure build`; confirm it does not reach the build phase |
| **QG-4 Compile failure blocks** | A compilation error causes the build to be blocked | Exit code of hook is non-zero when `dune build` fails | Introduce a syntax error; run `epure build`; confirm it does not reach the build phase |
| **QG-5 Hook itself compiles** | The hook implementation builds cleanly | `dune build` exits 0 on the commit introducing the hook | Run `git rebase --exec 'dune build' main` and confirm every commit passes |

---

## 6. Decisions Log

*Decisions made during story elicitation that constrain the implementation.*

### Decision D-1 — Use `dune fmt --check` for format verification

**Context:** OCaml projects under this codebase use Dune as their build system. A format check is required as part of the lint pipeline.

**Decision:** Use `dune fmt --check` (read-only, non-mutating format check) rather than `dune fmt` (which would silently reformat files).

**Alternatives considered:**
- `ocamlformat --check` directly: would require enumerating source files manually; `dune fmt` handles this via the build description.
- `dune fmt` (mutating): rejected because a hook should not modify working tree files; it should only report violations.

**Rationale:** `dune fmt --check` is non-destructive, works with the existing Dune build description, and exits non-zero on any formatting violation — a clean fit for a blocking gate.

---

### Decision D-2 — Use `dune build` for compilation verification

**Context:** A lint hook should verify that the code compiles, not just that it is formatted.

**Decision:** Use `dune build` (default target) as the compilation check.

**Alternatives considered:**
- `dune build @check`: type-checks without producing artifacts; faster, but does not exercise all build rules.
- Full `dune build`: chosen for completeness; verifies the entire build graph, not just type correctness.

**Rationale:** Consistent with the project law that every commit must compile independently. Using the same command (`dune build`) in the hook as developers use locally reduces surprise.

---

## 7. Open Questions

*These questions need answers before or during implementation.*

1. **Hook registration mechanism:** How does an OCaml lint hook get registered in Épure's hook system? Is registration declarative (a config file entry) or programmatic (a function call in Épure's API)? The acceptance criteria assumes the hook is "registered" but does not specify the registration interface.

2. **OCaml file detection:** How does the hook determine that a project "has OCaml files"? Is this based on file extensions (`.ml`, `.mli`, `.mly`, `.mll`), presence of a `dune-project` file, or an explicit project type declaration in Épure's config?

3. **Registry contribution:** The epic mentions contributing the hook back to Épure's registry once built. No story currently covers this work. Is there a follow-up story planned, or is it assumed to happen within this same epic?

4. **Order of checks:** Should `dune fmt --check` always run before `dune build`, or can they run concurrently? If `dune fmt --check` fails, should `dune build` still run (to surface all issues at once), or should the hook short-circuit after the first failure?

5. **Épure version compatibility:** Which version of Épure's hook API is this hook targeting? Are there stability guarantees on the pre-build hook interface?

---

## 8. Glossary

| Term | Definition |
|------|-----------|
| **Épure** | The build and validation framework used in this project. It provides a lifecycle with hook points (e.g., pre-build) where external checks can be registered and executed. |
| **Hook** | A callback or script registered with Épure that runs at a specific point in the build lifecycle. A pre-build hook runs before the main build phase. If it exits with a non-zero status, the build is blocked. |
| **Pre-build hook** | A hook that executes before the main compilation/build step. Used to enforce quality gates (formatting, linting, compilation) before artefacts are produced. |
| **Lint** | Automated static analysis of source code to detect style violations, formatting errors, or potential bugs — without running the program. |
| **`dune`** | The build system used by OCaml projects in this codebase. See [https://dune.build](https://dune.build) for official documentation. |
| **`dune fmt --check`** | A Dune command that verifies all OCaml source files in the project are formatted according to the project's `ocamlformat` configuration. Exits non-zero if any file is misformatted; does not modify files. |
| **`dune build`** | A Dune command that compiles all targets in the default build alias. Exits non-zero if any source file fails to compile. |
| **`ocamlformat`** | The standard auto-formatter for OCaml source code, invoked internally by `dune fmt`. See [https://github.com/ocaml-ppx/ocamlformat](https://github.com/ocaml-ppx/ocamlformat). |
| **Registry** | Épure's catalogue of available hooks. A hook contributed to the registry can be reused by any project that imports Épure, rather than being defined locally. |
| **Non-zero exit code** | A process exit status other than `0`. By Unix convention, `0` means success; any other value signals failure. Build tools like Épure use this to determine whether a hook passed or failed. |

_Full SPEC available at .epure/context/SPEC.md_