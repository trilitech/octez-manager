# Validator Feedback — Previous Attempt

Address ALL points below before running the build.

[architect] ```json
```

### Findings

1. **[high] Orphan library — single-source-of-truth violation between `tools/ocaml_lint_hook.ml` and `scripts/lint-ocaml.sh`.**
   The OCaml library (patch L571394–571448) is well-designed: pure, dependency-injected via `run_command`, with a documented `.mli` and 4 acceptance-criteria tests. But `scripts/lint-ocaml.sh` (L565432–565464) re-encodes the same sequence (`dune fmt --check` then `dune build`) directly in bash, and the Makefile target `lint-ocaml` (L565102–565104) calls the shell script — never the OCaml library. There is no executable wrapping `Ocaml_lint_hook.run_hook`, so the library is unreachable in production. Two independent implementations of one rule will drift; tests will green while the actual hook diverges.

2. **[high] Hook is registered as a target but not wired into any enforced pipeline.**
   The patch adds `lint-ocaml` to `.PHONY` and creates the target, but `test:` still reads `test: fmt-check completions-check lint-sync-io lint-indexer` (L565105) — `lint-ocaml` is absent. There is no `all:` or pre-build step calling it either. So the "Épure validation pipeline" registration is nominal: nothing fires the hook automatically. For a story whose unit of value is *registration*, this is the central acceptance criterion and it is missing.

3. **[high] Scope creep: ~40 unrelated files outside Story #13.**
   The diff includes view/I/O separation refactoring that belongs to Epic #7 (per `docs/epics/7-...md` added in this same patch): `src/ui/views/` library + dune + ppx_forbid, new `*_view.ml/.mli` for snapshots/sandbox/sandbox_key_alloc/topology/wallets/import_wizard/log_viewer/keys_page, edits to `manager_app.ml`, `themed_page.ml`, `binaries_view.ml`, `src/ui/dune`. Plus `test_keys_page.ml` (a deduplication test for an unrelated bug). These should be split into their epic's stories — bundling them under "Register OCaml lint hook" obscures review and breaks story-level traceability.

4. **[medium] Project rules (DB source-of-truth, single context injection, atomic DB transactions) are not applicable to this story.**
   Verified: no `src/db/` paths touched, no agent prompt-assembly code touched, no SQLite mutations introduced. The story is build-tooling only, so rules 1–3 are vacuously satisfied. Noting this explicitly so the rules check is on the record.

5. **[low] Library design itself is sound (positive finding).**
   `tools/ocaml_lint_hook.ml` correctly separates side-effects (the `run_command` parameter) from logic (sequencing, short-circuit on first failure, typed `hook_error`). The `.mli` documents both `run_hook` and `exit_code_of_result`. The four tests in `test_ocaml_lint_hook.ml` map 1:1 onto AC-1..AC-4. If finding #1 is fixed by adding a thin executable in `tools/dune` that calls `run_hook` with `Sys.command` and is invoked by the Makefile target instead of the shell script, the design is good as-is.

6. **[low] Build/tests pass.**
   Build log shows `ocaml_lint_hook` test suite green (4/4) and full `dune runtest` succeeds. No regressions surfaced.