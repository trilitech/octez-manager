# Epic #7: Enforce view/I/O separation in src/ui/ via library split

## Description

The core architectural invariant of octez-manager's TUI layer is: view functions must never perform I/O. The render loop runs at ~60 FPS. A single blocking call in a view function — a file read, an RPC call, an Eio fiber suspension — introduces a latency spike visible as a frozen or stuttering terminal. Schedulers exist precisely to push all I/O into background fibers that write to in-memory caches.

This invariant is documented in AGENTS.md and enforced today by code review alone. There are currently ZERO violations — confirmed by grep -rn "Eio\." src/ui/pages/ returning empty after filtering scheduler and action files. The risk is not what exists now; it is what future contributors can introduce without any compiler signal.

WHY ppx_forbid cannot fix this: ppx_forbid operates at the module level, not the function level. Blocking all Eio.* in src/ui/ would also break schedulers, background_runner, job_manager, and action modules — all of which legitimately use Eio in the same library.

WHY the library split is the right fix: octez_manager_ui is today a single flat library. Its dune file lists eio, eio_posix, eio_main, eio.unix as deps, and include_subdirs unqualified pulls every file into the same compilation unit. Every module — including view functions — has silent compile-time access to the full Eio API. The split puts view modules in a sub-library with no Eio dep, making any Eio call there a hard build error.

PREREQUISITE: Epic 1 (Add missing .mli interfaces) must be complete before this epic lands.

MANDATORY: refresh the architecture index before any spec or build work:
  dune build
  dune exec tools/arch_query.exe -- refresh
  dune exec tools/arch_query.exe -- large-files   (identify monolithic pages still to decompose)
  dune exec tools/arch_query.exe -- missing-mli   (verify Epic 1 is complete)

CURRENT STATE (2026-05-04):
Only src/ui/pages/binaries/ follows the full decomposition pattern (*_types, *_view, *_actions, *_data, *_page). It is the reference implementation.

Partially decomposed: diagnostics/, instances/ (has actions, view in monolithic instances.ml), rpc_browser/ (has types and actions, separate render files), rewards/ (multiple render files, partial).

Monolithic pages requiring Phase 1 decomposition:
- sandbox_page.ml (1173 lines) → sandbox_view.ml
- sandbox_key_alloc_page.ml (780 lines) → sandbox_key_alloc_view.ml
- install_index_form_v3.ml (608 lines) → install_index_form_v3_view.ml
- topology_page.ml (439 lines) → topology_view.ml
- sandbox_create_form.ml (395 lines) → sandbox_create_view.ml
- keys_page.ml → keys_view.ml
- rewards_page.ml → rewards_view.ml
- log_viewer_page.ml → log_viewer_view.ml
- main_shell.ml → main_shell_view.ml
- instances.ml → instances_view.ml
- wallets_page.ml → wallets_view.ml
- import_wizard.ml → import_wizard_view.ml
- snapshots.ml → snapshots_view.ml
- diagnostics_page.ml → diagnostics_view.ml

PHASE 1 — Decompose monolithic pages into *_view.ml + rest:
For each monolithic page, extract the view function and all pure rendering helpers into a <page>_view.ml file. Rules: no Eio.*, no mutation of shared state, no side effects beyond widget construction. Verify with: grep -n "Eio\." src/ui/pages/<page>_view.ml → zero results. dune build must pass after each extraction. This is a purely mechanical refactoring — zero behavioral change.

PHASE 2 — Create octez_manager_ui_views library:
Create a new dune library stanza octez_manager_ui_views covering all *_view.ml and *_types.ml modules. Dependencies: miaou-core.widgets.*, miaou-core.style, miaou-core.canvas, octez_manager_lib — explicitly NO eio, eio_main, eio_posix, eio.unix. Add a .ppx_forbid entry blocking Eio as belt-and-suspenders. Update octez_manager_ui to depend on octez_manager_ui_views, remove view/types modules from its source list, retain all Eio deps for schedulers and actions.

Out of scope: moving *_data.ml or *_actions.ml into views library (they use Eio legitimately), changing any page behavior or visual output, migrating rpc_browser render modules if they are not already Eio-free.

## Acceptance Criteria

1. **Given** dune build is run before Phase 1 begins  
   **When** execution completes  
   **Then** it passes with zero errors, confirming the baseline is clean

2. **Given** each *_view.ml file exists after Phase 1 decomposition  
   **When** grep -n 'Eio\.' <file> is run on it  
   **Then** it returns zero matches — the file contains no Eio calls

3. **Given** the octez_manager_ui_views library exists after Phase 2 and any *_view.ml module calls Eio.Fiber.fork or any other Eio.* function  
   **When** dune build is run  
   **Then** it fails with an unresolved module error on Eio, making the violation immediately visible

4. **Given** the octez_manager_ui runtime library contains a *_scheduler.ml or *_actions.ml module that calls Eio.Fiber.fork  
   **When** dune build is run  
   **Then** it passes without error — runtime modules retain full Eio access

5. **Given** both Phase 1 and Phase 2 are complete  
   **When** dune build && dune runtest is run  
   **Then** all existing tests pass with zero behavioral changes — this is a pure structural refactoring

6. **Given** Phase 2 has landed  
   **When** AGENTS.md is read  
   **Then** the no-I/O rule states it is compiler-enforced for all *_view.ml modules via library dependency isolation, and the ppx_forbid section is updated accordingly

## Linked Stories

- #7 [accepted] Decompose sandbox pages into *_view.ml files
- #8 [accepted] Decompose remaining monolithic UI pages into *_view.ml files
- #9 [accepted] Audit partially-decomposed pages for Eio cleanliness
- #10 [accepted] Create octez_manager_ui_views library and update dune
- #11 [accepted] Update AGENTS.md and .ppx_forbid to reflect compiler enforcement
