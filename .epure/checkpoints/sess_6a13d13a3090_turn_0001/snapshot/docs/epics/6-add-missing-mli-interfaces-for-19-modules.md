# Epic #6: Add missing .mli interfaces for 19 modules

## Description

In OCaml, a module without a .mli interface file exports every value, type, and submodule by default. This has three concrete consequences: (1) Internal helpers become silently reachable from any other module in the library. (2) odoc indexes .mli files; without one, documentation is inferred from the implementation — incomplete and noisy. (3) Without a declared interface, it is impossible to distinguish public functions from private helpers without reading all callers.

The arch_query CI gate tracks missing_mli and blocks regressions, but the current 19-module gap is grandfathered. This epic closes it.

MANDATORY: refresh the architecture index before any spec or build work:
  dune build
  dune exec tools/arch_query.exe -- refresh
  dune exec tools/arch_query.exe -- missing-mli

The list below was produced from a fresh refresh on 2026-05-04. Treat it as a starting point — run the refresh again at implementation time.

Missing modules (post-refresh 2026-05-04):
- src/ui/pages/sandbox_page.ml (1173 lines, 57 fns) — UI page
- src/ui/pages/sandbox_key_alloc_page.ml (780 lines, 33 fns) — UI page
- src/ui/pages/install_index_form_v3.ml (608 lines, 9 fns) — UI page
- src/ui/pages/topology_page.ml (439 lines, 29 fns) — UI page
- src/ui/pages/sandbox_create_form.ml (395 lines, 19 fns) — UI page
- src/installer_types.ml (219 lines, 1 fn) — core lib types
- src/cli/cmd_install_index.ml (215 lines, 1 fn) — CLI command [EXEMPT]
- src/capabilities.ml (202 lines, 1 fn) — capability registry
- src/manager_interfaces.ml (198 lines, 0 fns) — module type definitions
- src/ui/pages/snapshots.ml (194 lines, 20 fns) — UI page
- src/cli/cmd_self_update.ml (190 lines, 2 fns) — CLI command [EXEMPT]
- src/main.ml (171 lines, 3 fns) — binary entry point [EXEMPT]
- src/ui/job_manager.ml (109 lines, 8 fns) — UI infrastructure
- src/ui/self_update_scheduler.ml (106 lines, 16 fns) — UI scheduler
- src/ui/versions_scheduler.ml (73 lines, 7 fns) — UI scheduler
- src/service_backend.ml (54 lines, 0 fns) — core lib
- src/ui/background_runner.ml (44 lines, 5 fns) — UI infrastructure
- src/ui/pages/diagnostics.ml (17 lines, 3 fns) — UI page redirect
- src/ui/pages/binaries.ml (16 lines, 3 fns) — UI page redirect

Exemptions (must carry a (* no .mli: <reason> *) comment):
- src/main.ml — binary entry point, OCaml convention
- src/cli/cmd_self_update.ml — internal CLI command, only consumer is main.ml
- src/cli/cmd_install_index.ml — same as above

16 modules must receive .mli files.

What a correct .mli must contain:
- UI pages: only val page and val name. All view, update, and helper functions hidden.
- Core lib type modules (installer_types, manager_interfaces, capabilities, service_backend): all public types and module type signatures with (** ... *) documentation.
- UI schedulers (self_update_scheduler, versions_scheduler): expose start/get/stop; hide internal mutable state, cache records, fiber logic.
- UI infrastructure (job_manager, background_runner): expose public job/task API; hide internal queues, mutexes, domain pool handles.
- UI page redirects (diagnostics.ml, binaries.ml): mirror the inner *_page.ml interface exactly.

Out of scope: adding docs to modules that already have .mli files, splitting or restructuring modules, modules outside src/.

## Acceptance Criteria

1. **Given** dune exec tools/arch_query.exe -- refresh && dune exec tools/arch_query.exe -- missing-mli is run after this epic  
   **When** the output is inspected  
   **Then** it lists zero modules, or only those carrying an explicit (* no .mli: <reason> *) exemption comment in their .ml file

2. **Given** any non-exempt UI page module (sandbox_page, topology_page, snapshots, install_index_form_v3, etc.) has its .mli opened  
   **When** a contributor reads the interface  
   **Then** only val page and val name are visible; all view, update, and helper functions are absent from the interface

3. **Given** any core lib type module (installer_types, manager_interfaces, capabilities, service_backend) has its .mli opened  
   **When** a contributor reads the interface  
   **Then** all exported types and module type signatures carry (** ... *) documentation comments

4. **Given** dune build && dune runtest is run after all .mli files have been added  
   **When** execution completes  
   **Then** both pass with zero new errors introduced by the new interfaces

5. **Given** the CI gate is active and a future PR introduces a new non-entry-point .ml file without a corresponding .mli  
   **When** CI runs the arch_query check  
   **Then** the merge is blocked and the missing_mli metric shows a regression

## Linked Stories

- #2 [accepted] Add .mli files for core lib type modules
- #3 [accepted] Add .mli files for UI schedulers and infrastructure
- #4 [accepted] Add .mli files for UI page redirect modules and snapshots
- #5 [accepted] Add .mli files for large sandbox and topology pages
- #6 [accepted] Document exemptions for entry-point CLI modules
