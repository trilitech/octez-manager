# CLAUDE.md

Read and follow the guidelines in @AGENTS.md for all development work on this project.

## Active Technologies
- OCaml 5 / Dune 3.15+ + Miaou TUI framework (widgets: Pane_layout, List_widget, Description_list, Box_widget, Select_widget, Validated_textbox_widget, Tabs_widget, Qr_code_widget, Spinner_widget, Toast_widget, Modal_manager, Focus_ring) (001-keys-wallet-manager)
- JSON files (octez-client key files, tzkt alias cache, transfer MRU), in-memory Hashtbl caches with Mutex (001-keys-wallet-manager)
- OCaml 5 / Dune 3.15+ + Miaou TUI framework (Pane_layout, List_widget, Grid_layout, Tabs_widget, Sparkline_widget, Box_widget, Select_widget, Validated_textbox_widget, Modal_manager, Spinner_widget, Toast_widget, Description_list), Yojson.Safe, cmdliner (002-rewards-payouts)
- JSON files (payout config), CSV + JSON (reports), in-memory Hashtbl + Mutex (scheduler caches) (002-rewards-payouts)
- OCaml 5 / Dune 3.15+ + Miaou TUI framework, Eio (structured concurrency), Yojson.Safe, cmdliner (003-sandbox-mode)
- JSON files (group registry, wallet files, service registry), systemd env files (003-sandbox-mode)

## Recent Changes
- 001-keys-wallet-manager: Added OCaml 5 / Dune 3.15+ + Miaou TUI framework (widgets: Pane_layout, List_widget, Description_list, Box_widget, Select_widget, Validated_textbox_widget, Tabs_widget, Qr_code_widget, Spinner_widget, Toast_widget, Modal_manager, Focus_ring)
