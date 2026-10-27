; Test inventory: classification checked by check_test_inventory under
; dune runtest. Every executable stanza in test/dune must appear here.
;   deterministic  must run under dune runtest
;   env-gated      runs elsewhere; (gate ...) names where
;   manual         diagnostic tool; (reason ...) says why it is not a test
;   obsolete       stanza removed; its source may stay; (reason ...) required
; Headless suites registered from build-only executables:

((name tui_flow_tests) (class deterministic))
((name test_install_node_form) (class deterministic))
((name test_install_baker_form) (class deterministic))
((name test_install_accuser_form) (class deterministic))
((name test_install_dal_node_form) (class deterministic))
((name test_modal_interactions) (class deterministic))
((name test_instances_page) (class deterministic))
((name test_instance_details) (class deterministic))
((name test_log_viewer_page) (class deterministic))
((name test_import_wizard) (class deterministic))
((name test_help_modal_baseline) (class deterministic))
((name test_help_modal_improvements) (class deterministic))
((name test_help_modal_all_pages) (class deterministic))
((name test_help_modal_regression) (class deterministic))
((name test_help_modal_no_duplicates) (class deterministic))
((name test_help_modal_navigation) (class deterministic))

((name test_golden_path_tui_v2)
 (class env-gated)
 (gate
  "CI integration shard 2: test/integration/cli-tester/tests/headless/01-golden-path.sh (needs systemd and Octez binaries, CI=true)"))

((name test_help_modal_debug)
 (class manual)
 (reason "Prints help modal screens for inspection; makes no assertion"))

((name test_ui_regressions_forms)
 (class obsolete)
 (reason
  "Stanza commented out in test/dune; superseded by test_ui_regressions_forms_comprehensive"))
