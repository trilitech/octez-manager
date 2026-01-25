#!/bin/bash
# Run all tests (including TUI tests) with coverage measurement

set -euo pipefail

echo "=== Cleaning build artifacts ==="
dune clean

echo ""
echo "=== Running standard unit tests with coverage ==="
dune runtest --instrument-with bisect_ppx

echo ""
echo "=== Running TUI regression tests with coverage ==="
BISECT_FILE=_build/default/bisect dune exec --instrument-with bisect_ppx test/test_ui_regressions_minimal.exe
BISECT_FILE=_build/default/bisect dune exec --instrument-with bisect_ppx test/test_ui_regressions_forms.exe

echo ""
echo "=== Running other TUI tests with coverage ==="
BISECT_FILE=_build/default/bisect dune exec --instrument-with bisect_ppx test/test_install_node_form.exe || true
BISECT_FILE=_build/default/bisect dune exec --instrument-with bisect_ppx test/test_install_baker_form.exe || true
BISECT_FILE=_build/default/bisect dune exec --instrument-with bisect_ppx test/test_install_accuser_form.exe || true
BISECT_FILE=_build/default/bisect dune exec --instrument-with bisect_ppx test/test_install_dal_node_form.exe || true
BISECT_FILE=_build/default/bisect dune exec --instrument-with bisect_ppx test/test_modal_interactions.exe || true
BISECT_FILE=_build/default/bisect dune exec --instrument-with bisect_ppx test/test_instances_page.exe || true
BISECT_FILE=_build/default/bisect dune exec --instrument-with bisect_ppx test/tui_flow_tests.exe || true

echo ""
echo "=== Generating coverage report ==="
bisect-ppx-report summary --coverage-path=_build/default

echo ""
echo "=== Generating HTML coverage report ==="
bisect-ppx-report html --coverage-path=_build/default -o _coverage

echo ""
echo "✓ Coverage report generated in _coverage/index.html"
