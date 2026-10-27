#!/bin/bash
# Run all tests (including TUI tests) with coverage measurement

set -euo pipefail

echo "=== Cleaning build artifacts ==="
dune clean

echo ""
echo "=== Running all registered tests (unit, golden and headless TUI) with coverage ==="
dune runtest --instrument-with bisect_ppx

echo ""
echo "=== Generating coverage report ==="
bisect-ppx-report summary --coverage-path=_build/default

echo ""
echo "=== Generating HTML coverage report ==="
bisect-ppx-report html --coverage-path=_build/default -o _coverage

echo ""
echo "✓ Coverage report generated in _coverage/index.html"
