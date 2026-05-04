#!/bin/bash
# Pre-build lint hook for OCaml projects (Épure validation pipeline).
# Runs dune fmt --check and dune build in sequence; any failure exits non-zero,
# blocking the build from proceeding.
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

echo "Running OCaml pre-build lint hook..."
echo ""

echo "  Checking formatting (dune fmt --check)..."
if ! dune fmt --check 2>&1; then
  echo ""
  echo -e "${RED}FAIL${NC}: dune fmt --check — formatting violations found."
  echo "Run 'dune fmt' to auto-format, then commit the changes."
  exit 1
fi
echo -e "  ${GREEN}OK${NC}: formatting"

echo "  Checking build (dune build)..."
if ! dune build 2>&1; then
  echo ""
  echo -e "${RED}FAIL${NC}: dune build — compilation errors found."
  exit 1
fi
echo -e "  ${GREEN}OK${NC}: build"

echo ""
echo -e "${GREEN}OCaml pre-build lint hook passed.${NC}"
