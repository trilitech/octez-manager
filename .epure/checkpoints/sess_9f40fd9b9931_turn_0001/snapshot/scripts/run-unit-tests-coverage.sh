#!/bin/bash
# Run unit tests with coverage instrumentation
set -euo pipefail

COVERAGE_DIR="${1:-./_coverage}"

echo "Running unit tests with coverage..."
echo "Coverage output: $COVERAGE_DIR"

# Clean previous coverage data
rm -rf "$COVERAGE_DIR"
mkdir -p "$COVERAGE_DIR"

# Run tests with instrumentation
echo "Building and running tests..."
dune clean
dune runtest --instrument-with bisect_ppx --force

# Collect coverage files from build directory
echo "Collecting coverage files..."
find _build -name "bisect*.coverage" -exec cp {} "$COVERAGE_DIR/" \;

# Check results
COVERAGE_FILES=$(find "$COVERAGE_DIR" -name "*.coverage" 2>/dev/null | wc -l)
echo ""
echo "✓ Unit tests complete!"
echo "Generated $COVERAGE_FILES coverage file(s) in $COVERAGE_DIR"
