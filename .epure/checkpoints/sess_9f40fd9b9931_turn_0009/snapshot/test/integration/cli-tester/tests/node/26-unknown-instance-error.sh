#!/bin/bash
# Test: Unknown instance name shows helpful error message
# Verifies that 'octez-manager instance <unknown>' shows "Unknown instance"
# rather than the misleading "ACTION required" prompt.
set -euo pipefail
source /tests/lib.sh

test_init "Unknown instance name shows helpful error"

NONEXISTENT="nonexistent-instance-cli-bug-1"

# Running 'instance <name>' with no action and an unknown name should
# exit non-zero and say "Unknown instance", not "ACTION required".
output=$(om instance "$NONEXISTENT" 2>&1 || true)

assert_contains "$output" "Unknown instance" \
  "Expected 'Unknown instance' message for unknown instance name"

if [[ "$output" == *"ACTION required"* ]]; then
  echo "ERROR: Got old 'ACTION required' message instead of 'Unknown instance'"
  exit 1
fi

echo "Unknown instance error test passed"
