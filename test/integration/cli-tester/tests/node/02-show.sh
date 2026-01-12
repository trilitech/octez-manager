#!/bin/bash
# Test: Show node instance details
set -euo pipefail
source /tests/lib.sh

echo "Test: Show node instance"

# Instance should exist from previous test
if ! instance_exists "$TEST_INSTANCE"; then
    echo "ERROR: Instance does not exist (run 01-install first)"
    exit 1
fi

# Show instance details
echo "Getting instance details..."
SHOW_OUTPUT=$(om_instance "$TEST_INSTANCE" show 2>&1)

# Verify key fields are present
assert_contains "$SHOW_OUTPUT" "$TEST_INSTANCE" "Should show instance name"
assert_contains "$SHOW_OUTPUT" "node" "Should show role"
assert_contains "$SHOW_OUTPUT" "shadownet" "Should show network"

echo "Show instance test passed"
