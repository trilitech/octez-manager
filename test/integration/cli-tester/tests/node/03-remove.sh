#!/bin/bash
# Test: Remove node instance
set -euo pipefail
source /tests/lib.sh

echo "Test: Remove node instance"

# Instance should exist
if ! instance_exists "$TEST_INSTANCE"; then
    echo "ERROR: Instance does not exist (run previous tests first)"
    exit 1
fi

# Remove instance
echo "Removing instance..."
om_instance "$TEST_INSTANCE" remove

# Verify instance is not in list anymore
if instance_exists "$TEST_INSTANCE"; then
    echo "ERROR: Instance still exists after remove"
    exit 1
fi

echo "Remove instance test passed"
