#!/bin/bash
# Test: Import non-existent service shows clear error
set -euo pipefail
source /tests/lib.sh

echo "Test: Import non-existent service error handling"

# Try to import a service that doesn't exist
echo "Attempting to import non-existent service..."
if om import "octez-node@nonexistent-service" 2>&1 | tee /tmp/import_error.txt; then
    echo "ERROR: Import should have failed for non-existent service"
    exit 1
fi

# Verify error message is helpful
if ! grep -qi "not found\|does not exist\|cannot find" /tmp/import_error.txt; then
    echo "ERROR: Error message should mention service not found"
    cat /tmp/import_error.txt
    exit 1
fi

echo "Non-existent service error handling passed"
