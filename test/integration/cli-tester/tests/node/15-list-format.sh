#!/bin/bash
# Test: Verify 'om list' output format
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-list-fmt"

echo "Test: Verify list output format"

cleanup_instance "$INSTANCE" || true

# Install a node
om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --rpc-addr "127.0.0.1:8746" --net-addr "0.0.0.0:9746" \
    --service-user tezos \
    --no-enable 2>&1

# Get list output
LIST_OUTPUT=$(om list 2>&1)

# Verify instance appears
if ! echo "$LIST_OUTPUT" | grep -q "$INSTANCE"; then
    echo "ERROR: Instance not in list output"
    echo "Output: $LIST_OUTPUT"
    exit 1
fi
echo "Instance appears in list"

# Verify role appears
if ! echo "$LIST_OUTPUT" | grep -q "node"; then
    echo "ERROR: Role 'node' not in list output"
    echo "Output: $LIST_OUTPUT"
    exit 1
fi
echo "Role appears in list"

# Verify network appears
if ! echo "$LIST_OUTPUT" | grep -q "shadownet"; then
    echo "ERROR: Network not in list output"
    echo "Output: $LIST_OUTPUT"
    exit 1
fi
echo "Network appears in list"

# Cleanup
cleanup_instance "$INSTANCE"

# Verify removed from list
LIST_AFTER=$(om list 2>&1)
if echo "$LIST_AFTER" | grep -q "$INSTANCE"; then
    echo "ERROR: Instance still in list after removal"
    exit 1
fi
echo "Instance removed from list"

echo "List format test passed"
