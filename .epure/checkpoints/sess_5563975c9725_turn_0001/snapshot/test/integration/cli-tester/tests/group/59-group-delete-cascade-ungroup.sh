#!/bin/bash
# Test: Group delete with --cascade and --ungroup flags, and mutual exclusion
set -euo pipefail
source /tests/lib.sh

test_init "Group delete cascade and ungroup"

GROUP_NAME="test-cascade-$$"
TEST_INSTANCE="test-casc-node-$$"
RPC_PORT=$(alloc_port)

# Pre-cleanup
om group delete "$GROUP_NAME" 2>/dev/null || true
register_instance "$TEST_INSTANCE"

# ── Create group and install a service ──
echo "Creating group '$GROUP_NAME'..."
om group create "$GROUP_NAME" \
	--network shadownet \
	--app-bin-dir /usr/local/bin \
	--service-user tezos

echo "Installing node '$TEST_INSTANCE'..."
om install-node \
	--instance "$TEST_INSTANCE" \
	--network shadownet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:$RPC_PORT" \
	--service-user tezos \
	--no-enable 2>&1 || true

om group add "$GROUP_NAME" --instance "$TEST_INSTANCE"

# ── Mutual exclusion: --cascade and --ungroup together should fail ──
echo "Testing --cascade --ungroup mutual exclusion..."
if om group delete "$GROUP_NAME" --cascade --ungroup 2>&1; then
	echo "ERROR: Should have rejected --cascade --ungroup together"
	exit 1
fi
echo "Correctly rejected: --cascade --ungroup together"

# ── --ungroup: removes group, keeps service ──
echo "Deleting group with --ungroup..."
om group delete "$GROUP_NAME" --ungroup

# Service should still exist but without group
if ! instance_exists "$TEST_INSTANCE"; then
	echo "ERROR: Service should survive --ungroup delete"
	exit 1
fi
echo "Service survived --ungroup delete"

# Group should be gone
OUTPUT=$(om group list 2>&1)
if echo "$OUTPUT" | grep -q "$GROUP_NAME"; then
	echo "ERROR: Group still exists after --ungroup delete"
	exit 1
fi
echo "Group removed after --ungroup delete"

echo "Test passed"
