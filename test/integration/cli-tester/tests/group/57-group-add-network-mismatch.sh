#!/bin/bash
# Test: Adding a service to a group with a different network should fail
set -euo pipefail
source /tests/lib.sh

test_init "Group add network mismatch"

GROUP_NAME="test-netmismatch-$$"
TEST_INSTANCE="test-netmis-node-$$"
RPC_PORT=$(alloc_port)

# Pre-cleanup
om group delete "$GROUP_NAME" 2>/dev/null || true
register_instance "$TEST_INSTANCE"

# ── Create group on shadownet ──
echo "Creating group '$GROUP_NAME' on shadownet..."
om group create "$GROUP_NAME" \
	--network shadownet \
	--app-bin-dir /usr/local/bin \
	--service-user tezos

# ── Install a node on a DIFFERENT network (mainnet ≠ shadownet) ──
# Use mainnet which is a built-in network and always recognized.
echo "Installing node '$TEST_INSTANCE' on mainnet..."
om install-node \
	--instance "$TEST_INSTANCE" \
	--network mainnet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:$RPC_PORT" \
	--service-user tezos \
	--no-enable 2>&1 || true

if ! instance_exists "$TEST_INSTANCE"; then
	echo "ERROR: Node instance not created"
	exit 1
fi

# ── Try to add mismatched service to group ──
echo "Attempting to add ghostnet node to shadownet group (should fail)..."
if om group add "$GROUP_NAME" --instance "$TEST_INSTANCE" 2>&1; then
	echo "ERROR: group add should have failed due to network mismatch"
	exit 1
fi
echo "Correctly rejected: network mismatch"

# ── Cleanup ──
om group delete "$GROUP_NAME" 2>/dev/null || true

echo "Test passed"
