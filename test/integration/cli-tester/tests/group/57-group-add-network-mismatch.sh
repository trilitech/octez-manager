#!/bin/bash
# Test: Adding a service to a group with a different network should fail
set -euo pipefail
source /tests/lib.sh

test_init "Group add network mismatch"

GROUP_NAME="test-netmismatch-$$"
TEST_INSTANCE="test-netmis-node-$$"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)

# Pre-cleanup
om group delete "$GROUP_NAME" 2>/dev/null || true
register_instance "$TEST_INSTANCE"

# ── Install a node on shadownet ──
echo "Installing node '$TEST_INSTANCE' on shadownet..."
om install-node \
	--instance "$TEST_INSTANCE" \
	--network shadownet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:$RPC_PORT" \
	--net-addr "0.0.0.0:$NET_PORT" \
	--service-user tezos \
	--no-enable 2>&1

if ! instance_exists "$TEST_INSTANCE"; then
	echo "ERROR: Node instance not created"
	exit 1
fi

# ── Create group on a DIFFERENT network (mainnet ≠ shadownet) ──
# mainnet is a built-in network name that is always recognized without
# querying teztnets.com — it's statically defined in octez-manager.
echo "Creating group '$GROUP_NAME' on mainnet..."
om group create "$GROUP_NAME" \
	--network mainnet \
	--app-bin-dir /usr/local/bin \
	--service-user tezos

# ── Try to add shadownet service to mainnet group ──
echo "Attempting to add shadownet node to mainnet group (should fail)..."
if om group add "$GROUP_NAME" --instance "$TEST_INSTANCE" 2>&1; then
	echo "ERROR: group add should have failed due to network mismatch"
	exit 1
fi
echo "Correctly rejected: network mismatch"

# ── Cleanup ──
om group delete "$GROUP_NAME" 2>/dev/null || true

echo "Test passed"
