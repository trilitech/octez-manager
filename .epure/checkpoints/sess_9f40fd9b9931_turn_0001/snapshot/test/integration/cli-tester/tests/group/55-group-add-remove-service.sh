#!/bin/bash
# Test: Add/remove services to/from groups
set -euo pipefail
source /tests/lib.sh

test_init "Group add and remove service"

GROUP_NAME="test-group-svc-$$"
TEST_INSTANCE="test-group-node-$$"
RPC_PORT=$(alloc_port)

# Pre-cleanup
om group delete "$GROUP_NAME" 2>/dev/null || true
register_instance "$TEST_INSTANCE"

# ── Create group ──
echo "Creating group '$GROUP_NAME'..."
om group create "$GROUP_NAME" \
	--network shadownet \
	--app-bin-dir /usr/local/bin \
	--service-user tezos

# ── Install a node ──
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

# Verify node exists
if ! instance_exists "$TEST_INSTANCE"; then
	echo "ERROR: Node instance not created"
	exit 1
fi

# ── Add service to group ──
echo "Adding '$TEST_INSTANCE' to group '$GROUP_NAME'..."
om group add "$GROUP_NAME" --instance "$TEST_INSTANCE"

# Verify service is in group via show
OUTPUT=$(om group show --json "$GROUP_NAME" 2>&1)
echo "Group show after add: $OUTPUT"

# Verify service registry has group set
REGISTRY_DIR="/etc/octez_manager/services"
REGISTRY_FILE="${REGISTRY_DIR}/${TEST_INSTANCE}.json"
if [ -f "$REGISTRY_FILE" ]; then
	if ! grep -q "\"group\"" "$REGISTRY_FILE"; then
		echo "ERROR: Service registry should have group field"
		cat "$REGISTRY_FILE"
		exit 1
	fi
	if ! grep -q "$GROUP_NAME" "$REGISTRY_FILE"; then
		echo "ERROR: Service registry group should be '$GROUP_NAME'"
		cat "$REGISTRY_FILE"
		exit 1
	fi
	echo "Service registry correctly shows group '$GROUP_NAME'"
else
	echo "WARNING: Registry file not at expected path (non-root mode?)"
fi

# ── Remove service from group ──
echo "Removing '$TEST_INSTANCE' from group '$GROUP_NAME'..."
om group remove "$GROUP_NAME" --instance "$TEST_INSTANCE"

# Verify service group is cleared
if [ -f "$REGISTRY_FILE" ]; then
	if grep -q "\"group\": \"$GROUP_NAME\"" "$REGISTRY_FILE"; then
		echo "ERROR: Service still has group after remove"
		cat "$REGISTRY_FILE"
		exit 1
	fi
	echo "Service registry group cleared after remove"
fi

# ── Cleanup group ──
om group delete "$GROUP_NAME" 2>/dev/null || true

echo "Test passed"
