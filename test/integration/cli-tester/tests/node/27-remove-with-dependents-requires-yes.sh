#!/bin/bash
# Test: 'instance remove' on an instance with dependents refuses
# non-interactively unless --yes is given, and exits non-zero
# (regression test for #998: previously this printed a message
# telling the user to pass --yes, but exited 0 without removing
# anything, and --yes/-y did not even exist as a flag).
set -euo pipefail
source /tests/lib.sh

test_init "Remove with dependents requires --yes"

NODE_INSTANCE="test-remove-yes-node"
BAKER_INSTANCE="test-remove-yes-baker"
NODE_RPC="127.0.0.1:$(alloc_port)"
NODE_NET="0.0.0.0:$(alloc_port)"

register_instance "$NODE_INSTANCE"
register_instance "$BAKER_INSTANCE"

# Install a node (no snapshot needed - we never start it).
echo "Installing node..."
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--rpc-addr "$NODE_RPC" \
	--net-addr "$NODE_NET" \
	--service-user tezos \
	--no-enable 2>&1

# Install a baker depending on the node, so the node has a dependent.
echo "Installing baker depending on node..."
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

if ! instance_exists "$NODE_INSTANCE"; then
	echo "ERROR: Node instance not created"
	exit 1
fi

# --- Step 1: remove without --yes must be refused (non-zero exit,
#     nothing removed). The CLI runs non-interactively here (no tty).
echo "Attempting remove without --yes (should be refused)..."
set +e
output=$(om instance "$NODE_INSTANCE" remove 2>&1)
rc=$?
set -e

if [ "$rc" -eq 0 ]; then
	echo "ERROR: 'remove' without --yes exited 0; it should refuse and"
	echo "exit non-zero when the instance has dependents"
	echo "Output was: $output"
	exit 1
fi

assert_contains "$output" "dependents" \
	"Expected refusal message to mention dependents"
assert_contains "$output" "--yes" \
	"Expected refusal message to mention --yes"

if ! instance_exists "$NODE_INSTANCE"; then
	echo "ERROR: Node instance was removed even though removal was refused"
	exit 1
fi
echo "Refused as expected (exit $rc), instance still present"

# --- Step 2: remove with --yes must proceed.
echo "Attempting remove with --yes (should proceed)..."
om instance "$NODE_INSTANCE" remove --yes 2>&1

if instance_exists "$NODE_INSTANCE"; then
	echo "ERROR: Node instance still exists after 'remove --yes'"
	exit 1
fi
echo "Instance removed with --yes"

echo "Remove-with-dependents --yes test passed"
