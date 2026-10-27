#!/bin/bash
# Test: om list output format - verify instance appears correctly
# Note: Status is shown in UI, not CLI list (to avoid systemd query issues)
set -euo pipefail
source /tests/lib.sh

test_init "om list output format"

NODE_INSTANCE="test-list-status"
RPC_ADDR="127.0.0.1:$(alloc_port)"
NET_ADDR="0.0.0.0:$(alloc_port)"

# Register instance for automatic cleanup (also pre-cleans leftovers)
register_instance "$NODE_INSTANCE"

# Install a node
echo "Installing node..."
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--rpc-addr "$RPC_ADDR" \
	--net-addr "$NET_ADDR" \
	--service-user tezos \
	--no-enable 2>&1

# Instance should appear in list
LIST_OUTPUT=$(om list)
echo "om list output:"
echo "$LIST_OUTPUT"
if [[ "$LIST_OUTPUT" == *"$NODE_INSTANCE"* ]]; then
	echo "Instance appears in list"
else
	echo "ERROR: Instance not in list"
	exit 1
fi

# Verify role and network in output
MATCHED_OUTPUT=$(grep -F "$NODE_INSTANCE" <<<"$LIST_OUTPUT") || {
	echo "Expected row selection failed" >&2
	exit 1
}
if grep -q "node" <<<"$MATCHED_OUTPUT"; then
	echo "Role appears correctly"
else
	echo "ERROR: Role not in output"
	exit 1
fi

MATCHED_OUTPUT=$(grep -F "$NODE_INSTANCE" <<<"$LIST_OUTPUT") || {
	echo "Expected row selection failed" >&2
	exit 1
}
if grep -q "shadownet" <<<"$MATCHED_OUTPUT"; then
	echo "Network appears correctly"
else
	echo "ERROR: Network not in output"
	exit 1
fi

echo "om list format test passed"
