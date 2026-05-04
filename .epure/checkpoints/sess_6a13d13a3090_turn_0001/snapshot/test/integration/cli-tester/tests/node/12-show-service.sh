#!/bin/bash
# Test: show-service displays systemd unit content
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-show-svc"

test_init "show-service displays systemd unit content"

register_instance "$INSTANCE"

RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)

# Install node
om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--rpc-addr "127.0.0.1:$RPC_PORT" --net-addr "0.0.0.0:$NET_PORT" \
	--service-user tezos \
	--no-enable 2>&1

# Get show-service output
echo "Getting service unit..."
OUTPUT=$(om instance "$INSTANCE" show-service 2>&1)

# Verify it contains systemd unit markers
if ! echo "$OUTPUT" | grep -q "\[Unit\]\|\[Service\]"; then
	echo "ERROR: show-service doesn't show systemd unit format"
	echo "Output: $OUTPUT"
	exit 1
fi
echo "Shows [Unit] or [Service] section"

# Verify it references the instance
if ! echo "$OUTPUT" | grep -qi "octez\|node"; then
	echo "ERROR: show-service doesn't reference octez/node"
	echo "Output: $OUTPUT"
	exit 1
fi
echo "References octez/node"

echo "Show-service test passed"
