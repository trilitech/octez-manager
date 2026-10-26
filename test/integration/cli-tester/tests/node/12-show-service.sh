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
OUTPUT=$(assert_command_output 0 "" "capture output" -- om instance "$INSTANCE" show-service)

# Verify it contains systemd unit markers
if ! grep -q "\[Unit\]\|\[Service\]" <<<"$OUTPUT"; then
	echo "ERROR: show-service doesn't show systemd unit format"
	echo "Output: $OUTPUT"
	exit 1
fi
echo "Shows [Unit] or [Service] section"

# Verify it references the instance
if ! grep -qi "octez\|node" <<<"$OUTPUT"; then
	echo "ERROR: show-service doesn't reference octez/node"
	echo "Output: $OUTPUT"
	exit 1
fi
echo "References octez/node"

echo "Show-service test passed"
