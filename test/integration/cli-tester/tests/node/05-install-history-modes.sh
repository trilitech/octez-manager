#!/bin/bash
# Test: Install nodes with different history modes (rolling and full)
set -euo pipefail
source /tests/lib.sh

test_init "Install nodes with different history modes"

# Test rolling mode
INSTANCE_ROLLING="test-rolling"
register_instance "$INSTANCE_ROLLING"

RPC_PORT_ROLLING=$(alloc_port)
NET_PORT_ROLLING=$(alloc_port)

echo "Installing node with rolling history mode..."
om install-node \
	--instance "$INSTANCE_ROLLING" \
	--network shadownet \
	--history-mode rolling \
	--rpc-addr "127.0.0.1:$RPC_PORT_ROLLING" \
	--net-addr "0.0.0.0:$NET_PORT_ROLLING" \
	--service-user tezos \
	--no-enable 2>&1

# Verify rolling mode in env
ENV_ROLLING="/etc/octez/instances/$INSTANCE_ROLLING/node.env"
if ! grep -q "OCTEZ_HISTORY_MODE=rolling" "$ENV_ROLLING"; then
	echo "ERROR: Rolling mode not set in env file"
	cat "$ENV_ROLLING"
	exit 1
fi
echo "Rolling mode correctly configured"

# Test full mode
INSTANCE_FULL="test-full"
register_instance "$INSTANCE_FULL"

RPC_PORT_FULL=$(alloc_port)
NET_PORT_FULL=$(alloc_port)

echo "Installing node with full history mode..."
om install-node \
	--instance "$INSTANCE_FULL" \
	--network shadownet \
	--history-mode full \
	--rpc-addr "127.0.0.1:$RPC_PORT_FULL" \
	--net-addr "0.0.0.0:$NET_PORT_FULL" \
	--service-user tezos \
	--no-enable 2>&1

# Verify full mode in env
ENV_FULL="/etc/octez/instances/$INSTANCE_FULL/node.env"
if ! grep -q "OCTEZ_HISTORY_MODE=full" "$ENV_FULL"; then
	echo "ERROR: Full mode not set in env file"
	cat "$ENV_FULL"
	exit 1
fi
echo "Full mode correctly configured"

echo "History modes test passed"
