#!/bin/bash
# Test: Install node with custom RPC and network addresses
set -euo pipefail
source /tests/lib.sh

test_init "Install node with custom RPC and network addresses"

INSTANCE="test-custom-addrs"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
CUSTOM_RPC="0.0.0.0:$RPC_PORT"
CUSTOM_NET="0.0.0.0:$NET_PORT"

register_instance "$INSTANCE"

om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--rpc-addr "$CUSTOM_RPC" \
	--net-addr "$CUSTOM_NET" \
	--service-user tezos \
	--no-enable 2>&1

# Verify RPC address in env (stored in OCTEZ_NODE_ARGS)
ENV_FILE="/etc/octez/instances/$INSTANCE/node.env"
if ! grep -q "$RPC_PORT" "$ENV_FILE"; then
	echo "ERROR: Custom RPC port not in env file"
	cat "$ENV_FILE"
	exit 1
fi
echo "Custom RPC address configured: $CUSTOM_RPC"

# Verify net address in env
if ! grep -q "$NET_PORT" "$ENV_FILE"; then
	echo "ERROR: Custom net port not in env file"
	cat "$ENV_FILE"
	exit 1
fi
echo "Custom net address configured: $CUSTOM_NET"

echo "Custom RPC/net addresses test passed"
