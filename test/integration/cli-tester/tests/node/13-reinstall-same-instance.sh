#!/bin/bash
# Test: Reinstall existing instance updates configuration
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-reinstall"
DATA_DIR="/var/lib/octez/$INSTANCE"

test_init "Reinstall existing instance updates configuration"

register_instance "$INSTANCE"

RPC1=$(alloc_port)
NET1=$(alloc_port)
RPC2=$(alloc_port)
NET2=$(alloc_port)

# First install with one RPC address
echo "First install..."
om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--rpc-addr "127.0.0.1:$RPC1" --net-addr "0.0.0.0:$NET1" \
	--service-user tezos \
	--no-enable 2>&1

# Verify first config
ENV_FILE="/etc/octez/instances/$INSTANCE/node.env"
if ! grep -q "$RPC1" "$ENV_FILE"; then
	echo "ERROR: First install RPC not configured"
	exit 1
fi
echo "First install: RPC $RPC1"

# Reinstall with different RPC address (don't specify --network, it's in existing config)
echo "Reinstall with new RPC..."
om install-node \
	--instance "$INSTANCE" \
	--data-dir "$DATA_DIR" \
	--rpc-addr "127.0.0.1:$RPC2" --net-addr "0.0.0.0:$NET2" \
	--service-user tezos \
	--preserve-data \
	--no-enable 2>&1

# Verify updated config
if ! grep -q "$RPC2" "$ENV_FILE"; then
	echo "ERROR: Reinstall didn't update RPC"
	cat "$ENV_FILE"
	exit 1
fi
echo "Reinstall: RPC updated to $RPC2"

# Verify old RPC not present
if grep -q "$RPC1" "$ENV_FILE"; then
	echo "ERROR: Old RPC still in config"
	exit 1
fi
echo "Old RPC removed"

echo "Reinstall test passed"
