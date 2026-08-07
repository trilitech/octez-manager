#!/bin/bash
# Test: Multiple instances can coexist independently
set -euo pipefail
source /tests/lib.sh

INSTANCE1="test-multi-1"
INSTANCE2="test-multi-2"

test_init "Multiple instances can coexist"

register_instance "$INSTANCE1"
register_instance "$INSTANCE2"

RPC1=$(alloc_port)
NET1=$(alloc_port)
RPC2=$(alloc_port)
NET2=$(alloc_port)

# Install first instance
echo "Installing first instance..."
om install-node \
	--instance "$INSTANCE1" \
	--network shadownet \
	--rpc-addr "127.0.0.1:$RPC1" --net-addr "0.0.0.0:$NET1" \
	--service-user tezos \
	--no-enable 2>&1

# Install second instance
echo "Installing second instance..."
om install-node \
	--instance "$INSTANCE2" \
	--network shadownet \
	--rpc-addr "127.0.0.1:$RPC2" --net-addr "0.0.0.0:$NET2" \
	--service-user tezos \
	--no-enable 2>&1

# Verify both in registry
if ! instance_exists "$INSTANCE1"; then
	echo "ERROR: First instance not in registry"
	exit 1
fi
if ! instance_exists "$INSTANCE2"; then
	echo "ERROR: Second instance not in registry"
	exit 1
fi
echo "Both instances registered"

# Verify separate data directories
if [ ! -d "/var/lib/octez/$INSTANCE1" ]; then
	echo "ERROR: First instance data dir missing"
	exit 1
fi
if [ ! -d "/var/lib/octez/$INSTANCE2" ]; then
	echo "ERROR: Second instance data dir missing"
	exit 1
fi
echo "Separate data directories exist"

# Verify separate env files with different ports
if ! grep -q "$RPC1" "/etc/octez/instances/$INSTANCE1/node.env"; then
	echo "ERROR: First instance wrong port"
	exit 1
fi
if ! grep -q "$RPC2" "/etc/octez/instances/$INSTANCE2/node.env"; then
	echo "ERROR: Second instance wrong port"
	exit 1
fi
echo "Different RPC ports configured"

# Remove first, verify second still exists
echo "Removing first instance..."
om instance "$INSTANCE1" purge

if instance_exists "$INSTANCE1"; then
	echo "ERROR: First instance not removed"
	exit 1
fi
if ! instance_exists "$INSTANCE2"; then
	echo "ERROR: Second instance affected by first removal"
	exit 1
fi
echo "Instances are independent"

echo "Multiple instances test passed"
