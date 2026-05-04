#!/bin/bash
# Test: Purging baker/accuser with shared base-dir preserves directory for the other
# See: https://github.com/trilitech/octez-manager/issues/816
set -euo pipefail
source /tests/lib.sh

test_init "Shared base-dir purge protection (baker + accuser)"

NODE_INSTANCE="test-shared-basedir-mixed-node"
BAKER_INSTANCE="test-shared-basedir-mixed-baker"
ACCUSER_INSTANCE="test-shared-basedir-mixed-accuser"
SHARED_BASE_DIR="/tmp/test-shared-octez-client-basedir-mixed-$$"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"

register_instance "$BAKER_INSTANCE"
register_instance "$ACCUSER_INSTANCE"
register_instance "$NODE_INSTANCE"
register_data_dir "$SHARED_BASE_DIR"

# Create shared base directory
mkdir -p "$SHARED_BASE_DIR"
chown tezos:tezos "$SHARED_BASE_DIR"

# Install a node first (required for baker and accuser)
echo "Installing node..."
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--rpc-addr "$NODE_RPC" \
	--net-addr "$NODE_NET" \
	--service-user tezos \
	--no-enable 2>&1

# Verify node data directory exists
NODE_DATA_DIR="/var/lib/octez/$NODE_INSTANCE"
if [ ! -d "$NODE_DATA_DIR" ]; then
	echo "ERROR: Node data directory not created: $NODE_DATA_DIR"
	exit 1
fi
echo "Node data directory exists: $NODE_DATA_DIR"

# Install baker with custom shared base-dir
echo "Installing baker with shared base-dir..."
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--base-dir "$SHARED_BASE_DIR" \
	--dal-endpoint none \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

# Verify baker is installed
if ! instance_exists "$BAKER_INSTANCE"; then
	echo "ERROR: Baker instance not in registry"
	exit 1
fi
echo "Baker registered"

# Install accuser with the SAME shared base-dir
echo "Installing accuser with the same shared base-dir..."
om install-accuser \
	--instance "$ACCUSER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--base-dir "$SHARED_BASE_DIR" \
	--service-user tezos \
	--no-enable 2>&1

# Verify accuser is installed
if ! instance_exists "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser instance not in registry"
	exit 1
fi
echo "Accuser registered"

# Verify both baker and accuser use the same base-dir
BAKER_ENV="/etc/octez/instances/$BAKER_INSTANCE/node.env"
ACCUSER_ENV="/etc/octez/instances/$ACCUSER_INSTANCE/node.env"

if ! grep -q "OCTEZ_BAKER_BASE_DIR=$SHARED_BASE_DIR" "$BAKER_ENV"; then
	echo "ERROR: Baker does not use shared base-dir"
	cat "$BAKER_ENV"
	exit 1
fi

if ! grep -q "OCTEZ_CLIENT_BASE_DIR=$SHARED_BASE_DIR" "$ACCUSER_ENV"; then
	echo "ERROR: Accuser does not use shared base-dir"
	cat "$ACCUSER_ENV"
	exit 1
fi
echo "Both baker and accuser confirmed to use shared base-dir: $SHARED_BASE_DIR"

# Create a marker file in the shared directory to verify it's preserved
MARKER_FILE="$SHARED_BASE_DIR/.test-marker"
echo "test marker" >"$MARKER_FILE"
chown tezos:tezos "$MARKER_FILE"

# Now purge baker - this should NOT delete the shared base-dir
# because accuser still uses it
echo "Purging baker..."
om instance "$BAKER_INSTANCE" purge --force-purge 2>&1

# Verify baker is removed
if instance_exists "$BAKER_INSTANCE"; then
	echo "ERROR: Baker still in registry after purge"
	exit 1
fi
echo "Baker purged successfully"

# CRITICAL CHECK: Shared base-dir should still exist (accuser still uses it)
if [ ! -d "$SHARED_BASE_DIR" ]; then
	echo "ERROR: Shared base-dir was deleted when purging baker!"
	echo "Accuser still depends on this directory - this is bug #816"
	exit 1
fi
echo "✓ Shared base-dir preserved (accuser still uses it)"

# Verify marker file still exists
if [ ! -f "$MARKER_FILE" ]; then
	echo "ERROR: Marker file deleted - shared directory contents were removed"
	exit 1
fi
echo "✓ Shared base-dir contents intact"

# Verify accuser still exists
if ! instance_exists "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser removed from registry"
	exit 1
fi
echo "✓ Accuser still in registry"

# Now purge accuser
echo "Purging accuser..."
om instance "$ACCUSER_INSTANCE" purge --force-purge 2>&1

# Verify accuser is removed
if instance_exists "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser still in registry after purge"
	exit 1
fi
echo "Accuser purged successfully"

# After both baker and accuser are purged, the shared base-dir can be removed
# (either by the second purge, or left for manual cleanup)
# For this test, we accept either behavior as long as it wasn't deleted
# while accuser was still active.
if [ -d "$SHARED_BASE_DIR" ]; then
	echo "✓ Shared base-dir still exists (can be manually cleaned up)"
else
	echo "✓ Shared base-dir removed after last dependent was purged (acceptable)"
fi

echo "Test passed: Shared base-dir is not deleted while still in use by baker and accuser"
