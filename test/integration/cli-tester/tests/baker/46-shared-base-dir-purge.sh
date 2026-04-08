#!/bin/bash
# Test: Purging one baker with shared base-dir preserves directory for other bakers
# See: https://github.com/trilitech/octez-manager/issues/816
set -euo pipefail
source /tests/lib.sh

test_init "Shared base-dir purge protection"

NODE_INSTANCE="test-shared-basedir-node"
BAKER1_INSTANCE="test-shared-basedir-baker1"
BAKER2_INSTANCE="test-shared-basedir-baker2"
SHARED_BASE_DIR="/tmp/test-shared-octez-client-basedir-$$"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"

register_instance "$BAKER1_INSTANCE"
register_instance "$BAKER2_INSTANCE"
register_instance "$NODE_INSTANCE"
register_data_dir "$SHARED_BASE_DIR"

# Create shared base directory
mkdir -p "$SHARED_BASE_DIR"
chown tezos:tezos "$SHARED_BASE_DIR"

# Install a node first (required for bakers)
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

# Install first baker with custom shared base-dir
echo "Installing baker1 with shared base-dir..."
om install-baker \
	--instance "$BAKER1_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--base-dir "$SHARED_BASE_DIR" \
	--dal-endpoint none \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

# Verify first baker is installed
if ! instance_exists "$BAKER1_INSTANCE"; then
	echo "ERROR: Baker1 instance not in registry"
	exit 1
fi
echo "Baker1 registered"

# Install second baker with the SAME shared base-dir
echo "Installing baker2 with the same shared base-dir..."
om install-baker \
	--instance "$BAKER2_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--base-dir "$SHARED_BASE_DIR" \
	--dal-endpoint none \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

# Verify second baker is installed
if ! instance_exists "$BAKER2_INSTANCE"; then
	echo "ERROR: Baker2 instance not in registry"
	exit 1
fi
echo "Baker2 registered"

# Verify both bakers use the same base-dir
BAKER1_ENV="/etc/octez/instances/$BAKER1_INSTANCE/node.env"
BAKER2_ENV="/etc/octez/instances/$BAKER2_INSTANCE/node.env"

if ! grep -q "OCTEZ_BAKER_BASE_DIR=$SHARED_BASE_DIR" "$BAKER1_ENV"; then
	echo "ERROR: Baker1 does not use shared base-dir"
	cat "$BAKER1_ENV"
	exit 1
fi

if ! grep -q "OCTEZ_BAKER_BASE_DIR=$SHARED_BASE_DIR" "$BAKER2_ENV"; then
	echo "ERROR: Baker2 does not use shared base-dir"
	cat "$BAKER2_ENV"
	exit 1
fi
echo "Both bakers confirmed to use shared base-dir: $SHARED_BASE_DIR"

# Create a marker file in the shared directory to verify it's preserved
MARKER_FILE="$SHARED_BASE_DIR/.test-marker"
echo "test marker" >"$MARKER_FILE"
chown tezos:tezos "$MARKER_FILE"

# Now purge baker1 - this should NOT delete the shared base-dir
# because baker2 still uses it
echo "Purging baker1..."
om instance "$BAKER1_INSTANCE" purge --force-purge 2>&1

# Verify baker1 is removed
if instance_exists "$BAKER1_INSTANCE"; then
	echo "ERROR: Baker1 still in registry after purge"
	exit 1
fi
echo "Baker1 purged successfully"

# CRITICAL CHECK: Shared base-dir should still exist (baker2 still uses it)
if [ ! -d "$SHARED_BASE_DIR" ]; then
	echo "ERROR: Shared base-dir was deleted when purging baker1!"
	echo "Baker2 still depends on this directory - this is bug #816"
	exit 1
fi
echo "✓ Shared base-dir preserved (baker2 still uses it)"

# Verify marker file still exists
if [ ! -f "$MARKER_FILE" ]; then
	echo "ERROR: Marker file deleted - shared directory contents were removed"
	exit 1
fi
echo "✓ Shared base-dir contents intact"

# Verify baker2 still exists
if ! instance_exists "$BAKER2_INSTANCE"; then
	echo "ERROR: Baker2 removed from registry"
	exit 1
fi
echo "✓ Baker2 still in registry"

# Now purge baker2 - this should ALSO NOT delete the shared base-dir yet
# because we want to verify the directory isn't removed prematurely
echo "Purging baker2..."
om instance "$BAKER2_INSTANCE" purge --force-purge 2>&1

# Verify baker2 is removed
if instance_exists "$BAKER2_INSTANCE"; then
	echo "ERROR: Baker2 still in registry after purge"
	exit 1
fi
echo "Baker2 purged successfully"

# After both bakers are purged, the shared base-dir can be removed
# (either by the second purge, or left for manual cleanup)
# For this test, we accept either behavior as long as it wasn't deleted
# while baker2 was still active.
if [ -d "$SHARED_BASE_DIR" ]; then
	echo "✓ Shared base-dir still exists (can be manually cleaned up)"
else
	echo "✓ Shared base-dir removed after last dependent was purged (acceptable)"
fi

echo "Test passed: Shared base-dir is not deleted while still in use by another baker"
