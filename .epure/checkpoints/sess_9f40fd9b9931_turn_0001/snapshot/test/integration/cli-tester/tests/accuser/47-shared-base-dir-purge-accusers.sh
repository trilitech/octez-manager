#!/bin/bash
# Test: Purging one accuser with shared base-dir preserves directory for other accusers
# See: https://github.com/trilitech/octez-manager/issues/816
set -euo pipefail
source /tests/lib.sh

test_init "Shared base-dir purge protection (2 accusers)"

NODE_INSTANCE="test-shared-basedir-accuser-node"
ACCUSER1_INSTANCE="test-shared-basedir-accuser1"
ACCUSER2_INSTANCE="test-shared-basedir-accuser2"
SHARED_BASE_DIR="/tmp/test-shared-octez-client-basedir-accuser-$$"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"

register_instance "$ACCUSER1_INSTANCE"
register_instance "$ACCUSER2_INSTANCE"
register_instance "$NODE_INSTANCE"
register_data_dir "$SHARED_BASE_DIR"

# Create shared base directory
mkdir -p "$SHARED_BASE_DIR"
chown tezos:tezos "$SHARED_BASE_DIR"

# Install a node first (required for accusers)
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

# Install first accuser with custom shared base-dir
echo "Installing accuser1 with shared base-dir..."
om install-accuser \
	--instance "$ACCUSER1_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--base-dir "$SHARED_BASE_DIR" \
	--service-user tezos \
	--no-enable 2>&1

# Verify first accuser is installed
if ! instance_exists "$ACCUSER1_INSTANCE"; then
	echo "ERROR: Accuser1 instance not in registry"
	exit 1
fi
echo "Accuser1 registered"

# Install second accuser with the SAME shared base-dir
echo "Installing accuser2 with the same shared base-dir..."
om install-accuser \
	--instance "$ACCUSER2_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--base-dir "$SHARED_BASE_DIR" \
	--service-user tezos \
	--no-enable 2>&1

# Verify second accuser is installed
if ! instance_exists "$ACCUSER2_INSTANCE"; then
	echo "ERROR: Accuser2 instance not in registry"
	exit 1
fi
echo "Accuser2 registered"

# Verify both accusers use the same base-dir
ACCUSER1_ENV="/etc/octez/instances/$ACCUSER1_INSTANCE/node.env"
ACCUSER2_ENV="/etc/octez/instances/$ACCUSER2_INSTANCE/node.env"

if ! grep -q "OCTEZ_CLIENT_BASE_DIR=$SHARED_BASE_DIR" "$ACCUSER1_ENV"; then
	echo "ERROR: Accuser1 does not use shared base-dir"
	cat "$ACCUSER1_ENV"
	exit 1
fi

if ! grep -q "OCTEZ_CLIENT_BASE_DIR=$SHARED_BASE_DIR" "$ACCUSER2_ENV"; then
	echo "ERROR: Accuser2 does not use shared base-dir"
	cat "$ACCUSER2_ENV"
	exit 1
fi
echo "Both accusers confirmed to use shared base-dir: $SHARED_BASE_DIR"

# Create a marker file in the shared directory to verify it's preserved
MARKER_FILE="$SHARED_BASE_DIR/.test-marker"
echo "test marker" >"$MARKER_FILE"
chown tezos:tezos "$MARKER_FILE"

# Now purge accuser1 - this should NOT delete the shared base-dir
# because accuser2 still uses it
echo "Purging accuser1..."
om instance "$ACCUSER1_INSTANCE" purge --force-purge 2>&1

# Verify accuser1 is removed
if instance_exists "$ACCUSER1_INSTANCE"; then
	echo "ERROR: Accuser1 still in registry after purge"
	exit 1
fi
echo "Accuser1 purged successfully"

# CRITICAL CHECK: Shared base-dir should still exist (accuser2 still uses it)
if [ ! -d "$SHARED_BASE_DIR" ]; then
	echo "ERROR: Shared base-dir was deleted when purging accuser1!"
	echo "Accuser2 still depends on this directory - this is bug #816"
	exit 1
fi
echo "✓ Shared base-dir preserved (accuser2 still uses it)"

# Verify marker file still exists
if [ ! -f "$MARKER_FILE" ]; then
	echo "ERROR: Marker file deleted - shared directory contents were removed"
	exit 1
fi
echo "✓ Shared base-dir contents intact"

# Verify accuser2 still exists
if ! instance_exists "$ACCUSER2_INSTANCE"; then
	echo "ERROR: Accuser2 removed from registry"
	exit 1
fi
echo "✓ Accuser2 still in registry"

# Now purge accuser2
echo "Purging accuser2..."
om instance "$ACCUSER2_INSTANCE" purge --force-purge 2>&1

# Verify accuser2 is removed
if instance_exists "$ACCUSER2_INSTANCE"; then
	echo "ERROR: Accuser2 still in registry after purge"
	exit 1
fi
echo "Accuser2 purged successfully"

# After both accusers are purged, the shared base-dir can be removed
# (either by the second purge, or left for manual cleanup)
# For this test, we accept either behavior as long as it wasn't deleted
# while accuser2 was still active.
if [ -d "$SHARED_BASE_DIR" ]; then
	echo "✓ Shared base-dir still exists (can be manually cleaned up)"
else
	echo "✓ Shared base-dir removed after last dependent was purged (acceptable)"
fi

echo "Test passed: Shared base-dir is not deleted while still in use by another accuser"
