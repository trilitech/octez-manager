#!/bin/bash
# Test: Accuser installation with extra arguments
# Tests that extra args are split into global (before subcommand) and command (after subcommand)
# and that the accuser actually starts successfully with these args properly positioned.
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-accuser-extra-node"
ACCUSER_INSTANCE="test-accuser-extra"
NODE_RPC="127.0.0.1:18772"
NODE_NET="0.0.0.0:19792"

echo "Test: Accuser installation with extra arguments"

cleanup_instance "$ACCUSER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install node with snapshot (needed to actually start the accuser)
echo "Installing node with snapshot..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --snapshot \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Inject pre-generated identity to skip PoW
inject_identity "$NODE_INSTANCE"

# Start the node
echo "Starting node..."
om instance "$NODE_INSTANCE" start

if ! wait_for_service_active "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node service did not start"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi

if ! wait_for_node_ready "$NODE_RPC" 60; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi
echo "Node is ready"

# Create a dummy password file for testing -f option
PASSWORD_FILE="/tmp/test-accuser-password.txt"
echo "test-password" > "$PASSWORD_FILE"
chmod 644 "$PASSWORD_FILE"

# Install accuser with global args (-f) and command args (--preserved-levels)
# Global args must appear BEFORE 'run accuser', command args after.
echo "Installing accuser with global and command extra args..."
om install-accuser \
    --instance "$ACCUSER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --extra-arg="-f" \
    --extra-arg="$PASSWORD_FILE" \
    --extra-arg="--preserved-levels" \
    --extra-arg="10" \
    --service-user tezos \
    --no-enable 2>&1

# Verify env file contains extra args in the right variables
ENV_FILE="/etc/octez/instances/$ACCUSER_INSTANCE/node.env"

# -f and its value should be in GLOBAL_ARGS
if ! grep -q "OCTEZ_BAKER_GLOBAL_ARGS=.*-f.*$PASSWORD_FILE" "$ENV_FILE"; then
    echo "ERROR: Global arg -f not in OCTEZ_BAKER_GLOBAL_ARGS"
    echo "Contents of $ENV_FILE:"
    cat "$ENV_FILE"
    exit 1
fi
echo "Global args (-f) configured correctly in env file"

# --preserved-levels should be in COMMAND_ARGS
if ! grep -q "OCTEZ_BAKER_COMMAND_ARGS=.*--preserved-levels" "$ENV_FILE"; then
    echo "ERROR: Command arg --preserved-levels not in OCTEZ_BAKER_COMMAND_ARGS"
    echo "Contents of $ENV_FILE:"
    cat "$ENV_FILE"
    exit 1
fi
echo "Command args (--preserved-levels) configured correctly in env file"

# THE REAL TEST: Start the accuser and verify it doesn't crash
# If global args like -f are placed after 'run accuser', octez-baker will fail
echo "Starting accuser (will fail if args are misplaced)..."
om instance "$ACCUSER_INSTANCE" start

if ! wait_for_service_active "accuser" "$ACCUSER_INSTANCE" 30; then
    echo "ERROR: Accuser service did not start - args may be misplaced!"
    show_service_logs "accuser" "$ACCUSER_INSTANCE" 100
    exit 1
fi
echo "Accuser service started"

# Give it a moment to initialize and potentially crash
sleep 5

# Check accuser is still running
if ! service_is_active "accuser" "$ACCUSER_INSTANCE"; then
    echo "ERROR: Accuser service crashed after startup - check arg positioning"
    show_service_logs "accuser" "$ACCUSER_INSTANCE" 100
    exit 1
fi
echo "Accuser service is stable with extra args"

# Stop services
echo "Stopping accuser..."
om instance "$ACCUSER_INSTANCE" stop
wait_for_service_stopped "accuser" "$ACCUSER_INSTANCE" 30

echo "Stopping node..."
om instance "$NODE_INSTANCE" stop
wait_for_service_stopped "node" "$NODE_INSTANCE" 30

# Cleanup
cleanup_instance "$ACCUSER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"
rm -f "$PASSWORD_FILE"

echo "Accuser extra args test passed - global and command args correctly positioned"
