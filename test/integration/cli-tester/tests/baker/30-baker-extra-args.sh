#!/bin/bash
# Test: Baker installation with extra arguments
# Tests that extra args are split into global (before subcommand) and command (after subcommand)
# and that the baker actually starts successfully with these args properly positioned.
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-extra-node"
BAKER_INSTANCE="test-baker-extra"
NODE_RPC="127.0.0.1:18764"
NODE_NET="0.0.0.0:19784"

echo "Test: Baker installation with extra arguments"

cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install node with snapshot (needed to actually start the baker)
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

# Wait for node to be ready
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
PASSWORD_FILE="/tmp/test-password.txt"
echo "test-password" > "$PASSWORD_FILE"
chmod 644 "$PASSWORD_FILE"

# Install baker with global args (-f for password file) and command args (--keep-alive)
# Global args must appear BEFORE the run subcommand, command args after.
# If args are misplaced, the baker will fail to start with a CLI parsing error.
echo "Installing baker with global and command extra args..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --dal-endpoint none \
    --extra-arg="-f" \
    --extra-arg="$PASSWORD_FILE" \
    --extra-arg="--keep-alive" \
    --service-user tezos \
    --no-enable 2>&1

# Verify env file contains extra args in the right variables
ENV_FILE="/etc/octez/instances/$BAKER_INSTANCE/node.env"

# -f and its value should be in GLOBAL_ARGS (appears before subcommand)
if ! grep -q "OCTEZ_BAKER_GLOBAL_ARGS=.*-f.*$PASSWORD_FILE" "$ENV_FILE"; then
    echo "ERROR: Global arg -f not in OCTEZ_BAKER_GLOBAL_ARGS"
    echo "Contents of $ENV_FILE:"
    cat "$ENV_FILE"
    exit 1
fi
echo "Global args (-f) configured correctly in env file"

# --keep-alive should be in COMMAND_ARGS (appears after subcommand)
if ! grep -q "OCTEZ_BAKER_COMMAND_ARGS=.*--keep-alive" "$ENV_FILE"; then
    echo "ERROR: Command arg --keep-alive not in OCTEZ_BAKER_COMMAND_ARGS"
    echo "Contents of $ENV_FILE:"
    cat "$ENV_FILE"
    exit 1
fi
echo "Command args (--keep-alive) configured correctly in env file"

# THE REAL TEST: Start the baker and verify it doesn't crash
# If global args like -f are placed after the subcommand, octez-baker will fail
# with an error like "unknown option `-f'"
echo "Starting baker (will fail if args are misplaced)..."
om instance "$BAKER_INSTANCE" start

if ! wait_for_service_active "baker" "$BAKER_INSTANCE" 30; then
    echo "ERROR: Baker service did not start - args may be misplaced!"
    show_service_logs "baker" "$BAKER_INSTANCE" 100
    exit 1
fi
echo "Baker service started"

# Give it a moment to initialize and potentially crash
sleep 5

# Check baker is still running (didn't crash due to CLI parsing errors)
if ! service_is_active "baker" "$BAKER_INSTANCE"; then
    echo "ERROR: Baker service crashed after startup - check arg positioning"
    show_service_logs "baker" "$BAKER_INSTANCE" 100
    exit 1
fi
echo "Baker service is stable with extra args"

# Stop services
echo "Stopping baker..."
om instance "$BAKER_INSTANCE" stop
wait_for_service_stopped "baker" "$BAKER_INSTANCE" 30

echo "Stopping node..."
om instance "$NODE_INSTANCE" stop
wait_for_service_stopped "node" "$NODE_INSTANCE" 30

# Cleanup
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"
rm -f "$PASSWORD_FILE"

echo "Baker extra args test passed - global and command args correctly positioned"
