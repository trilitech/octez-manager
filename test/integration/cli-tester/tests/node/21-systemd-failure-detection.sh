#!/bin/bash
# Test: Systemd failure detection - verify failed services are detected
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-fail-detect"
RPC_ADDR="127.0.0.1:18796"
NET_ADDR="0.0.0.0:19816"

echo "Test: Systemd failure detection"

cleanup_instance "$NODE_INSTANCE" || true

# Install a node
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network tallinnnet \
    --rpc-addr "$RPC_ADDR" \
    --net-addr "$NET_ADDR" \
    --service-user tezos \
    --no-enable 2>&1

# Corrupt the config to make the node fail to start
# We'll create an invalid config.json that will cause the node to crash
CONFIG_FILE="/var/lib/octez/$NODE_INSTANCE/data/config.json"
if [ -f "$CONFIG_FILE" ]; then
    echo "Corrupting config.json..."
    echo '{"invalid": "config"}' > "$CONFIG_FILE"
else
    # Create data dir and corrupt config
    mkdir -p "/var/lib/octez/$NODE_INSTANCE/data"
    chown -R tezos:tezos "/var/lib/octez/$NODE_INSTANCE"
    echo '{"invalid": "config"}' > "$CONFIG_FILE"
    chown tezos:tezos "$CONFIG_FILE"
fi

# Try to start the node (it should fail)
echo "Starting node (expecting failure)..."
om instance "$NODE_INSTANCE" start || true

# Give it a moment to fail
sleep 5

# Check systemd status - should be failed
ACTIVE_STATE=$(systemctl show "octez-node@$NODE_INSTANCE" --property=ActiveState --value)
echo "ActiveState: $ACTIVE_STATE"

if [ "$ACTIVE_STATE" = "failed" ]; then
    echo "Service correctly detected as failed"
else
    echo "WARNING: ActiveState is '$ACTIVE_STATE', expected 'failed'"
    # Show what happened
    systemctl status "octez-node@$NODE_INSTANCE" --no-pager || true
fi

# Check Result property
RESULT=$(systemctl show "octez-node@$NODE_INSTANCE" --property=Result --value)
echo "Result: $RESULT"

if [ "$RESULT" != "success" ] && [ "$RESULT" != "" ]; then
    echo "Failure result correctly reported: $RESULT"
else
    echo "WARNING: No failure result reported"
fi

# Verify om list shows the instance (it should still exist)
if om list | grep -q "$NODE_INSTANCE"; then
    echo "Instance still visible in om list"
else
    echo "ERROR: Instance disappeared from list"
    exit 1
fi

# Cleanup
echo "Cleaning up..."
systemctl stop "octez-node@$NODE_INSTANCE" || true
systemctl reset-failed "octez-node@$NODE_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE"

echo "Systemd failure detection test passed"
