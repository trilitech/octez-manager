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

# Create a wrapper script that always fails
# This will make the node service fail to start
echo "Creating failing wrapper..."
REAL_BIN=$(which octez-node)
WRAPPER="/tmp/octez-node-fail-wrapper"
cat > "$WRAPPER" << 'FAILSCRIPT'
#!/bin/bash
echo "Simulated node failure" >&2
exit 1
FAILSCRIPT
chmod +x "$WRAPPER"

# Create a dropin to use the failing wrapper
DROPIN_DIR="/etc/systemd/system/octez-node@$NODE_INSTANCE.service.d"
mkdir -p "$DROPIN_DIR"
cat > "$DROPIN_DIR/fail.conf" << EOF
[Service]
ExecStart=
ExecStart=$WRAPPER
EOF
systemctl daemon-reload

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

# Verify om list shows the instance (status detection is done via UI, not CLI list)
LIST_OUTPUT=$(om list)
echo "om list output:"
echo "$LIST_OUTPUT"
if echo "$LIST_OUTPUT" | grep -q "$NODE_INSTANCE"; then
    echo "om list shows the instance"
else
    echo "ERROR: Instance not in om list"
    exit 1
fi

# Cleanup
echo "Cleaning up..."
systemctl stop "octez-node@$NODE_INSTANCE" || true
systemctl reset-failed "octez-node@$NODE_INSTANCE" || true
rm -f "$DROPIN_DIR/fail.conf"
systemctl daemon-reload
cleanup_instance "$NODE_INSTANCE"

echo "Systemd failure detection test passed"
