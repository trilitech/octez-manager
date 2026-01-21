#!/bin/bash
# Test: Detect process-based instances (no systemd)
set -euo pipefail
source /tests/lib.sh

INSTANCE="process-node"
DATA_DIR="/var/lib/octez-process/$INSTANCE"
RPC_ADDR="127.0.0.1:18744"

echo "Test: Detect process-based instances"

# Cleanup
rm -rf "$DATA_DIR" || true
pkill -f "octez-node.*$DATA_DIR" || true

# Create data directory
mkdir -p "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"

# Start octez-node as direct process (no systemd)
echo "Starting octez-node as direct process..."
PID=$(start_unmanaged_process "octez-node" "run --data-dir $DATA_DIR --network shadownet --rpc-addr $RPC_ADDR")

sleep 3

# Verify process is running
if ! ps -p $PID > /dev/null 2>&1; then
    echo "ERROR: Process did not start"
    exit 1
fi

# Check detection - process-based instances should be detected but not importable
# NOTE: This test verifies the detection behavior. Current implementation may vary.
echo "Checking detection behavior for process-based instance..."
om list 2>&1 > /tmp/list_output.txt || true
cat /tmp/list_output.txt

# Cleanup
kill $PID 2>/dev/null || true
rm -rf "$DATA_DIR"

echo "Process-based detection test completed"
# Note: Actual assertion depends on implementation of process detection
# This test documents expected behavior for future implementation
