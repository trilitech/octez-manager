#!/bin/bash
# Test: Export logs command
# Verifies that export-logs creates an archive with expected contents
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-export-logs"
NODE_RPC="127.0.0.1:18796"
NODE_NET="0.0.0.0:19796"

echo "Test: Export logs command"

# Cleanup any previous test instance
cleanup_instance "$NODE_INSTANCE" || true

# Install a node
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --history-mode rolling \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Start the node briefly to generate some logs
echo "Starting node to generate logs..."
om instance "$NODE_INSTANCE" start 2>&1
sleep 5

# Export logs
echo "Exporting logs..."
EXPORT_OUTPUT=$(om instance "$NODE_INSTANCE" export-logs 2>&1)
echo "$EXPORT_OUTPUT"

# Extract archive path from output
ARCHIVE_PATH=$(echo "$EXPORT_OUTPUT" | grep -oP 'Logs exported to: \K.*')
if [ -z "$ARCHIVE_PATH" ]; then
    echo "ERROR: Could not find archive path in output"
    exit 1
fi
echo "Archive created at: $ARCHIVE_PATH"

# Verify archive exists
if [ ! -f "$ARCHIVE_PATH" ]; then
    echo "ERROR: Archive file not found"
    exit 1
fi
echo "Archive file exists"

# Verify archive contains expected files
echo "Checking archive contents..."
CONTENTS=$(tar -tzf "$ARCHIVE_PATH")
echo "Archive contents:"
echo "$CONTENTS"

# Check for instance-details.txt
if ! echo "$CONTENTS" | grep -q "instance-details.txt"; then
    echo "ERROR: instance-details.txt not found in archive"
    exit 1
fi
echo "instance-details.txt found"

# Check for service.json
if ! echo "$CONTENTS" | grep -q "service.json"; then
    echo "ERROR: service.json not found in archive"
    exit 1
fi
echo "service.json found"

# Check for journald.log
if ! echo "$CONTENTS" | grep -q "journald.log"; then
    echo "ERROR: journald.log not found in archive"
    exit 1
fi
echo "journald.log found"

# Extract and verify instance-details.txt content
EXTRACT_DIR=$(mktemp -d)
tar -xzf "$ARCHIVE_PATH" -C "$EXTRACT_DIR"
DETAILS_FILE=$(find "$EXTRACT_DIR" -name "instance-details.txt")
if [ -z "$DETAILS_FILE" ]; then
    echo "ERROR: Could not find instance-details.txt after extraction"
    rm -rf "$EXTRACT_DIR"
    exit 1
fi

echo "Verifying instance-details.txt content..."
if ! grep -q "Instance.*:.*$NODE_INSTANCE" "$DETAILS_FILE"; then
    echo "ERROR: Instance name not found in details"
    cat "$DETAILS_FILE"
    rm -rf "$EXTRACT_DIR"
    exit 1
fi
echo "Instance name found in details"

if ! grep -q "Role.*:.*node" "$DETAILS_FILE"; then
    echo "ERROR: Role not found in details"
    cat "$DETAILS_FILE"
    rm -rf "$EXTRACT_DIR"
    exit 1
fi
echo "Role found in details"

# Cleanup
rm -rf "$EXTRACT_DIR"
rm -f "$ARCHIVE_PATH"
om instance "$NODE_INSTANCE" stop 2>&1 || true
cleanup_instance "$NODE_INSTANCE"

echo "Export logs test passed"
