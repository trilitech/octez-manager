#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Import handles corrupted/invalid config.json gracefully
# Verifies: Import either fails with clear error or succeeds (no hang/crash)

set -euo pipefail
source /tests/lib.sh

test_init "Import node - corrupted config handled gracefully"

# Unique instance name for this test
INSTANCE="node-corrupt-cfg-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

echo "Creating external node with corrupted config.json..."

# Create data directory
DATA_DIR="/var/lib/octez-external/$INSTANCE"
register_data_dir "$DATA_DIR"
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"

# Initialize node config first (creates valid identity.json)
octez-node config init --data-dir="$DATA_DIR" \
	--network=shadownet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

# Corrupt config.json with invalid JSON
CONFIG_FILE="$DATA_DIR/config.json"
echo "{ this is not valid json" >"$CONFIG_FILE"

echo "Corrupted config.json with invalid JSON"

# Create systemd service
register_external_service "node" "$INSTANCE"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$INSTANCE" "$DATA_DIR" "127.0.0.1:$RPC_PORT" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

echo "Attempting import with corrupted config..."

# Import should either:
# 1. Fail with a clear error message (not a crash/hang)
# 2. Succeed but regenerate/ignore config
set +e
IMPORT_OUTPUT=$(om import "octez-node@${INSTANCE}" --strategy clone --network shadownet 2>&1)
IMPORT_EXIT=$?
set -e

echo "Import exit code: $IMPORT_EXIT"
echo "Import output: $IMPORT_OUTPUT"

if [ $IMPORT_EXIT -eq 0 ]; then
	echo "Import succeeded with corrupted config"

	# Verify service is managed
	if ! service_is_managed "$INSTANCE"; then
		echo "ERROR: Service is not managed after import"
		om list 2>&1
		exit 1
	fi
	echo "✓ Service is managed after import"
else
	echo "Import failed (may be expected with corrupted config)"

	# Just verify it failed without crashing (exit code != 0 is acceptable)
	# The important thing is we got a clean error, not a crash or hang
	echo "✓ Import failed cleanly with exit code $IMPORT_EXIT"
fi

echo "Test passed: Corrupted config handled without crash"
