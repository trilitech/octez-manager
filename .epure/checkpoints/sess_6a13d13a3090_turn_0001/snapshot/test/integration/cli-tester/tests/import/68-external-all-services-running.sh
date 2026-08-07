#!/bin/bash
# Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

# Test: Create 4 external Octez services and verify they're running
set -euo pipefail
source /tests/lib.sh

test_init "Create and verify 4 external Octez services (node, baker, accuser, dal-node)"

# Unique instance names for this test
NODE_INSTANCE="ext-node-68"
BAKER_INSTANCE="ext-baker-68"
ACCUSER_INSTANCE="ext-accuser-68"
DAL_INSTANCE="ext-dal-68"

# Allocate unique ports
NODE_RPC_PORT=$(alloc_port)
NODE_P2P_PORT=$(alloc_port)
DAL_RPC_PORT=$(alloc_port)

# Data directories
NODE_DATA="/var/lib/octez-external/$NODE_INSTANCE"
BAKER_DATA="/var/lib/octez-external/$BAKER_INSTANCE"
ACCUSER_DATA="/var/lib/octez-external/$ACCUSER_INSTANCE"
DAL_DATA="/var/lib/octez-external/$DAL_INSTANCE"

# Register for cleanup
register_external_service "node" "$NODE_INSTANCE"
register_external_service "baker" "$BAKER_INSTANCE"
register_external_service "accuser" "$ACCUSER_INSTANCE"
register_external_service "dal-node" "$DAL_INSTANCE"
register_data_dir "$NODE_DATA"
register_data_dir "$BAKER_DATA"
register_data_dir "$ACCUSER_DATA"
register_data_dir "$DAL_DATA"

NODE_RPC_ADDR="127.0.0.1:$NODE_RPC_PORT"
NODE_ENDPOINT="http://$NODE_RPC_ADDR"

echo "=== Creating External Octez Node ==="
echo "Creating node data directory at $NODE_DATA..."
mkdir -p "$NODE_DATA"
inject_identity "$NODE_INSTANCE" "$NODE_DATA"
chown -R tezos:tezos "$NODE_DATA"

echo "Creating external node service..."
create_external_service "node" "$NODE_INSTANCE" "$NODE_DATA" "$NODE_RPC_ADDR" "shadownet"

echo "=== Creating External Octez DAL Node ==="
echo "Creating DAL data directory at $DAL_DATA..."
mkdir -p "$DAL_DATA"
chown -R tezos:tezos "$DAL_DATA"

echo "Creating external DAL service..."
create_external_service "dal-node" "$DAL_INSTANCE" "$DAL_DATA" "" "shadownet" "$NODE_ENDPOINT" "127.0.0.1:$DAL_RPC_PORT" "$NODE_INSTANCE"

echo "=== Creating External Octez Baker ==="
echo "Creating baker base directory at $BAKER_DATA..."
mkdir -p "$BAKER_DATA"
chown -R tezos:tezos "$BAKER_DATA"

echo "Creating external baker service..."
create_external_service "baker" "$BAKER_INSTANCE" "$BAKER_DATA" "" "shadownet" "$NODE_ENDPOINT" "$BAKER_DATA" "$NODE_INSTANCE"

echo "=== Creating External Octez Accuser ==="
echo "Creating accuser base directory at $ACCUSER_DATA..."
mkdir -p "$ACCUSER_DATA"
chown -R tezos:tezos "$ACCUSER_DATA"

echo "Creating external accuser service..."
create_external_service "accuser" "$ACCUSER_INSTANCE" "" "" "shadownet" "$NODE_ENDPOINT" "$ACCUSER_DATA" "$NODE_INSTANCE"

echo "=== Starting All Services ==="
systemctl daemon-reload

echo "Starting node service..."
systemctl enable "octez-node@${NODE_INSTANCE}.service"
systemctl start "octez-node@${NODE_INSTANCE}.service"

echo "Waiting for node to be active..."
wait_for_service_active "node" "$NODE_INSTANCE" 30

echo "Waiting for node RPC to be ready..."
wait_for_node_ready "$NODE_RPC_ADDR" 60

echo "Starting DAL node service..."
systemctl enable "octez-dal-node@${DAL_INSTANCE}.service"
systemctl start "octez-dal-node@${DAL_INSTANCE}.service"

echo "Waiting for DAL node to be active..."
wait_for_service_active "dal-node" "$DAL_INSTANCE" 30

echo "Starting baker service..."
systemctl enable "octez-baker@${BAKER_INSTANCE}.service"
systemctl start "octez-baker@${BAKER_INSTANCE}.service"

echo "Waiting for baker to be active..."
wait_for_service_active "baker" "$BAKER_INSTANCE" 30

echo "Starting accuser service..."
systemctl enable "octez-accuser@${ACCUSER_INSTANCE}.service"
systemctl start "octez-accuser@${ACCUSER_INSTANCE}.service"

echo "Waiting for accuser to be active..."
wait_for_service_active "accuser" "$ACCUSER_INSTANCE" 30

echo "=== Verifying All Services Are Running ==="

if ! service_is_active "node" "$NODE_INSTANCE"; then
	echo "ERROR: Node service is not active"
	show_service_status "node" "$NODE_INSTANCE"
	exit 1
fi
echo "✓ Node service is active"

if ! service_is_active "dal-node" "$DAL_INSTANCE"; then
	echo "ERROR: DAL node service is not active"
	show_service_status "dal-node" "$DAL_INSTANCE"
	exit 1
fi
echo "✓ DAL node service is active"

if ! service_is_active "baker" "$BAKER_INSTANCE"; then
	echo "ERROR: Baker service is not active"
	show_service_status "baker" "$BAKER_INSTANCE"
	exit 1
fi
echo "✓ Baker service is active"

if ! service_is_active "accuser" "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser service is not active"
	show_service_status "accuser" "$ACCUSER_INSTANCE"
	exit 1
fi
echo "✓ Accuser service is active"

echo ""
echo "=== Checking Service Logs for Errors ==="

# Dump the actual systemd unit files to see what commands are being run
echo "Baker service file:"
systemctl cat "octez-baker@${BAKER_INSTANCE}.service" | grep "ExecStart"
echo ""
echo "Accuser service file:"
systemctl cat "octez-accuser@${ACCUSER_INSTANCE}.service" | grep "ExecStart"
echo ""

# Check baker logs for errors
echo "Checking baker logs..."
BAKER_LOGS=$(journalctl -u "octez-baker@${BAKER_INSTANCE}.service" --no-pager --since "1 minute ago" 2>&1 || true)
echo "Baker logs:"
echo "$BAKER_LOGS"
if echo "$BAKER_LOGS" | grep -qi "error\|unknown option\|invalid\|unrecognized\|failed to parse"; then
	echo "ERROR: Baker service has errors in logs"
	show_service_status "baker" "$BAKER_INSTANCE"
	exit 1
fi

# Check accuser logs for errors
echo "Checking accuser logs..."
ACCUSER_LOGS=$(journalctl -u "octez-accuser@${ACCUSER_INSTANCE}.service" --no-pager --since "1 minute ago" 2>&1 || true)
echo "Accuser logs:"
echo "$ACCUSER_LOGS"
if echo "$ACCUSER_LOGS" | grep -qi "error\|unknown option\|invalid\|unrecognized\|failed to parse"; then
	echo "ERROR: Accuser service has errors in logs"
	show_service_status "accuser" "$ACCUSER_INSTANCE"
	exit 1
fi

echo ""
echo "=== Waiting 5 seconds to ensure services stay running ==="
sleep 5

# Verify services are still active after delay (catches crash-looping)
if ! service_is_active "baker" "$BAKER_INSTANCE"; then
	echo "ERROR: Baker service crashed after startup"
	show_service_status "baker" "$BAKER_INSTANCE"
	show_service_logs "baker" "$BAKER_INSTANCE" 50
	exit 1
fi
echo "✓ Baker service still active after 5 seconds"

if ! service_is_active "accuser" "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser service crashed after startup"
	show_service_status "accuser" "$ACCUSER_INSTANCE"
	show_service_logs "accuser" "$ACCUSER_INSTANCE" 50
	exit 1
fi
echo "✓ Accuser service still active after 5 seconds"

echo "=== Verifying Node RPC Endpoint ==="
if ! curl -sf "http://${NODE_RPC_ADDR}/chains/main/blocks/head/header" >/dev/null 2>&1; then
	echo "ERROR: Node RPC endpoint is not responding"
	show_service_logs "node" "$NODE_INSTANCE" 50
	exit 1
fi
echo "✓ Node RPC endpoint is responding"

echo ""
echo "=== Test Summary ==="
echo "✓ All 4 external services created successfully:"
echo "  - octez-node@${NODE_INSTANCE} (RPC: $NODE_RPC_ADDR)"
echo "  - octez-dal-node@${DAL_INSTANCE} (RPC: 127.0.0.1:$DAL_RPC_PORT)"
echo "  - octez-baker@${BAKER_INSTANCE}"
echo "  - octez-accuser@${ACCUSER_INSTANCE}"
echo ""
echo "Test passed: All external services running and ready"
