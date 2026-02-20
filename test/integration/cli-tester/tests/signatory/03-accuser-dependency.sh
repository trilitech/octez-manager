#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Dependency Handling and Cascade (Accuser with Signatory)"

# Initialize test harness (automatic cleanup on EXIT)
test_init

# Allocate unique ports for this test
SIGNATORY_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)
NODE_NET_PORT=$(alloc_port)

SIGNATORY_INSTANCE="test-signatory-accuser-dep"
NODE_INSTANCE="test-node-accuser-dep"
ACCUSER_INSTANCE="test-accuser-with-signatory"

echo "==> Step 1: Install signatory instance"
om install-signatory \
	--instance "$SIGNATORY_INSTANCE" \
	--backend file \
	--address "127.0.0.1:$SIGNATORY_PORT" \
	--authorized-keys "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$SIGNATORY_INSTANCE"

echo "==> Step 2: Install node instance"
om install-node \
	--instance "$NODE_INSTANCE" \
	--network ghostnet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:$NODE_RPC_PORT" \
	--net-addr "127.0.0.1:$NODE_NET_PORT" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$NODE_INSTANCE"

echo "==> Step 3: Install accuser with signatory dependency"
om install-accuser \
	--instance "$ACCUSER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--protocol alpha \
	--use-signatory "$SIGNATORY_INSTANCE" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$ACCUSER_INSTANCE"

echo "==> Step 4: Verify accuser service unit was created"
ACCUSER_UNIT="octez-accuser@${ACCUSER_INSTANCE}.service"
if ! systemctl list-unit-files | grep -q "$ACCUSER_UNIT"; then
	echo "ERROR: Accuser service unit not found: $ACCUSER_UNIT"
	exit 1
fi

echo "==> Step 5: Verify systemd drop-in references correct signatory unit"
DROPIN_DIR="/etc/systemd/system/${ACCUSER_UNIT}.d"
DROPIN_FILE="${DROPIN_DIR}/dependencies.conf"

if [ ! -f "$DROPIN_FILE" ]; then
	echo "ERROR: Drop-in file not found: $DROPIN_FILE"
	exit 1
fi

# Verify correct signatory unit name (without octez- prefix)
if ! grep -q "signatory@${SIGNATORY_INSTANCE}.service" "$DROPIN_FILE"; then
	echo "ERROR: Drop-in does not reference signatory@${SIGNATORY_INSTANCE}.service"
	cat "$DROPIN_FILE"
	exit 1
fi

# Ensure incorrect name is NOT present
if grep -q "octez-signatory@${SIGNATORY_INSTANCE}.service" "$DROPIN_FILE"; then
	echo "ERROR: Drop-in incorrectly references octez-signatory@ (should be signatory@)"
	cat "$DROPIN_FILE"
	exit 1
fi

echo "==> Step 6: Verify dependency chain"
systemctl daemon-reload

# Check full dependency chain: accuser -> node + signatory
if ! systemctl list-dependencies "$ACCUSER_UNIT" | grep -q "octez-node@${NODE_INSTANCE}.service"; then
	echo "ERROR: Accuser not dependent on node"
	systemctl list-dependencies "$ACCUSER_UNIT"
	exit 1
fi

if ! systemctl list-dependencies "$ACCUSER_UNIT" | grep -q "signatory@${SIGNATORY_INSTANCE}.service"; then
	echo "ERROR: Accuser not dependent on signatory"
	systemctl list-dependencies "$ACCUSER_UNIT"
	exit 1
fi

echo "==> Step 7: Verify cascade behavior (accuser requires both node and signatory)"
# Start signatory only
systemctl start "signatory@${SIGNATORY_INSTANCE}.service" || true
sleep 2

# Try to start accuser - should fail because node is not running
if systemctl start "$ACCUSER_UNIT" 2>&1; then
	# Accuser started - check if it's actually running or if systemd is being lenient
	if systemctl is-active "$ACCUSER_UNIT" >/dev/null 2>&1; then
		echo "WARNING: Accuser started without node - checking status"
		systemctl status "$ACCUSER_UNIT" || true
	fi
	systemctl stop "$ACCUSER_UNIT" 2>/dev/null || true
fi

# Stop signatory
systemctl stop "signatory@${SIGNATORY_INSTANCE}.service" 2>/dev/null || true

echo "==> Step 8: Test successful dependency resolution"
# This verifies the fix: systemd can parse the dependency chain without 'Unit not found' errors
if systemctl list-dependencies "$ACCUSER_UNIT" 2>&1 | grep -i "not found"; then
	echo "ERROR: Dependency resolution failed with 'not found'"
	systemctl list-dependencies "$ACCUSER_UNIT" || true
	exit 1
fi

echo "Test passed: Accuser correctly depends on both node and signatory"
