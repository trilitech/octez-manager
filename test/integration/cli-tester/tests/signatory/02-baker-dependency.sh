#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Baker with Signatory Dependency"

# Initialize test harness (automatic cleanup on EXIT)
test_init

# Allocate unique ports for this test
SIGNATORY_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)
NODE_NET_PORT=$(alloc_port)

SIGNATORY_INSTANCE="test-signatory-baker-dep"
NODE_INSTANCE="test-node-baker-dep"
BAKER_INSTANCE="test-baker-with-signatory"

# Register instances for cleanup (pre-cleanup removes any leftovers from previous runs)
register_instance "$SIGNATORY_INSTANCE"
register_instance "$NODE_INSTANCE"
register_instance "$BAKER_INSTANCE"

echo "==> Step 1: Install signatory instance"
om install-signatory \
	--instance "$SIGNATORY_INSTANCE" \
	--backend file \
	--address "127.0.0.1:$SIGNATORY_PORT" \
	--authorized-keys "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb,tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m" \
	--keys-dir "/var/lib/octez/signatory/keys" \
	--app-bin-dir /usr/local/bin \
	--service-user tezos \
	--no-enable 2>&1

echo "==> Step 2: Verify signatory appears in om list"
if ! om list 2>&1 | grep -q "$SIGNATORY_INSTANCE"; then
	echo "ERROR: Signatory instance '$SIGNATORY_INSTANCE' not found in om list"
	echo "Full om list output:"
	om list 2>&1
	exit 1
fi
echo "Signatory instance appears in om list"

echo "==> Step 3: Verify signatory service unit exists"
if ! systemctl list-unit-files | grep -q "signatory@.service"; then
	echo "ERROR: Signatory service template not found: signatory@.service"
	echo "Full systemctl output:"
	systemctl list-unit-files | head -50
	exit 1
fi

echo "==> Step 4: Install node instance"
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:$NODE_RPC_PORT" \
	--net-addr "127.0.0.1:$NODE_NET_PORT" \
	--service-user tezos \
	--no-enable 2>&1

echo "==> Step 5: Install baker with signatory dependency"
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--remote-signer-instance "$SIGNATORY_INSTANCE" \
	--delegate "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

echo "==> Step 6: Verify baker installation created drop-in directory"
BAKER_UNIT="octez-baker@${BAKER_INSTANCE}.service"
DROPIN_DIR="/etc/systemd/system/${BAKER_UNIT}.d"
if [ ! -d "$DROPIN_DIR" ]; then
	echo "ERROR: Drop-in directory not found: $DROPIN_DIR"
	echo "Baker installation may have failed to create systemd configuration"
	exit 1
fi

echo "==> Step 7: Verify systemd drop-in references correct signatory unit"
DROPIN_DIR="/etc/systemd/system/${BAKER_UNIT}.d"
if [ ! -d "$DROPIN_DIR" ]; then
	echo "ERROR: Drop-in directory not found: $DROPIN_DIR"
	exit 1
fi

DROPIN_FILE="${DROPIN_DIR}/override.conf"
if [ ! -f "$DROPIN_FILE" ]; then
	echo "ERROR: Drop-in file not found: $DROPIN_FILE"
	exit 1
fi

echo "==> Step 8: Check drop-in content for correct signatory unit name"
# The fix in src/systemd_dropin.ml ensures signatory uses "signatory@instance.service"
# NOT "octez-signatory@instance.service"
if ! grep -q "signatory@${SIGNATORY_INSTANCE}.service" "$DROPIN_FILE"; then
	echo "ERROR: Drop-in does not reference signatory@${SIGNATORY_INSTANCE}.service"
	echo "Drop-in content:"
	cat "$DROPIN_FILE"
	exit 1
fi

# Ensure the incorrect name is NOT present
if grep -q "octez-signatory@${SIGNATORY_INSTANCE}.service" "$DROPIN_FILE"; then
	echo "ERROR: Drop-in incorrectly references octez-signatory@${SIGNATORY_INSTANCE}.service"
	echo "This is the bug that was fixed - signatory should NOT have 'octez-' prefix"
	echo "Drop-in content:"
	cat "$DROPIN_FILE"
	exit 1
fi

echo "==> Step 9: Reload systemd to pick up new units"
systemctl daemon-reload

echo "==> Step 10: Verify systemd can resolve the dependency chain"
# This command would have failed with "Unit octez-signatory@X.service not found"
# before the fix in src/systemd_dropin.ml:53
if ! systemctl list-dependencies "$BAKER_UNIT" 2>&1 | grep -q "signatory@${SIGNATORY_INSTANCE}.service"; then
	echo "ERROR: systemd cannot resolve signatory dependency"
	systemctl list-dependencies "$BAKER_UNIT" || true
	exit 1
fi

echo "==> Step 11: Verify no dependency resolution errors"
# Try to start the baker (should fail gracefully since node isn't running, but dependency should resolve)
if systemctl start "$BAKER_UNIT" 2>&1 | grep -q "not found"; then
	echo "ERROR: systemd reports 'not found' when starting baker"
	systemctl status "$BAKER_UNIT" || true
	exit 1
fi

# Stop the baker (it's expected to fail since node isn't actually running)
systemctl stop "$BAKER_UNIT" 2>/dev/null || true

echo "Test passed: Baker correctly depends on signatory@${SIGNATORY_INSTANCE}.service"
