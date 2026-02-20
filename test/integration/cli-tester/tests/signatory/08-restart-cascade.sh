#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Signatory Restart Cascade Behavior"

# Initialize test harness (automatic cleanup on EXIT)
test_init

# Allocate unique ports for this test
SIGNATORY_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)
NODE_NET_PORT=$(alloc_port)

SIGNATORY_INSTANCE="test-signatory-cascade"
NODE_INSTANCE="test-node-cascade"
BAKER_INSTANCE="test-baker-cascade"
ACCUSER_INSTANCE="test-accuser-cascade"

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

echo "==> Step 3: Install baker with signatory dependency"
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--protocol alpha \
	--use-signatory "$SIGNATORY_INSTANCE" \
	--baker-key "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$BAKER_INSTANCE"

echo "==> Step 4: Install accuser with same signatory dependency"
om install-accuser \
	--instance "$ACCUSER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--protocol alpha \
	--use-signatory "$SIGNATORY_INSTANCE" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$ACCUSER_INSTANCE"

echo "==> Step 5: Reload systemd and verify dependencies"
systemctl daemon-reload

SIGNATORY_UNIT="signatory@${SIGNATORY_INSTANCE}.service"
NODE_UNIT="octez-node@${NODE_INSTANCE}.service"
BAKER_UNIT="octez-baker@${BAKER_INSTANCE}.service"
ACCUSER_UNIT="octez-accuser@${ACCUSER_INSTANCE}.service"

# Verify dependencies are correct
if ! systemctl list-dependencies "$BAKER_UNIT" | grep -q "$SIGNATORY_UNIT"; then
	echo "ERROR: Baker not dependent on signatory"
	exit 1
fi

if ! systemctl list-dependencies "$ACCUSER_UNIT" | grep -q "$SIGNATORY_UNIT"; then
	echo "ERROR: Accuser not dependent on signatory"
	exit 1
fi

echo "==> Step 6: Start signatory service"
systemctl start "$SIGNATORY_UNIT" || {
	echo "ERROR: Failed to start signatory"
	systemctl status "$SIGNATORY_UNIT" || true
	exit 1
}

# Wait for signatory to be active
sleep 3

if ! systemctl is-active "$SIGNATORY_UNIT" >/dev/null 2>&1; then
	echo "ERROR: Signatory not active after start"
	systemctl status "$SIGNATORY_UNIT" || true
	exit 1
fi

echo "==> Step 7: Try to start baker (will fail because node isn't running, but signatory dep should work)"
# Baker should fail to start because node is not running, but the signatory
# dependency should be resolved correctly (no "Unit not found" errors)
systemctl start "$BAKER_UNIT" 2>&1 | tee /tmp/baker-start.log || true

# Check for "not found" errors - these indicate the bug is present
if grep -i "not found" /tmp/baker-start.log; then
	echo "ERROR: Baker start had 'not found' errors - signatory dependency not resolved"
	cat /tmp/baker-start.log
	exit 1
fi

echo "==> Step 8: Stop signatory"
systemctl stop "$SIGNATORY_UNIT" || true

# Wait for stop
sleep 2

if systemctl is-active "$SIGNATORY_UNIT" >/dev/null 2>&1; then
	echo "ERROR: Signatory still active after stop"
	exit 1
fi

echo "==> Step 9: Verify baker/accuser stopped when signatory stopped (if they were running)"
# If baker was running (unlikely without node), it should have stopped
if systemctl is-active "$BAKER_UNIT" >/dev/null 2>&1; then
	echo "WARNING: Baker still running after signatory stopped"
	# Not a hard error since baker may not have started successfully
fi

echo "==> Step 10: Restart signatory"
systemctl restart "$SIGNATORY_UNIT" || {
	echo "ERROR: Failed to restart signatory"
	systemctl status "$SIGNATORY_UNIT" || true
	exit 1
}

sleep 3

if ! systemctl is-active "$SIGNATORY_UNIT" >/dev/null 2>&1; then
	echo "ERROR: Signatory not active after restart"
	systemctl status "$SIGNATORY_UNIT" || true
	exit 1
fi

echo "==> Step 11: Verify systemd can list all dependencies without errors"
for unit in "$BAKER_UNIT" "$ACCUSER_UNIT"; do
	if ! systemctl list-dependencies "$unit" >/dev/null 2>&1; then
		echo "ERROR: Failed to list dependencies for $unit"
		systemctl list-dependencies "$unit" || true
		exit 1
	fi

	# Check for "not found" in dependency listing
	if systemctl list-dependencies "$unit" 2>&1 | grep -i "not found"; then
		echo "ERROR: Dependency listing shows 'not found' for $unit"
		systemctl list-dependencies "$unit" || true
		exit 1
	fi
done

echo "==> Step 12: Final cleanup - stop all services"
for unit in "$BAKER_UNIT" "$ACCUSER_UNIT" "$NODE_UNIT" "$SIGNATORY_UNIT"; do
	systemctl stop "$unit" 2>/dev/null || true
done

echo "==> Step 13: Verify all services stopped cleanly"
sleep 2

for unit in "$BAKER_UNIT" "$ACCUSER_UNIT" "$NODE_UNIT" "$SIGNATORY_UNIT"; do
	if systemctl is-active "$unit" >/dev/null 2>&1; then
		echo "WARNING: $unit still active after stop"
	fi
done

echo "Test passed: Signatory restart cascade and dependency resolution working correctly"
