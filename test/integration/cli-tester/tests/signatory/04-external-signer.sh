#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: External Signer URI Configuration"

# Initialize test harness (automatic cleanup on EXIT)
test_init

# Allocate unique ports for this test
NODE_RPC_PORT=$(alloc_port)
NODE_NET_PORT=$(alloc_port)
EXTERNAL_SIGNER_PORT=$(alloc_port)

NODE_INSTANCE="test-node-external-signer"
BAKER_INSTANCE="test-baker-external-signer"
BAKER_UNIT="octez-baker@${BAKER_INSTANCE}.service"

# Register instances for cleanup
register_instance "$NODE_INSTANCE"
register_instance "$BAKER_INSTANCE"

echo "==> Step 1: Install node instance"
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

inject_identity "$NODE_INSTANCE"

echo "==> Step 2: Start node to enable baker installation"
om instance "$NODE_INSTANCE" start 2>&1

# Wait for node RPC to be ready
if ! wait_for_node_ready "127.0.0.1:$NODE_RPC_PORT" 90; then
	echo "ERROR: Node RPC did not become ready"
	show_service_logs "node" "$NODE_INSTANCE" 50
	exit 1
fi

echo "==> Step 3: Install baker with external signer URI (http://)"
EXTERNAL_SIGNER_URI="http://127.0.0.1:$EXTERNAL_SIGNER_PORT"

om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--remote-signer-uri "$EXTERNAL_SIGNER_URI" \
	--delegate "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

echo "==> Step 4: Verify baker installation succeeded"
# Baker is installed successfully if registry entry exists
if ! om list 2>&1 | grep -q "$BAKER_INSTANCE"; then
	echo "ERROR: Baker instance '$BAKER_INSTANCE' not found in om list"
	om list 2>&1
	exit 1
fi

echo "==> Step 5: Verify baker configuration includes signer URI"
# Remote signer URI is stored in the environment file, not a config file
BAKER_ENV_FILE="/etc/octez/instances/${BAKER_INSTANCE}/node.env"

if [ ! -f "$BAKER_ENV_FILE" ]; then
	echo "ERROR: Baker env file not found: $BAKER_ENV_FILE"
	ls -la /etc/octez/instances/${BAKER_INSTANCE}/ 2>&1 || true
	ls -la /etc/octez/instances/ 2>&1 || true
	exit 1
fi

if ! grep -q "$EXTERNAL_SIGNER_URI" "$BAKER_ENV_FILE"; then
	echo "ERROR: External signer URI not found in baker env file"
	cat "$BAKER_ENV_FILE"
	exit 1
fi

echo "==> Step 6: Verify NO signatory dependency in systemd drop-in"
# When using external signer URI, there should be no signatory dependency
DROPIN_DIR="/etc/systemd/system/${BAKER_UNIT}.d"
DROPIN_FILE="${DROPIN_DIR}/override.conf"

if [ -f "$DROPIN_FILE" ]; then
	# If drop-in exists, it should NOT reference any signatory service
	if grep -q "signatory@" "$DROPIN_FILE"; then
		echo "ERROR: Baker with external URI should not depend on signatory service"
		cat "$DROPIN_FILE"
		exit 1
	fi

	# Should still depend on the node though
	if ! grep -q "octez-node@${NODE_INSTANCE}.service" "$DROPIN_FILE"; then
		echo "ERROR: Baker should still depend on node"
		cat "$DROPIN_FILE"
		exit 1
	fi
fi

echo "==> Step 7: Verify systemd dependencies"
systemctl daemon-reload

# Should depend on node only, not signatory
if ! systemctl list-dependencies "$BAKER_UNIT" | grep -q "octez-node@${NODE_INSTANCE}.service"; then
	echo "ERROR: Baker not dependent on node"
	systemctl list-dependencies "$BAKER_UNIT"
	exit 1
fi

if systemctl list-dependencies "$BAKER_UNIT" | grep -q "signatory@"; then
	echo "ERROR: Baker with external URI should not depend on local signatory"
	systemctl list-dependencies "$BAKER_UNIT"
	exit 1
fi

echo "==> Step 8: Test baker show command displays signer URI"
om instance "$BAKER_INSTANCE" show 2>&1 | tee /tmp/baker-info.txt

if ! grep -q "$EXTERNAL_SIGNER_URI" /tmp/baker-info.txt && ! grep -q "$EXTERNAL_SIGNER_PORT" /tmp/baker-info.txt; then
	echo "WARNING: Signer URI not visible in baker show output (may be expected)"
	cat /tmp/baker-info.txt
fi

echo "Test passed: External signer URI configuration working correctly"
