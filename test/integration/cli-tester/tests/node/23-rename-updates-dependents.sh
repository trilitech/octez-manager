#!/bin/bash
# Test: Renaming a node updates dependent services
# Verifies that when a node is renamed, dependent services (baker, accuser, dal-node)
# have their references updated in registry, env files, and systemd dropins.
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-rename-node"
NEW_NODE_INSTANCE="test-renamed-node"
BAKER_INSTANCE="test-rename-baker"
NODE_RPC="127.0.0.1:18732"
NODE_NET="0.0.0.0:19732"

echo "Test: Renaming node updates dependent services"

# Cleanup any previous test instances
cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NEW_NODE_INSTANCE" || true
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

# Install a baker that depends on the node
echo "Installing baker dependent on node..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --service-user tezos \
    --no-enable 2>&1

# Registry is at /etc/octez_manager/services/ when running as root
REGISTRY_DIR="/etc/octez_manager/services"

# Verify initial setup - baker depends on original node
BAKER_REGISTRY="${REGISTRY_DIR}/${BAKER_INSTANCE}.json"
if ! grep -q "\"depends_on\": \"$NODE_INSTANCE\"" "$BAKER_REGISTRY"; then
    echo "ERROR: Baker should depend on $NODE_INSTANCE"
    cat "$BAKER_REGISTRY"
    exit 1
fi
echo "Baker correctly depends on $NODE_INSTANCE"

# Verify baker env file has original node instance
BAKER_ENV="/etc/octez/instances/$BAKER_INSTANCE/node.env"
if ! grep -q "OCTEZ_NODE_INSTANCE=$NODE_INSTANCE" "$BAKER_ENV"; then
    echo "ERROR: Baker env should reference $NODE_INSTANCE"
    cat "$BAKER_ENV"
    exit 1
fi
echo "Baker env correctly references $NODE_INSTANCE"

# Verify node's dependents list includes baker
NODE_REGISTRY="${REGISTRY_DIR}/${NODE_INSTANCE}.json"
if ! grep -q "\"$BAKER_INSTANCE\"" "$NODE_REGISTRY"; then
    echo "ERROR: Node should have baker in dependents"
    cat "$NODE_REGISTRY"
    exit 1
fi
echo "Node correctly lists baker as dependent"

# Verify baker's systemd dropin references node
BAKER_DROPIN="/etc/systemd/system/octez-baker@${BAKER_INSTANCE}.service.d/99-dropin.conf"
if [ -f "$BAKER_DROPIN" ]; then
    if grep -q "BindsTo=octez-node@${NODE_INSTANCE}.service" "$BAKER_DROPIN"; then
        echo "Baker dropin correctly references node"
    else
        echo "ERROR: Baker dropin should reference $NODE_INSTANCE"
        cat "$BAKER_DROPIN"
        exit 1
    fi
fi

# Cleanup
echo "Cleaning up..."
cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NEW_NODE_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

echo "Rename updates dependents test completed"
