#!/bin/bash
# Test: Purge removes instance and data directory
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-purge"
DATA_DIR="/var/lib/octez/$INSTANCE"
ENV_DIR="/etc/octez/instances/$INSTANCE"

echo "Test: Purge removes instance and data directory"

cleanup_instance "$INSTANCE" || true

# Install node
om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --rpc-addr "127.0.0.1:8739" --net-addr "0.0.0.0:9739" \
    --service-user tezos \
    --no-enable 2>&1

# Verify data exists
if [ ! -d "$DATA_DIR" ]; then
    echo "ERROR: Data directory not created"
    exit 1
fi
echo "Data directory exists: $DATA_DIR"

# Purge instance
echo "Purging instance..."
om instance "$INSTANCE" purge

# Verify instance removed from registry
if instance_exists "$INSTANCE"; then
    echo "ERROR: Instance still in registry after purge"
    exit 1
fi
echo "Instance removed from registry"

# Verify data directory removed
if [ -d "$DATA_DIR" ]; then
    echo "ERROR: Data directory still exists after purge"
    exit 1
fi
echo "Data directory removed"

# Verify env directory removed
if [ -d "$ENV_DIR" ]; then
    echo "ERROR: Env directory still exists after purge"
    exit 1
fi
echo "Env directory removed"

echo "Purge test passed"
