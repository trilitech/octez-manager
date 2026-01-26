#!/bin/bash
# Script to create a complete test Octez environment for import wizard testing
# This includes: node, DAL node, baker, and accuser

set -e

# Configuration
BASE_NAME="test-octez"
NETWORK="ghostnet"
BINARY_VERSION="v24.0"
BINARY_DIR="$HOME/.local/share/octez-manager/binaries/$BINARY_VERSION"

# Snapshot configuration
# Default snapshot URL for ghostnet (TzInit Europe)
# Set SNAPSHOT_URL="" to skip automatic snapshot import
SNAPSHOT_URL="${SNAPSHOT_URL:-https://snapshots.eu.tzinit.org/ghostnet/rolling}"

# Service names
NODE_SERVICE="${BASE_NAME}-node"
DAL_SERVICE="${BASE_NAME}-dal"
BAKER_SERVICE="${BASE_NAME}-baker"
ACCUSER_SERVICE="${BASE_NAME}-accuser"

# Ports
NODE_RPC_PORT="8733"
DAL_RPC_PORT="10733"
DAL_NET_PORT="11733"

# Data directories
NODE_DATA_DIR="/tmp/${BASE_NAME}-node-data"
DAL_DATA_DIR="/tmp/${BASE_NAME}-dal-data"
BAKER_BASE_DIR="/tmp/${BASE_NAME}-baker-data"
ACCUSER_BASE_DIR="/tmp/${BASE_NAME}-accuser-data"

# Endpoints
NODE_ENDPOINT="http://127.0.0.1:${NODE_RPC_PORT}"
DAL_ENDPOINT="http://127.0.0.1:${DAL_RPC_PORT}"

echo "Creating complete test Octez environment..."
echo "  Network: $NETWORK"
echo "  Binary version: $BINARY_VERSION"
if [ -n "$SNAPSHOT_URL" ]; then
  echo "  Snapshot: Will download from TzInit (${SNAPSHOT_URL})"
else
  echo "  Snapshot: Disabled (set SNAPSHOT_URL to enable)"
fi
echo ""
echo "Services to be created:"
echo "  - Node:    $NODE_SERVICE (RPC: 127.0.0.1:$NODE_RPC_PORT)"
echo "  - DAL:     $DAL_SERVICE (RPC: 127.0.0.1:$DAL_RPC_PORT, P2P: 0.0.0.0:$DAL_NET_PORT)"
echo "  - Baker:   $BAKER_SERVICE"
echo "  - Accuser: $ACCUSER_SERVICE"
echo ""

# Verify binaries exist
echo "Checking binaries..."
required_binaries=(
  "octez-node"
  "octez-dal-node"
  "octez-baker"
)

for binary in "${required_binaries[@]}"; do
  if [ ! -f "$BINARY_DIR/$binary" ]; then
    echo "ERROR: Binary not found: $BINARY_DIR/$binary"
    echo "Available versions:"
    ls -1 "$HOME/.local/share/octez-manager/binaries/" 2> /dev/null || echo "  (none)"
    exit 1
  fi
done
echo "✓ All binaries found"
echo ""

# Create systemd service directory
echo "Creating systemd user service directory..."
mkdir -p "$HOME/.config/systemd/user"

# Create data directories
echo "Creating data directories..."
mkdir -p "$NODE_DATA_DIR"
mkdir -p "$DAL_DATA_DIR"
mkdir -p "$BAKER_BASE_DIR"
mkdir -p "$ACCUSER_BASE_DIR"

# ============================================================================
# Snapshot Import (if configured)
# ============================================================================
if [ -n "$SNAPSHOT_URL" ]; then
  echo ""
  echo "Downloading and importing snapshot..."
  echo "  URL: $SNAPSHOT_URL"
  echo "  This may take several minutes..."
  echo ""

  SNAPSHOT_FILE="/tmp/${BASE_NAME}-snapshot.tmp"

  # Download snapshot
  echo "  Downloading snapshot..."
  if ! curl -L -f --progress-bar -o "$SNAPSHOT_FILE" "$SNAPSHOT_URL"; then
    echo "ERROR: Failed to download snapshot from $SNAPSHOT_URL"
    rm -f "$SNAPSHOT_FILE"
    exit 1
  fi

  echo "  Download complete: $(du -h "$SNAPSHOT_FILE" | cut -f1)"
  echo ""

  "$BINARY_DIR/octez-node" config init --data-dir "$NODE_DATA_DIR" --network "$NETWORK"

  # Import snapshot into node data directory
  echo "  Importing snapshot into $NODE_DATA_DIR..."
  if ! "$BINARY_DIR/octez-node" snapshot import "$SNAPSHOT_FILE" --data-dir "$NODE_DATA_DIR" --no-check; then
    echo "ERROR: Failed to import snapshot"
    rm -f "$SNAPSHOT_FILE"
    exit 1
  fi

  # Clean up snapshot file
  echo "  Cleaning up snapshot file..."
  rm -f "$SNAPSHOT_FILE"

  echo "✓ Snapshot imported successfully"
  echo ""
elif [ -d "$NODE_DATA_DIR/context" ]; then
  echo ""
  echo "⚠  Node data directory already has context data"
  echo "   Skipping snapshot import"
  echo ""
else
  echo ""
  echo "⚠  No snapshot URL configured"
  echo "   Node will sync from genesis (this will be slow)"
  echo "   To use a snapshot, set SNAPSHOT_URL environment variable:"
  echo "   SNAPSHOT_URL='https://snapshots.example.com/ghostnet/rolling' ./create-test-services-full.sh"
  echo ""
fi

# ============================================================================
# 1. Create Node Service
# ============================================================================
echo ""
echo "Creating node service: $NODE_SERVICE..."
cat > "$HOME/.config/systemd/user/$NODE_SERVICE.service" << EOF
[Unit]
Description=Test Octez Node ($NETWORK) for Import Testing
After=network.target

[Service]
Type=simple
ExecStart=$BINARY_DIR/octez-node run --data-dir $NODE_DATA_DIR --network $NETWORK --rpc-addr 127.0.0.1:$NODE_RPC_PORT
Restart=on-failure
RestartSec=5
Environment="HOME=$HOME"

[Install]
WantedBy=default.target
EOF

# ============================================================================
# 2. Create DAL Node Service
# ============================================================================
echo "Creating DAL node service: $DAL_SERVICE..."
cat > "$HOME/.config/systemd/user/$DAL_SERVICE.service" << EOF
[Unit]
Description=Test Octez DAL Node ($NETWORK) for Import Testing
After=network.target $NODE_SERVICE.service
Requires=$NODE_SERVICE.service

[Service]
Type=simple
ExecStart=$BINARY_DIR/octez-dal-node run --data-dir $DAL_DATA_DIR --endpoint $NODE_ENDPOINT --rpc-addr 127.0.0.1:$DAL_RPC_PORT --net-addr 0.0.0.0:$DAL_NET_PORT
Restart=on-failure
RestartSec=5
Environment="HOME=$HOME"

[Install]
WantedBy=default.target
EOF

# ============================================================================
# 3. Create Baker Service
# ============================================================================
echo "Creating baker service: $BAKER_SERVICE..."

# Note: For the baker to work properly, you need to import a delegate key:
# octez-client --base-dir $BAKER_BASE_DIR import secret key my_baker unencrypted:edsk...
# For testing purposes, this creates the service structure without keys

cat > "$HOME/.config/systemd/user/$BAKER_SERVICE.service" << EOF
[Unit]
Description=Test Octez Baker ($NETWORK) for Import Testing
After=network.target $NODE_SERVICE.service $DAL_SERVICE.service
Requires=$NODE_SERVICE.service
Wants=$DAL_SERVICE.service

[Service]
Type=simple
ExecStart=$BINARY_DIR/octez-baker --endpoint $NODE_ENDPOINT run with local node $NODE_DATA_DIR --dal-node $DAL_ENDPOINT --base-dir $BAKER_BASE_DIR --liquidity-baking-toggle-vote pass --node-version-check-bypass
Restart=on-failure
RestartSec=5
Environment="HOME=$HOME"

[Install]
WantedBy=default.target
EOF

# ============================================================================
# 4. Create Accuser Service
# ============================================================================
echo "Creating accuser service: $ACCUSER_SERVICE..."
cat > "$HOME/.config/systemd/user/$ACCUSER_SERVICE.service" << EOF
[Unit]
Description=Test Octez Accuser ($NETWORK) for Import Testing
After=network.target $NODE_SERVICE.service
Requires=$NODE_SERVICE.service

[Service]
Type=simple
ExecStart=$BINARY_DIR/octez-baker --endpoint $NODE_ENDPOINT run --base-dir $ACCUSER_BASE_DIR
Restart=on-failure
RestartSec=5
Environment="HOME=$HOME"

[Install]
WantedBy=default.target
EOF

# ============================================================================
# Start Services
# ============================================================================
echo ""
echo "Reloading systemd daemon..."
systemctl --user daemon-reload

echo "Starting services..."
echo "  Starting node..."
systemctl --user start "$NODE_SERVICE.service"

# Give node a moment to start
sleep 2

echo "  Starting DAL node..."
systemctl --user start "$DAL_SERVICE.service"

echo "  Starting baker..."
systemctl --user start "$BAKER_SERVICE.service"

echo "  Starting accuser..."
systemctl --user start "$ACCUSER_SERVICE.service"

# Wait for services to initialize
sleep 2

# ============================================================================
# Show Status
# ============================================================================
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Service Status:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

for service in "$NODE_SERVICE" "$DAL_SERVICE" "$BAKER_SERVICE" "$ACCUSER_SERVICE"; do
  echo "▸ $service:"
  if systemctl --user is-active "$service.service" > /dev/null 2>&1; then
    echo "  Status: ✓ Active"
  else
    echo "  Status: ✗ Failed or inactive"
  fi
  echo ""
done

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✓ Test services created!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Useful commands:"
echo ""
echo "  # Check all service statuses"
echo "  systemctl --user status $NODE_SERVICE.service"
echo "  systemctl --user status $DAL_SERVICE.service"
echo "  systemctl --user status $BAKER_SERVICE.service"
echo "  systemctl --user status $ACCUSER_SERVICE.service"
echo ""
echo "  # View logs (follow mode)"
echo "  journalctl --user -u $NODE_SERVICE.service -f"
echo "  journalctl --user -u $DAL_SERVICE.service -f"
echo "  journalctl --user -u $BAKER_SERVICE.service -f"
echo "  journalctl --user -u $ACCUSER_SERVICE.service -f"
echo ""
echo "  # View all logs together"
echo "  journalctl --user -u $NODE_SERVICE.service -u $DAL_SERVICE.service -u $BAKER_SERVICE.service -u $ACCUSER_SERVICE.service -f"
echo ""
echo "  # Stop all services"
echo "  systemctl --user stop $NODE_SERVICE.service $DAL_SERVICE.service $BAKER_SERVICE.service $ACCUSER_SERVICE.service"
echo ""
echo "  # Restart all services"
echo "  systemctl --user restart $NODE_SERVICE.service $DAL_SERVICE.service $BAKER_SERVICE.service $ACCUSER_SERVICE.service"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Important Notes:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
if [ -n "$SNAPSHOT_URL" ]; then
  echo "✓ Node data initialized from snapshot (TzInit)"
  echo "  The node should sync quickly from the imported snapshot state"
  echo "  To disable: SNAPSHOT_URL='' ./create-test-services-full.sh"
  echo ""
else
  echo "⚠  No snapshot imported - node will sync from genesis (very slow)"
  echo "  To enable: SNAPSHOT_URL='https://snapshots.eu.tzinit.org/ghostnet/rolling' ./create-test-services-full.sh"
  echo ""
fi
echo "⚠  Baker will not actively bake until you import a delegate key:"
echo "   octez-client --base-dir $BAKER_BASE_DIR import secret key my_delegate unencrypted:edsk..."
echo ""
echo "⚠  For baker to work, the delegate must have baking rights on $NETWORK"
echo ""
echo "📚 Check baker/accuser logs for any configuration errors"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "To clean up after testing:"
echo "  ./cleanup-test-services-full.sh"
echo ""
echo "⚠  Baker will not actively bake until you import a delegate key:"
echo "   octez-client --base-dir $BAKER_BASE_DIR import secret key my_delegate unencrypted:edsk..."
echo ""
echo "⚠  For baker to work, the delegate must have baking rights on $NETWORK"
echo ""
echo "📚 Check baker/accuser logs for any configuration errors"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "To clean up after testing:"
echo "  ./cleanup-test-services-full.sh"
echo ""
