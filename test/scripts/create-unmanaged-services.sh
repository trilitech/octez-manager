#!/bin/bash
# Script to create unmanaged Octez services for testing import functionality
# This creates systemd user services WITHOUT using octez-manager

set -euo pipefail

# Parse arguments
NETWORK="shadownet"
USE_SNAPSHOT=false
SNAPSHOT_PATH=""

show_usage() {
  cat << EOF
Usage: $0 [OPTIONS]

Create unmanaged Octez services for testing import functionality.

Options:
    --snapshot [FILE]       Import a snapshot before starting the node
                            If FILE is provided, use local snapshot file
                            Otherwise, download from default snapshot URL
    --network NETWORK       Network to use (default: shadownet)
    -h, --help             Show this help message

Examples:
    # Create services without snapshot (shadownet)
    $0

    # Create services for ghostnet without snapshot
    $0 --network ghostnet

    # Create services with snapshot download (shadownet)
    $0 --snapshot

    # Create services with local snapshot file
    $0 --snapshot /path/to/snapshot.rolling

    # Create services with snapshot for specific network
    $0 --snapshot --network ghostnet

    # Create services with local snapshot for mainnet
    $0 --snapshot snapshot_file --network mainnet
EOF
  exit 0
}

while [[ $# -gt 0 ]]; do
  case $1 in
  --snapshot)
    USE_SNAPSHOT=true
    # Check if next arg exists and is not a flag
    if [[ $# -gt 1 && ! "$2" =~ ^-- ]]; then
      SNAPSHOT_PATH="$2"
      shift 2
    else
      shift
    fi
    ;;
  --network)
    NETWORK="$2"
    shift 2
    ;;
  -h | --help)
    show_usage
    ;;
  *)
    echo "Error: Unknown argument '$1'"
    echo "Use --help for usage information"
    exit 1
    ;;
  esac
done

BASE_DIR="/tmp/octez-unmanaged-test"
NODE_DATA_DIR="${BASE_DIR}/node-data"
CLIENT_BASE_DIR="${BASE_DIR}/client"
DAL_DATA_DIR="${BASE_DIR}/dal-data"
ENV_DIR="${HOME}/.config/octez-unmanaged-env"
BIN_DIR="${HOME}/.local/share/octez-manager/binaries/v24.0"
USER_SYSTEMD_DIR="${HOME}/.config/systemd/user"

echo "Creating unmanaged Octez services for network: ${NETWORK}"
echo "Base directory: ${BASE_DIR}"
echo "Binary directory: ${BIN_DIR}"
if [ "$USE_SNAPSHOT" = true ]; then
  echo "Snapshot: enabled"
  if [ -n "$SNAPSHOT_PATH" ]; then
    echo "Snapshot file: ${SNAPSHOT_PATH}"
  fi
fi
echo ""

# Check if binaries exist
if [ ! -f "${BIN_DIR}/octez-node" ]; then
  echo "Error: Binaries not found at ${BIN_DIR}"
  echo "Please ensure octez binaries are installed"
  exit 1
fi

# Create directories
echo "[1/7] Creating directories..."
mkdir -p "${NODE_DATA_DIR}"
mkdir -p "${CLIENT_BASE_DIR}"
mkdir -p "${DAL_DATA_DIR}"
mkdir -p "${ENV_DIR}"
mkdir -p "${USER_SYSTEMD_DIR}"

# Initialize node config if not exists
if [ ! -f "${NODE_DATA_DIR}/config.json" ]; then
  echo "[2/7] Initializing node..."
  "${BIN_DIR}/octez-node" config init \
    --data-dir "${NODE_DATA_DIR}" \
    --network "${NETWORK}" \
    --rpc-addr 127.0.0.1:18732 \
    --net-addr 0.0.0.0:19732 \
    --history-mode rolling
else
  echo "[2/7] Node already initialized, skipping..."
fi

# Import snapshot if requested
if [ "$USE_SNAPSHOT" = true ] && [ ! -f "${NODE_DATA_DIR}/context/store.dict" ]; then
  echo "[3/7] Importing snapshot..."

  # Check if using local file or need to download
  if [ -n "$SNAPSHOT_PATH" ]; then
    # Use local snapshot file
    if [ ! -f "$SNAPSHOT_PATH" ]; then
      echo "Error: Snapshot file not found: ${SNAPSHOT_PATH}"
      exit 1
    fi
    echo "  Using local snapshot file: ${SNAPSHOT_PATH}"
    SNAPSHOT_FILE="$SNAPSHOT_PATH"
    CLEANUP_SNAPSHOT=false
  else
    # Download snapshot from default URL
    SNAPSHOT_FILE="${BASE_DIR}/snapshot.rolling"
    CLEANUP_SNAPSHOT=true

    # Determine snapshot URL based on network
    case "$NETWORK" in
    mainnet)
      SNAPSHOT_URL="https://snapshots.eu.tzinit.org/mainnet/rolling"
      ;;
    ghostnet)
      SNAPSHOT_URL="https://snapshots.eu.tzinit.org/ghostnet/rolling"
      ;;
    shadownet)
      echo "  Note: Using ghostnet snapshot for shadownet (no dedicated shadownet snapshots)"
      SNAPSHOT_URL="https://snapshots.eu.tzinit.org/ghostnet/rolling"
      ;;
    *)
      echo "Warning: No default snapshot URL for network ${NETWORK}"
      echo "Please provide a snapshot file: --snapshot /path/to/snapshot.rolling"
      USE_SNAPSHOT=false
      ;;
    esac

    if [ "$USE_SNAPSHOT" = true ]; then
      echo "  Downloading snapshot from ${SNAPSHOT_URL}..."
      if command -v wget &> /dev/null; then
        wget -O "${SNAPSHOT_FILE}" "${SNAPSHOT_URL}" --show-progress
      elif command -v curl &> /dev/null; then
        curl -L -o "${SNAPSHOT_FILE}" "${SNAPSHOT_URL}" --progress-bar
      else
        echo "Error: Neither wget nor curl found. Cannot download snapshot."
        exit 1
      fi
    fi
  fi

  if [ "$USE_SNAPSHOT" = true ]; then
    echo "  Importing snapshot into node..."
    "${BIN_DIR}/octez-node" snapshot import "${SNAPSHOT_FILE}" \
      --data-dir "${NODE_DATA_DIR}" \
      --no-check

    if [ "$CLEANUP_SNAPSHOT" = true ]; then
      echo "  Cleaning up downloaded snapshot file..."
      rm -f "${SNAPSHOT_FILE}"
    fi

    echo "  Snapshot imported successfully!"
  fi
else
  if [ "$USE_SNAPSHOT" = true ]; then
    echo "[3/7] Node already has data, skipping snapshot import..."
  else
    echo "[3/7] Skipping snapshot import..."
  fi
fi

# Initialize DAL config if not exists
if [ ! -f "${DAL_DATA_DIR}/config.json" ]; then
  echo "[4/7] Initializing DAL node..."
  "${BIN_DIR}/octez-dal-node" config init \
    --data-dir "${DAL_DATA_DIR}" \
    --rpc-addr 127.0.0.1:10732 \
    --net-addr 0.0.0.0:11732 \
    --endpoint http://127.0.0.1:18732
else
  echo "[4/7] DAL node already initialized, skipping..."
fi

# Create test wallet with some keys
echo "[5/7] Setting up test wallet..."
export TEZOS_CLIENT_DIR="${CLIENT_BASE_DIR}"
if [ ! -f "${CLIENT_BASE_DIR}/public_keys" ]; then
  "${BIN_DIR}/octez-client" -d "${CLIENT_BASE_DIR}" -E http://127.0.0.1:18732 gen keys alice --force || true
  "${BIN_DIR}/octez-client" -d "${CLIENT_BASE_DIR}" -E http://127.0.0.1:18732 gen keys bob --force || true
  echo "Created test keys: alice, bob"
else
  echo "Wallet already exists, skipping key generation..."
fi

# Create environment files
echo "[6/7] Creating environment files..."

# Node environment file
cat > "${ENV_DIR}/node.env" << EOF
OCTEZ_DATA_DIR=${NODE_DATA_DIR}
OCTEZ_RPC_ADDR=127.0.0.1:18732
OCTEZ_NET_ADDR=0.0.0.0:19732
OCTEZ_NETWORK=${NETWORK}
APP_BIN_DIR=${BIN_DIR}
EOF

# DAL environment file
cat > "${ENV_DIR}/dal.env" << EOF
OCTEZ_DAL_DATA_DIR=${DAL_DATA_DIR}
OCTEZ_DAL_RPC_ADDR=127.0.0.1:10732
OCTEZ_DAL_NET_ADDR=0.0.0.0:11732
OCTEZ_NETWORK=${NETWORK}
OCTEZ_NODE_ENDPOINT=http://127.0.0.1:18732
APP_BIN_DIR=${BIN_DIR}
EOF

# Baker environment file
cat > "${ENV_DIR}/baker.env" << EOF
OCTEZ_BAKER_BASE_DIR=${CLIENT_BASE_DIR}
OCTEZ_NODE_ENDPOINT=http://127.0.0.1:18732
OCTEZ_NETWORK=${NETWORK}
APP_BIN_DIR=${BIN_DIR}
EOF

# Accuser environment file
cat > "${ENV_DIR}/accuser.env" << EOF
OCTEZ_CLIENT_BASE_DIR=${CLIENT_BASE_DIR}
OCTEZ_NODE_ENDPOINT=http://127.0.0.1:18732
OCTEZ_NETWORK=${NETWORK}
APP_BIN_DIR=${BIN_DIR}
EOF

# Create systemd user service files
echo "[7/7] Creating systemd user services..."

cat > "${USER_SYSTEMD_DIR}/test-octez-node.service" << EOF
[Unit]
Description=Test Octez Node (${NETWORK})
After=network.target

[Service]
Type=simple
EnvironmentFile=${ENV_DIR}/node.env
ExecStart=/bin/sh -c 'exec "\${APP_BIN_DIR}/octez-node" run --data-dir "\${OCTEZ_DATA_DIR}" --rpc-addr "\${OCTEZ_RPC_ADDR}" --net-addr "\${OCTEZ_NET_ADDR}" --network "\${OCTEZ_NETWORK}" --history-mode rolling --connections 50'
Restart=on-failure
RestartSec=10

[Install]
WantedBy=default.target
EOF

cat > "${USER_SYSTEMD_DIR}/test-octez-dal-node.service" << EOF
[Unit]
Description=Test Octez DAL Node (${NETWORK})
After=network.target test-octez-node.service
Requires=test-octez-node.service

[Service]
Type=simple
EnvironmentFile=${ENV_DIR}/dal.env
ExecStart=/bin/sh -c 'exec "\${APP_BIN_DIR}/octez-dal-node" run --endpoint "\${OCTEZ_NODE_ENDPOINT}" --data-dir "\${OCTEZ_DAL_DATA_DIR}" --rpc-addr "\${OCTEZ_DAL_RPC_ADDR}" --net-addr "\${OCTEZ_DAL_NET_ADDR}" --disable-amplification'
Restart=on-failure
RestartSec=10

[Install]
WantedBy=default.target
EOF

cat > "${USER_SYSTEMD_DIR}/test-octez-baker.service" << EOF
[Unit]
Description=Test Octez Baker (${NETWORK})
After=network.target test-octez-node.service
Requires=test-octez-node.service

[Service]
Type=simple
EnvironmentFile=${ENV_DIR}/baker.env
ExecStart=/bin/sh -c 'exec "\${APP_BIN_DIR}/octez-baker" --endpoint "\${OCTEZ_NODE_ENDPOINT}" --base-dir "\${OCTEZ_BAKER_BASE_DIR}" run with local node ${NODE_DATA_DIR} alice bob --liquidity-baking-toggle-vote pass --dal-node http://127.0.0.1:10732'
Restart=on-failure
RestartSec=10

[Install]
WantedBy=default.target
EOF

cat > "${USER_SYSTEMD_DIR}/test-octez-accuser.service" << EOF
[Unit]
Description=Test Octez Accuser (${NETWORK})
After=network.target test-octez-node.service
Requires=test-octez-node.service

[Service]
Type=simple
EnvironmentFile=${ENV_DIR}/accuser.env
ExecStart=/bin/sh -c 'exec "\${APP_BIN_DIR}/octez-baker" --endpoint "\${OCTEZ_NODE_ENDPOINT}" --base-dir "\${OCTEZ_CLIENT_BASE_DIR}" run accuser'
Restart=on-failure
RestartSec=10

[Install]
WantedBy=default.target
EOF

# Reload systemd user daemon
echo ""
echo "Reloading systemd user daemon..."
systemctl --user daemon-reload

# Enable and start services
echo "Enabling and starting services..."
systemctl --user enable test-octez-node.service
systemctl --user enable test-octez-dal-node.service
systemctl --user enable test-octez-baker.service
systemctl --user enable test-octez-accuser.service

echo "Starting node..."
systemctl --user start test-octez-node.service

echo "Waiting 5 seconds for node to start..."
sleep 5

echo "Starting DAL node..."
systemctl --user start test-octez-dal-node.service

echo "Starting baker..."
systemctl --user start test-octez-baker.service

echo "Starting accuser..."
systemctl --user start test-octez-accuser.service

echo ""
echo "=========================================="
echo "✓ Unmanaged services created and started!"
echo "=========================================="
echo ""
echo "Services created:"
echo "  - test-octez-node.service"
echo "  - test-octez-dal-node.service"
echo "  - test-octez-baker.service"
echo "  - test-octez-accuser.service"
echo ""
echo "Data directories:"
echo "  - Node: ${NODE_DATA_DIR}"
echo "  - DAL: ${DAL_DATA_DIR}"
echo "  - Client: ${CLIENT_BASE_DIR}"
echo ""
echo "Environment files:"
echo "  - ${ENV_DIR}/"
echo ""
echo "Service status:"
systemctl --user status test-octez-node.service --no-pager || true
echo ""
systemctl --user status test-octez-dal-node.service --no-pager || true
echo ""
systemctl --user status test-octez-baker.service --no-pager || true
echo ""
systemctl --user status test-octez-accuser.service --no-pager || true
echo ""
echo "To check detailed status:"
echo "  systemctl --user status test-octez-node.service"
echo "  systemctl --user status test-octez-dal-node.service"
echo "  systemctl --user status test-octez-baker.service"
echo "  systemctl --user status test-octez-accuser.service"
echo ""
echo "To view logs:"
echo "  journalctl --user -u test-octez-node.service -f"
echo "  journalctl --user -u test-octez-dal-node.service -f"
echo ""
echo "To test import with octez-manager:"
echo "  om list-external"
echo "  om import test-octez-node --strategy takeover"
echo "  om import-cascade test-octez-node --strategy takeover"
echo ""
echo "To clean up:"
echo "  ./cleanup-unmanaged-services.sh"
echo ""
