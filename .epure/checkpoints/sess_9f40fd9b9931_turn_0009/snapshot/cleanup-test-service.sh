#!/bin/bash
# Script to clean up the test octez-node service

set -e

# Configuration (must match create-test-service.sh)
SERVICE_NAME="test-octez-node"
DATA_DIR="/tmp/test-octez-node-data"

echo "Cleaning up test octez-node service..."
echo ""

# Stop service if running
echo "Stopping service..."
systemctl --user stop "$SERVICE_NAME.service" 2>/dev/null || echo "  (service not running)"

# Remove service file
echo "Removing service file..."
rm -f "$HOME/.config/systemd/user/$SERVICE_NAME.service"

# Reload systemd
echo "Reloading systemd daemon..."
systemctl --user daemon-reload

# Remove data directory
echo "Removing data directory..."
rm -rf "$DATA_DIR"

echo ""
echo "✓ Test service cleaned up!"
echo ""
echo "Service file removed: ~/.config/systemd/user/$SERVICE_NAME.service"
echo "Data directory removed: $DATA_DIR"
