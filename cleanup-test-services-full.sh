#!/bin/bash
# Script to clean up the complete test Octez environment
# Removes: node, DAL node, baker, and accuser services

set -e

# Configuration (must match create-test-services-full.sh)
BASE_NAME="test-octez"

# Service names
NODE_SERVICE="${BASE_NAME}-node"
DAL_SERVICE="${BASE_NAME}-dal"
BAKER_SERVICE="${BASE_NAME}-baker"
ACCUSER_SERVICE="${BASE_NAME}-accuser"

# Data directories
NODE_DATA_DIR="/tmp/${BASE_NAME}-node-data"
DAL_DATA_DIR="/tmp/${BASE_NAME}-dal-data"
BAKER_BASE_DIR="/tmp/${BASE_NAME}-baker-data"
ACCUSER_BASE_DIR="/tmp/${BASE_NAME}-accuser-data"

echo "Cleaning up complete test Octez environment..."
echo ""
echo "Services to be removed:"
echo "  - $NODE_SERVICE"
echo "  - $DAL_SERVICE"
echo "  - $BAKER_SERVICE"
echo "  - $ACCUSER_SERVICE"
echo ""

# Stop all services
echo "Stopping services..."
for service in "$ACCUSER_SERVICE" "$BAKER_SERVICE" "$DAL_SERVICE" "$NODE_SERVICE"; do
	echo "  Stopping $service..."
	systemctl --user stop "$service.service" 2>/dev/null || echo "    (service not running)"
done

echo ""
echo "Removing service files..."
for service in "$NODE_SERVICE" "$DAL_SERVICE" "$BAKER_SERVICE" "$ACCUSER_SERVICE"; do
	echo "  Removing $service.service..."
	rm -f "$HOME/.config/systemd/user/$service.service"
done

# Reload systemd
echo ""
echo "Reloading systemd daemon..."
systemctl --user daemon-reload

# Remove data directories
echo ""
echo "Removing data directories..."
echo "  Removing $NODE_DATA_DIR..."
rm -rf "$NODE_DATA_DIR"

echo "  Removing $DAL_DATA_DIR..."
rm -rf "$DAL_DATA_DIR"

echo "  Removing $BAKER_BASE_DIR..."
rm -rf "$BAKER_BASE_DIR"

echo "  Removing $ACCUSER_BASE_DIR..."
rm -rf "$ACCUSER_BASE_DIR"

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✓ Test services cleaned up!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Removed services:"
echo "  - ~/.config/systemd/user/$NODE_SERVICE.service"
echo "  - ~/.config/systemd/user/$DAL_SERVICE.service"
echo "  - ~/.config/systemd/user/$BAKER_SERVICE.service"
echo "  - ~/.config/systemd/user/$ACCUSER_SERVICE.service"
echo ""
echo "Removed data directories:"
echo "  - $NODE_DATA_DIR"
echo "  - $DAL_DATA_DIR"
echo "  - $BAKER_BASE_DIR"
echo "  - $ACCUSER_BASE_DIR"
echo ""
