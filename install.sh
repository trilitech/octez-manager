#!/bin/sh
set -e

# Octez Manager installer
# Usage: curl -fsSL https://raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh

REPO="trilitech/octez-manager"
INSTALL_DIR="/usr/local/bin"
BINARY_NAME="octez-manager"

# Detect OS and architecture
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

case "$OS" in
    linux) OS="linux" ;;
    *) echo "Unsupported OS: $OS (only Linux is currently supported)"; exit 1 ;;
esac

case "$ARCH" in
    x86_64|amd64) ARCH="x86_64" ;;
    *) echo "Unsupported architecture: $ARCH (only x86_64 is currently supported)"; exit 1 ;;
esac

# Get latest version
echo "Fetching latest version..."
VERSION=$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | grep '"tag_name"' | sed -E 's/.*"([^"]+)".*/\1/')

if [ -z "$VERSION" ]; then
    echo "Failed to fetch latest version"
    exit 1
fi

# Download binary
ASSET_NAME="octez-manager-${VERSION}-${OS}-${ARCH}"
DOWNLOAD_URL="https://github.com/$REPO/releases/download/$VERSION/$ASSET_NAME"

echo "Downloading $ASSET_NAME..."
TMPDIR=$(mktemp -d)
curl -fsSL "$DOWNLOAD_URL" -o "$TMPDIR/$BINARY_NAME"
chmod +x "$TMPDIR/$BINARY_NAME"

# Install
echo "Installing to $INSTALL_DIR/$BINARY_NAME..."
if [ -w "$INSTALL_DIR" ]; then
    mv "$TMPDIR/$BINARY_NAME" "$INSTALL_DIR/$BINARY_NAME"
else
    sudo mv "$TMPDIR/$BINARY_NAME" "$INSTALL_DIR/$BINARY_NAME"
fi

rm -rf "$TMPDIR"

echo "Successfully installed $BINARY_NAME $VERSION"
echo "Run 'octez-manager --help' to get started"
