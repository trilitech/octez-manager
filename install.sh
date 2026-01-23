#!/bin/sh
set -e

# Octez Manager installer
#
# Usage:
#   Default installation:
#     curl -fsSL https://raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh
#
#   Custom installation directory:
#     curl -fsSL https://raw.githubusercontent.com/trilitech/octez-manager/main/install.sh | sh -s -- --prefix=/custom/path
#
# Default installation directories:
#   - Root user: /usr/local/bin (system-wide)
#   - Regular user: ~/.local/bin (user-local, XDG Base Directory)

REPO="trilitech/octez-manager"
BINARY_NAME="octez-manager"

# Parse arguments
PREFIX=""
while [ $# -gt 0 ]; do
	case "$1" in
	--prefix=*)
		PREFIX="${1#*=}"
		shift
		;;
	--prefix)
		PREFIX="$2"
		shift 2
		;;
	*)
		echo "Unknown option: $1"
		echo "Usage: $0 [--prefix=PATH]"
		exit 1
		;;
	esac
done

# Determine installation directory (XDG-based defaults)
if [ -n "$PREFIX" ]; then
	INSTALL_DIR="$PREFIX"
elif [ "$(id -u)" -eq 0 ]; then
	# Root user: install system-wide
	INSTALL_DIR="/usr/local/bin"
else
	# Non-root user: install to user directory (XDG Base Directory)
	# Use ~/.local/bin which is commonly in PATH
	INSTALL_DIR="${HOME}/.local/bin"
fi

# Detect OS and architecture
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

case "$OS" in
linux) OS="linux" ;;
*)
	echo "Unsupported OS: $OS (only Linux is currently supported)"
	exit 1
	;;
esac

case "$ARCH" in
x86_64 | amd64) ARCH="x86_64" ;;
*)
	echo "Unsupported architecture: $ARCH (only x86_64 is currently supported)"
	exit 1
	;;
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
trap 'rm -rf "$TMPDIR"' EXIT
curl -fsSL "$DOWNLOAD_URL" -o "$TMPDIR/$BINARY_NAME"
chmod +x "$TMPDIR/$BINARY_NAME"

# Create install directory if it doesn't exist
if [ ! -d "$INSTALL_DIR" ]; then
	echo "Creating directory $INSTALL_DIR..."
	if [ -w "$(dirname "$INSTALL_DIR")" ]; then
		mkdir -p "$INSTALL_DIR"
	else
		sudo mkdir -p "$INSTALL_DIR"
	fi
fi

# Install
echo "Installing to $INSTALL_DIR/$BINARY_NAME..."
if [ -w "$INSTALL_DIR" ]; then
	mv "$TMPDIR/$BINARY_NAME" "$INSTALL_DIR/$BINARY_NAME"
else
	sudo mv "$TMPDIR/$BINARY_NAME" "$INSTALL_DIR/$BINARY_NAME"
fi

echo ""
echo "Successfully installed $BINARY_NAME $VERSION to $INSTALL_DIR"

# Check if install directory is in PATH
case ":$PATH:" in
*":$INSTALL_DIR:"*)
	# Already in PATH
	echo "Run 'octez-manager --help' to get started"
	;;
*)
	# Not in PATH - provide instructions
	echo ""
	echo "⚠️  Note: $INSTALL_DIR is not in your PATH"
	echo ""
	echo "Add it to your PATH by running:"
	echo ""
	if [ -n "$BASH_VERSION" ] || [ -f "$HOME/.bashrc" ]; then
		echo "  echo 'export PATH=\"$INSTALL_DIR:\$PATH\"' >> ~/.bashrc"
		echo "  source ~/.bashrc"
	elif [ -n "$ZSH_VERSION" ] || [ -f "$HOME/.zshrc" ]; then
		echo "  echo 'export PATH=\"$INSTALL_DIR:\$PATH\"' >> ~/.zshrc"
		echo "  source ~/.zshrc"
	else
		echo "  export PATH=\"$INSTALL_DIR:\$PATH\""
		echo "  # Add the above line to your shell's rc file"
	fi
	echo ""
	echo "Or run directly: $INSTALL_DIR/octez-manager --help"
	;;
esac
