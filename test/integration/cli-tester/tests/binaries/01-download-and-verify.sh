#!/bin/bash
# Test: Download binaries and verify completeness
# Verifies that downloaded managed binaries include all required binaries
set -euo pipefail
source /tests/lib.sh

echo "Test: Download and verify managed binaries"

# Get the latest available version
echo "Fetching latest version..."
VERSION=$(om binaries list-remote --json 2>/dev/null | jq -r '.[0].version' || echo "")

if [ -z "$VERSION" ]; then
	echo "WARNING: Could not fetch available versions (no internet access)"
	echo "Skipping test - requires network connectivity"
	exit 0
fi

echo "Latest version: $VERSION"

# Check if already downloaded
MANAGED_DIR="$HOME/.local/share/octez-manager/binaries/v$VERSION"

if [ -d "$MANAGED_DIR" ]; then
	echo "Version v$VERSION already downloaded, removing for clean test..."
	rm -rf "$MANAGED_DIR"
fi

# Download the binaries
echo "Downloading v$VERSION binaries..."
om binaries download "$VERSION" 2>&1

# Verify the directory was created
if [ ! -d "$MANAGED_DIR" ]; then
	echo "ERROR: Managed binaries directory not created: $MANAGED_DIR"
	exit 1
fi

echo "✓ Binaries directory created: $MANAGED_DIR"

# Verify all required binaries are present
REQUIRED_BINARIES=(
	"octez-node"
	"octez-client"
	"octez-baker"
	"octez-dal-node"
)

echo "Verifying required binaries..."
for binary in "${REQUIRED_BINARIES[@]}"; do
	BINARY_PATH="$MANAGED_DIR/$binary"
	if [ ! -f "$BINARY_PATH" ]; then
		echo "ERROR: Missing required binary: $binary"
		echo "Directory contents:"
		ls -la "$MANAGED_DIR"
		exit 1
	fi

	# Verify it's executable
	if [ ! -x "$BINARY_PATH" ]; then
		echo "ERROR: Binary not executable: $binary"
		exit 1
	fi

	# Verify it can report version
	if ! timeout 5s "$BINARY_PATH" --version >/dev/null 2>&1; then
		echo "ERROR: Binary cannot report version: $binary"
		exit 1
	fi

	echo "  ✓ $binary"
done

echo "✓ All required binaries present and functional"

# Verify metadata file exists
METADATA_FILE="$MANAGED_DIR/.metadata.json"
if [ ! -f "$METADATA_FILE" ]; then
	echo "ERROR: Missing metadata file"
	exit 1
fi

echo "✓ Metadata file present"

# Verify we can list the downloaded version
if ! om binaries list 2>&1 | grep -q "v$VERSION"; then
	echo "ERROR: Downloaded version not shown in binaries list"
	exit 1
fi

echo "✓ Downloaded version appears in binaries list"

echo "Binary download and verification test passed"
