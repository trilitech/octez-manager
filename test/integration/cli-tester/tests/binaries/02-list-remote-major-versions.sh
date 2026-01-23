#!/bin/bash
# Test: Verify list-remote shows multiple major versions
# This test validates that the remote API provides versions from multiple major version families,
# which is a prerequisite for the TUI's 2-major-version filtering (issue #393).
set -euo pipefail
source /tests/lib.sh

echo "Test: Validate remote version list has multiple major versions"

# Fetch available versions
echo "Fetching available versions..."
VERSION_LIST=$(om binaries list-remote 2>&1 || echo "")

if [ -z "$VERSION_LIST" ]; then
	echo "WARNING: Could not fetch available versions (no internet access)"
	echo "Skipping test - requires network connectivity"
	exit 0
fi

echo "Available versions fetched"

# Extract version numbers (format: v24.0, v23.1, etc.)
# Skip the "Available versions:" header line
VERSIONS=$(echo "$VERSION_LIST" | grep -E '^\s+[0-9]+\.[0-9]+' | sed 's/^\s*//' | awk '{print $1}' | sed 's/-.*$//' || echo "")

if [ -z "$VERSIONS" ]; then
	echo "ERROR: No versions found in output"
	echo "Output was:"
	echo "$VERSION_LIST"
	exit 1
fi

echo "Extracted versions:"
echo "$VERSIONS"

# Extract major versions and count unique ones
# Use associative array to track unique major versions
declare -A MAJOR_VERSIONS
while IFS= read -r version; do
	# Extract major version (e.g., "24" from "24.0")
	major=$(echo "$version" | cut -d'.' -f1)
	if [ -n "$major" ]; then
		MAJOR_VERSIONS["$major"]=1
	fi
done <<<"$VERSIONS"

# Count unique major versions
MAJOR_COUNT=${#MAJOR_VERSIONS[@]}

echo "Found $MAJOR_COUNT unique major versions: ${!MAJOR_VERSIONS[*]}"

# Verify we have at least 2 major versions
# (This validates that the remote API provides enough data for TUI filtering)
if [ "$MAJOR_COUNT" -lt 2 ]; then
	echo "ERROR: Expected at least 2 major versions, found $MAJOR_COUNT"
	echo "The TUI's 2-major-version filter requires at least 2 major versions to be available"
	exit 1
fi

echo "✓ Remote API provides multiple major versions (prerequisite for TUI filtering)"

# Verify each major version has at least one version
for major in "${!MAJOR_VERSIONS[@]}"; do
	count=$(echo "$VERSIONS" | grep -c "^${major}\." || echo "0")
	if [ "$count" -eq 0 ]; then
		echo "ERROR: Major version $major has no versions"
		exit 1
	fi
	echo "  Major $major: $count version(s)"
done

echo "✓ All major versions have at least one version"

echo "Remote version list validation passed"
