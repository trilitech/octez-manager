#!/bin/bash
# Download fresh snapshot at startup, then serve
set -euo pipefail

SNAPSHOT_URL="https://snapshots.eu.tzinit.org/shadownet/rolling"
SNAPSHOT_PATH="/srv/http/snapshot.rolling"

echo "[entrypoint] Downloading fresh shadownet snapshot..."
curl -sfL "$SNAPSHOT_URL" -o "$SNAPSHOT_PATH"
echo "[entrypoint] Snapshot downloaded ($(du -h "$SNAPSHOT_PATH" | cut -f1))"

exec python3 /serve.py
