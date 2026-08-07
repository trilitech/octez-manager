#!/bin/bash
# Download fresh snapshot at startup, then serve
set -euo pipefail

SNAPSHOT_URL="https://snapshots.eu.tzinit.org/shadownet/rolling"
SNAPSHOT_PATH="/srv/http/snapshot.rolling"
MAX_RETRIES=3
RETRY_DELAY=5

echo "[entrypoint] Downloading fresh shadownet snapshot..."

# Retry logic for snapshot download
for attempt in $(seq 1 $MAX_RETRIES); do
  echo "[entrypoint] Download attempt $attempt of $MAX_RETRIES..."
  
  if curl -sfL --connect-timeout 30 --max-time 300 "$SNAPSHOT_URL" -o "$SNAPSHOT_PATH"; then
    echo "[entrypoint] Snapshot downloaded successfully ($(du -h "$SNAPSHOT_PATH" | cut -f1))"
    break
  else
    exit_code=$?
    echo "[entrypoint] Download failed with exit code $exit_code"
    
    if [ $attempt -lt $MAX_RETRIES ]; then
      echo "[entrypoint] Retrying in ${RETRY_DELAY}s..."
      sleep $RETRY_DELAY
    else
      echo "[entrypoint] All download attempts failed. Exiting."
      exit $exit_code
    fi
  fi
done

exec python3 /serve.py
