#!/bin/bash
set -e

# Wait for Grafana to be ready
until curl -sf http://localhost:3000/api/health > /dev/null 2>&1; do
  echo "Waiting for Grafana..."
  sleep 2
done

# Import each dashboard
for dash in /etc/grafana/dashboards/*.json; do
  echo "Importing dashboard: $(basename "$dash")"
  # Wrap dashboard JSON in the required format using mktemp
  tmpfile=$(mktemp /tmp/import.json.XXXXXX)
  printf '{"dashboard":' > "$tmpfile"
  cat "$dash" >> "$tmpfile"
  printf ',"overwrite":true}' >> "$tmpfile"
  
  curl -X POST -H "Content-Type: application/json" \
    -u "${GF_SECURITY_ADMIN_USER}:${GF_SECURITY_ADMIN_PASSWORD}" \
    http://localhost:3000/api/dashboards/db -d @"$tmpfile" || true
  rm -f "$tmpfile"
done

echo "Dashboard import complete"
