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
  # Wrap dashboard JSON in the required format
  printf '{"dashboard":' > /tmp/import.json
  cat "$dash" >> /tmp/import.json
  printf ',"overwrite":true}' >> /tmp/import.json
  
  curl -X POST -H "Content-Type: application/json" \
    -u "${GF_SECURITY_ADMIN_USER}:${GF_SECURITY_ADMIN_PASSWORD}" \
    http://localhost:3000/api/dashboards/db -d @/tmp/import.json || true
  rm -f /tmp/import.json
done

echo "Dashboard import complete"
