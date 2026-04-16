#!/usr/bin/env bash
# Record import_wizard.gif: create an external service, then import it
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

# Create a fake external octez-node systemd user service
UNIT_DIR="$HOME/.config/systemd/user"
mkdir -p "$UNIT_DIR"

cat >"$UNIT_DIR/octez-node-demo-external.service" <<'UNIT'
[Unit]
Description=Demo External Octez Node

[Service]
Type=simple
ExecStart=/home/valentin/.local/share/octez-manager/binaries/v24.0/octez-node run --data-dir /tmp/demo-ext-node --rpc-addr 127.0.0.1:18732 --net-addr 0.0.0.0:19732 --network shadownet
Restart=on-failure

[Install]
WantedBy=default.target
UNIT

systemctl --user daemon-reload
# Don't start it — import should detect it as stopped

NAME="import_wizard"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1

# Expand unmanaged instances section (press 'u')
k "$SESS" "u"
pause 2

# Navigate down to node-demo-external (10 downs from top)
# 4 managed instances + 6 "Add new" buttons = 10 items, then first unmanaged
go_down "$SESS" 10
pause 0.5

# Open action menu
k "$SESS" Enter
pause 1

# Select "Import to Managed" (1 down from Details)
go_down "$SESS" 1
pause 0.3
k "$SESS" Enter
pause 2

# Step 1: Select Service — already selected, press Enter
k "$SESS" Enter
pause 2

# Step 2: Configure — press Enter to accept defaults
k "$SESS" Enter
pause 2

# Step 3: Review & Confirm — press Enter to confirm
k "$SESS" Enter
pause 5

stop_and_convert "$NAME"

# Cleanup
"$OM" instance node-demo-external stop 2>/dev/null || true
"$OM" instance node-demo-external purge 2>/dev/null || true
rm -f "$UNIT_DIR/octez-node-demo-external.service"
systemctl --user daemon-reload
echo "Cleaned up node-demo-external"
