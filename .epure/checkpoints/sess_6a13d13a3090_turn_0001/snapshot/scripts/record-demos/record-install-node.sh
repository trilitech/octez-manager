#!/usr/bin/env bash
# Record install_node.gif: full install wizard for a node
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

NAME="install_node"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1

# Navigate to "Add new Node" (4 downs from top)
go_down "$SESS" 4
pause 0.5
k "$SESS" Enter # Open form
pause 1.5

# Show the form as-is for a moment
pause 1

# Navigate to Snapshot Import (2 downs from Network)
go_down "$SESS" 2
pause 0.3
k "$SESS" Enter # Open snapshot selector
pause 0.8
k "$SESS" Up    # Select "None (manual sync)"
k "$SESS" Enter # Confirm
pause 0.5

# Navigate to Instance Name (9 downs from Snapshot Import when snapshot=None)
go_down "$SESS" 9
pause 0.3
k "$SESS" Enter # Open text editor
pause 0.3
# Clear existing text
for i in {1..30}; do tmux send-keys -t "$SESS" BSpace; done
sleep 0.2
tmux send-keys -t "$SESS" "demo-node"
sleep 0.5
k "$SESS" Enter # Confirm name
pause 0.5

# Navigate to Confirm & Install (2 downs)
go_down "$SESS" 2
pause 0.5
k "$SESS" Enter # Submit!
pause 5         # Wait for install to complete

stop_and_convert "$NAME"

# Cleanup: purge the demo instance
"$OM" instance demo-node stop 2>/dev/null || true
"$OM" instance demo-node purge --yes 2>/dev/null || true
echo "Cleaned up demo-node"
