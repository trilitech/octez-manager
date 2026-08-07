#!/usr/bin/env bash
# Record install_baker.gif: install baker connected to existing node
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

NAME="install_baker"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1

# Navigate to "Add new Baker" (5 downs from top)
go_down "$SESS" 5
pause 0.5
k "$SESS" Enter # Open form
pause 1.5

# Show form, then select Parent Node (first field, already selected)
pause 0.8
k "$SESS" Enter # Open node selector
pause 0.8
# Select the existing test-octez-node (should be first option)
k "$SESS" Enter # Confirm node selection
pause 1

# Navigate to Instance Name
# Parent Node(0), DAL Node(1), App Bin Dir(2), Baker Base Dir(3), Remote Signer(4),
# Delegates(5), Liq Baking Vote(6), Node Endpoint(7), Node Data Dir(8),
# Extra Nodes(9), Extra Args(10), Service User(11), Enable on Boot(12),
# Start Now(13), Instance Name(14), Group(15), Confirm(16)
go_down "$SESS" 14
pause 0.3
k "$SESS" Enter # Open text editor
pause 0.3
for i in {1..40}; do tmux send-keys -t "$SESS" BSpace; done
sleep 0.2
tmux send-keys -t "$SESS" "demo-baker"
sleep 0.5
k "$SESS" Enter # Confirm
pause 0.5

# Navigate to Confirm & Install (2 downs)
go_down "$SESS" 2
pause 0.5
k "$SESS" Enter # Submit
pause 5

stop_and_convert "$NAME"

"$OM" instance demo-baker stop 2>/dev/null || true
"$OM" instance demo-baker purge --yes 2>/dev/null || true
echo "Cleaned up demo-baker"
