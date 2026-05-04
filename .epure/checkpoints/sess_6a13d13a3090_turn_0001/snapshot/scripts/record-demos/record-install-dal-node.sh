#!/usr/bin/env bash
# Record install_dal_node.gif: install DAL node connected to existing node
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

NAME="install_dal_node"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1

# Navigate to "Add new DAL Node" (7 downs from top)
go_down "$SESS" 7
pause 0.5
k "$SESS" Enter # Open form
pause 1.5

# Select Node (first field)
k "$SESS" Enter # Open node selector
pause 0.8
k "$SESS" Enter # Select test-octez-node
pause 1

# Navigate to Instance Name
# Node(0), App Bin Dir(1), DAL RPC Addr(2), DAL P2P Addr(3), DAL Data Dir(4),
# Extra Args(5), Service User(6), Enable on Boot(7), Start Now(8),
# Instance Name(9), Group(10), Confirm(11)
go_down "$SESS" 9
pause 0.3
k "$SESS" Enter # Open text editor
pause 0.3
for i in {1..40}; do tmux send-keys -t "$SESS" BSpace; done
sleep 0.2
tmux send-keys -t "$SESS" "demo-dal"
sleep 0.5
k "$SESS" Enter # Confirm
pause 0.5

# Confirm & Install (2 downs)
go_down "$SESS" 2
pause 0.5
k "$SESS" Enter
pause 5

stop_and_convert "$NAME"

"$OM" instance demo-dal stop 2>/dev/null || true
"$OM" instance demo-dal purge --yes 2>/dev/null || true
echo "Cleaned up demo-dal"
