#!/usr/bin/env bash
# Record install_accuser.gif: install accuser connected to existing node
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

NAME="install_accuser"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1

# Navigate to "Add new Accuser" (6 downs from top)
go_down "$SESS" 6
pause 0.5
k "$SESS" Enter # Open form
pause 1.5

# Select Node (first field)
k "$SESS" Enter # Open node selector
pause 0.8
k "$SESS" Enter # Select test-octez-node
pause 1

# Navigate to Instance Name
# Node(0), App Bin Dir(1), Base Dir(2), Extra Args(3), Service User(4),
# Enable on Boot(5), Start Now(6), Instance Name(7), Group(8), Confirm(9)
go_down "$SESS" 7
pause 0.3
k "$SESS" Enter # Open text editor
pause 0.3
for i in {1..40}; do tmux send-keys -t "$SESS" BSpace; done
sleep 0.2
tmux send-keys -t "$SESS" "demo-accuser"
sleep 0.5
k "$SESS" Enter # Confirm
pause 0.5

# Confirm & Install (2 downs)
go_down "$SESS" 2
pause 0.5
k "$SESS" Enter
pause 5

stop_and_convert "$NAME"

"$OM" instance demo-accuser stop 2>/dev/null || true
"$OM" instance demo-accuser purge --yes 2>/dev/null || true
echo "Cleaned up demo-accuser"
