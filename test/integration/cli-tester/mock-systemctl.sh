#!/bin/bash
# Mock systemctl for integration testing
# Simulates basic systemctl commands without requiring systemd to be running

# Log calls for debugging
echo "[mock-systemctl] $*" >&2

case "$1" in
    daemon-reload)
        # Always succeed
        exit 0
        ;;
    enable)
        # Pretend to enable services
        exit 0
        ;;
    start)
        # Pretend to start services
        exit 0
        ;;
    stop)
        # Pretend to stop services
        exit 0
        ;;
    restart)
        # Pretend to restart services
        exit 0
        ;;
    is-active)
        # Report services as inactive by default
        echo "inactive"
        exit 3
        ;;
    is-enabled)
        # Report services as disabled by default
        echo "disabled"
        exit 1
        ;;
    status)
        # Minimal status output
        echo "Unit: $2"
        echo "   Active: inactive (dead)"
        exit 3
        ;;
    list-unit-files)
        # List nothing by default
        exit 0
        ;;
    cat)
        # Cannot cat non-existent units
        exit 1
        ;;
    *)
        # Unknown commands succeed by default
        exit 0
        ;;
esac
