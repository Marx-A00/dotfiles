#!/bin/bash
# Wrapper script for Emacs daemon startup
# Kills any existing Emacs processes to prevent zombies, then spawns a frame

EMACS="/opt/homebrew/opt/emacs-plus@30/bin/emacs"
EMACSCLIENT="/opt/homebrew/opt/emacs-plus@30/bin/emacsclient"

# Kill any existing Emacs processes (daemon or otherwise)
pkill -9 -f "emacs" 2>/dev/null || true

# Clean up any stale socket files
rm -f /tmp/emacs$(id -u)/server 2>/dev/null || true

# Small delay to ensure everything is dead
sleep 1

# Start the daemon in background
$EMACS --daemon

# Wait for daemon to be ready, then spawn a frame
MAX_ATTEMPTS=30
ATTEMPT=0

while [ $ATTEMPT -lt $MAX_ATTEMPTS ]; do
    if $EMACSCLIENT -e '(+ 1 1)' &>/dev/null; then
        $EMACSCLIENT -c -n
        exit 0
    fi
    sleep 1
    ATTEMPT=$((ATTEMPT + 1))
done

echo "Emacs daemon did not start in time"
exit 1
