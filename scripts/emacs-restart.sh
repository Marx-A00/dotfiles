#!/bin/bash
# Restart Emacs daemon and open a new client frame

echo "Stopping Emacs daemon..."
launchctl unload ~/Library/LaunchAgents/com.marcosandrade.emacsdaemon.plist 2>/dev/null
pkill -f "emacs.*daemon" 2>/dev/null
sleep 1

echo "Starting Emacs daemon..."
launchctl load ~/Library/LaunchAgents/com.marcosandrade.emacsdaemon.plist

echo "Waiting for daemon..."
EMACSCLIENT="/opt/homebrew/opt/emacs-plus@30/bin/emacsclient"
MAX_ATTEMPTS=30
ATTEMPT=0

while [ $ATTEMPT -lt $MAX_ATTEMPTS ]; do
    if $EMACSCLIENT -e '(+ 1 1)' &>/dev/null; then
        echo "Daemon ready. Opening frame..."
        $EMACSCLIENT -c -n
        exit 0
    fi
    sleep 1
    ATTEMPT=$((ATTEMPT + 1))
done

echo "Daemon failed to start"
exit 1
