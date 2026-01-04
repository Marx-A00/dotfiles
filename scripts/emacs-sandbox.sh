#!/bin/bash
# Launch isolated sandbox Emacs
# Your real Emacs session is completely untouched

SANDBOX_DIR="$HOME/.emacs-sandbox"

# Kill any existing sandbox Emacs
pkill -f "emacs.*--init-directory.*emacs-sandbox" 2>/dev/null

# Small delay to ensure process is dead
sleep 0.3

if [[ ! -d "$SANDBOX_DIR" ]]; then
    echo "Sandbox not found. Creating from current config..."
    cp -r ~/.emacs.d "$SANDBOX_DIR"
fi

echo "Launching sandbox Emacs..."
echo "This is completely isolated from your main session."
echo ""
/opt/homebrew/opt/emacs-plus@30/bin/emacs --init-directory "$SANDBOX_DIR" &
