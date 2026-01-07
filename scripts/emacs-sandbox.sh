#!/bin/bash
# Launch isolated sandbox Emacs
# Your real Emacs session is completely untouched
#
# Usage:
#   emacs-sandbox.sh          # Launch sandbox (create if missing)
#   emacs-sandbox.sh --fresh  # Nuke sandbox and resync from main config
#   emacs-sandbox.sh --test   # Launch sandbox and auto-run test environment

SANDBOX_DIR="$HOME/.emacs-sandbox"
AUTO_TEST=""

# Kill any existing sandbox Emacs
pkill -f "emacs.*--init-directory.*emacs-sandbox" 2>/dev/null

# Small delay to ensure process is dead
sleep 0.3

# Handle flags
for arg in "$@"; do
    case $arg in
        --fresh)
            echo "Nuking sandbox and resyncing from main config..."
            rm -rf "$SANDBOX_DIR"
            ;;
        --test)
            AUTO_TEST="yes"
            ;;
    esac
done

if [[ ! -d "$SANDBOX_DIR" ]]; then
    echo "Creating sandbox from current config..."
    cp -r ~/.emacs.d "$SANDBOX_DIR"
    
    # Re-apply sandbox-specific changes
    # 1. Enable title bar (comment out undecorated-round)
    sed -i '' "s/(add-to-list 'default-frame-alist '(undecorated-round . t))/;; SANDBOX: (add-to-list 'default-frame-alist '(undecorated-round . t))/" "$SANDBOX_DIR/early-init.el"
    
    # 2. Create sandbox indicator file
    cat > "$SANDBOX_DIR/sandbox-indicator.el" << 'EOF'
;;; sandbox-indicator.el --- Visual indicator for sandbox Emacs -*- lexical-binding: t; -*-

;; Title bar shows SANDBOX
(setq frame-title-format '("SANDBOX - " "%b"))

;; Doom modeline custom segment
(with-eval-after-load 'doom-modeline
  (doom-modeline-def-segment sandbox
    "Sandbox indicator segment."
    (propertize " SANDBOX " 'face '(:background "#ff6b6b" :foreground "white" :weight bold)))
  
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number sandbox modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time)))

(message "Running in SANDBOX mode - your real config is safe!")

(provide 'sandbox-indicator)
;;; sandbox-indicator.el ends here
EOF
    
    # 3. Add loader to init.el
    echo '' >> "$SANDBOX_DIR/init.el"
    echo ';; Sandbox visual indicator' >> "$SANDBOX_DIR/init.el"
    echo '(load (expand-file-name "sandbox-indicator.el" user-emacs-directory) t)' >> "$SANDBOX_DIR/init.el"
    
    echo "Sandbox created with visual indicators."
fi

echo "Launching sandbox Emacs..."
echo "This is completely isolated from your main session."
echo ""
if [[ -n "$AUTO_TEST" ]]; then
    echo "Auto-running test environment..."
    /opt/homebrew/opt/emacs-plus@30/bin/emacs --init-directory "$SANDBOX_DIR" \
        --eval "(run-with-timer 1 nil #'mr-x/sandbox-test-env)" &
else
    /opt/homebrew/opt/emacs-plus@30/bin/emacs --init-directory "$SANDBOX_DIR" &
fi
