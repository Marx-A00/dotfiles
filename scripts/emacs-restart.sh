#!/bin/bash
# Restart Emacs daemon and open a new client frame

EMACSCLIENT="/opt/homebrew/opt/emacs-plus@30/bin/emacsclient"

echo "Checking for non-empty scratch buffers..."

# Check if there are non-empty scratch buffers and auto-save them
# This runs the save logic directly since kill-emacs-query-functions
# can't prompt interactively from a daemon without a frame
$EMACSCLIENT -e '(progn
  (let ((save-dir (expand-file-name "scratch-saves/" user-emacs-directory)))
    (unless (file-exists-p save-dir)
      (make-directory save-dir t))
    (dolist (buf (buffer-list))
      (when (and (string-match-p "\\*scratch\\*" (buffer-name buf))
                 (with-current-buffer buf
                   (and (> (buffer-size) 0)
                        (not (string-match-p "^# Clear your mind" (buffer-string))))))
        (with-current-buffer buf
          (write-file (expand-file-name
                       (format-time-string "scratch-%Y%m%d-%H%M%S.org")
                       save-dir))))))
  t)' 2>/dev/null

echo "Stopping Emacs daemon..."
$EMACSCLIENT -e '(kill-emacs)' 2>/dev/null || {
    echo "Graceful shutdown failed, force killing..."
    launchctl unload ~/Library/LaunchAgents/com.marcosandrade.emacsdaemon.plist 2>/dev/null
    pkill -f "emacs.*daemon" 2>/dev/null
}
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
