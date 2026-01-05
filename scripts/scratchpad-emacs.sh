#!/bin/bash

# Native yabai scratchpad Emacs script
# Creates or toggles Emacs scratchpad windows

SCRATCHPAD_NAME="${1:-scratch}"

case "$SCRATCHPAD_NAME" in
    "scratch")
        # Check if scratchpad exists
        if yabai -m query --windows | jq -e '.[] | select(.scratchpad == "scratch")' > /dev/null 2>&1; then
            # Toggle existing scratchpad
            yabai -m window --toggle scratch
        else
            # Create new scratch window
            emacsclient --no-wait --create-frame --eval '(progn (setq frame-title-format "SCRATCHPAD") (switch-to-buffer "*scratch*"))' &
            
            # Wait a moment for window to appear, then assign scratchpad
            sleep 1
            WINDOW_ID=$(yabai -m query --windows | jq -r '.[] | select(.app == "Emacs" and (.title | contains("SCRATCHPAD"))) | .id' | head -1)
            if [[ -n "$WINDOW_ID" ]]; then
                yabai -m window "$WINDOW_ID" --scratchpad scratch
            fi
        fi
        ;;
        
    "scratch-org")
        # Check if org scratchpad exists  
        if yabai -m query --windows | jq -e '.[] | select(.scratchpad == "scratch-org")' > /dev/null 2>&1; then
            # Toggle existing scratchpad
            yabai -m window --toggle scratch-org
        else
            # Create new org scratch window
            emacsclient --no-wait --create-frame --eval '(progn (setq frame-title-format "SCRATCHPAD-ORG") (switch-to-buffer "*SCRATCH-ORG*") (org-mode) (insert "#+TITLE: Scratch Pad\n#+DATE: " (format-time-string "%Y-%m-%d") "\n\n* Quick Notes\n\n") (goto-char (point-max)))' &
            
            # Wait a moment for window to appear, then assign scratchpad
            sleep 1
            WINDOW_ID=$(yabai -m query --windows | jq -r '.[] | select(.app == "Emacs" and (.title | contains("SCRATCHPAD-ORG"))) | .id' | head -1)
            if [[ -n "$WINDOW_ID" ]]; then
                yabai -m window "$WINDOW_ID" --scratchpad scratch-org
            fi
        fi
        ;;
        
    *)
        echo "Usage: $0 [scratch|scratch-org]"
        exit 1
        ;;
esac