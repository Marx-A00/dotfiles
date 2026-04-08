#!/bin/bash
# Dedicated org-agenda frame
# Opens custom projects+agenda view ("c") in a new emacsclient frame

# If an agenda frame already exists, focus it instead of creating a new one
EXISTING=$(yabai -m query --windows | jq -r '.[] | select(.app == "Emacs" and (.title | contains("Agenda"))) | .id' | head -1)

if [[ -n "$EXISTING" ]]; then
    yabai -m window "$EXISTING" --focus
else
    emacsclient --no-wait --create-frame --eval \
      '(progn
         (set-frame-parameter nil (quote title) "Agenda")
         (setq frame-title-format "Agenda")
         (org-agenda nil "c")
         (delete-other-windows)
         (set-window-dedicated-p (selected-window) t))'
fi
