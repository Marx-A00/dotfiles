#!/bin/bash

# Define the gruvbox green color
GRUVBOX_GREEN=0xFFb8bb26

# Update function — white underline for selected space
update() {
  if [ "$SELECTED" = "true" ]; then
    sketchybar --set $NAME background.drawing=on \
                          icon.color=0xFFFFFFFF
  else
    sketchybar --set $NAME background.drawing=off \
                          icon.color=0xFFFFFFFF
  fi
}

# Handle clicks if needed
case "$SENDER" in
  "space_change")
    update
    ;;
  *)
    update
    ;;
esac