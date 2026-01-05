#!/bin/bash

# Define the gruvbox green color
GRUVBOX_GREEN=0xFFb8bb26

# Update function to set icon highlight based on selection
update() {
  if [ "$SELECTED" = "true" ]; then
    sketchybar --animate tanh 20 --set $NAME icon.background.color=$GRUVBOX_GREEN \
                                            icon.color=0xFF1d2021
  else
    sketchybar --set $NAME icon.background.color=0x00000000 \
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