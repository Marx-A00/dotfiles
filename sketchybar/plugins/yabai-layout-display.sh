#!/bin/bash
# -*- mode: sh -*-

# This script is called for display-specific layout indicators
# Extract display number from NAME (e.g., yabai_layout.1 -> 1)
DISPLAY_INDEX="${NAME##*.}"

# Get the spaces on this display and pick the focused one
# If the display has focus, use the current space; otherwise use the first space
DISPLAY_HAS_FOCUS=$(yabai -m query --displays --display $DISPLAY_INDEX | jq -r '."has-focus"')

if [ "$DISPLAY_HAS_FOCUS" = "true" ]; then
  # This display has focus, so get the current space
  SPACE_INDEX=$(yabai -m query --spaces --space | jq -r '.index')
else
  # This display doesn't have focus, get its first space
  SPACE_INDEX=$(yabai -m query --displays --display $DISPLAY_INDEX | jq -r '.spaces[0]')
fi

# Query the layout of that space
if [ "$SPACE_INDEX" != "null" ] && [ "$SPACE_INDEX" != "" ]; then
  label=$(yabai -m query --spaces --space $SPACE_INDEX | jq -r '.type')
else
  label="?"
fi

case "$label" in
  bsp)
    icon="󰕰"  # Grid icon for tiling
    ;;
  float)
    icon="󰊠"  # Ghost stays! 
    ;;
  stack)
    icon="󰁉"  # Layers icon for stack
    ;;
  *)
    icon="?"
    ;;
esac

# Update the display-specific item
sketchybar --set "$NAME" icon="$icon" label="$label"