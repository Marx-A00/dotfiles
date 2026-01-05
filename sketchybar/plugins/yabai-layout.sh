#!/bin/bash
# -*- mode: sh -*-

# This script is called by sketchybar when events occur
# $SENDER contains the event that triggered the script
# $NAME contains the name of the item

# Query the current space's layout directly from yabai
label=$(yabai -m query --spaces --space | jq -r '.type')

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

# Update the item - use $NAME if provided, otherwise yabai_layout
ITEM_NAME="${NAME:-yabai_layout}"
sketchybar --set "$ITEM_NAME" icon="$icon" label="$label"