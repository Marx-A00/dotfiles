#!/bin/bash
# -*- mode: sh -*-

# This script dynamically creates layout indicators for each connected display

# First, remove all existing yabai_layout items (up to 10 displays should be enough)
for i in {1..10}; do
  sketchybar --remove yabai_layout.$i 2>/dev/null
done

# Get all connected displays
DISPLAYS=$(yabai -m query --displays | jq -r '.[].index')

# Create a layout indicator for each display
for display_index in $DISPLAYS; do
  sketchybar --add item yabai_layout.$display_index left \
             --set yabai_layout.$display_index \
               display=$display_index \
               script='bash ./plugins/yabai-layout-display.sh' \
               updates=on
  
  # Subscribe to events
  sketchybar --subscribe yabai_layout.$display_index layout_change space_change
done

# Initial update to set the icons
sketchybar --update