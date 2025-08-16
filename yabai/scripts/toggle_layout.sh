#!/bin/bash
# Get current layout of focused space
current_space=$(yabai -m query --spaces --space | jq '.index')
current_layout=$(yabai -m query --spaces --space | jq -r '.type')

if [ "$current_layout" = "bsp" ]; then
    yabai -m config --space "$current_space" layout float
    label="float"
elif [ "$current_layout" = "float" ]; then
    yabai -m config --space "$current_space" layout stack
    label="stack"
else
    yabai -m config --space "$current_space" layout bsp
    label="bsp"
fi

case "$label" in
  bsp) icon="" ;;
  float) icon="󰊠" ;;
  stack) icon="" ;;
esac

sketchybar --set yabai_layout icon="$icon" label="$label"

echo "$label" > ~/.config/yabai/current_layout

sketchybar --trigger layout_change
