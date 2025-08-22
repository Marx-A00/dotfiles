#!/bin/bash
# Get current layout of focused space
current_space=$(yabai -m query --spaces --space | jq '.index')
current_layout=$(yabai -m query --spaces --space | jq -r '.type')
current_display=$(yabai -m query --spaces --space | jq '.display')

if [ "$current_layout" = "bsp" ]; then
    yabai -m config --space "$current_space" layout float
elif [ "$current_layout" = "float" ]; then
    yabai -m config --space "$current_space" layout stack
else
    yabai -m config --space "$current_space" layout bsp
fi

# Trigger update for all displays (each will query its own current space)
sketchybar --trigger layout_change
sketchybar --update