#!/bin/bash
# Get current layout of focused space
current_space=$(yabai -m query --spaces --space | jq '.index')
current_layout=$(yabai -m query --spaces --space | jq -r '.type')

if [ "$current_layout" = "bsp" ]; then
    yabai -m config --space "$current_space" layout float
else
    yabai -m config --space "$current_space" layout bsp
fi
