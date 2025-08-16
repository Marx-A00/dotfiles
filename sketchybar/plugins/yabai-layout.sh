#!/bin/bash
# -*- mode: sh -*-

label=$(cat ~/.config/yabai/current_layout)

case "$label" in
  bsp)
    icon=""
    ;;
  float)
    icon="󰊠"
    ;;
  stack)
    icon=""
    ;;
  *)
    icon="?"
    ;;
esac

sketchybar --set yabai_layout icon="$icon" label="$label"