#!/bin/bash
# -*- mode: sh -*-
# Create the org-clock item in sketchybar (left side, after persp)

GRUVBOX_YELLOW=0xFFfabd2f
DIM=0x60FFFFFF

# Clean up old items
sketchybar --remove emacs_clock 2>/dev/null

# Clock icon + task label — hidden by default
sketchybar --add item emacs_clock left \
           --set emacs_clock \
             drawing=off \
             icon=󱎫 \
             icon.color=$GRUVBOX_YELLOW \
             icon.padding_left=10 \
             icon.padding_right=4 \
             label="" \
             label.color=0xFFFFFFFF \
             label.padding_left=4 \
             label.padding_right=10 \
             label.max_chars=40 \
             background.color=0x801e1e2e \
             background.corner_radius=15 \
             background.height=30 \
             background.border_width=1 \
             background.border_color=0x40FFFFFF \
             background.padding_left=18 \
             blur_radius=2 \
             script='bash ./plugins/emacs-clock.sh' \
             updates=on

# Custom event for Emacs to push clock state
sketchybar --add event emacs_clock_update
sketchybar --subscribe emacs_clock front_app_switched emacs_clock_update
