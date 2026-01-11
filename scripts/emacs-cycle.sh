#!/bin/bash
# Cycle through Emacs windows with yabai

# Get all Emacs window IDs, sorted for consistent ordering
ids=$(yabai -m query --windows | jq -r '[.[] | select(.app=="Emacs")] | sort_by(.id) | .[].id')
[[ -z "$ids" ]] && exit 0

# Convert to array
IFS=$'\n' read -r -d '' -a id_arr <<< "$ids"

# Get currently focused window
focused=$(yabai -m query --windows | jq -r '.[] | select(."has-focus"==true) | .id')

# Find current index and calculate next
count=${#id_arr[@]}
next_idx=0
for ((i=0; i<count; i++)); do
  if [[ "${id_arr[$i]}" == "$focused" ]]; then
    next_idx=$(( (i + 1) % count ))
    break
  fi
done

yabai -m window --focus "${id_arr[$next_idx]}"
