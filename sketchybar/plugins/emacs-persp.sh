#!/bin/bash
# -*- mode: sh -*-
# Emacs perspective display plugin for sketchybar
# One slot item per perspective name — positions stay fixed,
# background highlight + brightness move to the active one

EMACSCLIENT="/opt/homebrew/opt/emacs-plus@30/bin/emacsclient"
WHITE=0xFFFFFFFF
DIM=0x60FFFFFF
HIGHLIGHT=0x80fb4934

hide_all() {
  sketchybar --set emacs_persp_pad_l drawing=off 2>/dev/null
  sketchybar --set emacs_persp_pad_r drawing=off 2>/dev/null
  for i in {1..10}; do
    sketchybar --set emacs_persp_slot.$i drawing=off 2>/dev/null
  done
}

# Get all Emacs windows, excluding scratchpads
EMACS_WINDOWS=$(yabai -m query --windows | jq '[.[] | select(.app == "Emacs" and .scratchpad == "")]' 2>/dev/null)
WINDOW_COUNT=$(echo "$EMACS_WINDOWS" | jq 'length' 2>/dev/null)

if [ -z "$WINDOW_COUNT" ] || [ "$WINDOW_COUNT" -eq 0 ]; then
  hide_all
  exit 0
fi

# Find display: prefer focused Emacs window, fall back to any
FOCUSED=$(yabai -m query --windows --window 2>/dev/null)
FOCUSED_APP=$(echo "$FOCUSED" | jq -r '.app' 2>/dev/null)

if [ "$FOCUSED_APP" = "Emacs" ]; then
  DISPLAY_IDX=$(echo "$FOCUSED" | jq -r '.display')
else
  DISPLAY_IDX=$(echo "$EMACS_WINDOWS" | jq -r '.[0].display')
fi

# Multiple frames on same display — ambiguous
FRAMES_ON_DISPLAY=$(echo "$EMACS_WINDOWS" | jq "[.[] | select(.display == $DISPLAY_IDX)] | length")
if [ "$FRAMES_ON_DISPLAY" -gt 1 ]; then
  sketchybar --set emacs_persp_slot.1 \
    drawing=on display=$DISPLAY_IDX label="?" label.color=$WHITE label.padding_right=12
  for i in $(seq 2 10); do
    sketchybar --set emacs_persp_slot.$i drawing=off 2>/dev/null
  done
  exit 0
fi

# Get perspective info
TITLE=$(echo "$EMACS_WINDOWS" | jq -r ".[] | select(.display == $DISPLAY_IDX) | .title" | head -1)

if [ -n "$EMACS_PERSP_NAME" ] && [ "$TITLE" = "$EMACS_PERSP_FRAME_TITLE" ]; then
  ACTIVE="$EMACS_PERSP_NAME"
  ALL_CSV="$EMACS_PERSP_ALL"
else
  ACTIVE=$($EMACSCLIENT --eval "(when (fboundp 'mr-x/sketchybar-persp-name-for-title) (mr-x/sketchybar-persp-name-for-title \"$TITLE\"))" 2>/dev/null | tr -d '"')
  ALL_CSV=$($EMACSCLIENT --eval "(when (fboundp 'persp-names) (mapconcat #'identity (persp-names) \",\"))" 2>/dev/null | tr -d '"')
  [ "$ACTIVE" = "nil" ] || [ -z "$ACTIVE" ] && ACTIVE="main"
fi

# Fill slots with perspective names — batch into a single sketchybar call
# (spacers included in the batch below)
IFS=',' read -ra NAMES <<< "$ALL_CSV"
COUNT=${#NAMES[@]}

BATCH_ARGS=()
for i in $(seq 1 10); do
  idx=$((i - 1))
  if [ $idx -lt $COUNT ]; then
    name="${NAMES[$idx]}"
    if [ "$name" = "$ACTIVE" ]; then
      color=$WHITE
      bg=$HIGHLIGHT
      bg_drawing=on
    else
      color=$DIM
      bg=0x00000000
      bg_drawing=off
    fi

    BATCH_ARGS+=(--set emacs_persp_slot.$i
      drawing=on
      display=$DISPLAY_IDX
      "label=$name"
      label.color=$color
      label.padding_left=6
      label.padding_right=6
      background.drawing=$bg_drawing
      background.color=$bg)
  else
    BATCH_ARGS+=(--set emacs_persp_slot.$i drawing=off)
  fi
done

sketchybar \
  --set emacs_persp_pad_l drawing=on display=$DISPLAY_IDX \
  --set emacs_persp_pad_r drawing=on display=$DISPLAY_IDX \
  "${BATCH_ARGS[@]}" 2>/dev/null
