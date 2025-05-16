source "$HOME/.config/sketchybar/icon_map.sh"

TITLE=$(yabai -m query --windows --window)
if [ "$TITLE" = "" ]; then
    sketchybar --set title_proxy label=""
    sketchybar --animate circ 15 --set title y_offset=70
    sketchybar --set title label="" icon=""
else
    APP="$(echo $TITLE | jq -r '.app')"

    __icon_map "${APP}"
    ICON="${icon_result}"
    # LABEL="$(echo $TITLE | jq -r '.app')"
    # if [ "$LABEL" = "" ]; then
    #     LABEL="$(echo $TITLE | jq -r '.app')"
    # fi
    if [ "$(sketchybar --query title_proxy | jq -r '.label.value')" != "$APP" ]; then
        sketchybar --set title_proxy label="$APP"
        sketchybar --animate circ 15 --set title y_offset=70            \
                   --animate circ 10  --set title y_offset=7            \
                   --animate circ 15 --set title y_offset=0
        
        sketchybar --set title label="$APP" icon="$ICON"
    fi
fi
