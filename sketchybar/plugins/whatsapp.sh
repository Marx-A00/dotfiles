#!/bin/sh
# WhatsApp unread count plugin
# Reads dock badge via Accessibility API (AXStatusLabel)
# Requires: System Settings > Privacy & Security > Accessibility > sketchybar enabled
# Requires: System Settings > Notifications > WhatsApp > Badge app icon enabled

COUNT=$(osascript -e '
tell application "System Events"
    tell process "Dock"
        try
            set badgeValue to value of attribute "AXStatusLabel" of UI element "WhatsApp" of list 1
            if badgeValue is missing value then
                return 0
            else
                return badgeValue
            end if
        on error
            return 0
        end try
    end tell
end tell' 2>/dev/null)

if [ -z "$COUNT" ] || [ "$COUNT" -eq 0 ] 2>/dev/null; then
  sketchybar --set "$NAME" drawing=off
else
  sketchybar --set "$NAME" drawing=on label="$COUNT"
fi
