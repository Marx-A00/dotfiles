#!/bin/sh
# iMessage unread count plugin
# Requires Full Disk Access for the sketchybar process (or /bin/zsh / sqlite3)
# Grant in: System Settings > Privacy & Security > Full Disk Access

DB="$HOME/Library/Messages/chat.db"

if [ ! -r "$DB" ]; then
  sketchybar --set "$NAME" drawing=off
  exit 0
fi

# Dates are nanoseconds since 2001-01-01; filter to last 30 days to avoid stale unread flags
THRESHOLD="(strftime('%s','now') - strftime('%s','2001-01-01') - 30*86400) * 1000000000"
COUNT=$(sqlite3 "$DB" "SELECT COUNT(DISTINCT message.ROWID) FROM message WHERE message.is_read = 0 AND message.is_from_me = 0 AND message.date > $THRESHOLD;" 2>/dev/null)

if [ -z "$COUNT" ] || [ "$COUNT" -eq 0 ] 2>/dev/null; then
  sketchybar --set "$NAME" drawing=off
else
  sketchybar --set "$NAME" drawing=on label="$COUNT"
fi
