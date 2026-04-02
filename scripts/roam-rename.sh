#!/usr/bin/env zsh
# Bulk rename org-roam files: strip timestamp prefix, convert underscores to hyphens.
# Also updates file: links inside notes that reference old filenames.
#
# Usage:
#   ./roam-rename.sh           # dry run (preview changes)
#   ./roam-rename.sh --apply   # actually rename + update links + rebuild db

set -euo pipefail

NOTES_DIR="$HOME/roaming/notes"
DRY_RUN=true
MAPPING_FILE=$(mktemp)

if [[ "${1:-}" == "--apply" ]]; then
  DRY_RUN=false
fi

generate_slug() {
  local title="$1"
  echo "$title" | tr '[:upper:]' '[:lower:]' | sed 's/[^[:alnum:]]/-/g; s/--*/-/g; s/^-//; s/-$//'
}

echo "=== Scanning for files to rename ==="
echo ""

# Find all org files matching the timestamp pattern (recursively, excluding journal/)
find "$NOTES_DIR" -name '[0-9]*-*.org' -not -path '*/journal/*' -type f | while IFS= read -r filepath; do
  dir=$(dirname "$filepath")
  fname=$(basename "$filepath")

  # Extract title from file
  title=$(grep -m1 '#+title:' "$filepath" | sed 's/#+title: *//' | tr -d '\r')

  if [[ -z "$title" ]]; then
    echo "SKIP (no title): $fname"
    continue
  fi

  slug=$(generate_slug "$title")
  new_fname="${slug}.org"

  # Get relative dir from NOTES_DIR
  reldir=${dir#$NOTES_DIR}
  reldir=${reldir#/}

  if [[ -z "$reldir" ]]; then
    old_rel="$fname"
    new_rel="$new_fname"
  else
    old_rel="$reldir/$fname"
    new_rel="$reldir/$new_fname"
  fi

  if [[ "$fname" == "$new_fname" ]]; then
    continue
  fi

  # Check for collision (existing file or already planned in this run)
  if [[ -e "$dir/$new_fname" ]] || grep -q "	${new_rel}$" "$MAPPING_FILE" 2>/dev/null; then
    # Append a numeric suffix to disambiguate
    counter=2
    base_slug="$slug"
    while [[ -e "$dir/${base_slug}-${counter}.org" ]] || grep -q "	${new_rel%%.org}-${counter}.org$" "$MAPPING_FILE" 2>/dev/null; do
      ((counter++))
    done
    new_fname="${base_slug}-${counter}.org"
    if [[ -z "$reldir" ]]; then
      new_rel="$new_fname"
    else
      new_rel="$reldir/$new_fname"
    fi
    echo "  COLLISION resolved: $old_rel -> $new_rel"
  fi

  echo "$old_rel	$new_rel" >> "$MAPPING_FILE"
  echo "  $old_rel -> $new_rel"
done

echo ""
total=$(wc -l < "$MAPPING_FILE" | tr -d ' ')
echo "=== Total files to rename: $total ==="
echo ""

if [[ "$total" -eq 0 ]]; then
  echo "Nothing to rename!"
  rm -f "$MAPPING_FILE"
  exit 0
fi

# Phase 1: Update file: links inside all org files
echo "=== Updating file: links inside notes ==="
while IFS=$'\t' read -r old_rel new_rel; do
  old_fname=$(basename "$old_rel")
  new_fname=$(basename "$new_rel")

  if $DRY_RUN; then
    matches=$(grep -rl "$old_fname" "$NOTES_DIR" --include='*.org' 2>/dev/null || true)
    if [[ -n "$matches" ]]; then
      echo "  Would update links: $old_fname -> $new_fname in:"
      echo "$matches" | while read -r m; do echo "    $(basename "$m")"; done
    fi
  else
    find "$NOTES_DIR" -name '*.org' -type f -exec \
      sed -i '' "s|$old_fname|$new_fname|g" {} +
  fi
done < "$MAPPING_FILE"

echo ""

# Phase 2: Rename the files
echo "=== Renaming files ==="
while IFS=$'\t' read -r old_rel new_rel; do
  old_path="$NOTES_DIR/$old_rel"
  new_path="$NOTES_DIR/$new_rel"

  if $DRY_RUN; then
    echo "  [DRY RUN] mv $old_rel -> $new_rel"
  else
    mv "$old_path" "$new_path"
    echo "  RENAMED: $old_rel -> $new_rel"
  fi
done < "$MAPPING_FILE"

rm -f "$MAPPING_FILE"
echo ""

# Phase 3: Rebuild org-roam DB
if ! $DRY_RUN; then
  echo "=== Rebuilding org-roam database ==="
  emacsclient --eval "(org-roam-db-sync 'force)" 2>/dev/null && \
    echo "  Database rebuilt successfully!" || \
    echo "  WARNING: Could not rebuild DB via emacsclient. Run M-x org-roam-db-sync manually."
fi

echo ""
if $DRY_RUN; then
  echo "This was a DRY RUN. Run with --apply to execute changes."
else
  echo "Done! All files renamed and org-roam DB rebuilt."
fi
