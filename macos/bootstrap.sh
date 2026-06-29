#!/bin/bash

DOTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"   # …/dotfiles/macos

echo "Linking dotfiles..."

ln -sf "$DOTDIR/yabai/yabairc" "$HOME/.yabairc"
ln -sf "$DOTDIR/skhd/skhdrc" "$HOME/.skhdrc"
ln -sf "$DOTDIR/sketchybar" "$HOME/.config/sketchybar"
ln -sf "$DOTDIR/hammerspoon/init.lua" "$HOME/.hammerspoon/init.lua"

# Emacs LaunchAgents
ln -sf "$DOTDIR/emacs/com.marcosandrade.emacsdaemon.plist" "$HOME/Library/LaunchAgents/"
ln -sf "$DOTDIR/emacs/com.marcosandrade.emacsclient.plist" "$HOME/Library/LaunchAgents/"

echo "All symlinks created!"
echo "To activate Emacs LaunchAgents, run:"
echo "  launchctl load ~/Library/LaunchAgents/com.marcosandrade.emacsdaemon.plist"
echo "  launchctl load ~/Library/LaunchAgents/com.marcosandrade.emacsclient.plist"
