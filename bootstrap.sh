#!/bin/bash

echo "Linking dotfiles..."

ln -sf "$HOME/.dotfiles/yabai/yabairc" "$HOME/.yabairc"
ln -sf "$HOME/.dotfiles/skhd/skhdrc" "$HOME/.skhdrc"
ln -sf "$HOME/.dotfiles/sketchybar" "$HOME/.config/sketchybar"

# Emacs LaunchAgents
ln -sf "$HOME/.dotfiles/emacs/com.marcosandrade.emacsdaemon.plist" "$HOME/Library/LaunchAgents/"
ln -sf "$HOME/.dotfiles/emacs/com.marcosandrade.emacsclient.plist" "$HOME/Library/LaunchAgents/"

echo "All symlinks created!"
echo "To activate Emacs LaunchAgents, run:"
echo "  launchctl load ~/Library/LaunchAgents/com.marcosandrade.emacsdaemon.plist"
echo "  launchctl load ~/Library/LaunchAgents/com.marcosandrade.emacsclient.plist"
