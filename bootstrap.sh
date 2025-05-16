#!/bin/bash

echo "Linking dotfiles..."

ln -sf "$HOME/.dotfiles/yabai/yabairc" "$HOME/.yabairc"
ln -sf "$HOME/.dotfiles/skhd/skhdrc" "$HOME/.skhdrc"
ln -sf "$HOME/.dotfiles/sketchybar" "$HOME/.config/sketchybar"

echo "All symlinks created!"
