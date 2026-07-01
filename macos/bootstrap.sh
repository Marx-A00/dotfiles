#!/bin/bash
set -e

DOTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"   # …/dotfiles/macos

# ── Helpers ──────────────────────────────────────────────
step() { echo ""; echo "==> $1"; }

# ── 1. Homebrew ──────────────────────────────────────────
if ! command -v brew &>/dev/null; then
    step "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
else
    step "Homebrew already installed"
fi

# ── 2. Brew Bundle ───────────────────────────────────────
step "Installing packages from Brewfile..."
brew bundle --file="$DOTDIR/Brewfile" --no-lock

# ── 3. Symlinks ──────────────────────────────────────────
step "Creating symlinks..."

mkdir -p "$HOME/.config"
mkdir -p "$HOME/.hammerspoon"
mkdir -p "$HOME/Library/LaunchAgents"

ln -sf "$DOTDIR/yabai/yabairc" "$HOME/.yabairc"
ln -sf "$DOTDIR/skhd/skhdrc" "$HOME/.skhdrc"
ln -sf "$DOTDIR/sketchybar" "$HOME/.config/sketchybar"
ln -sf "$DOTDIR/hammerspoon/init.lua" "$HOME/.hammerspoon/init.lua"

# Emacs LaunchAgents
ln -sf "$DOTDIR/emacs/com.marcosandrade.emacsdaemon.plist" "$HOME/Library/LaunchAgents/"
ln -sf "$DOTDIR/emacs/com.marcosandrade.emacsclient.plist" "$HOME/Library/LaunchAgents/"

# ── 4. Emacs ─────────────────────────────────────────────
step "Setting up Emacs..."
if [ ! -d "$HOME/.emacs.d" ]; then
    ln -sf "$DOTDIR/emacs/.emacs.d" "$HOME/.emacs.d"
    echo "Linked ~/.emacs.d"
else
    echo "~/.emacs.d already exists, skipping (check it points to dotfiles)"
fi

# ── 5. Services ──────────────────────────────────────────
step "Starting services..."

if command -v sketchybar &>/dev/null; then
    brew services start sketchybar 2>/dev/null || true
    echo "sketchybar started"
fi

if command -v syncthing &>/dev/null; then
    brew services start syncthing 2>/dev/null || true
    echo "syncthing started"
fi

# ── 6. Emacs daemon ──────────────────────────────────────
step "Loading Emacs LaunchAgents..."
launchctl load "$HOME/Library/LaunchAgents/com.marcosandrade.emacsdaemon.plist" 2>/dev/null || true
launchctl load "$HOME/Library/LaunchAgents/com.marcosandrade.emacsclient.plist" 2>/dev/null || true

# ── Done ─────────────────────────────────────────────────
step "Done!"
echo ""
echo "Next steps:"
echo "  - Launch Emacs once to let Elpaca bootstrap all packages"
echo "  - If using yabai, partially disable SIP and start the service"
echo "  - Review commented-out items in Brewfile for extras you might want"
