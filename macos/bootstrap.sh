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
# Newer Homebrew refuses to load formulae from third-party taps until
# they're explicitly trusted, so trust the ones our Brewfile uses first.
step "Trusting third-party taps..."
for tap in d12frosted/emacs-plus felixkratz/formulae koekeishiya/formulae \
           shaunsingh/sfmono-nerd-font-ligaturized oven-sh/bun; do
    brew tap "$tap" 2>/dev/null || true
    brew trust "$tap" 2>/dev/null || true
done

step "Installing packages from Brewfile..."
brew bundle --file="$DOTDIR/Brewfile"

# ── 3. Node (via nvm) ────────────────────────────────────
# The Brewfile installs the nvm formula, but nvm is only a version
# manager — it ships no node. Lay down a default LTS so node/npm work
# out of the box (per-project versions still switch via .nvmrc).
step "Installing Node LTS via nvm..."
export NVM_DIR="$HOME/.nvm"
mkdir -p "$NVM_DIR"
if [ -s "$(brew --prefix nvm)/nvm.sh" ]; then
    source "$(brew --prefix nvm)/nvm.sh"
    nvm install --lts || echo "nvm install failed, skipping (install node manually later)"
else
    echo "nvm.sh not found, skipping node install"
fi

# ── 4. Claude Code ───────────────────────────────────────
# Not a brew package — ships as a standalone installer that bundles its
# own runtime, so it doesn't depend on node and survives nvm switching.
step "Installing Claude Code..."
if ! command -v claude &>/dev/null; then
    curl -fsSL https://claude.ai/install.sh | bash || echo "Claude Code install failed, skipping"
else
    echo "Claude Code already installed"
fi

# ── 5. Symlinks ──────────────────────────────────────────
step "Creating symlinks..."

mkdir -p "$HOME/.config"
mkdir -p "$HOME/.hammerspoon"
mkdir -p "$HOME/Library/LaunchAgents"

ln -sf "$DOTDIR/.zshrc" "$HOME/.zshrc"
ln -sf "$DOTDIR/yabai/yabairc" "$HOME/.yabairc"
ln -sf "$DOTDIR/skhd/skhdrc" "$HOME/.skhdrc"
ln -sf "$DOTDIR/sketchybar" "$HOME/.config/sketchybar"
ln -sf "$DOTDIR/hammerspoon/init.lua" "$HOME/.hammerspoon/init.lua"

# Emacs LaunchAgents
ln -sf "$DOTDIR/emacs/com.marcosandrade.emacsdaemon.plist" "$HOME/Library/LaunchAgents/"
ln -sf "$DOTDIR/emacs/com.marcosandrade.emacsclient.plist" "$HOME/Library/LaunchAgents/"

# ── 6. Emacs ─────────────────────────────────────────────
step "Setting up Emacs..."
if [ ! -d "$HOME/.emacs.d" ]; then
    ln -sf "$DOTDIR/emacs/.emacs.d" "$HOME/.emacs.d"
    echo "Linked ~/.emacs.d"
else
    echo "~/.emacs.d already exists, skipping (check it points to dotfiles)"
fi

# ── 7. Services ──────────────────────────────────────────
step "Starting services..."

if command -v sketchybar &>/dev/null; then
    brew services start sketchybar 2>/dev/null || true
    echo "sketchybar started"
fi

if command -v syncthing &>/dev/null; then
    brew services start syncthing 2>/dev/null || true
    echo "syncthing started"
fi

# yabai basic tiling works immediately; the scripting addition (needs SIP
# partially disabled) unlocks the extras — see Next steps below.
if command -v skhd &>/dev/null; then
    brew services start skhd 2>/dev/null || true
    echo "skhd started"
fi

if command -v yabai &>/dev/null; then
    brew services start yabai 2>/dev/null || true
    echo "yabai started"
fi

# ── 8. Emacs daemon ──────────────────────────────────────
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
