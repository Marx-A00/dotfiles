# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles repository for macOS containing configuration for:
- **Emacs**: Literate configuration using Org mode with Elpaca package manager
- **Yabai**: Tiling window manager for macOS
- **SKHD**: Hotkey daemon for macOS
- **Sketchybar**: Custom status bar for macOS

## Repository Structure

```
.dotfiles/
├── emacs/                  # Emacs configuration
│   └── .emacs.d/
│       ├── emacs.org      # Literate config source (tangles to init.el)
│       ├── init.el        # Generated from emacs.org
│       ├── early-init.el  # Early initialization
│       ├── elpaca/        # Package manager directory
│       ├── var/           # Variable data (managed by no-littering)
│       └── etc/           # Configuration files including custom.el
├── yabai/                  # Window manager config
│   ├── yabairc            # Main configuration
│   └── scripts/           # Helper scripts
├── skhd/                   # Hotkey daemon config
│   └── skhdrc             # Keybinding configuration
├── sketchybar/             # Status bar config
│   ├── sketchybarrc       # Main configuration
│   ├── plugins/           # Bar plugins/scripts
│   └── icon_map.sh       # Icon mappings
├── ebak/                   # Emacs backup (old straight.el config)
└── bootstrap.sh           # Symlink setup script
```

## Common Commands

### Initial Setup
```bash
# Create symlinks for all configurations
./bootstrap.sh
```

This creates:
- `~/.yabairc` → `~/.dotfiles/yabai/yabairc`
- `~/.skhdrc` → `~/.dotfiles/skhd/skhdrc`
- `~/.config/sketchybar` → `~/.dotfiles/sketchybar`

### Emacs Configuration

#### Regenerate init.el from emacs.org
After modifying `emacs.org`, tangle to regenerate `init.el`:
- In Emacs: `M-x org-babel-tangle` or `C-c C-v t`
- From command line: `emacs --batch -l org -f org-babel-tangle emacs/.emacs.d/emacs.org`

#### Live Eval Rule (IMPORTANT)
When editing `emacs.org`, ALWAYS run the changed elisp via emacsclient so it takes effect immediately:
```bash
emacsclient --eval "(elisp-expression-here)"
```
The file auto-tangles on save, but this makes changes live in the running Emacs session without manual eval.

Example: If you add `(setq foo 'bar)` to emacs.org, also run:
```bash
emacsclient --eval "(setq foo 'bar)"
```

#### Sandbox Emacs (IMPORTANT)
A separate, isolated Emacs instance exists for testing changes safely without affecting the main Emacs daemon.

**Location**: `~/.emacs-sandbox` - a complete copy of the main `~/.emacs.d`

**Launch**:
- **Keybinding**: `Cmd + Shift + S` (via skhd)
- **Script**: `~/.dotfiles/scripts/emacs-sandbox.sh`
- **Direct**: `/opt/homebrew/opt/emacs-plus@30/bin/emacs --init-directory ~/.emacs-sandbox`

**Visual distinction**:
- Title bar shows "SANDBOX - buffername" (title bar enabled, unlike main Emacs)
- Red "SANDBOX" badge in doom-modeline
- `sandbox-indicator.el` loads after init for visual indicators

**When to use**:
- Testing experimental Emacs config changes
- Debugging package issues without risking main session
- Trying new elisp code before committing to real config

**Workflow for testing changes**:
1. Edit `~/.emacs-sandbox/init.el` directly (NOT emacs.org - skip tangling for quick iteration)
2. Launch sandbox to test: `Cmd + Shift + S` or `~/.dotfiles/scripts/emacs-sandbox.sh`
3. Once changes work, manually promote them to the real config (`emacs/.emacs.d/emacs.org`)
4. Your main Emacs daemon stays completely untouched during testing

**IMPORTANT - Main vs Sandbox editing**:
- **Main config**: Edit `emacs.org`, then tangle to generate `init.el`
- **Sandbox**: Edit `init.el` directly - skip the org/tangle step to minimize friction

**Key files in sandbox**:
- `~/.emacs-sandbox/init.el` - edit this directly for testing (not emacs.org)
- `~/.emacs-sandbox/early-init.el` - has `undecorated-round` disabled for title bar visibility
- `~/.emacs-sandbox/sandbox-indicator.el` - doom-modeline segment and visual indicators

**Resync sandbox with main config**:
```bash
~/.dotfiles/scripts/emacs-sandbox.sh --fresh
```
This nukes the sandbox, copies fresh from main, and re-applies all sandbox-specific changes (title bar, doom-modeline indicator) automatically.

#### Package Management (Elpaca)
```elisp
;; Update all packages
M-x elpaca-update-all

;; Rebuild a package
M-x elpaca-rebuild

;; View package log
M-x elpaca-log
```

### Window Manager Services

#### Yabai
```bash
# Start/restart yabai
yabai --restart-service

# Reload configuration
yabai --reload-config

# Debug configuration
yabai -m config debug_output on
```

#### SKHD
```bash
# Start/restart skhd
skhd --restart-service

# Reload configuration
skhd --reload
```

#### Sketchybar
```bash
# Start/restart sketchybar
brew services restart sketchybar

# Reload configuration
sketchybar --reload
```

## Architecture Details

### Emacs Configuration
- **Package Manager**: Elpaca (migrated from straight.el)
- **Configuration Style**: Literate programming with Org mode
- **Key Features**:
  - Evil mode for Vim emulation
  - Projectile for project management
  - Magit for Git integration
  - Org-roam for note-taking
  - Ivy/Counsel/Swiper for completion
  - vterm for terminal integration

### Yabai Configuration
- Configured for BSP (binary space partitioning) layout
- Window gaps: 10px
- Top padding: 45px (for sketchybar)
- Focus follows mouse with autoraise
- Window borders enabled (3px width)
- System Settings app excluded from tiling

### SKHD Configuration
- Provides keyboard shortcuts for:
  - Window management via yabai
  - Application launching
  - Desktop/space navigation

### Sketchybar Configuration
- Height: 54px
- Margin: 12px
- Uses Liga SFMono Nerd Font
- Displays:
  - Space indicators (1-10)
  - Application icons
  - System information via plugins

## Development Workflow

### Git Branching
- **Main branch**: `main`
- **Current working branch**: `elpaca-migration-stable`
- **Feature branches**: Various feature/* branches for specific enhancements

### Active Migration
Currently migrating Emacs configuration from package.el/straight.el to Elpaca package manager. The old configuration is preserved in `ebak/` directory.

## macOS Integration Notes

- Requires SIP (System Integrity Protection) partially disabled for yabai scripting additions
- Uses sudo for yabai scripting additions (configured in yabairc)
- All configurations are macOS-specific and tested on Darwin
- Font preference: Iosevka (Emacs) and Liga SFMono Nerd Font (Sketchybar)

---
*Crafted with care for the perfect macOS development environment* ✨

## Task Master AI Instructions
**Import Task Master's development workflow commands and guidelines, treat as if import is in the main CLAUDE.md file.**
@./.taskmaster/CLAUDE.md
