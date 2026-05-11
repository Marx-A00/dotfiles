# Ideas from Gleek's Dotfiles

Source: https://github.com/Gleek/dotfiles / https://github.com/Gleek/.emacs

---

## Adopted

### 1. agent-shell-tool-group ✅
- **What**: Collapses consecutive tool calls under foldable headers in agent-shell
- **How**: `n`/`p` to navigate groups, `TAB`/`RET` to toggle open/closed
- **Source**: https://github.com/Gleek/agent-shell-tool-group
- **Status**: Added to emacs.org, live in prod

### 2. diff-hl ✅
- **What**: Inline git diff markers in the fringe — added/modified/deleted lines
- **How**: Hooks into prog-mode, text-mode, dired; auto-refreshes after magit ops; flydiff for live updates
- **Status**: Added to emacs.org, live in prod

### 3. treesit-fold ✅
- **What**: Code folding via tree-sitter
- **How**: Hydra on `z f` in normal mode — open/close/toggle/all
- **Source**: https://github.com/emacs-tree-sitter/treesit-fold
- **Status**: Added to emacs.org, live in prod

### 6. Progressive ESC (escape-quit) ✅
- **What**: Layered escape that tries multiple things before hard quitting
- **How**: `mr-x/escape-quit` replaces `keyboard-escape-quit` — minibuffer > region > escape-hook > macros > keyboard-quit
- **Source**: `core-ux.el` in Gleek's config
- **Status**: Added to emacs.org UX section, live in prod

### 9. point-stack ✅
- **What**: Automatic cursor position breadcrumbs during navigation
- **How**: Vendored in `lisp/point-stack.el`, advises xref/imenu/isearch/counsel-rg. `s-[` back, `s-]` forward
- **Source**: https://github.com/Gleek/.emacs/blob/master/packages/point-stack.el
- **Status**: Added to emacs.org UX section, live in prod

---

## Skipped

### 4. avy
- **Reason**: Redundant — already have evil-snipe (`s`/`S`) for cross-screen jumps and `f`/`t` for inline, plus link-hint on `SPC f`

### 5. Inactive Window Opacity (yabai)
- **Reason**: Personal preference — don't like the faded window look

### 11. Vertico + Consult + Orderless + Embark
- **Reason**: Too much migration pain — Ivy works, bindings are locked in

### 12. GPTel + Custom Tool System
- **Reason**: Redundant with mr-x/quick-ask

---

## Still To Try

### 7. emergency.el
- **What**: Single portable init.el (~200 lines) for SSH boxes, containers, pair programming
- **Why**: Familiar keybindings and theme anywhere without cloning full config
- **Source**: https://github.com/Gleek/.emacs/blob/master/emergency.el
- **Effort**: Write once, carry everywhere

### 8. ediff-buffers-dwim
- **What**: Auto-picks 2-way or 3-way diff comparison based on visible buffers
- **Source**: `core-vc.el` in Gleek's config
- **Reason**: Using vdiff + vdiff-magit instead of ediff

### 10. zoom (auto-resize active window)
- **What**: Automatically resizes the focused Emacs window to 70% of frame
- **How**: `zoom-mode` with `zoom-size` set to `(0.7 . 0.7)`
- **Effort**: One use-package block

---

## Other Notable Things in Gleek's Config

- **ultra-scroll**: Smooth scrolling with wheel events — ADDED, pending daemon restart
- **core-metrics (keyfreq + esup)**: Command frequency tracking + startup profiler — ADDED, live
- **persistent desktop sessions**: Full Emacs state save/restore via `--restore` CLI flag (`core-session.el`)
- **rotate-windows**: Cycles buffer contents across panes preserving scroll + cursor (`core-window.el`)
- **tab-bar workspaces**: Named workspaces with undo/redo (alternative to perspective)
- **incremental-reading**: Spaced repetition reading system (custom package)
- **copilot.el**: GitHub Copilot in-buffer suggestions
- **speak-region**: Text-to-speech for selected text
- **remind**: Timed notifications with seconds/minutes/hours format
- **agent-shell-attention**: Mode-line indicator for agent buffers needing input (`AS:n`)
