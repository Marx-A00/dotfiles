# TODO(MrX): fix agent-shell interrupt desync — stale elpaca packages

**Status:** diagnosed, not yet fixed. Fix must run **on MrX** (the main Mac). The
Air (MrX2) is already fine — do not "fix" it.

## Symptom (MrX only)
In Agent Shell, after interrupting a turn with `C-c C-c`:
1. Prompt shows normally.
2. You type a prompt → it immediately returns an **empty prompt**.
3. You type another prompt → you get the **previous** prompt's response, and the
   shell resets to normal.

## Root cause
**Stale, elpaca-managed packages on MrX — NOT the config.** MrX's daemon froze on
June-era package versions and was never `elpaca-pull`ed. The Air runs ~1 month
newer code that contains the fix, which is why the *same config* behaves
differently on the two machines.

These packages live outside the dotfiles repo (elpaca runtime state in
`~/.emacs.d/elpaca/sources/`), so the config merge on 2026-07-18 did **not**
touch them. Restarting MrX's emacs alone will **not** fix it — it reloads the
same old packages. You must update the packages first.

## Evidence (as of 2026-07-18)
| package | MrX (buggy) | Air (clean) |
|---|---|---|
| `agent-shell` | `e58909d` (Jun 7) — pre-fix | `b0de9d8` (Jul 1) — has fix |
| `shell-maker` | `adce7ba` (Jun 1) | `2c9d4a4` (Jul 1) |
| `acp` | `3ddfa90` + a harmless local `M acp.el` (stderr-buffer hidden via leading space) | `3ddfa90` |

MrX's `agent-shell e58909d` is a direct **ancestor** of the Air's `b0de9d8`
(confirmed via `git merge-base --is-ancestor`), i.e. MrX is simply behind.

**The two commits that fix it** (both missing on MrX, present on the Air):
- `e9930a0` — *Remove busy guards for cases when queuing prompts is possible*
  → fixes the empty-prompt-on-submit-while-busy (steps 1–2).
- `a2915b8` — *Tweak out of turn messaging*
  → fixes the reply landing on the wrong turn (step 3).

Newer upstream tags now exist too: agent-shell `v0.60.2`, shell-maker `v0.93.5`.

## The fix (run on MrX)
1. Update the packages (either from a running emacs or a fresh `emacs`):
   ```
   M-x elpaca-update RET agent-shell RET
   M-x elpaca-update RET shell-maker RET
   ```
   (or `M-x elpaca-fetch-all` then `M-x elpaca-merge-all` to update everything.)
2. **Restart the emacs daemon** so the newer code is actually loaded — an
   in-place update won't fully take effect in the long-running daemon.
   ```
   pkill -f "Emacs.*bg-daemon"   # or: emacsclient -e '(kill-emacs)'
   # then relaunch however you normally start the daemon
   ```
3. Verify the fix landed:
   ```
   git -C ~/.emacs.d/elpaca/sources/agent-shell log --oneline -1   # expect >= b0de9d8 / v0.60.2
   git -C ~/.emacs.d/elpaca/sources/shell-maker log --oneline -1   # expect >= 2c9d4a4 / v0.93.5
   ```
4. Reproduce-test: start a turn, `C-c C-c` to interrupt, immediately type a new
   prompt. It should submit cleanly with no empty-return / out-of-turn reply.

## Optional cleanup
- MrX's local `M acp.el` edit (leading space to hide the `acp-client-stderr`
  buffer) is harmless but will conflict with future acp updates. The config
  already hides that buffer via a `display-buffer-alist` rule for
  `` `acp-client-stderr `` — so consider `git -C ~/.emacs.d/elpaca/sources/acp
  checkout acp.el` to drop the manual patch before updating acp.
- MrX still has a stale `~/.emacs.d/elpaca/repos/` from an older elpaca layout
  (the active clones are in `sources/`). Not urgent, just noise.
