# TODO(MrX): fix agent-shell interrupt desync — WRONG AGENT VERSION

**Status:** root cause identified, fix ready to apply + verify. Run **on MrX**.
The Air (MrX2) is already fine — do not "fix" it.

> ⚠️ **Correction:** an earlier version of this doc blamed stale `agent-shell` /
> `shell-maker` elpaca packages. **That was wrong** — a sandbox test on MrX
> disproved it (agent-shell `b0de9d8`, the Air's "clean" rev, is *also* buggy on
> MrX). Do NOT pin agent-shell. The real cause is below.

## Symptom (MrX only)
In Agent Shell with **Claude**, after interrupting a turn with `C-c C-c`:
1. Prompt shows normally.
2. You type a prompt → it immediately returns an **empty prompt**.
3. You type another prompt → you get the **previous** prompt's response, and the
   shell resets to normal.

## Root cause — the Claude ACP *agent* binary, not agent-shell
MrX and the Air run **different Claude ACP agent packages, wildly different
versions**. The agent is what emits `agent_message_chunk` notifications after a
cancel; the old one streams trailing chunks out-of-turn, which desyncs
*any* agent-shell version.

| machine | agent package | version | binary | result |
|---|---|---|---|---|
| **Air** (clean) | `@agentclientprotocol/claude-agent-acp` | **0.54.1** | `~/.nvm/.../v24.18.0/bin/claude-agent-acp` | ✅ |
| **MrX** (buggy) | `@zed-industries/claude-agent-acp` | **0.23.1** | `/opt/homebrew/bin/claude-agent-acp` | 🐛 |

### How we know it's the agent and not anything else (evidence)
- agent-shell `b0de9d8` (Air = clean) vs `3ed71cc` (latest) → **both buggy on MrX**
  (verified in the sandbox; sentinel fn `agent-shell-viewport--compose-queue`
  confirmed absent, i.e. it really loaded `b0de9d8`). → agent-shell version is
  NOT the cause.
- `acp-multiplex` vs direct `claude-agent-acp` → the MrX **sandbox** runs Claude
  **direct** (no multiplex) and is **still buggy**. → the multiplexer is NOT the
  cause.
- The only variable that tracks clean-vs-buggy perfectly is the **agent binary
  version** (Air `@agentclientprotocol` 0.54.1 vs MrX `@zed-industries` 0.23.1).

## The fix (run on MrX)

### 1. Replace the agent (use Homebrew's npm — old agent lives under /opt/homebrew)
```bash
/opt/homebrew/bin/npm uninstall -g @zed-industries/claude-agent-acp
/opt/homebrew/bin/npm install -g @agentclientprotocol/claude-agent-acp@0.54.1

# verify the bin now points at the new package + version:
readlink -f "$(command -v claude-agent-acp)"   # expect .../@agentclientprotocol/...
claude-agent-acp --version                     # expect 0.54.1
```
- If `uninstall` says "not installed," the old one is a brew formula — `brew
  uninstall` it (or just confirm `readlink` points to `@agentclientprotocol`
  after install; last install wins the `claude-agent-acp` symlink).
- If npm complains about a node engine: the Air is on node 24 — bump Homebrew node.

### 2. Test in the sandbox first (controlled A/B — only the agent changed)
The sandbox is deliberately left on `agent-shell b0de9d8`, which we PROVED buggy
with the old agent. Change only the agent and it should go clean → agent confirmed.
```bash
emacsclient -s sandbox -e '(kill-emacs)'                     # stop sandbox daemon
emacs --bg-daemon=sandbox --init-directory ~/.emacs-sandbox  # relaunch it
emacsclient -s sandbox -c                                    # open a frame
```
Start Claude, do the `C-c C-c`-then-reprompt dance. **Expect: finally clean.**

### 3. If clean → roll to MrX main
Main just needs a restart to pick up the new global binary (its `acp-multiplex`
spawns the new agent):
```bash
emacsclient -e '(kill-emacs)'    # then relaunch your main daemon however you do
```
Test the interrupt there too.

### 4. Restore the sandbox control (after the test)
```bash
git -C ~/.emacs-sandbox/elpaca/sources/agent-shell checkout -   # back to e58909d
git -C ~/.emacs-sandbox/elpaca/sources/shell-maker checkout -   # back to adce7ba
```
Then `M-x elpaca-rebuild agent-shell shell-maker` in the sandbox (or re-clear the
`.elc`). NOTE: sandbox sources were left detached at `b0de9d8` / `2c9d4a4` and
their `.elc` deleted during diagnosis — that's why this restore step exists.

## Follow-ups (config side — Claude/Air will do once MrX is confirmed)
- Fix the config comment at `agent-shell-config.el:~720` — it says
  `claude-agent-acp (0.23.x)`, which is the **buggy** package. Should document
  requiring `@agentclientprotocol/claude-agent-acp` 0.54.x so both machines align.
- Consider adding the agent package to the machine bootstrap so a fresh box gets
  the right one, not whatever Homebrew/npm resolves.
