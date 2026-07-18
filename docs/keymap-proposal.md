# mrx Keymap Proposal — hotdox76v2

Full recommendations from the keymap audit (2026-07-16). Interactive visualization:
- Local page: `docs/keymap-explorer.html` (self-contained, `open docs/keymap-explorer.html`)
- Artifact mirror: https://claude.ai/code/artifact/b48d351c-1ba9-4f01-bcb1-bd09b9aa2659

Sources audited:
- `~/qmk_firmware/keyboards/hotdox76v2/keymaps/mrx/` (commit 8eb6039, matches Desktop backup byte-for-byte)
- `~/.dotfiles/macos/skhd/skhdrc`
- Emacs leader config (`mr-x/leader-def`, SPC prefix, evil)

---

## Diagnosis — problems in the current keymap

1. **3 dead keys.** `BL_STEP`, `BL_BRTG`, `BL_TOGG` on the base layer bottom row are
   *backlight* codes — the hotdox76v2 is an RGB-matrix board and backlight isn't
   enabled in rules.mk. These keys do literally nothing.
2. **Layer 3 is unreachable.** It contains only `QK_BOOT`, but no `MO(3)`, `LT(3)`,
   or `TG(3)` exists anywhere in the keymap. Flashing requires the physical reset
   button.
3. **~10 duplicate keys on base.** Space ×2, Bksp ×3, Tab ×3, Enter ×2, Ctrl ×3
   (left hand alone). Arrows + Home/End/PgUp/PgDn exist on base *and* on the nav
   layer.
4. **Nav layer (L2) right hand is completely empty** — every key transparent.
5. **Both layer-hold thumbs are on the right hand** (`LT(1,KC_SPC)` on R54,
   `LT(2,KC_ENT)` on R51), so both layers can never be held at once → no tri-layer
   possible.
6. **No Hyper/Meh key.** skhdrc juggles `alt`, `cmd+shift`, `ctrl+alt`,
   `shift+alt`, `cmd+ctrl` — and `alt - h/j/k/l` for yabai **steals Meta from
   Emacs globally** (M-h/j/k/l never reach Emacs while skhd runs).
7. **`EXTRAKEY_ENABLE = no`** — media/volume keys physically cannot work.

What's good and should NOT change:
- Dvorak base with GACS home-row mods (`LCTL_T(A)` `LALT_T(O)` `LGUI_T(E)`
  `LSFT_T(U)` / mirrored `RSFT_T(H)` `RGUI_T(T)` `RALT_T(N)` `RCTL_T(S)`)
- Symbols layer (L1) — space-cadet pairs `" < { (` / `) } > =` on home row. Keep 100% as-is.

---

## Proposed changes

### Base layer (L0)

| Position | Current | Proposed |
|---|---|---|
| L54 (small mid thumb, left) | `LCTL_T(KC_BSPC)` | `ALL_T(KC_NO)` — **Hyper ✦** |
| R54 (small mid thumb, right) | `LT(1,KC_SPC)` | `MEH_T(KC_NO)` — **Meh ◆** |
| L53 (big left thumb) | `KC_SPC` | `LT(1,KC_SPC)` — Space tap / **Symbols hold** |
| L52 (big left thumb inner) | `KC_BSPC` | `LCTL_T(KC_BSPC)` — Bksp tap / Ctrl hold |
| R53 (big right thumb) | `KC_ENT` | `LT(2,KC_ENT)` — Enter tap / **Nav hold** |
| R51 (small bottom thumb, right) | `LT(2,KC_ENT)` | `CW_TOGG` — Caps Word |
| L36 (left inner column) | `LT(2,MS_WHLU)` | `MS_WHLU` (plain — layer access consolidated on thumbs) |
| R36 (right inner column) | `LT(1,MS_WHLD)` | `MS_WHLD` (plain) |
| L40 (bottom row, was dup Ctrl) | `KC_LCTL` | `TG(2)` — Nav layer lock |
| L41 (dead) | `BL_STEP` | `KC_VOLD` |
| L42 (dead) | `BL_BRTG` | `KC_VOLU` |
| R40 (dead) | `BL_TOGG` | `KC_MPLY` |

Key idea: Symbol hold moves to the **left** thumb, Nav stays on the **right** —
opposite hands, which enables the tri-layer and comfortable hold-and-type.

### Symbols layer (L1) — unchanged

Only the activator moves (left big Space instead of right thumb).

### Nav layer (L2) — fill the empty right hand

Keep left side exactly as-is (arrows on home row: `← ↓ ↑ →` on A/O/E/U positions;
Home/End/PgUp/PgDn on the row above).

Add right hand:
- **Numpad** under natural columns: `7 8 9` (R14/R13/R12), `4 5 6` (R24/R23/R22),
  `1 2 3` (R34/R33/R32), `0` (R43), `.` (R42)
- `KC_DEL` on R00 (top-right corner, Bksp position)

### System layer (L3) — new + actually reachable

Access: **tri-layer** — hold Space (L1) + Enter (L2) together. The firmware
already compiles `process_tri_layer.o`, so this is nearly free:

```c
layer_state_t layer_state_set_user(layer_state_t state) {
    return update_tri_layer_state(state, 1, 2, 3);
}
```

Contents:
- `QK_BOOT` (L11, keeps current position) — flash without the paperclip
- `EE_CLR` (L12) — factory reset
- RGB controls on left home area: `RGB_TOG` `RGB_MOD` `RGB_HUI` `RGB_SAI`
  `RGB_VAI` / `RGB_HUD` `RGB_SAD` `RGB_VAD` (replaces the dead BL_ keys' intent)
- Media on right: `KC_MPRV` `KC_MPLY` `KC_MNXT` / `KC_VOLD` `KC_MUTE` `KC_VOLU`

### Game layer — QWERTY for VENGEANCE (added 2026-07-17)

**Why:** the Dvorak lives in *firmware*, so the board physically sends Dvorak
keycodes. Games bind positionally and read what the keyboard sends — the WASD
zone emits `<AOE`-garbage and every game needs manual rebinds. An OS-level
layout would've been immune; firmware layout needs a firmware answer.

**Design rules:**

1. **Left half = plain QWERTY**: `Esc` `1-5`, `Tab Q W E R T`, `A S D F G`,
   `Shift Z X C V B`, plain `KC_LCTL`, plain `KC_LALT`, and **plain `KC_SPC` on
   the big thumb — no `LT()`**.
2. **No mod-taps anywhere on this layer.** Home-row mods are death in games:
   the tap-hold decision delay eats held movement keys. Plain mods only.
3. **Right half = mostly `KC_TRNS`** (Dvorak + mods stay for chat), except the
   exit key.
4. **Exit**: dedicated `TG()` on the layer itself (suggest R54 — the Meh thumb,
   useless mid-game). Must be ON the layer: the tri-layer thumbs are overridden
   by plain Space/Enter here, so there's no other way out.
5. **RGB indicator**: solid red while active via
   `rgb_matrix_indicators_user` layer check — never wonder which mode the
   board is in mid-lobby.

**Two-phase rollout:**

- **Phase 1 — today, zero flash (VIA only):** L3 currently holds nothing but
  `QK_BOOT`. Park the game layer there NOW: remap one dead `BL_*` key on base
  (L40/L41/L42 — they do literally nothing) to `TG(3)`, then paint the QWERTY
  left half onto L3 in VIA. Lives in EEPROM, survives replug, done in 10 min.
- **Phase 2 — the grand flash:** game layer moves to **L4** in `keymap.c`
  (bump `DYNAMIC_KEYMAP_LAYER_COUNT` to 5 for VIA visibility), L3 becomes the
  system tri-layer per this proposal, entry moves to L3+`G` = `TG(4)`.

Pairs with the USB-switch doctrine (see monitor-setup.org): ErgoDox follows you
to VENGEANCE via the switch → tap into game layer → WASD is WASD.

### rules.mk changes

```make
VIA_ENABLE = yes        # keep
CONSOLE_ENABLE = no     # keep
EXTRAKEY_ENABLE = yes   # WAS no — required for media/volume keys
MOUSEKEY_ENABLE = yes   # keep
CAPS_WORD_ENABLE = yes  # for CW_TOGG
TRI_LAYER_ENABLE = yes  # or just use layer_state_set_user, no flag needed
```

---

## skhd migration — Hyper frees everything

With ✦ Hyper (⌘⌃⌥⇧) and ◆ Meh (⌃⌥⇧) on the thumbs, migrate skhdrc so no
binding collides with any app. **Biggest win: `alt - h/j/k/l` freed → Meta works
in Emacs again.**

Proposed skhdrc mapping (skhd supports `hyper` and `meh` keywords natively):

```sh
# window focus / warp
hyper - h/j/k/l : yabai -m window --focus west/south/north/east
meh   - h/j/k/l : yabai -m window --warp  west/south/north/east

# displays
hyper - 1..4 : yabai -m window --display N; yabai -m display --focus N
meh   - 1..4 : yabai -m display --focus N

# window ops
hyper - t : yabai -m window --toggle float
hyper - f : yabai -m window --toggle zoom-fullscreen
hyper - b : yabai -m space --balance

# emacs / apps
hyper - e     : ~/.dotfiles/macos/scripts/emacs-cycle.sh
hyper - a     : ~/.dotfiles/macos/scripts/org-agenda-frame.sh
hyper - space : ~/.dotfiles/macos/scripts/scratchpad-emacs.sh scratch
hyper - o     : ~/.dotfiles/macos/scripts/scratchpad-emacs.sh scratch-org
hyper - g     : # magit frame, or whatever you reach for most
```

Bindings freed by this migration: `alt-hjkl`, `alt-t`, `alt-f`, `alt-1..4`,
`shift+alt-hjkl`, `ctrl-1..4`, most `cmd+shift-*` claw chords.
Keep the f13–f16 macro pad bindings as-is (already collision-free).

---

## "Emacs layer" verdict — don't do it in firmware

A QMK layer sending `SPC a f`-style sequences breaks in insert mode, vterm, and
outside Emacs (evil-state dependent). The already-proven pattern is better:
**keyboard sends clean signal → skhd → `emacsclient --eval`** (this is exactly
how f13–f16 work today). Global Emacs actions belong on Hyper bindings in skhd,
not in firmware.

---

## Flashing checklist (when ready)

1. Branch in `~/qmk_firmware`, edit `keyboards/hotdox76v2/keymaps/mrx/`
2. `qmk compile -kb hotdox76v2 -km mrx`
3. Flash; verify layer 3 reachable via Space+Enter
4. Caveat: VIA is enabled — any remaps ever made through the VIA app live in
   EEPROM and override the hex. Consider `EE_CLR` after flashing if behavior
   looks stale.
5. Update Desktop backup (`~/Desktop/keymaps-bckup/mrx/`) after it's proven.
