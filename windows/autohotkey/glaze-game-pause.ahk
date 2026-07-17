#Requires AutoHotkey v2.0
#SingleInstance Force
; -----------------------------------------------------------------------------
; GlazeWM helpers (AutoHotkey). Three things:
;   1. Auto-pause GlazeWM while a fullscreen app is focused (the timer below).
;   2. Win+Shift+G  -> restart GlazeWM (kill + relaunch from the repo config),
;      the WM counterpart to Win+Shift+R for Emacs. AHK is a separate process,
;      so this still works even if GlazeWM has wedged.
;   3. A system-tray menu ("Toggle GlazeWM tiling" / "Restart GlazeWM") — a
;      click-to-pause button that never depends on a hotkey being delivered.
;      Super+P (the GlazeWM binding) dies whenever GlazeWM can't grab the key
;      (over an elevated window, or when kanata's Super mod doesn't register);
;      a tray click goes through the mouse instead, so it always works. Double-
;      click the tray icon = toggle; right-click for the menu.
;
; --- 1. Auto-pause while fullscreen ---
;
; Why: GlazeWM has no native "is this a game / is this fullscreen" rule — window
; rules only match process name / class / title, never a path, so there's no way
; in config.yaml to catch *every* game (see glzr-io/glazewm discussion #837,
; issue #699). Without this, the WM keeps trying to tile borderless-fullscreen
; games and they flicker in and out of fullscreen.
;
; Instead of an ever-growing per-game ignore list, this polls the focused window
; and, when it covers an entire monitor (taskbar included — that's what excludes
; a merely *maximized* window), pauses GlazeWM via its IPC CLI. When you leave
; fullscreen (alt-tab, close the game), it un-pauses. Covers every game and
; fullscreen video automatically, zero config.
;
; Caveat: GlazeWM only exposes `wm-toggle-pause` (a toggle, not set-state), so we
; track the WM's pause state ourselves in `paused`. If you also hit the GlazeWM
; Super+P binding by hand the two can desync — just use the tray toggle (or
; Super+P again) to line them back up.
;
; State model: `manualPause` is the user's tray/button override; `paused` is the
; WM's actual pause state as best we track it. Reconcile() pauses whenever EITHER
; the user forced it OR a fullscreen app is focused, so a manual pause survives
; games and the 750ms timer never fights the button.
; -----------------------------------------------------------------------------

glazewm     := EnvGet("USERPROFILE") "\scoop\shims\glazewm.exe"
paused      := false   ; actual GlazeWM pause state, as best we track it
manualPause := false   ; user-forced pause via the tray menu (independent of fullscreen)

; --- 3. Tray menu: a click-to-toggle that doesn't rely on hotkey delivery ---
A_TrayMenu.Insert("1&", "Toggle GlazeWM tiling", ManualToggle)
A_TrayMenu.Insert("2&", "Restart GlazeWM", RestartGlaze)
A_TrayMenu.Insert("3&")                        ; separator above AHK's defaults
A_TrayMenu.Default := "Toggle GlazeWM tiling"  ; double-click the tray icon = toggle
UpdateTrayTip()

SetTimer(CheckFullscreen, 750)

; --- 2. Win+Shift+G: restart GlazeWM (kill + relaunch from the repo config) ---
#+g:: RestartGlaze()

RestartGlaze(*) {
    script := A_ScriptDir "\..\scripts\glazewm-restart.ps1"
    Run('powershell -NoProfile -ExecutionPolicy Bypass -File "' script '"', , "Hide")
}

ManualToggle(*) {
    global manualPause
    manualPause := !manualPause
    Reconcile()
    UpdateTrayTip()
}

UpdateTrayTip() {
    global manualPause
    A_IconTip := manualPause
        ? "GlazeWM: tiling PAUSED — click tray to resume"
        : "GlazeWM: tiling on"
}

CheckFullscreen(*) {
    Reconcile()
}

; Pause if the user forced it OR a fullscreen app is focused; toggle only on change.
Reconcile() {
    global paused, manualPause
    desired := manualPause || IsActiveWindowFullscreen()
    if (desired != paused) {
        TogglePause()
        paused := desired
    }
}

TogglePause() {
    global glazewm
    if FileExist(glazewm)
        Run('"' glazewm '" command wm-toggle-pause', , "Hide")
}

IsActiveWindowFullscreen() {
    hwnd := WinExist("A")
    if (!hwnd)
        return false
    ; Ignore the desktop / shell so an empty desktop never counts as fullscreen.
    cls := WinGetClass("ahk_id " hwnd)
    if (cls = "WorkerW" || cls = "Progman" || cls = "Shell_TrayWnd" || cls = "")
        return false

    WinGetPos(&wx, &wy, &ww, &wh, "ahk_id " hwnd)

    ; Full bounds (taskbar included) of the monitor the window sits on.
    ; MONITOR_DEFAULTTONEAREST = 2.
    hMon := DllCall("MonitorFromWindow", "Ptr", hwnd, "UInt", 2, "Ptr")
    mi := Buffer(40, 0)
    NumPut("UInt", 40, mi, 0)                 ; cbSize
    if (!DllCall("GetMonitorInfo", "Ptr", hMon, "Ptr", mi))
        return false
    ml := NumGet(mi,  4, "Int"), mt := NumGet(mi,  8, "Int")   ; rcMonitor.left/top
    mr := NumGet(mi, 12, "Int"), mb := NumGet(mi, 16, "Int")   ; rcMonitor.right/bottom

    ; Fullscreen = window covers the whole monitor (small tolerance for borders).
    ; A maximized window stops at the work area (taskbar visible), so it fails
    ; the right/bottom check and is correctly treated as NOT fullscreen.
    tol := 2
    return (wx <= ml + tol) && (wy <= mt + tol)
        && (wx + ww >= mr - tol) && (wy + wh >= mb - tol)
}
