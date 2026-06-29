# agent-shell-snip.ps1 — Windows interactive screenshot for agent-shell's SPC c s.
#
# agent-shell appends the destination file path as the final argument, so it
# arrives here as $OutPath. We clear the clipboard, launch the native Windows
# snip overlay (same drag-a-region UX as macOS `screencapture -i`), wait for the
# captured image to land on the clipboard, then save it as PNG to $OutPath.
#
# Cancellation: agent-shell calls this synchronously, so Emacs is BLOCKED while
# we poll. To avoid a 60s freeze when the user presses Esc, we track the snip
# overlay window and bail out the moment it's destroyed (with a short grace
# window so a *completed* snip still has time to drop its image on the clipboard).
#
# Must run under Windows PowerShell with -Sta: clipboard access requires a
# single-threaded apartment.
#
# Diagnostics: each run OVERWRITES $env:TEMP\agent-shell-snip.log, so it always
# holds just the most recent snip's trace (no unbounded growth).
param([string]$OutPath)

$ErrorActionPreference = 'Stop'
$log = Join-Path $env:TEMP 'agent-shell-snip.log'
function Note($m) { "[{0}] {1}" -f (Get-Date -Format 'HH:mm:ss.fff'), $m | Add-Content -Path $log }

Add-Type @"
using System;
using System.Runtime.InteropServices;
public static class Win {
  [DllImport("user32.dll")] public static extern IntPtr GetForegroundWindow();
  [DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr h, out uint pid);
  [DllImport("user32.dll")] public static extern bool IsWindow(IntPtr h);
  [DllImport("user32.dll")] public static extern bool IsWindowVisible(IntPtr h);
}
"@

# Truncate at the start of each run (Set-Content), then append the rest.
"[{0}] ---- run start ----" -f (Get-Date -Format 'HH:mm:ss.fff') | Set-Content -Path $log
Note "OutPath = [$OutPath]"

try {
    if ([string]::IsNullOrWhiteSpace($OutPath)) { Note "ERROR: empty OutPath"; exit 10 }

    Add-Type -AssemblyName System.Windows.Forms
    Add-Type -AssemblyName System.Drawing

    # Defensive: make sure the parent directory exists (agent-shell normally creates it).
    $dir = Split-Path -Parent $OutPath
    if ($dir -and -not (Test-Path -LiteralPath $dir)) {
        New-Item -ItemType Directory -Force -Path $dir | Out-Null
        Note "created parent dir [$dir]"
    }

    # Clear first so we can tell when the *new* capture arrives.
    [System.Windows.Forms.Clipboard]::Clear()

    # Launch the Windows screen-clip overlay (region select -> clipboard).
    Start-Process "ms-screenclip:"
    Note "launched ms-screenclip:"

    # Identify the overlay's top-level window: the foreground window owned by a
    # known snip process. The SnippingTool *process* lingers in memory after the
    # snip ends, so we watch the WINDOW (destroyed on cancel/complete), not the
    # process. If we can't spot it, $overlay stays Zero and we simply fall back
    # to the plain 60s clipboard poll (no fast-cancel, but no regression either).
    $snipNames = @('SnippingTool', 'ScreenClippingHost', 'ScreenSketch')
    $overlay = [IntPtr]::Zero
    $appearDeadline = (Get-Date).AddSeconds(4)
    do {
        Start-Sleep -Milliseconds 100
        $fg = [Win]::GetForegroundWindow()
        if ($fg -ne [IntPtr]::Zero -and [Win]::IsWindowVisible($fg)) {
            $fgpid = 0; [void][Win]::GetWindowThreadProcessId($fg, [ref]$fgpid)
            $pn = (Get-Process -Id $fgpid -ErrorAction SilentlyContinue).Name
            if ($pn -and ($snipNames -contains $pn)) { $overlay = $fg; break }
        }
    } while ((Get-Date) -lt $appearDeadline)
    Note "overlay window = $overlay"

    # Poll the clipboard until an image shows up, the overlay closes, or we time out.
    $deadline = (Get-Date).AddSeconds(60)
    $img = $null
    $graceUntil = $null
    do {
        Start-Sleep -Milliseconds 150
        try { $img = [System.Windows.Forms.Clipboard]::GetImage() }
        catch { $img = $null }   # clipboard momentarily locked by another app
        if ($img) { break }

        # Fast-cancel: overlay window is gone. A completed snip drops its image at
        # the same instant, so give the clipboard a brief grace window (the image
        # check above runs first each loop) before declaring the snip cancelled.
        if ($overlay -ne [IntPtr]::Zero -and
            -not ([Win]::IsWindow($overlay) -and [Win]::IsWindowVisible($overlay))) {
            if (-not $graceUntil) { $graceUntil = (Get-Date).AddMilliseconds(1200); Note "overlay closed; grace window started" }
            elseif ((Get-Date) -gt $graceUntil) { Note "cancelled (overlay closed, no image)"; exit 1 }
        }
    } while ((Get-Date) -lt $deadline)

    if (-not $img) { Note "ERROR: no image before deadline (timed out)"; exit 1 }
    Note "got image: $($img.Width)x$($img.Height)"

    $img.Save($OutPath, [System.Drawing.Imaging.ImageFormat]::Png)
    $exists = Test-Path -LiteralPath $OutPath
    $size = if ($exists) { (Get-Item -LiteralPath $OutPath).Length } else { 0 }
    Note "after Save: exists=$exists size=$size"
    if (-not $exists) { Note "ERROR: Save returned but file is absent at OutPath"; exit 3 }

    Note "OK"
    exit 0
}
catch {
    Note "EXCEPTION: $($_.Exception.GetType().Name): $($_.Exception.Message)"
    exit 2
}
