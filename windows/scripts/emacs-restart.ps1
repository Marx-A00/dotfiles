<#
.SYNOPSIS
  Restart the Emacs daemon and open a fresh client frame.
.DESCRIPTION
  Windows port of the macOS scripts/emacs-restart.sh (which skhd triggers).
  Instead of launchd we just relaunch `runemacs --daemon` directly.
  Bind this to a hotkey (AutoHotkey, or a .lnk Shortcut key).
#>
$ErrorActionPreference = "SilentlyContinue"

$bin      = "$env:USERPROFILE\scoop\apps\emacs\current\bin"
$ec       = "$bin\emacsclient.exe"
$ecw      = "$bin\emacsclientw.exe"
$runemacs = "$bin\runemacs.exe"

# 1. Stop the running daemon gracefully (no-op if it isn't running).
& $ec -e '(kill-emacs)' 2>$null | Out-Null

# 2. Let the server socket clear, then start a fresh daemon (windowless).
Start-Sleep -Milliseconds 600
Start-Process -FilePath $runemacs -ArgumentList "--daemon" -WindowStyle Hidden

# 3. Wait (up to ~30s) for the daemon to accept connections.
$ready = $false
foreach ($i in 1..30) {
    Start-Sleep -Seconds 1
    & $ec -e '(+ 1 1)' 2>$null | Out-Null
    if ($LASTEXITCODE -eq 0) { $ready = $true; break }
}

# 4. Open a frame (empty -a "" also auto-starts the daemon if step 2 was slow).
if ($ready) {
    Start-Process -FilePath $ecw -ArgumentList '-c -a ""'
} else {
    Write-Warning "Emacs daemon did not become ready within 30s."
}
