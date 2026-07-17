<#
.SYNOPSIS
  Restart GlazeWM, reading the config straight from the dotfiles repo.
.DESCRIPTION
  Kill the running GlazeWM (if any) and relaunch it with the repo config, the
  same invocation bootstrap.ps1's startup shortcut uses. Use this when a reload
  (Super+o) isn't enough, e.g. after GlazeWM wedges or you want a clean process.
  Bind it to a hotkey (see autohotkey\glaze-game-pause.ahk) or run by hand.
  Mirrors scripts\emacs-restart.ps1.

  Keep this file ASCII-only: Windows PowerShell 5.1 reads BOM-less .ps1 as CP1252,
  so a stray Unicode dash/quote becomes a parser error.
#>
$ErrorActionPreference = "SilentlyContinue"

$glazewm   = "$env:USERPROFILE\scoop\shims\glazewm.exe"
$glazeConf = "$env:USERPROFILE\dotfiles\windows\glazewm\config.yaml"

if (-not (Test-Path $glazewm)) {
    Write-Warning "GlazeWM not found at $glazewm (scoop install glazewm)."
    return
}

# 1. Stop the running instance(s) (no-op if none).
Get-Process glazewm -ErrorAction SilentlyContinue | Stop-Process -Force

# 2. Let it release its IPC port / window hooks before relaunching.
Start-Sleep -Milliseconds 600

# 3. Start fresh from the repo config (windowless, like the login shortcut).
Start-Process -FilePath $glazewm `
    -ArgumentList "start", "--config", "`"$glazeConf`"" `
    -WindowStyle Hidden

# 4. Confirm it came back up.
Start-Sleep -Milliseconds 800
if (Get-Process glazewm -ErrorAction SilentlyContinue) {
    Write-Host "GlazeWM restarted." -ForegroundColor Green
} else {
    Write-Warning "GlazeWM did not come back up - check the config for errors."
}
