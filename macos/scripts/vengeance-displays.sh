#!/usr/bin/env bash
# Audit VENGEANCE's monitor wiring over SSH.
# Reports which physical connector (HDMI vs DisplayPort) each monitor
# is on, per Windows' WmiMonitorConnectionParams.
#
# Expected when wired right: both S2719DGFs -> DisplayPort.
#
# Notes:
# - PowerShell is sent as -EncodedCommand: plain quoting/heredocs get
#   mangled by Windows' default cmd shell over SSH.
# - Monitors enumerate even when their active input is the Mac.

set -euo pipefail

PS_SCRIPT='
$conn = Get-CimInstance -Namespace root/wmi -ClassName WmiMonitorConnectionParams
$ids  = Get-CimInstance -Namespace root/wmi -ClassName WmiMonitorID
foreach ($c in $conn) {
  $id = $ids | Where-Object { $_.InstanceName -eq $c.InstanceName }
  $name = if ($id) { -join ($id.UserFriendlyName | Where-Object { $_ -ne 0 } | ForEach-Object { [char]$_ }) } else { "?" }
  $t = switch ([int]$c.VideoOutputTechnology) {
    0 { "VGA" } 4 { "DVI" } 5 { "HDMI" }
    10 { "DisplayPort" } 11 { "DisplayPort (embedded)" }
    default { "code $($c.VideoOutputTechnology)" }
  }
  "{0} -> {1}" -f $name, $t
}
'
ENC=$(printf '%s' "$PS_SCRIPT" | iconv -f utf-8 -t utf-16le | base64)

ssh -o ConnectTimeout=8 vengeance "powershell -NoProfile -EncodedCommand $ENC" 2>&1 |
  grep -v "post-quantum\|store now\|openssh.com/pq\|WARNING\|CLIXML\|^<Objs"
