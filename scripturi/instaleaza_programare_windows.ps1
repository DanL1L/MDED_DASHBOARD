# Run from PowerShell under the Windows account that will run the update.
# Requires scripturi/actualizare_windows.ps1 and actualizare_programata.R in the project.
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$entry = Join-Path $PSScriptRoot 'actualizare_windows.ps1'
if (-not (Test-Path $entry)) { throw "Lipseste $entry. Instalati intai patch-ul portabil." }
if (-not (Test-Path (Join-Path $PSScriptRoot 'actualizare_programata.R'))) { throw 'Lipseste actualizare_programata.R.' }
$taskName = 'Moldova Dashboard - Actualizare date'
$ps = (Get-Command powershell.exe -ErrorAction Stop).Source
$arguments = '-NoProfile -NonInteractive -ExecutionPolicy Bypass -File "' + $entry + '"'
$action = New-ScheduledTaskAction -Execute $ps -Argument $arguments -WorkingDirectory $root
$trigger = New-ScheduledTaskTrigger -Daily -At '08:00'
$settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -MultipleInstances IgnoreNew -ExecutionTimeLimit (New-TimeSpan -Hours 2) -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries
# InteractiveToken: runs only while the user is signed in; no password is stored.
$user = [System.Security.Principal.WindowsIdentity]::GetCurrent().Name
$principal = New-ScheduledTaskPrincipal -UserId $user -LogonType Interactive -RunLevel Limited
Register-ScheduledTask -TaskName $taskName -Action $action -Trigger $trigger -Settings $settings -Principal $principal -Force | Out-Null
Write-Host "Sarcina instalata: $taskName"
Write-Host "Cont: $user"
Write-Host 'Zilnic la 08:00; o actualizare reusita pe saptamana, reincercare dupa esec.'
Write-Host 'Important: in aceasta configuratie, utilizatorul trebuie sa fie autentificat.'
Get-ScheduledTaskInfo -TaskName $taskName | Select-Object LastRunTime, LastTaskResult, NextRunTime | Format-List
