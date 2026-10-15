# Windows Task Scheduler entrypoint; resolves installed Rscript dynamically.
$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
Set-Location $root
$rscript = Get-Command Rscript.exe -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source -First 1
if (-not $rscript) {
  $candidates = @(Get-ChildItem 'C:\Program Files\R' -Filter 'R-*' -Directory -ErrorAction SilentlyContinue |
    Sort-Object Name -Descending | ForEach-Object { Join-Path $_.FullName 'bin\Rscript.exe' })
  $rscript = $candidates | Where-Object { Test-Path $_ } | Select-Object -First 1
}
if (-not $rscript) { throw 'Rscript.exe nu a fost gasit. Instalati R sau adaugati Rscript la PATH.' }
& $rscript --vanilla 'scripturi/actualizare_programata.R'
exit $LASTEXITCODE
