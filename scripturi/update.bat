@echo off
cd /d "%~dp0.."
"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripturi\update.R %*
pause
