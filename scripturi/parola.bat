@echo off
cd /d "%~dp0.."
set /p u=Utilizator: 
set /p p=Parola noua (fara ghilimele): 
"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" scripturi\parola.R "%u%" "%p%"
pause
