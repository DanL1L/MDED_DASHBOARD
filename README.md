# Dashboard macroeconomic — pachet pentru beneficiar

Aplicație R Shiny pentru indicatorii BNS, BNM și MF (frecvențe lunare, trimestriale și anuale).

**Documentație:** `Documentatie_tehnica_si_ghid_operational.docx` — arhitectură, fișiere, adăugarea indicatorilor pentru fiecare sursă, instalare, automatizare, publicare și integrare în website.

**Pornire:** `Rscript scripturi/install.R` apoi `Rscript -e "shiny::runApp('.', launch.browser=TRUE)"`.

**Verificare fără scriere:** `Rscript scripturi/update.R --dry-run`.

**Actualizare:** `Rscript scripturi/update.R`.

**Notă:** Pachetul nu include credențiale. Creați local `credentials.R` conform modulului `R/app/02_login.R` și nu îl distribuiți. Pentru un server, configurați separat actualizarea și publicarea aplicației. Fișierele de date din `data/` sunt necesare aplicației. Cache-ul din `data/cache/` este păstrat pentru portabilitate.

**Structură:** `app.R`, `R/`, `R/app/`, `R/surse/`, `scripturi/`, `data/`. Nu există fișiere `.Rmd` în versiunea-sursă; fișierele `.R` sunt cod executabil indispensabil.
