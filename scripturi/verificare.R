#!/usr/bin/env Rscript
# Read-only smoke test: no HTTP calls, no writes to data store.
common <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
source_files <- file.path("R/surse", c("10_bns.R", "11_bnm.R", "12_mf.R", "20_actualizare.R"))
stopifnot(all(file.exists(c(common, source_files))))
for (f in c(common, source_files)) source(f, encoding = "UTF-8")
stopifnot(grepl("365L", paste(deparse(body(bnm_credite_noi)), collapse = " ")))
stopifnot(all(c("bns", "bnm", "mf") %in% names(SURSE)))
stopifnot(file.exists(CONFIG$store_xlsx))
tabel <- citeste_tabel()
valideaza_tabel(tabel)
cat("OK: module, BNM v3, surse, baza de date validate. Observatii:", nrow(tabel), "\n")
cat("Ultimele perioade:\n")
print(rezumat_tabel(tabel), row.names = FALSE)
