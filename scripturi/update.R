#!/usr/bin/env Rscript
# Rscript scripturi/update.R [--sursa=bns,bnm,mf,...] [--din-anul=2019] [--dry-run]
# sources: bns, bnm, mf

argumente <- commandArgs(trailingOnly = TRUE)
optiune <- function(nume, implicit = NULL) {
  gasit <- grep(paste0("^--", nume, "="), argumente, value = TRUE)
  if (length(gasit)) sub(paste0("^--", nume, "="), "", gasit[1]) else implicit
}

script <- grep("^--file=", commandArgs(), value = TRUE)
# the project root is the parent of scripturi
if (length(script)) setwd(dirname(dirname(normalizePath(sub("^--file=", "", script[1])))))
# shared files and macroeconomic fetchers; dashboard UI stays out
fisiere_comune <- list.files("R", pattern = "[.]R$", full.names = TRUE)
# Explicit allowlist: backups and temporary R scripts must never be executed.
fisiere_surse <- file.path("R/surse", c(
  "10_bns.R", "11_bnm.R", "12_mf.R", "20_actualizare.R"
))
fisiere_de_incarcat <- c(sort(fisiere_comune), fisiere_surse)
lipsa <- fisiere_de_incarcat[!file.exists(fisiere_de_incarcat)]
if (length(lipsa)) stop("Fisiere obligatorii lipsa: ", paste(lipsa, collapse = ", "))
for (f in fisiere_de_incarcat) source(f, encoding = "UTF-8")

toate <- SURSE
alese <- strsplit(optiune("sursa", paste(names(toate), collapse = ",")), ",")[[1]]
necunoscute <- setdiff(alese, names(toate))
if (length(necunoscute)) stop("sursă necunoscută: ", paste(necunoscute, collapse = ", "))

din_anul <- as.integer(optiune("din-anul", CONFIG$din_anul))
rezultat <- actualizeaza(surse = toate[alese], din_anul = din_anul, dry_run = "--dry-run" %in% argumente)
cat("\n--- Rezumat ---\n")
print(rezumat_tabel(rezultat$tabel), row.names = FALSE)

# Non-zero exit code alerts schedulers/CI that at least one source failed.
if (isTRUE(rezultat$erori)) {
  cat("ATENTIE: una sau mai multe surse au esuat; consultati data/update_log.csv\n")
  quit(save = "no", status = 1L)
}
