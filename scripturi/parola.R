#!/usr/bin/env Rscript
# Rscript scripturi/parola.R utilizator parola   adds or replaces a user in credentials.R

argumente <- commandArgs(trailingOnly = TRUE)
script <- grep("^--file=", commandArgs(), value = TRUE)
# the project root is the parent of scripturi
if (length(script)) setwd(dirname(dirname(normalizePath(sub("^--file=", "", script[1])))))
if (length(argumente) != 2 || !nzchar(argumente[1]) || nchar(argumente[2]) < 8) {
  stop("folosire: Rscript scripturi/parola.R utilizator parola   (parola de cel putin 8 caractere)")
}
source("R/01_baza.R", encoding = "UTF-8")
source("R/app/02_login.R", encoding = "UTF-8")

utilizator <- argumente[1]
parola <- argumente[2]
niciunul <- data.frame(user = character(0), salt = character(0), hash = character(0), stringsAsFactors = FALSE)
existenti <- if (is.null(CREDENTIALE)) niciunul else CREDENTIALE$utilizatori
secret <- CREDENTIALE$secret %||% hex(openssl::rand_bytes(32))
sare <- sare_noua()
nou <- data.frame(user = utilizator, salt = sare, hash = parola_hash(parola, sare), stringsAsFactors = FALSE)
utilizatori <- rbind(existenti[existenti$user != utilizator, ], nou)

citat <- function(x) paste0("\"", x, "\"", collapse = ", ")
writeLines(c(
  "# login users, salts, hashes and cookie secret, edit with parola.R",
  "list(",
  sprintf("  secret = \"%s\",", secret),
  "  utilizatori = data.frame(",
  sprintf("    user = c(%s),", citat(utilizatori$user)),
  sprintf("    salt = c(%s),", citat(utilizatori$salt)),
  sprintf("    hash = c(%s),", citat(utilizatori$hash)),
  "    stringsAsFactors = FALSE",
  "  )",
  ")"
), "credentials.R", useBytes = TRUE)
cat("credentials.R scris, utilizatori:", paste(utilizatori$user, collapse = ", "), "\n")
