# The master table, read, write, merge and journal

COLOANE_TABEL <- c(
  "indicator", "frecventa", "perioada", "an", "trimestru", "luna", "zi", "valoare", "interval_min", "interval_max",
  "unitate", "tip", "sursa", "sursa_detaliu", "vintage", "data_actualizarii", "nota"
)

tabel_gol <- function() {
  coloane <- setNames(replicate(length(COLOANE_TABEL), character(0), simplify = FALSE), COLOANE_TABEL)
  normalizeaza_tabel(as.data.frame(coloane, stringsAsFactors = FALSE))
}

# every column present with the right type, period codes complete, plus the sort key
normalizeaza_tabel <- function(tabel) {
  tabel <- cu_coloane_standard(tabel)
  tabel <- cu_tipuri(tabel)
  tabel <- cu_perioade_complete(tabel)
  tabel$ordine <- perioada_ordine(tabel$perioada)
  tabel
}

cu_coloane_standard <- function(tabel) {
  for (col in COLOANE_TABEL) if (!col %in% names(tabel)) tabel[[col]] <- NA
  tabel[COLOANE_TABEL]
}

cu_tipuri <- function(tabel) {
  tabel$indicator <- as.character(tabel$indicator)
  tabel$frecventa <- toupper(trimws(as.character(tabel$frecventa)))
  for (col in c("an", "trimestru", "luna", "zi")) tabel[[col]] <- suppressWarnings(as.integer(tabel[[col]]))
  for (col in c("valoare", "interval_min", "interval_max")) tabel[[col]] <- numar(tabel[[col]])
  for (col in c("unitate", "sursa_detaliu", "vintage", "data_actualizarii",
                "nota")) tabel[[col]] <- as.character(tabel[[col]])
  tabel$tip <- ifelse(is.na(tabel$tip) | tabel$tip == "", "efectiv", as.character(tabel$tip))
  tabel$sursa <- ifelse(is.na(tabel$sursa) | tabel$sursa == "", "manual", as.character(tabel$sursa))
  tabel
}

# hand typed rows may lack the period or its components, both directions are filled
cu_perioade_complete <- function(tabel) {
  lipsa <- is.na(tabel$perioada) | tabel$perioada == ""
  tabel$perioada[lipsa] <- with(tabel[lipsa, ], ifelse(frecventa == "T", perioada_T(an, trimestru),
    ifelse(frecventa == "L", perioada_L(an, luna),
      ifelse(frecventa == "Z", perioada_Z(an, luna, zi), perioada_A(an)))))
  tabel$perioada <- toupper(gsub("\\s|Q", "T", as.character(tabel$perioada)))
  tabel$perioada <- sub("^(\\d{4})M", "\\1L", tabel$perioada)

  fara_an <- is.na(tabel$an)
  tabel$an[fara_an] <- as.integer(substr(tabel$perioada[fara_an], 1, 4))
  fara_trimestru <- tabel$frecventa %in% "T" & is.na(tabel$trimestru)
  tabel$trimestru[fara_trimestru] <- as.integer(sub("^\\d{4}T", "", tabel$perioada[fara_trimestru]))
  fara_luna <- tabel$frecventa %in% "L" & is.na(tabel$luna)
  tabel$luna[fara_luna] <- as.integer(sub("^\\d{4}L", "", tabel$perioada[fara_luna]))
  zilnic <- tabel$frecventa %in% "Z"
  tabel$luna[zilnic] <- as.integer(substr(tabel$perioada[zilnic], 6, 7))
  tabel$zi[zilnic] <- as.integer(substr(tabel$perioada[zilnic], 9, 10))
  tabel
}

citeste_tabel <- function(cale = CONFIG$store_xlsx) {
  if (!file.exists(cale)) {
    stop("Baza de date lipseste: ", cale, ". Restaurati-o din data/backups/ sau din arhiva initiala.")
  }
  foaie <- readxl::read_excel(cale, sheet = "date", col_types = "text")
  foaie <- as.data.frame(foaie, stringsAsFactors = FALSE)
  foaie <- foaie[!is.na(foaie$indicator) & foaie$indicator != "", , drop = FALSE]
  normalizeaza_tabel(foaie)
}

# Minimal integrity checks before publishing a new dataset.
valideaza_tabel <- function(tabel) {
  t <- normalizeaza_tabel(tabel)
  if (!nrow(t)) stop("Validare: tabelul este gol")
  if (anyNA(t$indicator) || any(!nzchar(t$indicator))) stop("Validare: indicator lipsa")
  if (anyNA(t$frecventa) || any(!t$frecventa %in% c("A", "T", "L"))) stop("Validare: frecventa invalida")
  if (anyNA(t$valoare) || any(!is.finite(t$valoare))) stop("Validare: valori numerice lipsa/invalide")
  if (anyNA(t$an) || any(t$an < 1900 | t$an > as.integer(format(Sys.Date(), "%Y")) + 1L)) stop("Validare: an invalid")
  if (any(t$frecventa == "T" & (is.na(t$trimestru) | t$trimestru < 1 | t$trimestru > 4))) stop("Validare: trimestru invalid")
  if (any(t$frecventa == "L" & (is.na(t$luna) | t$luna < 1 | t$luna > 12))) stop("Validare: luna invalida")
  asteptat <- ifelse(t$frecventa == "T", perioada_T(t$an, t$trimestru),
                ifelse(t$frecventa == "L", perioada_L(t$an, t$luna), perioada_A(t$an)))
  if (anyNA(t$perioada) || any(t$perioada != asteptat)) stop("Validare: cod de perioada inconsistent")
  chei <- paste(t$indicator, t$frecventa, t$perioada, t$tip, sep = "|")
  if (anyDuplicated(chei)) stop("Validare: observatii duplicate")
  invisible(t)
}

# Write both output files to staging paths, verify them, then replace the live files.
# A timestamped persistent backup is retained before each successful replacement.
scrie_tabel <- function(tabel, cale = CONFIG$store_xlsx) {
  dir.create(dirname(cale), showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(CONFIG$store_csv), showWarnings = FALSE, recursive = TRUE)
  tabel <- valideaza_tabel(tabel)
  tabel <- tabel[order(tabel$indicator, tabel$frecventa, tabel$ordine, tabel$tip), ]
  temp_xlsx <- tempfile(pattern = ".indicatori_", tmpdir = dirname(cale), fileext = ".xlsx")
  temp_csv <- tempfile(pattern = ".indicatori_", tmpdir = dirname(CONFIG$store_csv), fileext = ".csv")
  on.exit(unlink(c(temp_xlsx, temp_csv)), add = TRUE)
  writexl::write_xlsx(list(date = tabel[COLOANE_TABEL], indicatori = INDICATORI, ghid = ghid_coloane()), temp_xlsx)
  utils::write.csv(tabel[COLOANE_TABEL], temp_csv, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  test_xlsx <- as.data.frame(readxl::read_excel(temp_xlsx, sheet = "date", col_types = "text"))
  test_csv <- utils::read.csv(temp_csv, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(test_xlsx) != nrow(tabel) || nrow(test_csv) != nrow(tabel)) stop("Validare: fisierele temporare sunt incomplete")

  backup_dir <- file.path(dirname(cale), "backups")
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  fisiere <- c(cale, CONFIG$store_csv)
  temp <- c(temp_xlsx, temp_csv)
  # Copy the previous live files to permanent backups before changing either.
  for (f in fisiere[file.exists(fisiere)]) {
    destinatie <- file.path(backup_dir, paste0(stamp, "_", basename(f)))
    if (!file.copy(f, destinatie, overwrite = FALSE)) stop("Backup esuat: ", f)
  }
  # On Windows file.rename() does not reliably overwrite existing files.
  # Move live files aside first; restore them if publishing either file fails.
  rollback <- paste0(fisiere, ".rollback")
  if (any(file.exists(rollback))) stop("Fisiere .rollback existente; verificati o recuperare precedenta")
  mutate_ok <- FALSE
  on.exit({
    if (!mutate_ok) {
      for (i in seq_along(fisiere)) {
        if (file.exists(rollback[i])) {
          if (file.exists(fisiere[i])) unlink(fisiere[i])
          if (!file.rename(rollback[i], fisiere[i])) {
            warning("RESTAURARE MANUALA NECESARA: ", rollback[i])
          }
        }
      }
    }
  }, add = TRUE)
  for (i in seq_along(fisiere)) {
    if (file.exists(fisiere[i]) && !file.rename(fisiere[i], rollback[i])) {
      stop("Nu pot pregati inlocuirea fisierului: ", fisiere[i])
    }
  }
  for (i in seq_along(fisiere)) {
    if (!file.rename(temp[i], fisiere[i])) stop("Publicare esuata: ", fisiere[i])
  }
  mutate_ok <- TRUE
  unlink(rollback)
  # Keep the last 10 backup generations (two files per generation).
  copii <- sort(list.files(backup_dir, pattern = "^[0-9]{8}_[0-9]{6}_indicatori[.](xlsx|csv)$", full.names = TRUE))
  generatii <- unique(sub("_indicatori[.](xlsx|csv)$", "", basename(copii)))
  if (length(generatii) > 10L) {
    vechi <- generatii[seq_len(length(generatii) - 10L)]
    unlink(copii[sub("_indicatori[.](xlsx|csv)$", "", basename(copii)) %in% vechi])
  }
  invisible(tabel)
}

ghid_coloane <- function() {
  data.frame(
    coloana = COLOANE_TABEL,
    descriere = c(
      paste0("Codul indicatorului: ", paste(INDICATORI$cod, collapse = ", ")),
      "T = trimestrial, A = anual, L = lunar, Z = zilnic",
      "2026T1 (trimestru), 2026L07 (lună), 2026-09-15 (zi) sau 2026 (an); se completează automat dacă lipsește",
      "Anul (număr)",
      "Trimestrul 1-4 (doar la trimestrial)",
      "Luna 1-12 (lunar și zilnic)",
      "Ziua 1-31 (doar la zilnic)",
      "Valoarea numerică (separator zecimal punct)",
      "Neutilizat (rezervat)",
      "Neutilizat (rezervat)",
      "Unitatea de măsură (informativ)",
      "efectiv (gol = efectiv)",
      "Numele sursei sau manual (gol = manual). Rândurile manual nu sunt suprascrise de actualizator.",
      "Tabel PxWeb, URL fișier etc. (informativ)",
      "Neutilizat (rezervat)",
      "Data ultimei actualizări a rândului",
      "Observații"
    ),
    stringsAsFactors = FALSE
  )
}

# new rows are added, changed values replace the old ones, manual rows are never touched
imbina_randuri <- function(tabel, noi) {
  nimic <- list(tabel = tabel, inserate = 0L, revizuite = 0L, neschimbate = 0L, protejate = 0L)
  if (is.null(noi) || nrow(noi) == 0) {
    return(nimic)
  }
  noi <- normalizeaza_tabel(noi)
  cheie <- function(d) paste(d$indicator, d$frecventa, d$perioada, d$tip, sep = "|")
  # last row wins when a source repeats a period
  noi <- noi[!duplicated(cheie(noi), fromLast = TRUE), ]

  pozitie <- match(cheie(noi), cheie(tabel))
  nou <- is.na(pozitie)
  protejat <- !nou
  protejat[!nou] <- tabel$sursa[pozitie[!nou]] %in% "manual"
  la_fel <- !nou & !protejat
  vechi <- tabel$valoare[pozitie[la_fel]]
  la_fel[la_fel] <- abs(vechi - noi$valoare[la_fel]) <= 1e-9 * pmax(1, abs(vechi))
  la_fel[is.na(la_fel)] <- FALSE
  revizuit <- !nou & !protejat & !la_fel

  for (i in which(revizuit)) {
    noteaza("  revizuire %s %s %s: %s -> %s", noi$indicator[i], noi$frecventa[i], noi$perioada[i],
            format(tabel$valoare[pozitie[i]]), format(noi$valoare[i]))
  }
  tabel[pozitie[revizuit], ] <- noi[revizuit, ]
  tabel <- rbind(tabel, noi[nou, ])
  list(tabel = tabel, inserate = sum(nou), revizuite = sum(revizuit), neschimbate = sum(la_fel),
       protejate = sum(protejat))
}

adauga_in_jurnal <- function(intrari) {
  dir.create(dirname(CONFIG$log_csv), showWarnings = FALSE, recursive = TRUE)
  # local time of Chisinau, the GitHub runner is on UTC
  intrari$data <- format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Europe/Chisinau")
  intrari <- intrari[c("data", setdiff(names(intrari), "data"))]
  write.table(intrari, CONFIG$log_csv,
    sep = ",", row.names = FALSE, col.names = !file.exists(CONFIG$log_csv),
    append = file.exists(CONFIG$log_csv), fileEncoding = "UTF-8", qmethod = "double"
  )
}

# the updater keeps the table in memory so fetchers can ask for the last stored day
TABEL_IN_MEMORIE <- new.env()

tine_minte_tabel <- function(tabel) TABEL_IN_MEMORIE$tabel <- tabel
