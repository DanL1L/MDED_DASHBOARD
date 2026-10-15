# Runs the sources and merges their rows into the table

SURSE <- list(bns = bns_toate, bnm = bnm_toate, mf = mf_toate)

# named list of macroeconomic source fetchers
actualizeaza <- function(surse = SURSE, din_anul = CONFIG$din_anul, dry_run = FALSE) {
  tabel <- citeste_tabel()
  valideaza_tabel(tabel)
  tine_minte_tabel(tabel)
  noteaza("Tabel existent: %d randuri", nrow(tabel))
  jurnal <- list()
  for (nume in names(surse)) {
    noteaza("=== %s ===", toupper(nume))
    inceput <- Sys.time()
    eroare <- ""
    noi <- tryCatch(surse[[nume]](din_anul), error = function(e) {
      eroare <<- conditionMessage(e)
      NULL
    })
    if (!is.null(noi) && !is.data.frame(noi)) {
      eroare <- "Sursa nu a returnat un data.frame"
      noi <- NULL
    }
    if (!is.null(noi) && !nrow(noi) && !nzchar(eroare)) {
      eroare <- attr(noi, "eroare") %||% "Sursa a returnat zero observatii"
    }
    if (!is.null(noi) && nrow(noi) > 0) {
      eroare_sursa <- attr(noi, "eroare") %||% ""
      if (nzchar(eroare_sursa)) eroare <- eroare_sursa
      rezultat <- tryCatch({
        candidat <- imbina_randuri(tabel, noi)
        valideaza_tabel(candidat$tabel)
        candidat
      }, error = function(e) {
        eroare <<- paste("Validare/imbinare:", conditionMessage(e))
        NULL
      })
      if (!is.null(rezultat)) tabel <- rezultat$tabel
    } else rezultat <- NULL
    if (is.null(rezultat)) {
      inserate <- revizuite <- neschimbate <- protejate <- 0L
    } else {
      inserate <- rezultat$inserate
      revizuite <- rezultat$revizuite
      neschimbate <- rezultat$neschimbate
      protejate <- rezultat$protejate
    }
    status <- if (nzchar(eroare)) "EROARE" else "OK"
    noteaza("%s [%s]: inserate=%d revizuite=%d neschimbate=%d; %s", nume,
            status, inserate, revizuite, neschimbate, eroare)
    jurnal[[nume]] <- data.frame(sursa = nume,
      randuri = if (is.null(noi)) 0L else nrow(noi),
      inserate = inserate, revizuite = revizuite, neschimbate = neschimbate,
      protejate = protejate, eroare = eroare, stringsAsFactors = FALSE)
  }
  raport <- if (length(jurnal)) do.call(rbind, jurnal) else data.frame()
  if (dry_run) {
    noteaza("dry-run: tabelul NU a fost scris")
  } else {
    valideaza_tabel(tabel)
    scrie_tabel(tabel)
    if (nrow(raport)) adauga_in_jurnal(raport)
    noteaza("Scris: %s (%d randuri)", CONFIG$store_xlsx, nrow(tabel))
  }
  # A failed source does not prevent successful sources from being saved;
  # the journal and exit status still signal the partial failure.
  invisible(list(tabel = tabel, jurnal = raport, erori = if (nrow(raport)) any(nzchar(raport$eroare)) else FALSE))
}

# last value of every series, printed at the end of a run
rezumat_tabel <- function(tabel) {
  linii <- list()
  for (cod in INDICATORI$cod) {
    for (frecventa in c("T", "A", "L")) {
      serie <- tabel[tabel$indicator == cod & tabel$frecventa == frecventa, ]
      if (!nrow(serie)) next
      serie <- serie[order(serie$ordine), ]
      linii[[length(linii) + 1]] <- data.frame(
        indicator = cod, frecventa = frecventa, randuri = nrow(serie),
        ultima_perioada = utils::tail(serie$perioada, 1), ultima_valoare = utils::tail(serie$valoare, 1),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, linii)
}
