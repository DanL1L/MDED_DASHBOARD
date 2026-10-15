# MF, monthly BPN execution reports (xls) from the open data catalogue

LUNI_SCURTE <- c(
  ian = 1, feb = 2, mar = 3, apr = 4, mai = 5, iun = 6,
  iul = 7, aug = 8, sep = 9, oct = 10, noi = 11, dec = 12
)

# ---- catalogue ----

# the current year has its own page, older years one page each, either may move
catalog_html <- function(an) {
  an_curent <- as.integer(format(Sys.Date(), "%Y"))
  cale <- if (an >= an_curent) CONFIG$mf_catalog_curent else sprintf(CONFIG$mf_catalog_an, an)
  alternativa <- if (an >= an_curent) sprintf(CONFIG$mf_catalog_an, an) else CONFIG$mf_catalog_curent
  tryCatch(
    text_de_la(paste0(CONFIG$mf_base, cale)),
    error = function(e) text_de_la(paste0(CONFIG$mf_base, alternativa))
  )
}

# "RAPORT BPN 30.06.2026.xls" or "RAPORT BPN 30 iunie 2026.xls" as a date
data_din_nume <- function(nume) {
  nume <- tolower(utils::URLdecode(nume))
  tipar <- "(\\d{1,2})[._ -]+(\\d{1,2}|[a-zăâîșşțţ]+)[._ -]+(\\d{2,4})"
  gasit <- regmatches(nume, regexec(tipar, nume, perl = TRUE))[[1]]
  if (length(gasit) < 4) {
    return(NA)
  }
  zi <- as.integer(gasit[2])
  an <- as.integer(gasit[4])
  if (an < 100) an <- an + 2000
  luna <- suppressWarnings(as.integer(gasit[3]))
  if (is.na(luna)) luna <- unname(LUNI_SCURTE[substr(gasit[3], 1, 3)])
  if (is.na(luna) || is.na(zi)) {
    return(NA)
  }
  as.Date(sprintf("%d-%02d-%02d", an, luna, min(zi, 28)))
}

# one link per monthly report of the requested years, the December report may sit on next year's page
rapoarte_bpn <- function(ani) {
  gasite <- list()
  for (an_pagina in unique(c(ani, max(ani) + 1))) {
    html <- tryCatch(catalog_html(an_pagina), error = function(e) NULL)
    if (is.null(html)) next
    for (url in linkuri(html, "RAPORT(%20| )BPN[^\"']*\\.xlsx?$", baza = CONFIG$mf_base)) {
      data <- data_din_nume(basename(url))
      if (is.na(data)) next
      gasite[[length(gasite) + 1]] <- data.frame(
        url = url, data = data, an = as.integer(format(data, "%Y")), luna = as.integer(format(data, "%m")),
        pagina = an_pagina, stringsAsFactors = FALSE
      )
    }
  }
  lista <- do.call(rbind, gasite)
  if (is.null(lista)) stop("nu am găsit rapoarte BPN în catalogul MF")
  lista <- lista[lista$an %in% ani, ]
  lista <- lista[order(lista$data, lista$pagina), ]
  lista[!duplicated(lista$data, fromLast = TRUE), ]
}

# ---- reading a report ----

# every cell of a sheet as text
celule_excel <- function(fisier, foaie) {
  foaie <- readxl::read_excel(fisier, sheet = foaie, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  as.matrix(foaie)
}

# cumulative "Executat" value of each code, as a named vector
coduri_din_raport <- function(fisier, coduri) {
  foi <- readxl::excel_sheets(fisier)
  foaie <- if ("BPN" %in% foi) "BPN" else foi[1]
  celule <- celule_excel(fisier, foaie)
  celule[is.na(celule)] <- ""

  # the header row holds "Cod", year-end reports add Aprobat and Precizat columns before Executat
  antet <- which(apply(celule, 1, function(rand) any(grepl("^cod", trimws(rand), ignore.case = TRUE))))[1]
  if (is.na(antet)) stop("nu găsesc antetul cu 'Cod' în ", basename(fisier))
  coloana_cod <- which(grepl("^cod", trimws(celule[antet, ]), ignore.case = TRUE))[1]
  coloana_executat <- NA
  for (r in antet:min(antet + 3, nrow(celule))) {
    lovit <- which(grepl("^executat", trimws(celule[r, ]), ignore.case = TRUE))
    lovit <- lovit[!grepl("preced", celule[r, lovit], ignore.case = TRUE)]
    if (length(lovit)) {
      coloana_executat <- min(lovit)
      break
    }
  }
  if (is.na(coloana_executat)) stop("nu găsesc coloana 'Executat' în ", basename(fisier))

  coduri_foaie <- sub("\\s.*$", "", trimws(celule[, coloana_cod]))
  valori <- setNames(rep(NA_real_, length(coduri)), coduri)
  for (cod in coduri) {
    rand <- which(coduri_foaie == cod & seq_len(nrow(celule)) > antet)[1]
    if (!is.na(rand)) valori[[cod]] <- numar(celule[rand, coloana_executat])
  }
  lipsa <- names(valori)[is.na(valori)]
  if (length(lipsa)) stop("nu găsesc codurile ", paste(lipsa, collapse = ","), " în ", basename(fisier))
  valori
}

# every monthly report of the requested years with the cumulative values of all needed codes
cumulate_bpn <- function(ani) {
  coduri <- unique(c(CONFIG$mf_coduri_venituri_fiscale, CONFIG$mf_coduri_bpn_lunar))
  rapoarte <- rapoarte_bpn(ani)
  valori <- matrix(NA_real_, nrow(rapoarte), length(coduri), dimnames = list(NULL, coduri))
  for (i in seq_len(nrow(rapoarte))) {
    fisier <- tryCatch(descarca(rapoarte$url[i]), error = function(e) NULL)
    if (is.null(fisier)) next
    citite <- tryCatch(coduri_din_raport(fisier, coduri), error = function(e) {
      noteaza("  %s: %s", basename(fisier), conditionMessage(e))
      NULL
    })
    if (!is.null(citite)) valori[i, ] <- citite[coduri]
  }
  cbind(rapoarte, valori)
}

# monthly flow of a cumulative series, January is its own value
flux_lunar <- function(cumulat, luna) {
  ordine <- order(luna)
  cumulat <- cumulat[ordine]
  luna <- luna[ordine]
  precedent <- cumulat[match(luna - 1, luna)]
  precedent[luna == 1] <- 0
  data.frame(luna = luna, flux = cumulat - precedent)[!is.na(precedent) & !is.na(cumulat), ]
}

# ---- indicators ----

# tax revenues, quarterly flow from the March, June, September and December reports
mf_venituri_fiscale <- function(cumulate) {
  cumulate$impozite <- rowSums(cumulate[, CONFIG$mf_coduri_venituri_fiscale, drop = FALSE])
  cumulate <- cumulate[!is.na(cumulate$impozite) & cumulate$luna %in% c(3, 6, 9, 12), ]
  rezultate <- list()
  for (an in unique(cumulate$an)) {
    al_anului <- cumulate[cumulate$an == an, ]
    pe_luni <- setNames(al_anului$impozite, al_anului$luna)
    for (trimestru in 1:4) {
      luna <- as.character(trimestru * 3)
      luna_precedenta <- as.character(trimestru * 3 - 3)
      if (!luna %in% names(pe_luni)) next
      if (trimestru > 1 && !luna_precedenta %in% names(pe_luni)) next
      flux <- if (trimestru == 1) pe_luni[[luna]] else pe_luni[[luna]] - pe_luni[[luna_precedenta]]
      rezultate[[length(rezultate) + 1]] <- randuri_trimestriale(
        "venituri_fiscale", an, trimestru, flux, "MF",
        sursa_detaliu = al_anului$url[al_anului$luna == trimestru * 3][1],
        nota = "flux trimestrial = diferența cumulatelor"
      )
    }
    if ("12" %in% names(pe_luni)) {
      rezultate[[length(rezultate) + 1]] <- randuri_anuale(
        "venituri_fiscale", an, pe_luni[["12"]], "MF", sursa_detaliu = al_anului$url[al_anului$luna == 12][1]
      )
    }
  }
  lipeste(rezultate)
}

# monthly total revenues and expenditures, flow of the month
mf_bpn_lunar <- function(cumulate) {
  rezultate <- list()
  for (cod in names(CONFIG$mf_coduri_bpn_lunar)) {
    coloana <- CONFIG$mf_coduri_bpn_lunar[[cod]]
    for (an in unique(cumulate$an)) {
      al_anului <- cumulate[cumulate$an == an & !is.na(cumulate[[coloana]]), ]
      if (!nrow(al_anului)) next
      flux <- flux_lunar(al_anului[[coloana]], al_anului$luna)
      rezultate[[length(rezultate) + 1]] <- randuri_lunare(
        cod, an, flux$luna, flux$flux, "MF",
        sursa_detaliu = al_anului$url[match(flux$luna, al_anului$luna)],
        nota = "flux lunar = diferența cumulatelor"
      )
    }
  }
  lipeste(rezultate)
}

# annual tax revenues from the BPN 2016-20xx file, the reference for the annual series
mf_bpn_anual <- function(din_anul) {
  html <- catalog_html(as.integer(format(Sys.Date(), "%Y")))
  adrese <- linkuri(html, "BPN-?(%20| )?2016-?(%20| )?20[0-9]{2}\\.xlsx?$", baza = CONFIG$mf_base)
  if (!length(adrese)) stop("nu găsesc fișierul 'BPN 2016-20xx' în catalogul MF")
  url <- adrese[length(adrese)]
  fisier <- descarca(url, refresh = TRUE)
  foi <- readxl::excel_sheets(fisier)
  celule <- celule_excel(fisier, foi[grepl("^BPN", foi)][1])

  este_an <- function(x) !is.na(x) & grepl("^20[0-9]{2}$", trimws(x))
  antet <- which(rowSums(matrix(este_an(celule), nrow = nrow(celule))) >= 3)[1]
  if (is.na(antet)) stop("nu găsesc rândul cu anii în ", basename(fisier))
  coloane_ani <- which(este_an(celule[antet, ]))
  ani <- as.integer(trimws(celule[antet, coloane_ani]))

  total <- setNames(rep(0, length(ani)), ani)
  for (cod in CONFIG$mf_coduri_venituri_fiscale) {
    rand <- which(trimws(celule[, 1]) == cod)[1]
    if (is.na(rand)) stop("nu găsesc codul veniturilor fiscale în ", basename(fisier))
    total <- total + numar(celule[rand, coloane_ani])
  }
  alese <- ani >= din_anul & !is.na(total)
  randuri_anuale("venituri_fiscale", ani[alese], total[alese], "MF", sursa_detaliu = url, nota = "seria anuală BPN")
}

mf_toate <- function(din_anul) {
  ani <- seq(din_anul, as.integer(format(Sys.Date(), "%Y")))
  cumulate <- ruleaza_sursa("MF rapoarte BPN", cumulate_bpn, ani)
  lunare <- if (nrow(cumulate)) {
    lipeste(list(
      ruleaza_sursa("MF venituri fiscale", mf_venituri_fiscale, cumulate),
      ruleaza_sursa("MF venituri și cheltuieli BPN", mf_bpn_lunar, cumulate)
    ))
  } else {
    cumulate
  }
  anual <- ruleaza_sursa("MF seria anuală", mf_bpn_anual, din_anul)
  # the annual file wins over the December reports, they differ by rounding
  if (nrow(anual) && nrow(lunare)) {
    lunare <- lunare[!(lunare$indicator == "venituri_fiscale" & lunare$frecventa == "A" & lunare$an %in% anual$an), ]
  }
  lipeste(list(lunare, anual))
}
