# BNM monthly series from the interactive database BDI
# v3: correct online request start dates; CONFIG$zile_istoric is not defined
# in R/00_config.R. Use a rolling 365-day window.

# ---- BDI reports ----

# the browser form as an ajax post, the answer names a temporary html file holding the report
bdi_genereaza <- function(url, campuri) {
  campuri <- c(list(
    `javax.faces.partial.ajax` = "true", `javax.faces.source` = "downloadReport",
    `javax.faces.partial.execute` = "@all",
    `javax.faces.partial.render` = "input iFrameContent iReportParams", downloadReport = "downloadReport",
    f_content = "f_content", paramId = "", paramIdTest = "?id=0&lang=ro",
    `report-tabview-comm_activeIndex` = "0", `javax.faces.ViewState` = "stateless"
  ), campuri)
  raspuns <- cerere(url) |>
    httr2::req_headers(`Faces-Request` = "partial/ajax", `X-Requested-With` = "XMLHttpRequest") |>
    httr2::req_body_form(!!!campuri, .multi = "explode") |>
    httr2::req_perform()
  xml <- httr2::resp_body_string(raspuns)
  cale <- regmatches(xml, regexpr("/bdi/pages/user/www/we/temp/[^\"' ]+\\.html", xml))
  if (!length(cale)) stop("BDI nu a generat raportul (validare eșuată sau structura paginii s-a schimbat)")
  text_de_la(paste0(CONFIG$bnm_base, cale))
}

# Parse BDI HTML, including HTML entities and merged header cells.
bdi_curata <- function(x) {
  x <- gsub("<div[^>]*style=[\"']display: none;?[\"'][^>]*>.*?</div>", "", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x, perl = TRUE)
  x <- gsub("&#x[aA]0;|&#160;|&nbsp;|\u00a0", " ", x, perl = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

bdi_randuri <- function(html) {
  tabel <- regmatches(html, regexpr("(?s)<table[^>]*id=[\"']__bookmark_1[\"'].*?</table>", html, perl = TRUE))
  if (!length(tabel)) stop("raportul BDI nu conține tabelul așteptat")
  randuri <- regmatches(tabel, gregexpr("(?s)<tr\\b[^>]*>.*?</tr>", tabel, perl = TRUE))[[1]]
  lapply(randuri, function(rand) {
    celule <- regmatches(rand, gregexpr("(?s)<t[dh]\\b[^>]*>.*?</t[dh]>", rand, perl = TRUE))[[1]]
    bdi_curata(celule)
  })
}

bdi_raport <- function(url, campuri) bdi_randuri(bdi_genereaza(url, campuri))
numar_bdi <- function(x) suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
luna_fara_zero <- function(zi) as.character(as.integer(format(zi, "%m")))

# Month labels may be full Romanian names or abbreviations, with HTML nbsp and irregular spacing.
bdi_luna <- function(x) {
  x <- tolower(trimws(x))
  x <- chartr("ăâîșşțţ", "aaisstt", x)
  x <- gsub("\\.", "", x)
  nume <- c("ian", "feb", "mar", "apr", "mai", "iun", "iul", "aug", "sep", "oct", "noi", "dec")
  abrev <- substr(x, 1, 3)
  abrev[abrev == "nov"] <- "noi"
  idx <- match(abrev, nume)
  as.integer(idx)
}

antet_lunar <- function(randuri) {
  este_luna <- function(x) grepl("^[[:alpha:]ăâîșşțĂÂÎȘŞȚ]+\\.? +[0-9]{4}$", x, perl = TRUE)
  pozitie <- which(vapply(randuri, function(celule) any(este_luna(celule)), logical(1)))[1]
  if (is.na(pozitie)) stop("raportul BDI nu are coloane lunare")
  celule <- randuri[[pozitie]]
  coloane <- which(este_luna(celule))
  luni <- bdi_luna(sub(" +[0-9]{4}$", "", celule[coloane]))
  if (anyNA(luni)) stop("lunile din antetul BDI nu au putut fi identificate")
  list(pozitie = pozitie, coloane = coloane, luni = luni,
       ani = as.integer(sub("^.* +", "", celule[coloane])))
}

# ---- transfers ----

# BDI report DBP4, net transfers to individuals per month
bnm_transferuri <- function(din_anul) {
  randuri <- bdi_raport(CONFIG$bnm_bdi_transferuri, list(
    treeMenu_selection = "0_4_6", treeMenu_scrollState = "0,0",
    # report rows, required or the report is empty
    itemTree_selection = "0,1,2,2_0,2_1,2_2", itemTree_scrollState = "0,0",
    yearBeg_focus = "", yearBeg_input = as.character(din_anul),
    yearEnd_focus = "", yearEnd_input = format(Sys.Date(), "%Y"),
    periodicity_focus = "", periodicity_input = "1", months_focus = "", months = as.character(1:12),
    quarters_focus = "", quarters = as.character(1:4)
  ))
  antet <- randuri[[1]]
  coloane <- which(grepl("^\\d{2}\\.\\d{4}$", antet))
  total <- Find(function(celule) any(grepl("^Total intr", celule)), randuri[-1])
  if (is.null(total) || !length(coloane)) stop("nu găsesc rândul 'Total intrări' în raportul DBP4")
  perioade <- antet[coloane]
  randuri_lunare("transferuri", substr(perioade, 4, 7), substr(perioade, 1, 2), numar(total[coloane]), "BNM",
                 sursa_detaliu = CONFIG$bnm_bdi_transferuri, nota = "bază netă, total intrări")
}

# ---- treasury bills ----
# DOP5: column 1 = period (timestamp + Romanian label), 3 = effective 91d, 7 = effective 364d.
bnm_vms_din_randuri <- function(randuri) {
  rezultate <- list()
  an <- NA_integer_
  for (celule in randuri) {
    if (length(celule) < 7L) next
    perioada <- celule[1]
    # The HTML contains e.g. "Sep 1, 2026, 12:00 AM Septembrie".
    data <- regmatches(perioada, regexpr("^[A-Za-z]{3} +[0-9]{1,2}, +[0-9]{4}", perioada))
    if (length(data)) {
      an <- as.integer(sub("^.*,[[:space:]]*([0-9]{4})$", "\\1", data))
      luna <- match(substr(tolower(data), 1, 3),
                    c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
    } else {
      nume <- sub(".*?([[:alpha:]ăâîșşțĂÂÎȘŞȚ]+)( +[0-9]{4})?$", "\\1", perioada, perl = TRUE)
      luna <- bdi_luna(nume)
      if (grepl("[0-9]{4}$", perioada)) an <- as.integer(sub("^.*([0-9]{4})$", "\\1", perioada))
    }
    if (is.na(luna) || is.na(an)) next
    v91 <- numar_bdi(celule[3]); v364 <- numar_bdi(celule[7])
    if (is.na(v91) && is.na(v364)) next
    rezultate[[length(rezultate) + 1L]] <- lipeste(list(
      randuri_lunare("vms_91", an, luna, v91, "BNM",
                    sursa_detaliu = CONFIG$bnm_bdi_vms, nota = "rata efectivă medie ponderată, piața primară"),
      randuri_lunare("vms_364", an, luna, v364, "BNM",
                    sursa_detaliu = CONFIG$bnm_bdi_vms, nota = "rata efectivă medie ponderată, piața primară")
    ))
  }
  if (!length(rezultate)) stop("raportul DOP5 nu are rânduri lunare")
  lipeste(rezultate)
}

bnm_vms <- function(...) {
  azi <- Sys.Date()
  inceput <- azi - 365L
  randuri <- bdi_raport(CONFIG$bnm_bdi_vms, list(
    treeMenu_selection = "0_1_2", treeMenu_scrollState = "0,0",
    yearBeg_focus = "", yearBeg_input = format(inceput, "%Y"),
    yearEnd_focus = "", yearEnd_input = format(azi, "%Y")
  ))
  bnm_vms_din_randuri(randuri)
}

# ---- FX interventions ----
# DOVRE2: spot section A, purchase row 1 and sale row 2, values in EUR/USD.
bnm_interventii_din_randuri <- function(randuri) {
  antet <- antet_lunar(randuri)
  n <- length(antet$coloane)
  valute <- c(bnm_net_usd = "Dolar S.U.A.", bnm_net_eur = "Euro")
  sume <- list(); sectiune <- ""; operatie <- ""
  for (celule in randuri[-seq_len(antet$pozitie)]) {
    if (any(celule %in% LETTERS)) sectiune <- celule[celule %in% LETTERS][1]
    if (any(grepl("cumpărare|cumparare", celule, ignore.case = TRUE))) operatie <- "cumparare"
    if (any(grepl("vânzare|vanzare", celule, ignore.case = TRUE))) operatie <- "vanzare"
    if (sectiune != "A" || !nzchar(operatie)) next
    for (cod in names(valute)) {
      if (!(valute[[cod]] %in% celule)) next
      valori <- numar_bdi(utils::tail(celule, n))
      valori[is.na(valori)] <- 0
      sume[[paste(cod, operatie)]] <- valori
    }
  }
  # Never silently turn an unrecognized report into an all-zero time series.
  if (!length(sume)) stop("DOVRE2: nu au fost identificate operațiunile spot")
  zero <- rep(0, n)
  lipeste(lapply(names(valute), function(cod) {
    net <- (sume[[paste(cod, "cumparare")]] %||% zero) -
           (sume[[paste(cod, "vanzare")]] %||% zero)
    randuri_lunare(cod, antet$ani, antet$luni, round(net / 1e6, 2), "BNM",
                   sursa_detaliu = CONFIG$bnm_bdi_interventii,
                   nota = "cumpărări minus vânzări la vedere, milioane")
  }))
}

bnm_interventii <- function(...) {
  azi <- Sys.Date(); inceput <- azi - 365L
  randuri <- bdi_raport(CONFIG$bnm_bdi_interventii, list(
    treeMenu_selection = "0_5_7", treeMenu_scrollState = "0,0",
    monthBeg_focus = "", monthBeg_input = luna_fara_zero(inceput),
    yearBeg_focus = "", yearBeg_input = format(inceput, "%Y"),
    monthEnd_focus = "", monthEnd_input = luna_fara_zero(azi),
    yearEnd_focus = "", yearEnd_input = format(azi, "%Y"),
    currency_focus = "", currency = c("44", "47"),
    operationTradeType_focus = "", operationTradeType = c("1", "3", "2"),
    operationDirectType_focus = "", operationDirectType = c("sale", "purchase")
  ))
  bnm_interventii_din_randuri(randuri)
}

# ---- new loans ----
# DPMC8: 12 month labels, followed by 2 columns/month (nominal rate and volume).
bnm_credite_noi_din_randuri <- function(randuri) {
  antet <- antet_lunar(randuri)
  n <- length(antet$coloane)
  serii <- list(
    credite_noi_lei = list(moneda = "moneda", categorie = "^1 ", coloana = "volum"),
    credite_noi_valuta = list(moneda = "valut", categorie = "^1 ", coloana = "volum"),
    rata_credite_lei = list(moneda = "moneda", categorie = "^1 ", coloana = "rata"),
    rata_credite_juridice = list(moneda = "moneda", categorie = "^2 ", coloana = "rata"),
    rata_credite_imobil = list(moneda = "moneda", categorie = "^4 ", coloana = "rata")
  )
  rezultate <- list(); moneda <- ""
  for (celule in randuri[-seq_len(antet$pozitie)]) {
    if (length(celule) < 2L * n + 2L) next
    if (length(celule) >= 2L * n + 3L) moneda <- celule[1]
    categorie <- celule[length(celule) - 2L * n - 1L]
    valori <- numar_bdi(utils::tail(celule, 2L * n))
    rata <- valori[seq.int(1L, 2L * n, by = 2L)]
    volum <- valori[seq.int(2L, 2L * n, by = 2L)]
    for (cod in names(serii)) {
      serie <- serii[[cod]]
      if (!grepl(serie$moneda, moneda, ignore.case = TRUE) ||
          !grepl(serie$categorie, categorie)) next
      valori_seriei <- if (serie$coloana == "rata") rata else volum
      rezultate[[cod]] <- randuri_lunare(cod, antet$ani, antet$luni, valori_seriei, "BNM",
                                         sursa_detaliu = CONFIG$bnm_bdi_credite,
                                         nota = "total pe termene")
    }
  }
  lipsa <- setdiff(names(serii), names(rezultate))
  if (length(lipsa)) stop("DPMC8: lipsesc seriile: ", paste(lipsa, collapse = ", "))
  lipeste(rezultate)
}

bnm_credite_noi <- function(...) {
  azi <- Sys.Date(); inceput <- azi - 365L
  randuri <- bdi_raport(CONFIG$bnm_bdi_credite, list(
    treeMenu_selection = "0_0_3_0", treeMenu_scrollState = "0,0",
    itemTree_selection = "0,1", itemTree_scrollState = "0,0",
    periodicity_focus = "", periodicity_input = "1",
    yearBeg_focus = "", yearBeg_input = format(inceput, "%Y"),
    monthBeg_focus = "", monthBeg_input = luna_fara_zero(inceput),
    yearEnd_focus = "", yearEnd_input = format(azi, "%Y"),
    monthEnd_focus = "", monthEnd_input = luna_fara_zero(azi),
    positionOfOperation_focus = "", positionOfOperation = c("1", "2"),
    positionOfCategory_focus = "", positionOfCategory = c("1", "2", "4"),
    positionOfTerm_focus = "", positionOfTerm = "10"
  ))
  bnm_credite_noi_din_randuri(randuri)
}

bnm_toate <- function(din_anul) {
  lipeste(list(
    ruleaza_sursa("BNM transferuri", bnm_transferuri, din_anul),
    ruleaza_sursa("BNM rate VMS", bnm_vms),
    ruleaza_sursa("BNM intervenții valutare", bnm_interventii),
    ruleaza_sursa("BNM credite noi", bnm_credite_noi)
  ))
}
