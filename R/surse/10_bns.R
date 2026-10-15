# BNS, PxWeb tables and the quarterly GDP press releases

# ---- PxWeb ----

# one row per cell of the table, dimension codes as columns plus valoare
pxweb_citeste <- function(tabel, selectie) {
  url <- paste0(CONFIG$pxweb_base, "/", utils::URLencode(tabel, reserved = FALSE))
  interogare <- lapply(names(selectie), function(cod) {
    valori <- selectie[[cod]]
    if (identical(valori, "*")) {
      list(code = cod, selection = list(filter = "all", values = list("*")))
    } else {
      list(code = cod, selection = list(filter = "item", values = as.list(as.character(valori))))
    }
  })
  corp <- list(query = interogare, response = list(format = "json-stat2"))
  cub_in_tabel(json_de_la(httr2::req_body_json(cerere(url), corp, auto_unbox = TRUE)))
}

# json-stat cube flattened, the last dimension varies fastest
cub_in_tabel <- function(cub) {
  dimensiuni <- unlist(cub$id)
  coduri <- lapply(dimensiuni, function(d) {
    index <- unlist(cub$dimension[[d]]$category$index)
    names(index)[order(unlist(index))]
  })
  grila <- do.call(expand.grid, c(rev(coduri), stringsAsFactors = FALSE))
  grila <- grila[, rev(seq_along(dimensiuni)), drop = FALSE]
  names(grila) <- dimensiuni
  grila$valoare <- vapply(cub$value, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))
  grila[!is.na(grila$valoare), ]
}

ani_disponibili <- function(tabel, din_anul) {
  url <- paste0(CONFIG$pxweb_base, "/", utils::URLencode(tabel, reserved = FALSE))
  meta <- jsonlite::fromJSON(text_de_la(url), simplifyVector = FALSE)
  ani <- as.integer(unlist(Filter(function(v) v$code == "Ani", meta$variables)[[1]]$values))
  ani[ani >= din_anul]
}

# ---- exploring, used from the console when adding a table ----

# folders and tables one level down, "" for the root
pxweb_lista <- function(cale = "") {
  url <- paste0(CONFIG$pxweb_base, if (nzchar(cale)) paste0("/", utils::URLencode(cale, reserved = FALSE)) else "")
  jsonlite::fromJSON(text_de_la(url))
}

# dimensions of a table with their codes and labels, printed
pxweb_variabile <- function(tabel) {
  url <- paste0(CONFIG$pxweb_base, "/", utils::URLencode(tabel, reserved = FALSE))
  meta <- jsonlite::fromJSON(text_de_la(url), simplifyVector = FALSE)
  cat(meta$title, "
")
  for (v in meta$variables) {
    cat(sprintf("
[%s] %s
", v$code, v$text))
    print(data.frame(cod = unlist(v$values), eticheta = unlist(v$valueTexts)), row.names = FALSE)
  }
  invisible(meta)
}

# PxWeb GDP values come in thousand lei
in_mil_lei <- function(x) if (stats::median(x, na.rm = TRUE) > 5e6) x / 1000 else x

# ---- indicators ----

bns_inflatie <- function(din_anul) {
  tabel_lunar <- CONFIG$px$ipc_lunar
  tabel_anual <- CONFIG$px$ipc_anual
  lunar <- pxweb_citeste(tabel_lunar, list(
    `Grupe de marfuri si servicii` = "0", Ani = ani_disponibili(tabel_lunar, din_anul), Luni = "*"
  ))
  lunar$an <- as.integer(lunar$Ani)
  lunar$luna <- as.integer(lunar$Luni)
  lunar$trimestru <- (lunar$luna - 1) %/% 3 + 1

  # quarterly rate is the mean of the three monthly rates, only full quarters
  medie_si_numar <- function(v) c(medie = mean(v), n = length(v))
  pe_trimestre <- stats::aggregate(valoare ~ an + trimestru, data = lunar, FUN = medie_si_numar)
  pe_trimestre <- data.frame(
    an = pe_trimestre$an, trimestru = pe_trimestre$trimestru,
    medie = pe_trimestre$valoare[, "medie"], n = pe_trimestre$valoare[, "n"]
  )
  pe_trimestre <- pe_trimestre[pe_trimestre$n == 3, ]

  anual <- pxweb_citeste(tabel_anual, list(
    `Grupe de marfuri si servicii` = "0", Ani = ani_disponibili(tabel_anual, din_anul)
  ))

  lipeste(list(
    randuri_lunare("inflatie", lunar$an, lunar$luna, lunar$valoare - 100, "BNS",
                   sursa_detaliu = basename(tabel_lunar), nota = "IPC, luna corespunzătoare a anului precedent = 100"),
    randuri_trimestriale("inflatie", pe_trimestre$an, pe_trimestre$trimestru, pe_trimestre$medie - 100, "BNS",
                         sursa_detaliu = basename(tabel_lunar), nota = "media ratelor anuale lunare din trimestru"),
    randuri_anuale("inflatie", anual$Ani, anual$valoare - 100, "BNS",
                   sursa_detaliu = basename(tabel_anual), nota = "IPC mediu anual, anul precedent = 100")
  ))
}

# customs exports and imports of goods, mil. USD, months are coded 0 to 11
bns_comert_lunar <- function(din_anul) {
  tabel <- CONFIG$px$comert_lunar
  date <- pxweb_citeste(tabel, list(
    Indicatori = c("0", "1"), `Grupe de tari` = "0", Ani = ani_disponibili(tabel, din_anul),
    `Unitatea de masura` = "0", Luni = as.character(0:11)
  ))
  date$luna <- as.integer(date$Luni) + 1L
  exporturi <- date[date$Indicatori == "0", ]
  importuri <- date[date$Indicatori == "1", ]

  lipeste(list(
    randuri_lunare("exporturi_bunuri", exporturi$Ani, exporturi$luna, exporturi$valoare, "BNS",
                   sursa_detaliu = basename(tabel)),
    randuri_lunare("importuri_bunuri", importuri$Ani, importuri$luna, importuri$valoare, "BNS",
                   sursa_detaliu = basename(tabel))
  ))
}

# gross wage, quarters are coded 0 to 3 and the annual value 4
bns_salariu <- function(din_anul) {
  tabel <- CONFIG$px$salariu_trim
  date <- pxweb_citeste(tabel, list(
    Indicatori = "0", `Unitatea de masura` = "0", Ani = ani_disponibili(tabel, din_anul), Trimestre = "*"
  ))
  trimestrial <- date[date$Trimestre %in% c("0", "1", "2", "3"), ]
  anual <- date[date$Trimestre == "4", ]

  lipeste(list(
    randuri_trimestriale("salariu", trimestrial$Ani, as.integer(trimestrial$Trimestre) + 1L, trimestrial$valoare, "BNS",
                         sursa_detaliu = basename(tabel)),
    randuri_anuale("salariu", anual$Ani, anual$valoare, "BNS", sursa_detaliu = basename(tabel))
  ))
}

# revised national accounts, quarters q1 to q4 and the annual value coded 0
bns_pib_pxweb <- function(din_anul) {
  rezultate <- list()

  tabel <- CONFIG$px$pib_trim
  ani <- ani_disponibili(tabel, din_anul)
  if (length(ani)) {
    date <- pxweb_citeste(tabel, list(Indicatori = c("0", "9"), Ani = ani, Trimestrul = "*"))
    date$valoare <- in_mil_lei(date$valoare)
    coduri <- c(pib = "0", exporturi = "9")
    for (indicator in names(coduri)) {
      serie <- date[date$Indicatori == coduri[[indicator]], ]
      trimestrial <- serie[serie$Trimestrul != "0", ]
      anual <- serie[serie$Trimestrul == "0", ]
      rezultate[[paste(indicator, "T")]] <- randuri_trimestriale(
        indicator, trimestrial$Ani, sub("q", "", trimestrial$Trimestrul), trimestrial$valoare, "BNS",
        sursa_detaliu = basename(tabel)
      )
      rezultate[[paste(indicator, "A")]] <- randuri_anuale(
        indicator, anual$Ani, anual$valoare, "BNS", sursa_detaliu = basename(tabel)
      )
    }
  }

  tabel_real <- CONFIG$px$pib_trim_real
  ani <- ani_disponibili(tabel_real, din_anul)
  if (length(ani)) {
    indici <- pxweb_citeste(tabel_real, list(Indicatori = "0", Ani = ani, Trimestrul = "*"))
    trimestrial <- indici[indici$Trimestrul != "0", ]
    anual <- indici[indici$Trimestrul == "0", ]
    rezultate[["pib_real T"]] <- randuri_trimestriale(
      "pib_real", trimestrial$Ani, sub("q", "", trimestrial$Trimestrul), trimestrial$valoare - 100, "BNS",
      sursa_detaliu = basename(tabel_real)
    )
    rezultate[["pib_real A"]] <- randuri_anuale(
      "pib_real", anual$Ani, anual$valoare - 100, "BNS", sursa_detaliu = basename(tabel_real)
    )
  }
  lipeste(rezultate)
}

# ---- press releases ----

# year and quarter of each GDP press release on the national accounts page, oldest first
comunicate_pib <- function(din_anul) {
  pagina <- text_de_la(CONFIG$bns_conturi_nationale)
  adrese <- sub("\\s+$", "", linkuri(pagina, CONFIG$bns_pib_slug_regex, baza = "https://statistica.gov.md"))
  slug <- basename(adrese)
  bucata <- function(tipar) {
    vapply(slug, function(s) {
      gasit <- regmatches(s, regexpr(tipar, s, perl = TRUE))
      if (length(gasit)) gasit else NA_character_
    }, "", USE.NAMES = FALSE)
  }
  lista <- data.frame(
    url = adrese,
    an = as.integer(bucata("20[0-9]{2}(?=-9497_)")),
    trimestru = numar_roman(sub("trimestrul-", "", bucata("trimestrul-(iv|iii|ii|i)(?=-)"))),
    id = as.integer(sub(".*_(\\d+)\\.html$", "\\1", slug)),
    stringsAsFactors = FALSE
  )
  lista <- lista[!is.na(lista$an) & !is.na(lista$trimestru) & lista$an >= din_anul, ]
  lista[order(lista$id), ]
}

# the xlsx annex of one press release, downloaded once into the cache
anexa_comunicat <- function(url) {
  pagina <- text_sau_nimic(url)
  if (is.null(pagina)) {
    return(NULL)
  }
  xlsx <- linkuri(pagina, "Res[-_]Utiliz[-_]PIB[^\"']*trim[^\"']*\\.xlsx?$", baza = "https://statistica.gov.md")
  xlsx <- xlsx[!grepl("eng", xlsx, ignore.case = TRUE)]
  if (!length(xlsx)) {
    return(NULL)
  }
  fisier <- tryCatch(descarca(xlsx[1]), error = function(e) NULL)
  if (is.null(fisier)) NULL else list(url = xlsx[1], fisier = fisier)
}

# first numeric cell to the right of a label, a_cata picks a later one
valoare_dupa_eticheta <- function(foaie, tipar, a_cata = 1) {
  celule <- as.matrix(foaie)
  for (r in seq_len(nrow(celule))) {
    lovit <- which(grepl(tipar, celule[r, ], ignore.case = TRUE, perl = TRUE))
    if (!length(lovit)) next
    numere <- numar(celule[r, (lovit[1] + 1):ncol(celule)])
    numere <- numere[!is.na(numere)]
    if (length(numere) >= a_cata) {
      return(numere[a_cata])
    }
  }
  NA_real_
}

# GDP, real growth and exports from one annex, quarterly sheet A1 and, in Q4, the annual sheet A4
citeste_anexa <- function(fisier, an, trimestru, url) {
  din_foaie <- function(foaie, frecventa) {
    celule <- readxl::read_excel(fisier, sheet = foaie, col_names = FALSE, col_types = "text", .name_repair = "minimal")
    pib <- valoare_dupa_eticheta(celule, "^\\s*PRODUSUL INTERN BRUT\\s*$", 1)
    indice <- valoare_dupa_eticheta(celule, "^\\s*PRODUSUL INTERN BRUT\\s*$", 3)
    export <- valoare_dupa_eticheta(celule, "^\\s*Exportul de bunuri .{1,3} servicii", 1)
    if (is.na(pib)) stop("nu găsesc rândul PIB în foaia ", foaie)
    trim <- if (frecventa == "T") trimestru else NA
    lipeste(list(
      randuri("pib", frecventa, an, in_mil_lei(pib), "BNS", trimestru = trim,
              sursa_detaliu = url, nota = "comunicat BNS"),
      if (!is.na(indice) && indice > 50 && indice < 200) {
        randuri("pib_real", frecventa, an, indice - 100, "BNS", trimestru = trim,
                sursa_detaliu = url, nota = "comunicat BNS")
      },
      if (!is.na(export)) randuri("exporturi", frecventa, an, in_mil_lei(export), "BNS", trimestru = trim,
                                  sursa_detaliu = url, nota = "comunicat BNS")
    ))
  }
  foi <- readxl::excel_sheets(fisier)
  a1 <- foi[grepl("^A1", foi)][1]
  a4 <- foi[grepl("^A4", foi)][1]
  lipeste(list(
    if (!is.na(a1)) din_foaie(a1, "T"),
    if (!is.na(a4) && trimestru == 4) din_foaie(a4, "A")
  ))
}

# preliminary quarters from the press releases, for years the revised PxWeb tables do not cover yet
bns_pib_comunicate <- function(din_anul) {
  lista <- comunicate_pib(din_anul)
  rezultate <- list()
  for (i in seq_len(nrow(lista))) {
    anexa <- anexa_comunicat(lista$url[i])
    if (is.null(anexa)) {
      noteaza("  comunicat %dT%d: fără anexă xlsx", lista$an[i], lista$trimestru[i])
      next
    }
    rezultate[[i]] <- tryCatch(
      citeste_anexa(anexa$fisier, lista$an[i], lista$trimestru[i], anexa$url),
      error = function(e) {
        noteaza("  anexa %s: %s", basename(anexa$fisier), conditionMessage(e))
        NULL
      }
    )
  }
  lipeste(rezultate)
}

bns_toate <- function(din_anul) {
  pxweb <- ruleaza_sursa("BNS PIB/exporturi PxWeb", bns_pib_pxweb, din_anul)
  # press releases only for years after the revised series
  din_comunicate <- if (nrow(pxweb)) max(pxweb$an) + 1L else din_anul
  lipeste(list(
    ruleaza_sursa("BNS inflație", bns_inflatie, din_anul),
    ruleaza_sursa("BNS salariu", bns_salariu, din_anul),
    ruleaza_sursa("BNS comerț lunar", bns_comert_lunar, din_anul),
    pxweb,
    ruleaza_sursa("BNS PIB comunicate", bns_pib_comunicate, max(din_anul, din_comunicate))
  ))
}
