# Row builders, every fetcher returns rows made here

# one row per value, the period code comes from the frequency and its components
randuri <- function(indicator, frecventa, an, valoare, sursa, trimestru = NA, luna = NA, zi = NA,
                    sursa_detaliu = NA, nota = NA) {
  n <- max(length(an), length(valoare))
  if (n == 0) {
    return(data.frame())
  }
  an <- rep_len(as.integer(an), n)
  trimestru <- rep_len(as.integer(trimestru), n)
  luna <- rep_len(as.integer(luna), n)
  zi <- rep_len(as.integer(zi), n)
  valoare <- round(as.numeric(rep_len(valoare, n)), 4)
  perioada <- switch(frecventa,
    A = perioada_A(an),
    T = perioada_T(an, trimestru),
    L = perioada_L(an, luna),
    Z = perioada_Z(an, luna, zi)
  )

  rezultat <- data.frame(
    indicator = indicator,
    frecventa = frecventa,
    perioada = perioada,
    an = an,
    trimestru = if (frecventa == "T") trimestru else NA_integer_,
    luna = if (frecventa %in% c("L", "Z")) luna else NA_integer_,
    zi = if (frecventa == "Z") zi else NA_integer_,
    valoare = valoare,
    unitate = atribut(indicator, "unitate_ro"),
    tip = "efectiv",
    sursa = sursa,
    sursa_detaliu = as.character(sursa_detaliu),
    data_actualizarii = as.character(Sys.Date()),
    nota = as.character(nota),
    stringsAsFactors = FALSE
  )
  rezultat[!is.na(valoare), ]
}

randuri_anuale <- function(indicator, an, valoare, sursa, ...) {
  randuri(indicator, "A", an, valoare, sursa, ...)
}

randuri_trimestriale <- function(indicator, an, trimestru, valoare, sursa, ...) {
  randuri(indicator, "T", an, valoare, sursa, trimestru = trimestru, ...)
}

randuri_lunare <- function(indicator, an, luna, valoare, sursa, ...) {
  randuri(indicator, "L", an, valoare, sursa, luna = luna, ...)
}

randuri_zilnice <- function(indicator, zi, valoare, sursa, ...) {
  zi <- as.Date(zi)
  randuri(indicator, "Z", format(zi, "%Y"), valoare, sursa, luna = format(zi, "%m"), zi = format(zi, "%d"), ...)
}

# rows of several results in one frame, empty results dropped, their error messages kept
lipeste <- function(lista) {
  erori <- unlist(lapply(lista, attr, "eroare"))
  pline <- Filter(function(x) !is.null(x) && nrow(x) > 0, lista)
  rezultat <- if (length(pline)) do.call(rbind, pline) else data.frame()
  if (length(erori)) attr(rezultat, "eroare") <- paste(erori, collapse = " | ")
  rezultat
}

# runs one fetcher, an error becomes an empty result with the message attached
ruleaza_sursa <- function(nume, fetcher, ...) {
  tryCatch(
    {
      rezultat <- fetcher(...)
      noteaza("%s: %d rânduri", nume, nrow(rezultat))
      rezultat
    },
    error = function(e) {
      noteaza("%s: EROARE - %s", nume, conditionMessage(e))
      gol <- data.frame()
      attr(gol, "eroare") <- paste0(nume, ": ", conditionMessage(e))
      gol
    }
  )
}
