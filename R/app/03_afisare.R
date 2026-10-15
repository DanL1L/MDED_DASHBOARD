# Dashboard pieces, each function builds one part of the page

CULOARE <- "#1f4e79"
TEMA <- bs_theme(version = 5, bootswatch = "flatly", primary = CULOARE)

# ---- data for the current view ----

# rows of the frequencies the app shows, the table may hold more
citeste_tabel_afisat <- function(cale) {
  tabel <- citeste_tabel(cale)
  tabel[tabel$frecventa %in% frecvente_cu_indicatori(), ]
}

# rows of the chosen frequency and years, sorted by indicator and period
randuri_alese <- function(tabel, frecventa, ani) {
  ani <- ani %||% c(max(tabel$an, na.rm = TRUE) - 6, max(tabel$an, na.rm = TRUE))
  alese <- tabel[tabel$frecventa == frecventa & tabel$an >= ani[1] & tabel$an <= ani[2], ]
  alese[order(alese$indicator, alese$ordine), ]
}

serie_efectiva <- function(tabel, frecventa, cod) {
  serie <- tabel[tabel$indicator == cod & tabel$frecventa == frecventa & tabel$tip == "efectiv", ]
  serie[order(serie$ordine), ]
}

# ---- numbers ----

formateaza <- function(x, cod) {
  if (is.na(x)) {
    return("—")
  }
  zecimale <- atribut(cod, "zecimale")
  paste0(format(round(x, zecimale), big.mark = " ", nsmall = zecimale, scientific = FALSE), atribut(cod, "sufix"))
}

# daily compares with the previous value, the others with the same period a year earlier
valoare_precedenta <- function(serie, frecventa) {
  ultima <- serie[nrow(serie), ]
  if (frecventa == "Z") {
    return(serie[nrow(serie) - 1, , drop = FALSE])
  }
  aceeasi_perioada <- switch(frecventa,
    A = rep(TRUE, nrow(serie)),
    T = serie$trimestru == ultima$trimestru,
    L = serie$luna == ultima$luna
  )
  serie[serie$an == ultima$an - 1 & aceeasi_perioada %in% TRUE, ]
}

# percent change, absolute difference or percentage points, as the indicator table says
text_delta <- function(cod, ultima, precedenta, txt) {
  diferenta <- ultima - precedenta
  switch(atribut(cod, "delta"),
    pp = sprintf("%+.1f p.p. %s", diferenta, txt$fata_de),
    abs = sprintf("%+.1f %s", diferenta, txt$fata_de),
    if (precedenta == 0) {
      sprintf("%+.1f %s", diferenta, txt$fata_de)
    } else {
      sprintf("%+.1f%% %s", 100 * diferenta / precedenta, txt$fata_de)
    }
  )
}

# ---- sidebar ----

# only the frequencies that have indicators loaded
selector_frecventa <- function(txt, curenta) {
  alegeri <- txt$frecvente[txt$frecvente %in% frecvente_cu_indicatori()]
  radioButtons("frecventa", txt$frecventa, alegeri, selected = curenta %||% "L", inline = TRUE)
}

selector_ani <- function(tabel, txt, curenti) {
  if (!nrow(tabel)) {
    return(NULL)
  }
  interval <- range(tabel$an, na.rm = TRUE)
  implicit <- c(max(interval[1], interval[2] - 6), interval[2])
  sliderInput("ani", txt$ani, min = interval[1], max = interval[2], value = curenti %||% implicit, step = 1, sep = "")
}

# last update with the hour from the log, the rows only carry the date
ultima_actualizare <- function(tabel) {
  if (file.exists(CONFIG$log_csv)) {
    citeste <- function() read.csv(CONFIG$log_csv, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    jurnal <- tryCatch(citeste(), error = function(e) NULL)
    if (!is.null(jurnal) && nrow(jurnal)) {
      return(substr(max(jurnal$data), 1, 16))
    }
  }
  if (nrow(tabel)) max(tabel$data_actualizarii, na.rm = TRUE) else "—"
}

info_tabel <- function(tabel, txt) {
  tagList(
    p(class = "small mb-1", strong(paste0(txt$tabel_lbl, " ")), CONFIG$store_xlsx),
    p(class = "small mb-1", sprintf(txt$info, nrow(tabel), ultima_actualizare(tabel)))
  )
}

dialog_jurnal <- function(txt) {
  modalDialog(title = txt$jurnal_titlu, size = "xl", easyClose = TRUE, footer = modalButton(txt$inchide),
              DTOutput("jurnal"))
}

# ---- cards ----

# one value box, number, period and the comparison line
cutie <- function(tabel, frecventa, cod, txt) {
  serie <- serie_efectiva(tabel, frecventa, cod)
  are_date <- nrow(serie) > 0
  ultima <- if (are_date) serie[nrow(serie), ] else NULL
  precedenta <- if (are_date) valoare_precedenta(serie, frecventa) else serie
  crestere <- if (cod == "pib") text_crestere_reala(tabel, frecventa, txt)
  unitate <- if (nzchar(atribut(cod, "sufix"))) "" else txt$unit[[cod]]
  # number and unit stay whole, they wrap as two pieces
  intreg <- function(x) tags$span(style = "white-space:nowrap", x)

  value_box(
    title = txt$ind[[cod]],
    value = tags$span(intreg(if (are_date) formateaza(ultima$valoare, cod) else "—"), " ", tags$small(unitate)),
    theme = "primary",
    p(if (are_date) tagList(paste0(txt$perioada, ": "), intreg(ultima$perioada)) else txt$fara_date),
    if (nrow(precedenta)) {
      p(text_delta(cod, ultima$valoare, precedenta$valoare[1], txt), " ", intreg(precedenta$perioada[1]))
    },
    if (!is.null(crestere)) p(crestere)
  )
}

# the GDP card also shows the latest real growth
text_crestere_reala <- function(tabel, frecventa, txt) {
  serie <- serie_efectiva(tabel, frecventa, "pib_real")
  if (!nrow(serie)) {
    return(NULL)
  }
  sprintf("%s %s: %+.1f%%", txt$crestere_reala, serie$perioada[nrow(serie)], serie$valoare[nrow(serie)])
}

# value boxes of one view, in sections when the indicators carry a group
carduri_ui <- function(tabel, frecventa, txt) {
  sectiuni <- sectiuni_pentru(carduri_pentru(frecventa))
  pe_sectiuni(sectiuni, txt, function(coduri) {
    cutii <- lapply(coduri, function(cod) cutie(tabel, frecventa, cod, txt))
    layout_column_wrap(width = "230px", fill = FALSE, heights_equal = "row", !!!cutii)
  })
}

# ---- charts ----

card_grafic <- function(cod) {
  card(
    min_height = 360, full_screen = TRUE,
    card_header(textOutput(paste0("h_", cod), inline = TRUE)),
    plotlyOutput(paste0("g_", cod), height = "100%")
  )
}

grafice_ui <- function(frecventa, txt) {
  sectiuni <- sectiuni_pentru(indicatori_pentru(frecventa))
  pe_sectiuni(sectiuni, txt, function(coduri) layout_columns(col_widths = c(6, 6), !!!lapply(coduri, card_grafic)))
}

# a heading per section when there is more than one
pe_sectiuni <- function(sectiuni, txt, continut) {
  tagList(lapply(names(sectiuni), function(g) {
    tagList(if (length(sectiuni) > 1) h5(txt$grupe[[g]], class = "sectiune"), continut(sectiuni[[g]]))
  }))
}

# line or bars, daily series on a real date axis
grafic <- function(alese, cod, frecventa, txt) {
  serie <- alese[alese$indicator == cod, ]
  validate(need(nrow(serie) > 0, txt$nu_date))
  zilnic <- frecventa == "Z"
  niveluri <- unique(serie$perioada)
  serie$x <- if (zilnic) as.Date(serie$perioada) else factor(serie$perioada, levels = niveluri)
  # notes can be typed by hand, escape them before they enter the hover html
  eticheta <- sprintf("%s<br>%s<br>%s", serie$perioada, vapply(serie$valoare, formateaza, "", cod = cod),
                      ifelse(is.na(serie$nota), "", htmltools::htmlEscape(serie$nota)))

  figura <- if (atribut(cod, "grafic") == "linie") {
    plot_ly(serie,
      x = ~x, y = ~valoare, type = "scatter", mode = if (zilnic) "lines" else "lines+markers",
      text = eticheta, hoverinfo = "text",
      line = list(color = CULOARE), marker = if (zilnic) NULL else list(color = CULOARE)
    )
  } else {
    plot_ly(serie,
      x = ~x, y = ~valoare, type = "bar",
      hovertext = eticheta, hoverinfo = "text", textposition = "none",
      marker = list(color = CULOARE)
    )
  }
  axa_x <- if (zilnic) {
    list(title = "", type = "date")
  } else {
    list(title = "", categoryorder = "array", categoryarray = niveluri)
  }
  figura |>
    layout(showlegend = FALSE, xaxis = axa_x, yaxis = list(title = txt$unit[[cod]], separatethousands = TRUE),
           margin = list(t = 30)) |>
    config(displaylogo = FALSE, displayModeBar = FALSE, responsive = TRUE)
}

# ---- tables ----

tabel_dt <- function(alese, txt) {
  alese$unitate <- unname(txt$unit[alese$indicator])
  alese$indicator <- unname(txt$ind[alese$indicator])
  datatable(alese[names(txt$cols)],
    colnames = unname(txt$cols), rownames = FALSE, filter = "top",
    options = list(pageLength = 50, lengthMenu = c(25, 50, 100, 500), scrollX = TRUE)
  ) |>
    formatRound("valoare", 2, mark = " ")
}

definitii_dt <- function(txt) {
  datatable(txt$def, colnames = txt$def_cols, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
}

jurnal_dt <- function(txt) {
  if (!file.exists(CONFIG$log_csv)) {
    return(datatable(setNames(data.frame(txt$jurnal_gol), txt$nav[["tabel"]]), rownames = FALSE,
                     options = list(dom = "t")))
  }
  jurnal <- read.csv(CONFIG$log_csv, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  jurnal <- jurnal[order(jurnal$data, decreasing = TRUE), ]
  etichete <- ifelse(names(jurnal) %in% names(txt$jurnal_cols), txt$jurnal_cols[names(jurnal)], names(jurnal))
  datatable(jurnal, colnames = unname(etichete), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
}
