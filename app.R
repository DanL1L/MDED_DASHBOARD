# Economic Data Dashboard, Shiny entry point
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(DT)
})
# shared files and dashboard UI; source fetchers run only in the updater
fisiere_comune <- list.files("R", pattern = "[.]R$", full.names = TRUE)
fisiere_app <- list.files("R/app", pattern = "[.]R$", full.names = TRUE)
for (f in c(sort(fisiere_comune), sort(fisiere_app))) source(f, encoding = "UTF-8")

# rows of the shown frequencies, read once per process and re-read within a minute after a change
TABEL <- reactiveFileReader(60000, NULL, CONFIG$store_xlsx, citeste_tabel_afisat)

STIL <- "
  .bslib-value-box .value-box-title { font-size: 1.15rem; }
  .bslib-value-box .value-box-value { font-size: 2rem; line-height: 1.15; }
  .bslib-value-box .value-box-value small { font-size: 1rem; white-space: nowrap; }
  .bslib-value-box .value-box-area { overflow: hidden; }
  .bslib-value-box p { font-size: 1rem; margin-bottom: 0.15rem; }
  .sectiune { margin: 1.2rem 0 0.6rem 0; font-weight: 600; color: #1f4e79; border-bottom: 2px solid #1f4e79; }
  .nav-underline .form-check { margin-right: 0.6rem; } .nav-underline .form-check label { font-weight: 600; }
  @media (max-width: 576px) {
    .bslib-value-box .value-box-value { font-size: 1.6rem; }
    .bslib-value-box p { font-size: 0.95rem; }
    .nav-underline { flex-wrap: wrap; }
    .card-header { font-size: 0.95rem; }
  }
"

ui_panou <- page_sidebar(
  title = "Economic Data Dashboard",
  fillable = FALSE,
  theme = TEMA,
  tags$head(
    script_cookie(),
    tags$style(HTML(STIL)),
    # charts resize after the sidebar opens or closes
    tags$script(HTML(
      "$(document).on('bslib.sidebar', function() { setTimeout(function() { $(window).trigger('resize'); }, 400); });"
    ))
  ),
  sidebar = sidebar(
    width = 290, open = "desktop",
    uiOutput("frecventa_ui"),
    uiOutput("ani_ui"),
    hr(),
    uiOutput("ajutor_ui"),
    uiOutput("info_tabel"),
    div(style = "margin-top:auto", uiOutput("jurnal_ui"), if (!is.null(CREDENTIALE)) uiOutput("iesire_ui"))
  ),
  navset_underline(
    nav_panel(textOutput("nav_panou", inline = TRUE),
      value = "panou", class = "pt-3",
      uiOutput("carduri"),
      uiOutput("grafice_ui")
    ),
    nav_panel(textOutput("nav_tabel", inline = TRUE),
      value = "tabel", class = "pt-3",
      div(
        class = "d-flex justify-content-between align-items-center mb-2",
        textOutput("tabel_titlu", inline = TRUE),
        uiOutput("descarca_ui", inline = TRUE)
      ),
      DTOutput("tabel")
    ),
    nav_panel(textOutput("nav_surse", inline = TRUE),
      value = "surse", class = "pt-3",
      card(card_header(textOutput("def_titlu", inline = TRUE)), DTOutput("def"))
    ),
    nav_spacer(),
    nav_item(radioButtons("lang", NULL, c("RO" = "ro", "EN" = "en"), inline = TRUE))
  )
)

# the request decides between login page and dashboard
ui <- function(req) if (autentificat(req)) ui_panou else ui_login()

server <- function(input, output, session) {
  if (!autentificat(session$request)) {
    server_login(input, output, session)
    return(invisible())
  }
  txt <- reactive(TEXTE[[input$lang %||% "ro"]])
  frecventa <- reactive(input$frecventa %||% "L")
  alese <- reactive(randuri_alese(TABEL(), frecventa(), input$ani))

  # sidebar
  output$frecventa_ui <- renderUI(selector_frecventa(txt(), isolate(input$frecventa)))
  output$ani_ui <- renderUI(selector_ani(TABEL(), txt(), isolate(input$ani)))
  output$ajutor_ui <- renderUI(helpText(txt()$ajutor))
  output$info_tabel <- renderUI(info_tabel(TABEL(), txt()))
  output$jurnal_ui <- renderUI(actionLink("jurnal_btn", txt()$jurnal, class = "small text-muted"))
  output$iesire_ui <- renderUI(actionLink("iesire_btn", txt()$iesire, class = "small text-muted d-block mt-1"))
  observeEvent(input$jurnal_btn, showModal(dialog_jurnal(txt())))
  observeEvent(input$iesire_btn, iesire(session))

  # tab titles
  output$nav_panou <- renderText(txt()$nav[["panou"]])
  output$nav_tabel <- renderText(txt()$nav[["tabel"]])
  output$nav_surse <- renderText(txt()$nav[["surse"]])
  output$tabel_titlu <- renderText(txt()$tabel_titlu)
  output$def_titlu <- renderText(txt()$def_titlu)

  # dashboard tab
  output$carduri <- renderUI(carduri_ui(TABEL(), frecventa(), txt()))
  output$grafice_ui <- renderUI(grafice_ui(frecventa(), txt()))
  for (cod in INDICATORI$cod) {
    local({
      cod <- cod
      output[[paste0("h_", cod)]] <- renderText(txt()$ind[[cod]])
      output[[paste0("g_", cod)]] <- renderPlotly(grafic(alese(), cod, frecventa(), txt()))
    })
  }

  # table and sources tabs
  output$tabel <- renderDT(tabel_dt(alese(), txt()))
  output$descarca_ui <- renderUI(downloadButton("descarca", txt()$descarca, class = "btn-sm btn-outline-primary"))
  output$descarca <- downloadHandler(
    filename = function() sprintf("indicatori_%s_%s.csv", frecventa(), Sys.Date()),
    content = function(f) write.csv(alese()[COLOANE_TABEL], f, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  )
  output$def <- renderDT(definitii_dt(txt()))
  output$jurnal <- renderDT(jurnal_dt(txt()))

  # sidebar and tab titles render even when their tab is hidden
  for (o in c("frecventa_ui", "ani_ui", "ajutor_ui", "info_tabel", "jurnal_ui", "iesire_ui", "nav_panou",
              "nav_tabel", "nav_surse")) {
    outputOptions(output, o, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui, server)
