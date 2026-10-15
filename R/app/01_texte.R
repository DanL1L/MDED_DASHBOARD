# Interface strings per language, indicator names and units come from the table in R/02_indicatori.R

TEXTE_UI <- list(
  ro = list(
    frecventa = "Frecvența",
    frecvente = c("Lunar" = "L", "Trimestrial" = "T", "Anual" = "A"),
    ani = "Ani",
    ajutor = "Actualizarea poate dura câteva minute",
    tabel_lbl = "Tabel",
    info = "%d rânduri, ultima actualizare %s, ora Chișinău",
    jurnal = "Jurnal actualizări",
    jurnal_titlu = "Jurnalul actualizărilor",
    jurnal_gol = "Nicio actualizare încă.",
    jurnal_cols = c(data = "Data", sursa = "Sursa", randuri = "Rânduri", inserate = "Inserate", revizuite = "Revizuite",
                    neschimbate = "Neschimbate", protejate = "Protejate", eroare = "Eroare"),
    inchide = "Închide",
    iesire = "Ieșire",
    nav = c(panou = "Panou", tabel = "Tabel", surse = "Surse"),
    perioada = "Perioada",
    fata_de = "față de",
    crestere_reala = "creștere reală",
    fara_date = "fără date",
    nu_date = "Nu există date pentru selecția curentă.",
    tabel_titlu = "Toate rândurile (filtrate după frecvență și ani)",
    descarca = "Descarcă CSV",
    def_titlu = "Definițiile indicatorilor",
    def_cols = c("Cod", "Indicator", "Unitate", "Descriere", "Sursa"),
    cols = c(indicator = "Indicator", perioada = "Perioada", valoare = "Valoare", unitate = "Unitate", sursa = "Sursa",
             data_actualizarii = "Actualizat", nota = "Notă")
  ),
  en = list(
    frecventa = "Frequency",
    frecvente = c("Monthly" = "L", "Quarterly" = "T", "Annual" = "A"),
    ani = "Years",
    ajutor = "Updating can take a few minutes",
    tabel_lbl = "Table",
    info = "%d rows, last update %s, Chișinău time",
    jurnal = "Update log",
    jurnal_titlu = "Update log",
    jurnal_gol = "No update yet.",
    jurnal_cols = c(data = "Date", sursa = "Source", randuri = "Rows", inserate = "Inserted", revizuite = "Revised",
                    neschimbate = "Unchanged", protejate = "Protected", eroare = "Error"),
    inchide = "Close",
    iesire = "Log out",
    nav = c(panou = "Dashboard", tabel = "Table", surse = "Sources"),
    perioada = "Period",
    fata_de = "vs",
    crestere_reala = "real growth",
    fara_date = "no data",
    nu_date = "No data for the current selection.",
    tabel_titlu = "All rows (filtered by frequency and years)",
    descarca = "Download CSV",
    def_titlu = "Indicator definitions",
    def_cols = c("Code", "Indicator", "Unit", "Description", "Source"),
    cols = c(indicator = "Indicator", perioada = "Period", valoare = "Value", unitate = "Unit", sursa = "Source",
             data_actualizarii = "Updated", nota = "Note")
  )
)

# interface strings plus the indicator columns of one language
texte <- function(lang) {
  coloana <- function(nume) setNames(INDICATORI[[paste0(nume, "_", lang)]], INDICATORI$cod)
  definitii <- data.frame(
    cod = INDICATORI$cod, nume = coloana("nume"), unitate = coloana("unitate"),
    descriere = coloana("descriere"), sursa = coloana("sursa"),
    stringsAsFactors = FALSE, row.names = NULL
  )
  c(TEXTE_UI[[lang]], list(
    ind = coloana("nume"),
    unit = coloana("unitate"),
    grupe = setNames(GRUPE[[lang]], GRUPE$grupa),
    def = definitii
  ))
}

TEXTE <- list(ro = texte("ro"), en = texte("en"))
