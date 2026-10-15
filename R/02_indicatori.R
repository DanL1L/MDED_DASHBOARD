# Indicator table, one row per indicator, everything the app and the updater need to know about it
#
# vederi    frequency views the indicator appears in, T A L
# card      1 shows a value box, 0 chart only
# grupa     section of the view, empty when the view has no sections
# zecimale  decimals shown, sufix appended after the number
# delta     card comparison, pct percent change, abs difference, pp percentage points
# grafic    linie or bare

AFISARE <- utils::read.delim(text = "
cod                  | vederi | card | grupa     | zecimale | sufix | delta | grafic | nume_ro                                        | nume_en                          | unitate_ro      | unitate_en
pib                  | T,A    | 1    |           | 0        |       | pct   | bare   | PIB nominal                                    | Nominal GDP                      | mil. lei        | MDL million
pib_real             | T,A    | 0    |           | 1        | %     | pp    | linie  | PIB, creștere reală                            | GDP, real growth                 | %               | %
inflatie             | T,A,L  | 1    |           | 1        | %     | pp    | linie  | Rata inflației                                 | Inflation rate                   | %               | %
venituri_fiscale     | T,A    | 1    |           | 0        |       | pct   | bare   | Venituri fiscale (BPN)                         | Tax revenues (NPB)               | mil. lei        | MDL million
exporturi            | T,A    | 1    |           | 0        |       | pct   | bare   | Exporturi de bunuri și servicii                | Exports of goods and services    | mil. lei        | MDL million
salariu              | T,A    | 1    |           | 0        |       | pct   | bare   | Salariul mediu brut                            | Average gross wage               | lei / lună      | MDL / month
transferuri          | L      | 1    |           | 0        |       | pct   | bare   | Transferuri nete către persoane fizice         | Net transfers to individuals     | mil. USD        | USD million
exporturi_bunuri     | L      | 1    |           | 0        |       | pct   | bare   | Exporturi de bunuri                            | Exports of goods                 | mil. USD        | USD million
importuri_bunuri     | L      | 1    |           | 0        |       | pct   | bare   | Importuri de bunuri                            | Imports of goods                 | mil. USD        | USD million
venituri_bpn         | L      | 1    |           | 0        |       | pct   | bare   | Venituri BPN                                   | NPB revenues                     | mil. lei        | MDL million
cheltuieli_bpn       | L      | 1    |           | 0        |       | pct   | bare   | Cheltuieli BPN                                 | NPB expenditures                 | mil. lei        | MDL million
vms_91               | L      | 1    |           | 2        |       | abs   | linie  | Rata VMS 91 zile                               | T-bill yield 91 days             | % anual         | % p.a.
vms_364              | L      | 1    |           | 2        |       | abs   | linie  | Rata VMS 364 zile                              | T-bill yield 364 days            | % anual         | % p.a.
bnm_net_eur          | L      | 1    |           | 2        |       | abs   | bare   | Intervenții nete BNM, EUR                      | NBM net FX purchases, EUR        | mil. EUR        | EUR million
bnm_net_usd          | L      | 1    |           | 2        |       | abs   | bare   | Intervenții nete BNM, USD                      | NBM net FX purchases, USD        | mil. USD        | USD million
credite_noi_lei      | L      | 1    |           | 0        |       | pct   | bare   | Credite noi în lei                             | New loans in MDL                 | mil. lei        | MDL million
credite_noi_valuta   | L      | 1    |           | 0        |       | pct   | bare   | Credite noi în valută                          | New loans in foreign currency    | mil. lei        | MDL million
rata_credite_lei     | L      | 1    |           | 2        |       | abs   | linie  | Rata creditelor noi în lei                     | Rate on new MDL loans            | % anual         | % p.a.
rata_credite_juridice| L      | 1    |           | 2        |       | abs   | linie  | Rata creditelor noi, persoane juridice         | Rate on new loans to companies   | % anual         | % p.a.
rata_credite_imobil  | L      | 1    |           | 2        |       | abs   | linie  | Rata creditelor noi ipotecare                  | Rate on new mortgage loans       | % anual         | % p.a.
", sep = "|", quote = "", strip.white = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8",
  colClasses = c(grupa = "character", sufix = "character"))

# source and description per language, shown on the Surse tab and in the Excel sheet indicatori
DESCRIERI <- utils::read.delim(text = "
cod                  | sursa_ro                      | sursa_en                              | descriere_ro | descriere_en
pib                  | BNS PxWeb + comunicate PIB    | NBS PxWeb + GDP press releases        | Produsul intern brut în prețuri curente (BNS, conturi naționale). | Gross domestic product at current prices (NBS, national accounts).
pib_real             | BNS PxWeb + comunicate PIB    | NBS PxWeb + GDP press releases        | Indicele de volum al PIB față de trimestrul/anul corespunzător al anului precedent, minus 100. | GDP volume index vs the same quarter/year of the previous year, minus 100.
inflatie             | BNS PxWeb                     | NBS PxWeb                             | Lunar: IPC față de aceeași lună a anului precedent. Trimestrial: media celor trei rate lunare. Anual: IPC mediu anual față de anul precedent. | Monthly: CPI vs the same month of the previous year. Quarterly: mean of the three monthly rates. Annual: average annual CPI vs previous year.
venituri_fiscale     | MF, rapoarte BPN (xls)        | Ministry of Finance, NPB reports (xls) | Bugetul public național, rândul Impozite și taxe (cod 11). Trimestrial: fluxul din trimestru (diferența cumulatelor). | National public budget, row Taxes and duties (code 11). Quarterly: flow in the quarter (difference of cumulative values).
exporturi            | BNS PxWeb + comunicate PIB    | NBS PxWeb + GDP press releases        | P.6 Export de bunuri și servicii din PIB pe categorii de utilizări (conturi naționale), prețuri curente. | P.6 Exports of goods and services from GDP by expenditure (national accounts), current prices.
salariu              | BNS PxWeb                     | NBS PxWeb                             | Câștigul salarial mediu lunar brut pe economie (unități cu 4+ salariați), lei. | Average monthly gross wage, whole economy (units with 4+ employees), MDL.
transferuri          | BNM BDI (DBP4)                | NBM BDI (DBP4)                        | Transferuri de mijloace bănești din străinătate în favoarea persoanelor fizice prin bănci, în bază netă (BNM, BDI raport DBP4). | Money transfers from abroad to individuals through banks, net basis (NBM interactive database, report DBP4).
exporturi_bunuri     | BNS PxWeb (EXT015000)         | NBS PxWeb (EXT015000)                 | Exporturi de bunuri (date vamale, BNS), mil. USD. | Exports of goods (customs data, NBS), USD million.
importuri_bunuri     | BNS PxWeb (EXT015000)         | NBS PxWeb (EXT015000)                 | Importuri de bunuri (date vamale, BNS), mil. USD. | Imports of goods (customs data, NBS), USD million.
venituri_bpn         | MF, rapoarte BPN (xls)        | Ministry of Finance, NPB reports (xls) | Veniturile totale ale bugetului public național, cod 1. Lunar: fluxul lunii (diferența cumulatelor). | Total revenues of the national public budget, code 1. Monthly flow (difference of cumulative values).
cheltuieli_bpn       | MF, rapoarte BPN (xls)        | Ministry of Finance, NPB reports (xls) | Cheltuielile bugetului public național inclusiv activele nefinanciare, cod 2+3. Lunar: fluxul lunii. | Expenditures of the national public budget including non-financial assets, code 2+3. Monthly flow.
vms_91               | BNM BDI (DOP5)                | NBM BDI (DOP5)                        | Rata efectivă medie ponderată a dobânzii la bonurile de trezorerie de 91 zile, piața primară (BDI, raport DOP5). | Weighted average effective yield of 91-day treasury bills, primary market (NBM BDI, report DOP5).
vms_364              | BNM BDI (DOP5)                | NBM BDI (DOP5)                        | Rata efectivă medie ponderată a dobânzii la VMS de 364 zile, piața primară (BDI, raport DOP5). | Weighted average effective yield of 364-day state securities, primary market (NBM BDI, report DOP5).
bnm_net_eur          | BNM BDI (DOVRE2)              | NBM BDI (DOVRE2)                      | Cumpărările minus vânzările la vedere de euro ale BNM pe piața valutară, milioane (BDI, raport DOVRE2). | NBM spot purchases minus sales of euro on the FX market, million (NBM BDI, report DOVRE2).
bnm_net_usd          | BNM BDI (DOVRE2)              | NBM BDI (DOVRE2)                      | Cumpărările minus vânzările la vedere de dolari SUA ale BNM pe piața valutară, milioane (BDI, raport DOVRE2). | NBM spot purchases minus sales of US dollars on the FX market, million (NBM BDI, report DOVRE2).
credite_noi_lei      | BNM BDI (DPMC8)               | NBM BDI (DPMC8)                       | Volumul creditelor noi acordate în lei, total pe sectorul bancar, toate termenele (BDI, raport DPMC8). | Volume of new loans granted in MDL, whole banking sector, all maturities (NBM BDI, report DPMC8).
credite_noi_valuta   | BNM BDI (DPMC8)               | NBM BDI (DPMC8)                       | Volumul creditelor noi acordate în valută, total pe sectorul bancar, toate termenele, echivalent lei (BDI, raport DPMC8). | Volume of new loans granted in foreign currency, whole banking sector, all maturities, MDL equivalent (NBM BDI, report DPMC8).
rata_credite_lei     | BNM BDI (DPMC8)               | NBM BDI (DPMC8)                       | Rata nominală medie a creditelor noi în lei, total pe sectorul bancar, toate termenele (BDI, raport DPMC8). | Average nominal rate on new MDL loans, whole banking sector, all maturities (NBM BDI, report DPMC8).
rata_credite_juridice| BNM BDI (DPMC8)               | NBM BDI (DPMC8)                       | Rata nominală medie a creditelor noi în lei acordate persoanelor juridice, toate termenele (BDI, raport DPMC8). | Average nominal rate on new MDL loans to legal entities, all maturities (NBM BDI, report DPMC8).
rata_credite_imobil  | BNM BDI (DPMC8)               | NBM BDI (DPMC8)                       | Rata nominală medie a creditelor noi în lei acordate persoanelor fizice pentru imobil, toate termenele (BDI, raport DPMC8). | Average nominal rate on new MDL mortgage loans to individuals, all maturities (NBM BDI, report DPMC8).
", sep = "|", quote = "", strip.white = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

# optional sections for frequency views
GRUPE <- data.frame(grupa = character(0), ro = character(0), en = character(0), stringsAsFactors = FALSE)

# afisare and descrieri joined on cod, a typo in a code or a group stops the load
tabel_indicatori <- function(afisare, descrieri, grupe = GRUPE) {
  stopifnot(
    !anyDuplicated(afisare$cod),
    setequal(afisare$cod, descrieri$cod),
    all(afisare$grupa %in% c("", grupe$grupa))
  )
  cbind(afisare, descrieri[match(afisare$cod, descrieri$cod), -1])
}

INDICATORI <- tabel_indicatori(AFISARE, DESCRIERI)

atribut <- function(cod, col) INDICATORI[[col]][match(cod, INDICATORI$cod)]

# codes of one frequency view, table order
indicatori_pentru <- function(fr) {
  in_vedere <- vapply(strsplit(INDICATORI$vederi, ","), function(v) fr %in% v, logical(1))
  INDICATORI$cod[in_vedere]
}

carduri_pentru <- function(fr) {
  coduri <- indicatori_pentru(fr)
  coduri[atribut(coduri, "card") == 1]
}

# frequency views that have at least one indicator loaded
frecvente_cu_indicatori <- function() unique(unlist(strsplit(INDICATORI$vederi, ",")))

# a view splits in sections when its indicators carry a group, otherwise it is one section
sectiuni_pentru <- function(coduri) {
  grupe <- atribut(coduri, "grupa")
  grupe[is.na(grupe)] <- ""
  if (!any(nzchar(grupe))) {
    return(list(toate = coduri))
  }
  Filter(length, split(coduri, factor(grupe, levels = GRUPE$grupa)))
}
