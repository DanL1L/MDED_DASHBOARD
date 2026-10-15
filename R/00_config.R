# Sources, paths and settings for monthly, quarterly and annual series

CONFIG <- list(
  store_xlsx = "data/indicatori.xlsx",
  store_csv = "data/indicatori.csv",
  log_csv = "data/update_log.csv",
  cache_dir = "data/cache",

  # first year fetched for the monthly, quarterly and annual series
  din_anul = 2015,
  user_agent = "moldova-macro-dashboard/1.0 (R; contact: analist)",
  timeout_sec = 60,

  # ---- BNS ----
  pxweb_base = "https://statbank.statistica.md/PxWeb/api/v1/ro",
  px = list(
    # CPI vs same month last year
    ipc_lunar = "40 Statistica economica/05 PRE/PRE010/serii lunare/PRE012600.px",
    # annual average CPI vs previous year
    ipc_anual = "40 Statistica economica/05 PRE/PRE010/serii anuale/PRE010400.px",
    # gross wage, quarterly and annual
    salariu_trim = "30 Statistica sociala/03 FM/SAL010/serii lunare/SAL014800.px",
    # quarterly GDP, current prices, thousand lei
    pib_trim = "40 Statistica economica/13 CNT/CNT250/Utilizari/CNT255000.px",
    # GDP volume index vs last year
    pib_trim_real = "40 Statistica economica/13 CNT/CNT250/Utilizari/CNT255300.px",
    # monthly goods trade, mil. USD
    comert_lunar = "40 Statistica economica/21 EXT/EXT010/serii lunare/EXT015000.px"
  ),
  bns_conturi_nationale = "https://statistica.gov.md/ro/conturi-nationale-9494.html",
  bns_pib_slug_regex = "produsul-intern-brut-in-trimestrul-[^\"']*9497_[0-9]+\\.html",

  # ---- BNM ----
  bnm_base = "https://www.bnm.md",
  # reports of the interactive database, DBP4 net transfers, DOP5 treasury bill yields,
  # DOVRE2 BNM FX operations, DPMC8 new loans
  bnm_bdi_transferuri = "https://www.bnm.md/bdi/pages/reports/dbp/DBP4.xhtml?id=0&lang=ro",
  bnm_bdi_vms = "https://www.bnm.md/bdi/pages/reports/dop/DOP5.xhtml?id=0&lang=ro",
  bnm_bdi_interventii = "https://www.bnm.md/bdi/pages/reports/dovre/DOVRE2.xhtml?id=0&lang=ro",
  bnm_bdi_credite = "https://www.bnm.md/bdi/pages/reports/dpmc/DPMC8.xhtml?id=0&lang=ro",

  # ---- Ministry of Finance ----
  mf_base = "https://mf.gov.md",
  mf_catalog_curent = "/ro/ministerul-finan%C8%9Belor/catalogul-de-date-deschise-al-ministerului-finan%C8%9Belor",
  mf_catalog_an = "/ro/content/catalogul-de-date-deschise-al-mf-pentru-anul-%d",
  # BPN codes summed as tax revenues
  mf_coduri_venituri_fiscale = c("11"),
  mf_coduri_bpn_lunar = c(venituri_bpn = "1", cheltuieli_bpn = "2+3")
)
