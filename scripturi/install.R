pachete <- c("shiny", "bslib", "readxl", "writexl", "httr2", "jsonlite", "plotly", "DT", "pdftools", "openssl", "rsconnect")
lipsa <- pachete[!pachete %in% rownames(installed.packages())]
if (length(lipsa)) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  lib <- Sys.getenv("R_LIBS_USER")
  if (nzchar(lib) && !dir.exists(lib)) dir.create(lib, recursive = TRUE)
  install.packages(lipsa, lib = if (nzchar(lib)) lib else NULL)
}
cat("Pachete OK:", paste(pachete, collapse = ", "), "\n")
