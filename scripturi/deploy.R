#!/usr/bin/env Rscript
# Rscript scripturi/deploy.R   publishes the app to shinyapps.io, used by deploy.bat and the workflow

script <- grep("^--file=", commandArgs(), value = TRUE)
# the project root is the parent of scripturi
if (length(script)) setwd(dirname(dirname(normalizePath(sub("^--file=", "", script[1])))))

# the app needs shared files and dashboard UI, not source fetchers
fisiere <- c(
  "app.R",
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  list.files("R/app", pattern = "[.]R$", full.names = TRUE),
  "data/indicatori.xlsx", "data/indicatori.csv", "data/update_log.csv"
)

# account from the environment on GitHub, from the saved token on a PC
if (nzchar(Sys.getenv("SHINYAPPS_TOKEN"))) {
  rsconnect::setAccountInfo(
    name = Sys.getenv("SHINYAPPS_NAME"),
    token = Sys.getenv("SHINYAPPS_TOKEN"),
    secret = Sys.getenv("SHINYAPPS_SECRET")
  )
}

# Include credentials only if the administrator explicitly created this local file.
# Never distribute the populated credentials file with the generic project archive.
if (file.exists("credentials.R")) fisiere <- c(fisiere, "credentials.R")

pachete <- rsconnect::appDependencies(".", appFiles = fisiere)
cat("pachete în bundle:", nrow(pachete), "\n")
# never ship a bundle without packages, the app would not start
stopifnot(nrow(pachete) > 40, "shiny" %in% pachete$Package)
rsconnect::deployApp(".", appName = "economic-data", appFiles = fisiere, forceUpdate = TRUE,
                     launch.browser = FALSE, logLevel = "normal")
