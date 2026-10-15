#!/usr/bin/env Rscript
# Weekly catch-up runner. Independent of Shiny; no modification to extractors.
# Manual forced run: Rscript scripturi/actualizare_programata.R --force
ruleaza <- function() {
args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (!length(script_arg)) stop("Executati prin Rscript, nu prin source().")
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE)
setwd(dirname(dirname(script_path)))

state_dir <- "data/automatizare"
log_dir <- file.path(state_dir, "jurnale")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
lock_dir <- file.path(state_dir, "actualizare.lock")
state_file <- file.path(state_dir, "ultima_reusita.rds")
force <- "--force" %in% args
# One run per ISO week. A missed Monday is recovered at the next scheduler trigger.
week_id <- function(d) format(as.Date(d), "%G-W%V")
last <- if (file.exists(state_file)) {
  tryCatch(readRDS(state_file), error = function(e) {
    warning("Fisier de stare invalid: ", conditionMessage(e))
    NULL
  })
} else NULL
if (!force && !is.null(last) && identical(last$saptamana, week_id(Sys.Date()))) {
  cat("Actualizarea saptamanii", week_id(Sys.Date()), "este deja finalizata.\n")
  return(0L)
}
if (!dir.create(lock_dir, showWarnings = FALSE)) {
  cat("O actualizare este deja in curs sau exista un lock vechi:", lock_dir, "\n")
  return(2L)
}
on.exit(unlink(lock_dir, recursive = TRUE), add = TRUE)
# On abnormal process termination the lock remains, deliberately requiring inspection.
writeLines(c(paste("started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
             paste("pid:", Sys.getpid())), file.path(lock_dir, "owner.txt"))

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("actualizare_", stamp, "_", Sys.getpid(), ".log"))
cmd <- file.path(R.home("bin"), "Rscript.exe")
if (.Platform$OS.type != "windows") cmd <- file.path(R.home("bin"), "Rscript")
if (!file.exists(cmd)) stop("Rscript indisponibil: ", cmd)
cat("Actualizare inceputa. Jurnal:", log_file, "\n")
# Run update.R in a fresh R process; capture its true exit code.
result <- suppressWarnings(system2(cmd, c("--vanilla", "scripturi/update.R"),
                                   stdout = log_file, stderr = log_file))
status <- as.integer(result)
if (!length(status) || is.na(status)) status <- 1L
cat(paste(readLines(log_file, warn = FALSE), collapse = "\n"), "\n")
# update.R emits a warning (rather than a nonzero exit status) on source failures.
# Detect that warning to avoid marking partial updates as fully successful.
log_lines <- readLines(log_file, warn = FALSE)
partial_failure <- any(grepl("\\[EROARE\\]|Una sau mai multe surse au esuat", log_lines))
if (partial_failure && status == 0L) status <- 1L
if (status == 0L) {
  # Only a fully successful run advances the weekly state.
  tmp <- tempfile("stare_", tmpdir = state_dir)
  saveRDS(list(saptamana = week_id(Sys.Date()),
               finalizat = Sys.time(), jurnal = log_file), tmp)
  if (file.exists(state_file)) unlink(state_file)
  if (!file.rename(tmp, state_file)) stop("Nu pot salva starea actualizarii")
  cat("Actualizare completa. Stare salvata.\n")
} else {
  cat("Actualizare partiala/esuată (cod ", status,
      "). Starea saptamanala NU a fost avansata.\n", sep = "")
}
status
}
quit(save = "no", status = ruleaza())
