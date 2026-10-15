# Two helpers used everywhere

# fallback for a missing value, NULL, empty or NA
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# console line with a timestamp
noteaza <- function(...) cat(format(Sys.time(), "%H:%M:%S"), " ", sprintf(...), "\n", sep = "")
