# HTTP helpers, every download goes through cerere()

cerere <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(CONFIG$user_agent) |>
    httr2::req_timeout(CONFIG$timeout_sec) |>
    httr2::req_retry(max_tries = 3, backoff = ~2)
}

# page body as text
text_de_la <- function(url) httr2::resp_body_string(httr2::req_perform(cerere(url)))

# page body as text, NULL when the site does not answer
text_sau_nimic <- function(url) tryCatch(text_de_la(url), error = function(e) NULL)

# json body of a prepared request
json_de_la <- function(cerere, simplifica = FALSE) {
  raspuns <- httr2::req_perform(cerere)
  jsonlite::fromJSON(httr2::resp_body_string(raspuns), simplifyVector = simplifica)
}

# file saved in data/cache, downloaded once unless refresh
descarca <- function(url, nume = NULL, refresh = FALSE) {
  dir.create(CONFIG$cache_dir, showWarnings = FALSE, recursive = TRUE)
  nume <- nume %||% utils::URLdecode(basename(sub("\\?.*$", "", url)))
  cale <- file.path(CONFIG$cache_dir, gsub("[^A-Za-z0-9._-]+", "_", nume))
  if (!refresh && file.exists(cale) && file.size(cale) > 0) {
    return(cale)
  }

  raspuns <- httr2::req_perform(cerere(url), path = cale)
  if (grepl("text/html", httr2::resp_content_type(raspuns) %||% "")) {
    unlink(cale)
    stop("Link mort (a returnat HTML): ", url)
  }
  cale
}

# href values on a page, optionally filtered by a pattern and made absolute
linkuri <- function(html, tipar = NULL, baza = NULL) {
  gasite <- regmatches(html, gregexpr("href=[\"']([^\"']+)[\"']", html))[[1]]
  gasite <- sub("^href=[\"']", "", sub("[\"']$", "", gasite))
  gasite <- gsub("&amp;", "&", gasite)
  if (!is.null(tipar)) gasite <- gasite[grepl(tipar, gasite, ignore.case = TRUE)]
  if (!is.null(baza)) gasite <- ifelse(grepl("^https?://", gasite), gasite, paste0(baza, gasite))
  unique(gasite)
}
