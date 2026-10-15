# Period codes, their sort key, month names and number parsing

perioada_A <- function(an) as.character(as.integer(an))
perioada_T <- function(an, trimestru) sprintf("%dT%d", as.integer(an), as.integer(trimestru))
perioada_L <- function(an, luna) sprintf("%dL%02d", as.integer(an), as.integer(luna))
perioada_Z <- function(an, luna, zi) sprintf("%04d-%02d-%02d", as.integer(an), as.integer(luna), as.integer(zi))

# numeric key that sorts 2026 before 2026T1 and 2026L03 next to 2026T1
perioada_ordine <- function(perioada) {
  an <- as.numeric(substr(perioada, 1, 4))
  rest <- substr(perioada, 5, 20)
  trimestru <- suppressWarnings(as.numeric(sub("^T", "", rest)))
  luna <- suppressWarnings(as.numeric(sub("^L", "", rest)))
  zi_din_an <- suppressWarnings(as.numeric(format(as.Date(ifelse(grepl("^-", rest), perioada, NA)), "%j")))
  ifelse(grepl("^T", rest), an + trimestru / 4,
    ifelse(grepl("^L", rest), an + luna / 12,
      ifelse(grepl("^-", rest), an + (zi_din_an - 1) / 366, an)))
}

LUNI <- c(
  "ianuarie", "februarie", "martie", "aprilie", "mai", "iunie",
  "iulie", "august", "septembrie", "octombrie", "noiembrie", "decembrie"
)

luna_din_nume <- function(x) match(tolower(trimws(x)), LUNI)

# abbreviated names such as "iun." or "nov."
luna_din_abreviere <- function(x) {
  primele <- substr(tolower(trimws(x)), 1, 3)
  luna <- match(primele, substr(LUNI, 1, 3))
  luna[primele == "nov"] <- 11L
  luna
}

numar_roman <- function(x) match(toupper(x), c("I", "II", "III", "IV"))

# "1 234,5" as a number, NA when it is not one
numar <- function(x) {
  x <- gsub("\\s", "", as.character(x))
  x <- gsub("−|–", "-", x)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}
