# Login with salted hashes and a signed cookie

COOKIE_ORE <- 12
LIMITA_SESIUNE <- 5 # failed attempts per browser session
LIMITA_PROCES <- 30 # failed attempts per R process in the window
FEREASTRA_MIN <- 15

# credentials.R holds users, salts, hashes and the cookie secret, edit with parola.R
CREDENTIALE <- if (file.exists("credentials.R")) source("credentials.R", local = TRUE)$value else NULL

# ---- passwords ----

hex <- function(x) paste(sprintf("%02x", as.integer(x)), collapse = "")
din_hex <- function(x) as.raw(strtoi(substring(x, seq(1, nchar(x), 2), seq(2, nchar(x), 2)), 16L))
sare_noua <- function() hex(openssl::rand_bytes(16))

# bcrypt key stretching, about 60 ms per check
parola_hash <- function(parola, sare) hex(openssl::bcrypt_pbkdf(parola, din_hex(sare), rounds = 12L, size = 32L))

parola_ok <- function(user, parola) {
  utilizatori <- CREDENTIALE$utilizatori
  i <- match(user, utilizatori$user)
  !is.na(i) && nzchar(parola) && identical(parola_hash(parola, utilizatori$salt[i]), utilizatori$hash[i])
}

# ---- cookie ----

# secret mixes the file secret with the hashes, changing a password logs everyone out
SECRET <- if (is.null(CREDENTIALE)) {
  ""
} else {
  as.character(openssl::sha256(paste(CREDENTIALE$secret, CREDENTIALE$utilizatori$hash, collapse = "|")))
}

semnatura <- function(mesaj) as.character(openssl::sha256(mesaj, key = SECRET))

# user, expiry and signature, separated by bars
token_nou <- function(user) {
  mesaj <- paste(user, as.integer(Sys.time()) + COOKIE_ORE * 3600, sep = "|")
  paste(mesaj, semnatura(mesaj), sep = "|")
}

token_valid <- function(token) {
  if (is.null(token) || is.na(token) || token == "") {
    return(FALSE)
  }
  parti <- strsplit(token, "|", fixed = TRUE)[[1]]
  length(parti) == 3 &&
    parti[1] %in% CREDENTIALE$utilizatori$user &&
    identical(semnatura(paste(parti[1], parti[2], sep = "|")), parti[3]) &&
    suppressWarnings(as.numeric(parti[2])) > as.numeric(Sys.time())
}

token_din_cookie <- function(cookie) {
  if (is.null(cookie)) {
    return("")
  }
  gasit <- regmatches(cookie, regexpr("(^|; ?)auth=[^;]*", cookie))
  if (length(gasit)) sub("^.*auth=", "", gasit) else ""
}

autentificat <- function(req) is.null(CREDENTIALE) || token_valid(token_din_cookie(req$HTTP_COOKIE))

# cookie limited to this app's path, other apps on the same host never see it
cale_cookie <- function(session) {
  cale <- session$clientData$url_pathname %||% "/"
  if (grepl("/$", cale)) cale else paste0(cale, "/")
}

# the browser stores the cookie and reloads, the next request lands on the dashboard
script_cookie <- function() {
  tags$script(HTML("Shiny.addCustomMessageHandler('cookie', function(m) { document.cookie = m; location.reload(); });"))
}

trimite_cookie <- function(session, user) {
  sigur <- if (identical(session$clientData$url_protocol, "https:")) "; Secure" else ""
  cookie <- sprintf("auth=%s; Max-Age=%d; Path=%s; SameSite=Lax%s", token_nou(user), COOKIE_ORE * 3600,
                    cale_cookie(session), sigur)
  session$sendCustomMessage("cookie", cookie)
}

iesire <- function(session) {
  session$sendCustomMessage("cookie", sprintf("auth=; Max-Age=0; Path=%s; SameSite=Lax", cale_cookie(session)))
}

# ---- brute force limit ----

# failed logins across all sessions of this process
ESECURI <- new.env()
ESECURI$cand <- numeric(0)

login_blocat <- function() {
  ESECURI$cand <- ESECURI$cand[ESECURI$cand > as.numeric(Sys.time()) - FEREASTRA_MIN * 60]
  length(ESECURI$cand) >= LIMITA_PROCES
}

noteaza_esec <- function(user) {
  ESECURI$cand <- c(ESECURI$cand, as.numeric(Sys.time()))
  noteaza("login eșuat pentru '%s'", substr(user, 1, 40))
}

# ---- page and server ----

ui_login <- function() {
  page_fillable(
    title = "Economic Data Dashboard",
    theme = TEMA,
    tags$head(
      script_cookie(),
      # Enter in the password field submits
      tags$script(HTML(
        "$(document).on('keypress', '#login_pwd', function(e) { if (e.which == 13) $('#login_btn').click(); });"
      ))
    ),
    div(
      style = "max-width: 420px; margin: 12vh auto 0 auto",
      h2("Economic Data Dashboard", style = "text-align:center"),
      h4("Authenticate", style = "text-align:center; margin-bottom: 1.5rem"),
      textInput("login_user", "Username", width = "100%"),
      passwordInput("login_pwd", "Password", width = "100%"),
      actionButton("login_btn", "Login", class = "btn-primary w-100"),
      div(textOutput("login_msg"), style = "color:#b00020; margin-top:0.8rem; text-align:center")
    )
  )
}

server_login <- function(input, output, session) {
  mesaj <- reactiveVal("")
  output$login_msg <- renderText(mesaj())
  esecuri_sesiune <- 0

  observeEvent(input$login_btn, {
    user <- input$login_user %||% ""
    if (esecuri_sesiune >= LIMITA_SESIUNE) {
      mesaj("Too many attempts, reload the page")
    } else if (login_blocat()) {
      mesaj("Login temporarily locked, try again later")
    } else if (!parola_ok(user, input$login_pwd %||% "")) {
      esecuri_sesiune <<- esecuri_sesiune + 1
      noteaza_esec(user)
      mesaj("Wrong username or password")
    } else {
      trimite_cookie(session, user)
    }
  })
}
