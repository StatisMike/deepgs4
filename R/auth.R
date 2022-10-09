## This file is the interface between deepgsheets4 and the
## auth functionality in gargle.

# Initialization happens in .onLoad
.auth <- NULL

## The roxygen comments for these functions are mostly generated from data
## in this list and template text maintained in gargle.
gargle_lookup_table <- list(
  PACKAGE     = "deepgsheets4",
  YOUR_STUFF  = "your Google Sheets",
  PRODUCT     = "Google Sheets",
  API         = "Sheets API",
  PREFIX      = "deepgs"
)

#' Authorize deepgsheets4
#'
#' @eval gargle:::PREFIX_auth_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_details(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_params()
#'
#' @family auth functions
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' # load/refresh existing credentials, if available
#' # otherwise, go to browser for authentication and authorization
#' deepgs_auth()
#'
#' # force use of a token associated with a specific email
#' deepgs_auth(email = "jenny@example.com")
#'
#' # use a 'read only' scope, so it's impossible to edit or delete Sheets
#' deepgs_auth(
#'   scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
#' )
#'
#' # use a service account token
#' deepgs_auth(path = "foofy-83ee9e7c9c48.json")
deepgs_auth <- function(email = gargle::gargle_oauth_email(),
                        path = NULL,
                        scopes = "https://www.googleapis.com/auth/spreadsheets",
                        cache = gargle::gargle_oauth_cache(),
                        use_oob = gargle::gargle_oob_default(),
                        token = NULL) {
  # I have called `deepgs_auth(token = drive_token())` multiple times now,
  # without attaching googledrive. Expose this error noisily, before it gets
  # muffled by the `tryCatch()` treatment of `token_fetch()`.

  force(token)

  cred <- gargle::token_fetch(
    scopes = scopes,
    app = if (!is.null(deepgs_oauth_app())) deepgs_oauth_app() else deepgs_default_app(),
    email = email,
    path = path,
    package = "deepgsheets4",
    cache = cache,
    use_oob = use_oob,
    token = token
  )
  if (!inherits(cred, "Token2.0")) {
    cli::cli_abort(c(
      "Can't get Google credentials.",
      "i" = "Are you running {.pkg deepgsheets4} in a non-interactive \\
             session? Consider:",
      "*" = "Call {.fun deepgs_deauth} to prevent the attempt to get credentials.",
      "*" = "Call {.fun deepgs_auth} directly with all necessary specifics.",
      "i" = "See gargle's \"Non-interactive auth\" vignette for more details:",
      "i" = "{.url https://gargle.r-lib.org/articles/non-interactive-auth.html}"
    ))
  }
  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)

  invisible()
}

#' Suspend authorization
#'
#' @eval gargle:::PREFIX_deauth_description_with_api_key(gargle_lookup_table)
#'
#' @family auth functions
#' @export
#' @examplesIf rlang::is_interactive()
#' deepgs_deauth()
#' deepgs_user()
#'
#' # get metadata on the public 'deaths' spreadsheet
#' deepgs_example("deaths") %>%
#'   deepgs_get()
deepgs_deauth <- function() {
  .auth$set_auth_active(FALSE)
  .auth$clear_cred()
  invisible()
}

#' Produce configured token
#'
#' @eval gargle:::PREFIX_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_token_return()
#'
#' @family low-level API functions
#' @export
#' @examplesIf deepgs_has_token()
#' req <- request_generate(
#'   "sheets.spreadsheets.get",
#'   list(spreadsheetId = "abc"),
#'   token = deepgs_token()
#' )
#' req
deepgs_token <- function() {
  if (isFALSE(.auth$auth_active)) {
    return(NULL)
  }
  if (!deepgs_has_token()) {
    deepgs_auth()
  }
  httr::config(token = .auth$cred)
}

#' Is there a token on hand?
#'
#' @eval gargle:::PREFIX_has_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_has_token_return()
#'
#' @family low-level API functions
#' @export
#'
#' @examples
#' deepgs_has_token()
deepgs_has_token <- function() {
  inherits(.auth$cred, "Token2.0")
}

#' Edit and view auth configuration
#'
#' @eval gargle:::PREFIX_auth_configure_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_configure_params()
#' @eval gargle:::PREFIX_auth_configure_return(gargle_lookup_table)
#'
#' @family auth functions
#' @export
#' @examples
#' # see and store the current user-configured OAuth app (probably `NULL`)
#' (original_app <- deepgs_oauth_app())
#'
#' # see and store the current user-configured API key (probably `NULL`)
#' (original_api_key <- deepgs_api_key())
#'
#' if (require(httr)) {
#'   # bring your own app via client id (aka key) and secret
#'   google_app <- httr::oauth_app(
#'     "my-awesome-google-api-wrapping-package",
#'     key = "YOUR_CLIENT_ID_GOES_HERE",
#'     secret = "YOUR_SECRET_GOES_HERE"
#'   )
#'   google_key <- "YOUR_API_KEY"
#'   deepgs_auth_configure(app = google_app, api_key = google_key)
#'
#'   # confirm the changes
#'   deepgs_oauth_app()
#'   deepgs_api_key()
#'
#'   # bring your own app via JSON downloaded from Google Developers Console
#'   # this file has the same structure as the JSON from Google
#'   app_path <- system.file(
#'     "extdata", "fake-oauth-client-id-and-secret.json",
#'     package = "deepgsheets4"
#'   )
#'   deepgs_auth_configure(path = app_path)
#'
#'   # confirm the changes
#'   deepgs_oauth_app()
#' }
#'
#' # restore original auth config
#' deepgs_auth_configure(app = original_app, api_key = original_api_key)
deepgs_auth_configure <- function(app, path, api_key) {
  if (!missing(app) && !missing(path)) {
    cli::cli_abort("Must supply exactly one of {.arg app} and {.arg path}, not both.")
  }
  stopifnot(missing(api_key) || is.null(api_key) || is_string(api_key))

  if (!missing(path)) {
    stopifnot(is_string(path))
    app <- gargle::oauth_app_from_json(path)
  }
  stopifnot(missing(app) || is.null(app) || inherits(app, "oauth_app"))

  if (!missing(app) || !missing(path)) {
    .auth$set_app(app)
  }

  if (!missing(api_key)) {
    .auth$set_api_key(api_key)
  }

  invisible(.auth)
}

#' @export
#' @rdname deepgs_auth_configure
deepgs_api_key <- function() .auth$api_key

#' @export
#' @rdname deepgs_auth_configure
deepgs_oauth_app <- function() .auth$app

#' Get info on current user
#'
#' @eval gargle:::PREFIX_user_description()
#' @eval gargle:::PREFIX_user_seealso()
#' @eval gargle:::PREFIX_user_return()
#'
#' @export
#' @examples
#' deepgs_user()
deepgs_user <- function() {
  if (!deepgs_has_token()) {
    cli::cli_bullets(c(i = "Not logged in as any specific Google user."))
    return(invisible())
  }

  email <- gargle::token_email(deepgs_token())
  cli::cli_bullets(c(i = "Logged in to {.pkg deepgsheets4} as {.email {email}}."))
  invisible(email)
}

deepgs_default_app <- function() {

  httr::oauth_app(
    appname = "deepgs4_dev2",
    key = "616888423467-3s2fcdde4nt0oaaem6gs30s8ms30qjt2.apps.googleusercontent.com",
    secret = "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",
    redirect_uri = "http://localhost:1410/"
  )

}

deepgs_default_api_key <- function() {
  "AIzaSyCQLqEeBo8PEPH0tbm7JhhA9fcKcRD2H7k"
}

# unexported helpers that are nice for internal use ----
deepgs_auth_internal <- function(account = c("docs", "testing"),
                                 scopes = NULL) {
  account <- match.arg(account)
  can_decrypt <- gargle:::secret_can_decrypt("deepgsheets4")
  online <- !is.null(curl::nslookup("sheets.googleapis.com", error = FALSE))
  if (!can_decrypt || !online) {
    cli::cli_abort(
      message = c(
        "Auth unsuccessful:",
        if (!can_decrypt) {
          c("x" = "Can't decrypt the {.field {account}} service account token.")
        },
        if (!online) {
          c("x" = "We don't appear to be online. Or maybe the Sheets API is down?")
        }
      ),
      class = "deepgsheets4_auth_internal_error",
      can_decrypt = can_decrypt, online = online
    )
  }

  if (!is_interactive()) local_deepgs_quiet()
  filename <- glue("deepgsheets4-{account}.json")
  # TODO: revisit when I do PKG_scopes()
  # https://github.com/r-lib/gargle/issues/103
  scopes <- scopes %||% "https://www.googleapis.com/auth/drive"
  json <- gargle:::secret_read("deepgsheets4", filename)
  deepgs_auth(scopes = scopes, path = rawToChar(json))
  deepgs_user()
  invisible(TRUE)
}

deepgs_auth_docs <- function(scopes = NULL) {
  deepgs_auth_internal("docs", scopes = scopes)
}

deepgs_auth_testing <- function(scopes = NULL) {
  deepgs_auth_internal("testing", scopes = scopes)
}

local_deauth <- function(env = parent.frame()) {
  original_cred <- .auth$get_cred()
  original_auth_active <- .auth$auth_active
  cli::cli_bullets(c(i = "Going into deauthorized state."))
  withr::defer(
    cli::cli_bullets(c("i" = "Restoring previous auth state.")),
    envir = env
  )
  withr::defer(
    {
      .auth$set_cred(original_cred)
      .auth$set_auth_active(original_auth_active)
    },
    envir = env
  )
  deepgs_deauth()
}


# deepgs_default_app <- function() {
#
#   httr::oauth_app(
#     appname = "deepgs4_dev",
#     key = "616888423467-bibvheuhs4lh4620ss1dtjfk332bfljh.apps.googleusercontent.com",
#     secret = "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",
#     redirect_uri = "http://localhost:1410/"
#   )
#
# }