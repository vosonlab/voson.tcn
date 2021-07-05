#' Get a twitter API access token
#'
#' Assigns a bearer token to the token object or retrieves a bearer token from the Twitter API using a Twitter apps
#' consumer keys.
#'
#' @param bearer Character string. App bearer token.
#' @param consumer_key Character string. App consumer key.
#' @param consumer_secret Character string. App consumer secret.
#'
#' @return Named list containing the token.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # assign bearer token
#' token <- tcn_token(bearer = "xxxxxxxx")
#'
#' # retrieve twitter app bearer token
#' token <- tcn_token(consumer_key = "xxxxxxxx",
#'                    consumer_secret = "xxxxxxxx")
#' }
#'
tcn_token <- function(bearer = NULL,
                      consumer_key = NULL,
                      consumer_secret = NULL) {
  if (!is.null(bearer)) {
    if (is.character(bearer)) {
      return(list(bearer = bearer))
    }
    stop("invalid bearer token.")
  }

  if (is.null(consumer_key) || is.null(consumer_secret) ||
      !is.character(consumer_key) ||
      !is.character(consumer_secret)) {
    stop("invalid consumer key or secret.")
  }

  saved_opts <- save_set_opts()
  on.exit(restore_opts(saved_opts), add = TRUE)

  token <- get_bearer(consumer_key, consumer_secret)

  if (is.null(token)) {
    stop("failed to retrieve app oauth2 access token.")
  }

  list(bearer = token)
}

# get bearer token
get_bearer <- function(consumer_key, consumer_secret) {
  rlang::check_installed("openssl", "get_bearer")
  app_keys <-
    openssl::base64_encode(paste0(consumer_key, ":", consumer_secret))
  resp <- httr::POST(
    "https://api.twitter.com/oauth2/token",
    httr::add_headers(Authorization = paste0("Basic ", app_keys)),
    body = list(grant_type = "client_credentials")
  )
  httr::stop_for_status(resp, "get_bearer")
  resp_content <- httr::content(resp, encoding = "UTF-8")
  resp_content$access_token
}
