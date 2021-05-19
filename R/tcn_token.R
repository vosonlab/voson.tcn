#' Get a twitter API access token
#'
# @param type Character string. Either app based (bearer) or user based authentication. Default is app.
# @param app_name Character string. Twitter project app name. Default is twitter-app.
#' @param bearer Character string. App bearer token.
#' @param consumer_key Character string. App consumer key.
#' @param consumer_secret Character string. App consumer secret.
# @param access_token Character string. Developer access token.
# @param access_secret Character string. Developer access secret.
#'
#' @return Named list containing the token.
#' @export
#'
tcn_token <- function(
  # type = "app",
  # app_name = "twitter-app",
  bearer = NULL,
  consumer_key = NULL,
  consumer_secret = NULL # ,
  # access_token = NULL,
  # access_secret = NULL
  ) {

  # if (!type %in% c("app", "user")) {
  #   stop("invalid token type.")
  # }

  if (!is.null(bearer)) {
    if (is.character(bearer)) {
      return(list(bearer = bearer))
    }
    stop("invalid bearer token.")
  }

  if (is.null(consumer_key) || is.null(consumer_secret) ||
      !is.character(consumer_key) || !is.character(consumer_secret)) {
    stop("invalid consumer key or secret.")
  }

  saved_opts <- save_set_opts()
  on.exit(restore_opts(saved_opts), add = TRUE)

  # -- bearer token
  # if (type == "app") {
    token <- get_bearer(consumer_key, consumer_secret)

    if (is.null(token)) {
      stop("failed to retrieve app oauth2 access token.")
    }

    return(list(bearer = token))
  # }

  # -- user token
  # if (type == "user") {
  #   if (is.null(app_name) || !is.character(app_name) || nchar(app_name) > 32) {
  #     stop("invalid app name.")
  #   }
  #
  #   app <- httr::oauth_app(
  #     appname = app_name,
  #     key = consumer_key,
  #     secret = consumer_secret,
  #     redirect_uri = "http://127.0.0.1:1410/"
  #   )
  #
  #   # -- user sign in access token
  #   if (is.null(access_token) || is.null(access_secret)) {
  #     token <- httr::oauth1.0_token(
  #       endpoint = httr::oauth_endpoints("twitter"),
  #       app = app)
  #   }
  #
  #   # -- user provided access token
  #   if (is.null(access_token) || is.null(access_secret) ||
  #       !is.character(access_token) || !is.character(access_secret)) {
  #     stop("invalid access token or secret.")
  #   }
  #
  #   credentials <- list(
  #     oauth_token = access_token,
  #     oauth_token_secret = access_secret
  #   )
  # }

  list()
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
