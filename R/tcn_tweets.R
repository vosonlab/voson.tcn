#' Get tweets
#'
#' Collects tweets for a list of tweet ids.
#'
#' @param tweet_ids List. Tweet ids or tweet URLs.
#' @param token List. Twitter API tokens.
#'
#' @return A named list. Dataframes of tweets, users and errors.
#' @export
#'
#' @examples
#' \dontrun{
#' # get twitter conversation threads by tweet ids or urls
#' tweet_ids <- c("xxxxxxxx",
#'                "https://twitter.com/xxxxxxxx/status/xxxxxxxx")

#' tweets <- tcn_tweets(tweet_ids, token)
#' }
#'
tcn_tweets <-
  function(tweet_ids = NULL,
           token = NULL) {

    # check params
    if (is.null(token$bearer) ||
        !is.character(token$bearer)) {
      stop("missing or invalid bearer token.")
    }

    tweet_ids <- ids_from_urls(tweet_ids)

    if (is.null(tweet_ids) ||
        is.na(as.numeric(tweet_ids))) {
      stop("invalid id in tweet_ids.")
    }

    if (length(tweet_ids) > 100) {
      tweet_ids <- tweet_ids[1:100]
      warning("only 100 tweets can be requested.", call. = FALSE)
    }

    df <- get_tweets(tweet_ids, token)

    if (nrow(df$tweets) < 1) {
      warning("failed to retrieve any tweets", call. = FALSE)
    }

    # df$tweets <- df$tweets %>%
    #   tidyr::chop(c(.data$public_metrics)) %>%
    #   tidyr::unnest_wider(.data$public_metrics, names_sep = ".") %>%
    #   tidyr::unnest(dplyr::starts_with("public_metrics"))

    if (!is.null(df$users) && nrow(df$users)) {
      df$users <- df$users %>%
        dplyr::rename_with(~ paste0("profile.", .x)) %>%
        dplyr::rename(user_id = .data$profile.id) %>%
        # tidyr::chop(c(.data$profile.public_metrics)) %>%
        # tidyr::unnest_wider(.data$profile.public_metrics, names_sep = ".") %>%
        # tidyr::unnest(dplyr::starts_with("profile.public_metrics")) %>%
        dplyr::arrange(dplyr::desc(as.numeric(
          .data$profile.public_metrics.tweet_count
        ))) # %>%
        # dplyr::distinct(.data$user_id, .keep_all = TRUE)
    }

    df
  }

# get tweets from ids
get_tweets <-
  function(tweet_ids = NULL,
           token = NULL) {

    results <- list(tweets = NULL, users = NULL, errors = NULL)

    query_url <- tweets_url(tweet_ids)
    req_header <- req_auth_header(token)
    resp <- httr::GET(query_url, req_header)

    # resp_rate_limit(resp, "GET 2/tweets")

    if (resp$status != 200) {
      warning(
        paste0(
          "twitter api response status: ",
          resp$status
        ),
        call. = FALSE
      )
      return(results)
    }

    # parse response
    resp_data <- resp_content(resp)

    results$tweets <- resp_data$tweets
    results$users <- resp_data$users
    results$errors <- resp_data$errors

    next_token <- resp_data$meta$next_token

    while (!is.null(next_token)) {
      url <- paste0(query_url, "&next_token=", next_token)
      resp <- httr::GET(url, req_header)

      # resp_rate_limit(resp, "GET 2/tweets")

      if (resp$status == 200) {
        resp_data <- resp_content(resp)

        results$tweets <- dplyr::bind_rows(results$tweets, resp_data$tweets)
        results$users <- dplyr::bind_rows(results$users, resp_data$users)
        results$errors <- dplyr::bind_rows(results$errors, resp_data$errors)

        next_token <- resp_data$meta$next_token
      } else {
        warning(
          paste0(
            "twitter api response status (tweets): ",
            resp$status,
            ".\n",
            " next_token: ",
            next_token
          )
        , call. = FALSE)
        next_token <- NULL
      }
    }

    results
  }

tweets_url <- function(ids) {
  paste0(
    "https://api.twitter.com/2/tweets",
    "?ids=",
    paste0(ids, collapse = ","),
    "&",
    query_tweet_fields(),
    "&",
    query_expansions(),
    "&",
    query_user_fields())
}
