#' Get tweets
#'
#' Collects tweets for a list of tweet ids.
#'
#' @param tweet_ids List. Tweet ids or tweet URLs.
#' @param token List. Twitter API tokens.
#'
#' @return A named list. Dataframes of tweets and users.
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

    df <- get_tweets(tweet_ids, token)

    if (is.null(df) || nrow(df$tweets) < 1) {
      warning("failed to retrieve any tweets", call. = FALSE)
      return(list(tweets = tibble::tibble(), users = tibble::tibble()))
    }

    if (!is.null(df$tweets) && nrow(df$tweets)) {
      df$tweets <- df$tweets %>%
        tidyr::chop(c(.data$public_metrics)) %>%
        tidyr::unnest_wider(.data$public_metrics, names_sep = ".") %>%
        tidyr::unnest(dplyr::starts_with("public_metrics"))
    }

    if (!is.null(df$users) && nrow(df$users)) {
      df$users <- df$users %>%
        dplyr::rename_with(~ paste0("profile.", .x)) %>%
        dplyr::rename(user_id = .data$profile.id) %>%
        tidyr::chop(c(.data$profile.public_metrics)) %>%
        tidyr::unnest_wider(.data$profile.public_metrics, names_sep = ".") %>%
        tidyr::unnest(dplyr::starts_with("profile.public_metrics")) %>%
        dplyr::arrange(dplyr::desc(as.numeric(
          .data$profile.public_metrics.tweet_count
        ))) %>%
        dplyr::distinct(.data$user_id, .keep_all = TRUE)
    }

    df
  }

# get tweets from ids
get_tweets <-
  function(tweet_ids = NULL,
           token = NULL) {

    df_tweets <- df_users <- tibble::tibble()
    query_url <- tweets_url(tweet_ids)
    req_header <- request_header(token)

    resp <- httr::GET(query_url, req_header)
    if (resp$status != 200) {
      warning(
        paste0(
          "twitter api response status: ",
          resp$status
        ),
        call. = FALSE
      )
      return(NULL)
    }

    # parse response
    resp_obj <- resp_data(resp)
    df_tweets <- tibble::as_tibble(unnest_ref_tweets(resp_obj$data))
    df_users <- tibble::as_tibble(resp_obj$includes$users)

    next_token <- resp_obj$meta$next_token

    while (!is.null(next_token)) {
      url <- paste0(query_url, "&next_token=", next_token)
      resp <- httr::GET(url, req_header)

      if (resp$status == 200) {
        resp_obj <- resp_data(resp)

        if (!is.null(resp_obj$data)) {
          tweets <- tibble::as_tibble(unnest_ref_tweets(resp_obj$data))
          df_tweets <- dplyr::bind_rows(df_tweets, tweets)
          df_users <- dplyr::bind_rows(df_users, tibble::as_tibble(resp_obj$includes$users))
        }

        next_token <- resp_obj$meta$next_token
      } else {
        message(
          paste0(
            "twitter api response status (tweets): ",
            resp$status,
            ".\n",
            " next_token: ",
            next_token
          )
        )
      }
    }

    list(tweets = df_tweets, users = df_users)
  }

tweets_url <- function(ids) {
  paste0(
    "https://api.twitter.com/2/tweets",
    "?ids=",
    paste0(ids, collapse = ","), # ids_from_urls(ids)
    "&",
    query_tweet_fields(),
    "&",
    query_expansions(),
    "&",
    query_user_fields())
}
