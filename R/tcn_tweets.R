#' Get tweets
#'
#' Collects tweets for a list of tweet ids.
#'
#' @param tweet_ids List. Tweet ids or tweet URLs.
#' @param token List. Twitter API tokens.
#' @param referenced_tweets Logical. Also retrieve tweets referenced by requested tweets. Default is FALSE.
#' @param retry_on_limit Logical. When the API v2 rate-limit has been reached wait for reset time. Default
#'   is TRUE.
#' @param clean Logical. Clean results.
#'
#' @return A named list. Dataframes of tweets, users, errors and request metadata.
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
           token = NULL,
           referenced_tweets = FALSE,
           retry_on_limit = TRUE,
           clean = TRUE) {

    # check params
    if (is.null(token$bearer) ||
        !is.character(token$bearer)) {
      stop("missing or invalid bearer token.")
    }

    tweet_ids <- ids_from_urls(tweet_ids)

    if (is.null(tweet_ids)) {
      stop("please provide tweet_ids.")
    }

    if (any(is.na(tweet_ids))) {
      na_indexes <- which(is.na(tweet_ids))

      stop(paste0("invalid ids in tweet_ids.\n",
           "index: ",
           paste0(na_indexes, collapse = ",")))
    }

    results <-
      list(
        tweets = NULL,
        users = NULL,
        errors = NULL,
        meta = NULL
      )

    chunks <- split(tweet_ids, ceiling(seq_along(tweet_ids) / 100))
    i <- 1
    for (x in chunks) {
      chunk_tweets <-
        get_tweets(
          tweet_ids = x,
          token = token,
          retry_on_limit = retry_on_limit,
          referenced_tweets = referenced_tweets
        )

      results$tweets <- dplyr::bind_rows(results$tweets, chunk_tweets$tweets)
      results$users <- dplyr::bind_rows(results$users, chunk_tweets$users)
      results$errors <- dplyr::bind_rows(results$errors, chunk_tweets$errors)
      results$meta <- dplyr::bind_rows(results$meta, chunk_tweets$meta)

      if (!is.null(results$rl_abort) && results$rl_abort == TRUE) {
        message(paste0("aborting GET 2/tweets as retry_on_limit = FALSE. tweet ids chunk: ", i))
        results$rl_abort <- NULL
        break
      }

      i <- i + 1
    }

    if (!is.null(results$tweets) && nrow(results$tweets) < 1) {
      message("failed to retrieve any tweets.")
    }

    # tidy up results
    if (!clean) return(results)
    clean_results(results)
  }

# get tweets from ids
get_tweets <-
  function(tweet_ids = NULL,
           token = NULL,
           referenced_tweets = FALSE,
           retry_on_limit = TRUE) {

    results <-
      list(
        tweets = NULL,
        users = NULL,
        errors = NULL,
        meta = NULL,
        rl_abort = FALSE
      )

    # endpoint description
    endpoint_desc <- "GET 2/tweets"

    query_url <- tweets_url(tweet_ids, ref_tweets = referenced_tweets)
    req_header <- req_auth_header(token)
    resp <- httr::GET(query_url, req_header)

    # check rate-limit
    if (resp$status == 429) {
      message(paste0("twitter api rate-limit reached at ", Sys.time()))
      reset <- resp$headers$`x-rate-limit-reset`
      if (retry_on_limit & !is.null(reset)) {
        rl_status <- resp_rate_limit(resp$headers, endpoint_desc, sleep = TRUE)

        # repeat request after reset
        if (rl_status) {
          resp <- httr::GET(query_url, req_header)
        }
      } else {
        rl_status <- resp_rate_limit(resp$headers, endpoint_desc, sleep = FALSE)
        results$rl_abort <- TRUE
      }
    }

    if (resp$status == 200) {
      # parse response
      resp_data <- resp_content(resp)

      results$tweets <- resp_data$tweets
      results$users <- resp_data$users
      results$errors <- resp_data$errors
      results$meta <- resp_data$meta
      results$rl_abort <- FALSE
    } else {
      message(
        paste0(
          "twitter api response status (",
          endpoint_desc,
          "): ",
          resp$status
        ))
    }

    results
  }
