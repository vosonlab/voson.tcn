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
#' @param verbose Logical. Output additional information. Default is \code{TRUE}.
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
           clean = TRUE,
           verbose = TRUE) {

    msg <- f_verbose(verbose)

    # check params
    if (is.null(token$bearer) || !is.character(token$bearer)) {
      stop("missing or invalid bearer token.")
    }

    tweet_ids <- ids_from_urls(tweet_ids)

    if (is.null(tweet_ids)) stop("please provide tweet_ids.")

    if (any(is.na(tweet_ids))) {
      na_indexes <- which(is.na(tweet_ids))

      stop(paste0("invalid ids in tweet_ids.\n", "index: ", paste0(na_indexes, collapse = ",")))
    }

    results <- lst_tweets_results()

    chunks <- split(tweet_ids, ceiling(seq_along(tweet_ids) / 100))

    msg(paste0("voson.tcn tweets: n=", length(tweet_ids), ", min-api-requests=", length(chunks)))

    if (verbose) {
      pb <- prog_bar(length(chunks), "")
      pb$tick(0)
    }

    i <- 1
    for (x in chunks) {
      chunk_tweets <-
        get_tweets(
          tweet_ids = x,
          token = token,
          retry_on_limit = retry_on_limit,
          referenced_tweets = referenced_tweets,
          verbose = verbose
        )

      results <- add_tweets_results(results, chunk_tweets)

      if (!is.null(results$rl_abort) && results$rl_abort == TRUE) {
        err_msg(paste0("\naborting GET 2/tweets as retry_on_limit = FALSE. tweet ids chunk: ", i))
        results$rl_abort <- NULL
        break
      }

      if (verbose) pb$tick(1)
      i <- i + 1
    }

    if (!is.null(results$tweets) && nrow(results$tweets) < 1) {
      err_msg("error: failed to retrieve any tweets.")
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
           retry_on_limit = TRUE,
           verbose = TRUE) {

    msg <- f_verbose(verbose)

    results <- lst_tweets_results(rl_abort = FALSE)

    # endpoint description
    endpoint_desc <- "GET 2/tweets"

    query_url <- tweets_url(tweet_ids, ref_tweets = referenced_tweets)
    req_header <- req_auth_header(token)
    resp <- httr::GET(query_url, req_header)

    # check rate-limit
    if (resp$status == 429) {
      msg(paste0("\ntwitter api rate-limit reached at ", Sys.time()))
      reset <- resp$headers$`x-rate-limit-reset`
      if (retry_on_limit & !is.null(reset)) {
        rl_status <- resp_rate_limit(resp$headers, endpoint_desc, sleep = TRUE, verbose = verbose)

        # repeat request after reset
        if (rl_status) resp <- httr::GET(query_url, req_header)

      } else {
        rl_status <- resp_rate_limit(resp$headers, endpoint_desc, sleep = FALSE, verbose = verbose)
        results$rl_abort <- TRUE
      }
    }

    if (resp$status == 200) {
      # parse response
      resp_data <- resp_content(resp)

      # assign response data to results
      results <- add_tweets_results(results, resp_data)
      results$rl_abort <- FALSE
    } else {
      err_msg(paste0("\nresponse error:\n",
                     "api status (", endpoint_desc, "): ", resp$status))
    }

    results
  }
