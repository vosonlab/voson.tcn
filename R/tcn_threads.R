#' Get threaded conversation tweets
#'
#' Collects tweets that share the same Twitter conversation ID as supplied tweets.
#'
#' @param tweet_ids List. Tweet ids of any tweet that are part of the threaded conversations of interest. Also accepts a
#'   list of tweet URLs or a mixed list.
#' @param token List. Twitter API tokens.
#' @param endpoint Character string. Twitter API v2 search endpoint. Can be either "recent" for the last 7 days or "all"
#'   if users app has access to historical "full-archive" tweets. Default is "recent".
#' @param start_time Character string. Earliest tweet timestamp to return (UTC in ISO 8601 format). If NULL API will
#'   default to 30 days before end_time. Default is NULL.
#' @param end_time Character string. Latest tweet timestamp to return (UTC in ISO 8601 format). If NULL API will default
#'   to now - 30 seconds. Default is NULL.
#' @param annotations Logical. Include tweet context annotation field in results. If TRUE Twitter imposes a max_results
#'   limit of 100. Default is FALSE.
#' @param max_results Numeric. Set maximum number of tweets to collect per API v2 request. Up to 100 tweets for standard
#'   or 500 tweets for academic projects can be collected per request. Default is NULL to set maximum allowed.
#' @param max_total Numeric. Set maximum total number of tweets to collect as a cap limit precaution. Will only be
#'   accurate to within one search request count (being max_results, or 100 for standard or 500 tweets for academic
#'   project). This will not be ideal for most cases as an API search generally retrieves the most recent tweets first
#'   (reverse-chronological order), therefore the beginning part of the last conversation thread may be absent. Default
#'   is NULL.
#' @param retry_on_limit Logical. When the API v2 rate-limit has been reached wait for reset time. Default
#'   is TRUE.
#' @param skip_list Character vector. List of tweet conversation IDs to skip searching if found.
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

#' tweets <- tcn_threads(tweet_ids, token, endpoint = "recent")
#'
#' # get twitter conversation threads by tweet ids or urls using historical endpoint
#' # starting from May 01, 2021.
#' tweet_ids <- c("xxxxxxxx",
#'                "https://twitter.com/xxxxxxxx/status/xxxxxxxx")

#' tweets <- tcn_threads(tweet_ids,
#'                       token = token,
#'                       endpoint = "all",
#'                       start_time = "2021-05-01T00:00:00Z")
#' }
#'
tcn_threads <-
  function(tweet_ids = NULL,
           token = NULL,
           endpoint = "recent",
           start_time = NULL,
           end_time = NULL,
           annotations = FALSE,
           max_results = NULL,
           max_total = NULL,
           retry_on_limit = TRUE,
           skip_list = NULL,
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

    if (!is.null(start_time)) {
      start_time <- rm_dt_tail(start_time)
      if (!check_fmt_datetime(start_time)) {
        stop("invalid start_time. string must be datetime in ISO 8601 format e.g 2021-05-01T00:00:00Z.")
      }
    }

    if (!is.null(end_time)) {
      end_time <- rm_dt_tail(end_time)
      if (!check_fmt_datetime(end_time)) {
        stop("invalid end_time. string must be datetime in ISO 8601 format e.g 2021-05-01T00:00:00Z.")
      }
    }

    # search endpoints
    if (!endpoint %in% c("recent", "all")) endpoint <- "recent"

    ep_max_results <- 100
    if (endpoint == "all") ep_max_results <- 500

    # set default max results
    if (is.null(max_results)) {
      max_results <- 100
      if (endpoint == "all" & !annotations) max_results <- 500
    }

    if (!is.numeric(max_results)) {
      stop("invalid max_results. must be a number.")
    }

    if (max_results < 1 | max_results > ep_max_results) {
      stop(paste0("invalid max_results. must be between 1 and ", ep_max_results, "."))
    }

    if (!is.null(max_total) && !is.numeric(max_total)) {
      stop("invalid max_total must be a number.")
    }

    if (!is.logical(retry_on_limit)) {
      stop("invalid retry_on_limit must be logical.")
    }

    results <- lst_tweets_results()
    total_results <- 0

    # get tweets
    tweets <- tcn_tweets(
      tweet_ids = tweet_ids,
      token = token,
      referenced_tweets = FALSE,
      retry_on_limit = TRUE,
      clean = FALSE,
      verbose = verbose)

    results <- add_tweets_results(results, tweets)

    # get conversation_ids
    cids <- unique(tweets$tweets$conversation_id)

    if (!is.null(skip_list)) {
      skip_list <- as.character(skip_list)
      cids <- setdiff(cids, skip_list)
    }

    if (is.null(cids)) {
      err_msg("failed to get any conversation_ids")
      return(NULL)
    }

    chunks <- build_chunks(cids, endpoint = endpoint)

    msg(paste0("voson.tcn conversation threads: n=", length(cids), ", min-api-requests=", length(chunks), ", endpoint=", endpoint))

    if (verbose) {
      pb <- prog_bar(length(chunks), "")
      pb$tick(0)
    }

    i <- 1
    for (cids_chunk in chunks) {
      if (!is.null(max_total) && total_results >= max_total) {
        msg(
          paste0("\nexceeded max total results.\nmax total: ", max_total, ", total results: ", total_results)
        )
        break
      }

      df <- get_thread(
        conversation_ids = cids_chunk,
        token = token$bearer,
        endpoint = endpoint,
        start_time = start_time,
        end_time = end_time,
        annotations = annotations,
        max_results = max_results,
        max_total = max_total,
        total_results = total_results,
        retry_on_limit = retry_on_limit,
        verbose = verbose
      )

      if (!is.null(df$tweets) && nrow(df$tweets)) {
        results <- add_tweets_results(results, df)
      }
      skip_list <- union(skip_list, cids_chunk)

      if ("result_count" %in% names(df$meta)) {
        if (!is.null(df$meta$result_count)) {
          total_results <- total_results + sum(df$meta$result_count, na.rm = TRUE)
        }
      }

      if (!is.null(results$rl_abort) && results$rl_abort == TRUE) {
        err_msg(paste0("\naborting GET 2/tweets/search/ as retry_on_limit = FALSE. tweet ids chunk: ", i))
        results$rl_abort <- NULL
        break
      }

      if (verbose) pb$tick(1)
      i <- i + 1
    }

    clean_results(results)
  }

# get conversation
get_thread <-
  function(conversation_ids = NULL,
           token = NULL,
           endpoint = "recent",
           start_time = NULL,
           end_time = NULL,
           annotations = FALSE,
           max_results = 100,
           max_total = NULL,
           total_results = 0,
           retry_on_limit = FALSE,
           verbose = TRUE) {

    msg <- f_verbose(verbose)

    results <- lst_tweets_results(rl_abort = FALSE)

    chr_cids <- gsub("%20OR%20", ",", conversation_ids)
    chr_cids <- gsub("conversation_id:", "", chr_cids)

    # search for tweets

    # ensure only 1 request per second full-archive search
    if (endpoint == "all") Sys.sleep(1)

    # endpoint description
    endpoint_desc <- paste0("GET 2/tweets/search/", endpoint)

    query_url <- search_url(endpoint, conversation_ids, start_time, end_time, annotations, max_results)

    req_header <- req_auth_header(token)
    resp <- httr::GET(query_url, req_header)

    # check rate-limit
    if (resp$status == 429) {
      msg(paste0("\ntwitter api rate-limit reached at ", Sys.time()))
      reset <- resp$headers$`x-rate-limit-reset`

      if (retry_on_limit & !is.null(reset)) {
        rl_status <- resp_rate_limit(resp$headers, endpoint_desc, sleep = TRUE, verbose = verbose)

        # repeat request after reset
        if (rl_status) {
          resp <- httr::GET(query_url, req_header)
        } else {
          next_token <- NULL
        }

      } else {
        rl_status <- resp_rate_limit(resp$headers, endpoint_desc, sleep = FALSE, verbose = verbose)
        next_token <- NULL
      }
    }

    if (resp$status == 200) {
      resp_data <- resp_content(resp)
      results <- add_tweets_results(results, resp_data)
      next_token <- resp_data$meta[["next_token"]]
    } else {
      err_msg(
        paste0(
          "\nresponse error:\n",
          "api status (", endpoint_desc, "): ", resp$status, "\n",
          "conversation_id: ", chr_cids, ", next_token: -")
      )
      next_token <- NULL
    }

    # request timestamp
    req_ts <- 0

    # if more pages of results keep going
    while (!is.null(next_token)) {
      # stop pagination if exceeded max results value
      if (!is.null(max_total)) {
        if (!is.null(resp_data$meta$result_count)) {
          result_tally <- total_results + sum(resp_data$meta$result_count, na.rm = TRUE)
          if (result_tally >= max_total) {
            msg(
              paste0(
                "\nreached max_total tweets: ", result_tally, "\n",
                "conversation_id: ", chr_cids, ", next_token: ", next_token
              ))
            break
          }
        }
      }

      # ensure only 1 request per second full-archive search
      if (endpoint == "all" & req_ts == as.integer(as.POSIXct(Sys.time()))) {
        Sys.sleep(1)
      }

      query_url_next <- paste0(query_url, "&next_token=", next_token)
      resp <- httr::GET(query_url_next, req_header)

      # check rate-limit
      if (resp$status == 429) {
        msg(paste0("\ntwitter api rate-limit reached at ", Sys.time()))
        reset <- resp$headers$`x-rate-limit-reset`
        if (retry_on_limit & !is.null(reset)) {
          rl_status <- resp_rate_limit(resp$headers, endpoint_desc, sleep = TRUE, verbose = verbose)

          # repeat request after reset
          if (rl_status) {
            resp <- httr::GET(query_url_next, req_header)
          } else {
            next_token <- NULL
          }

        } else {
          rl_status <- resp_rate_limit(resp$headers, endpoint_desc, sleep = FALSE, verbose = verbose)
          results$rl_abort <- TRUE
          next_token <- NULL
        }
      }

      if (resp$status == 200) {
        resp_data <- resp_content(resp)
        results <- add_tweets_results(results, resp_data)
        results$rl_abort <- FALSE
        next_token <- resp_data$meta[["next_token"]]
      } else {
        err_msg(
          paste0(
            "\nresponse error:\n",
            "api status (", endpoint_desc, "): ", resp$status, "\n",
            "conversation_id: ", chr_cids, ", next_token: ", next_token
          ))
        next_token <- NULL
      }
      req_ts <- as.integer(as.POSIXct(Sys.time()))
    }

    results
  }

# extract data from response content
resp_content <- function(resp) {
  ts <- as.integer(as.POSIXct(Sys.time()))
  tweets <- users <- errors <- meta <- NULL

  content <- tryCatch({
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  }, error = function(e) {
    err_msg(paste0("JSON content error: ", e))
    return(NULL)
  })

  if (!is.null(content)) {
    tweets <-
      tibble::as_tibble(content$data) |>
      dplyr::mutate(includes = "FALSE", timestamp = ts)

    if (!is.null(content$includes$tweets)) {
      incl_tweets <-
        tibble::as_tibble(content$includes$tweets) |>
        dplyr::mutate(includes = "TRUE", timestamp = ts)

      # there is duplication between a search results for conversation_id tweets and
      # referenced_tweet objects in the includes section

      # this is because tweets as seen in conversation threads are referenced_tweets (replied_to, quoted),
      # includes will also contain conversation starter tweet and any quoted tweets not part of conversation

      # find any tweets that occur in includes but are not in content tweets
      incl_tweets <- dplyr::anti_join(incl_tweets, tweets, by = "id")

      tweets <- dplyr::bind_rows(tweets, incl_tweets)
    }

    users <- tibble::as_tibble(content$includes$users) |> dplyr::mutate(timestamp = ts)
    if (!is.null(content$errors)) {
      errors <- tibble::as_tibble(content$errors) |> dplyr::mutate(timestamp = ts)
    }
    if (!is.null(content$meta)) {
      meta <- tibble::as_tibble(content$meta) |> dplyr::mutate(timestamp = ts)
    }
  }

  list(tweets = tweets, users = users, errors = errors, meta = meta)
}
