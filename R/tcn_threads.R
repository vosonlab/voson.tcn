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
#' @param max_results Numeric. Set maximum number of tweets to collect per API v2 request. Up to 100 tweets for standard
#'   or 500 tweets for academic projects can be collected per request. Default is 100.
#' @param max_total Numeric. Set maximum total number of tweets to collect as a cap limit precaution. Will only be
#'   accurate to within one search request count (being max_results, or 100 for standard or 500 tweets for academic
#'   project). This will not be ideal for most cases as an API search generally retrieves the most recent tweets first
#'   (reverse-chronological order), therefore the beginning part of the last conversation thread may be absent. Default
#'   is NULL.
#' @param retry_on_limit Logical. When the API v2 rate-limit has been reached wait for reset time. Default
#'   is FALSE.
#' @param skip_list Character vector. List of tweet conversation IDs to skip searching if found. This list is
#'   automatically appended with conversation_id's when collecting multiple conversation threads to prevent search
#'   duplication.
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
           max_results = 100,
           max_total = NULL,
           retry_on_limit = FALSE,
           skip_list = NULL) {
    # check params
    if (is.null(token$bearer) ||
        !is.character(token$bearer)) {
      stop("missing or invalid bearer token.")
    }

    tweet_ids <- ids_from_urls(tweet_ids)

    if (is.null(tweet_ids) || !check_numeric(tweet_ids)) {
      stop("invalid id in tweet_ids.")
    }

    datetime_pattern <-
      "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"

    if (!is.null(start_time)) {
      if (!stringr::str_detect(toupper(start_time), datetime_pattern)) {
        stop(
          "invalid start_time. string must be datetime in ISO 8601 format e.g 2021-05-01T00:00:00Z."
        )
      }
    }

    if (!is.null(end_time)) {
      if (!stringr::str_detect(toupper(end_time), datetime_pattern)) {
        stop(
          "invalid end_time. string must be datetime in ISO 8601 format e.g 2021-05-01T00:00:00Z."
        )
      }
    }

    # search endpoints
    if (!endpoint %in% c("recent", "all")) {
      endpoint <- "recent"
    }

    if (!is.null(max_results) && !check_numeric(max_results)) {
      stop("invalid max_results. must be a number.")
    }

    if (!is.null(max_total) && !check_numeric(max_total)) {
      stop("invalid max_total must be a number.")
    }

    if (!is.logical(retry_on_limit)) {
      stop("invalid retry_on_limit must be logical.")
    }

    results <-
      list(
        tweets = NULL,
        users = NULL,
        errors = NULL,
        meta = NULL
      )

    total_results <- 0

    for (id in tweet_ids) {
      if (!is.null(max_total) && total_results >= max_total) {
        warning(
          paste0(
            "exceeded max total results.\nmax total: ",
            max_total,
            ", total results: ",
            total_results
          )
        )
        break
      }

      df <- get_thread(
        tweet_id = id,
        token = token$bearer,
        endpoint = endpoint,
        start_time = start_time,
        end_time = end_time,
        max_results = max_results,
        max_total = max_total,
        total_results = total_results,
        retry_on_limit = retry_on_limit,
        skip_list = skip_list
      )

      if (!is.null(df$tweets) && nrow(df$tweets)) {
        skip_list <- union(
          skip_list,
          unlist(
            df$tweets %>% dplyr::filter(.data$includes == "FALSE") %>% #
              dplyr::select(.data$conversation_id) %>%
              dplyr::distinct()
          )
        )

        results$tweets <-
          dplyr::bind_rows(results$tweets, df$tweets)
        results$users <- dplyr::bind_rows(results$users, df$users)
        results$errors <-
          dplyr::bind_rows(results$errors, df$errors)
        results$meta <- dplyr::bind_rows(results$meta, df$meta)
      }

      if (!is.null(df$tweets) && nrow(df$tweets) == 1) {
        warning(
          paste0(
            "no additional tweets were collected for tweet id: ",
            id,
            "\n",
            "* check conversation is threaded",
            ifelse(
              endpoint == "recent",
              " and that tweets are not older than 7 days.",
              " and thread occurs after start_time."
            )
          )
        )
      }

      if (!is.null(df$meta$result_count)) {
        total_results <-
          total_results + sum(df$meta$result_count, na.rm = TRUE)
      }
    }

    # tidy up results
    clean_results(results)
  }

# get conversation
get_thread <-
  function(tweet_id = NULL,
           token = NULL,
           endpoint = "recent",
           start_time = NULL,
           end_time = NULL,
           max_results = 100,
           max_total = NULL,
           total_results = 0,
           retry_on_limit = FALSE,
           skip_list = NULL) {
    init_tweet <- get_tweets(tweet_ids = tweet_id, token = token)

    if (is.null(init_tweet) || nrow(init_tweet$tweets) < 1) {
      warning(paste0("failed to retrieve tweet id: ", tweet_id), call. = FALSE)
      return(NULL)
    }

    convo_id <-
      (init_tweet$tweets %>% dplyr::slice_head())$conversation_id

    if (is.null(convo_id)) {
      warning(paste0("failed to get tweet conversation_id for tweet id: ", tweet_id),
              call. = FALSE)
      return(NULL)
    }

    if (convo_id %in% skip_list) {
      warning(
        paste0(
          "skipping tweet id as conversation_id in skip list.\ntweet id:",
          tweet_id,
          ", ",
          "conversation id: ",
          convo_id
        )
      )
      return(NULL)
    }

    ts <- as.integer(as.POSIXct(Sys.time()))

    results <-
      list(
        tweets = init_tweet$tweets,
        users = init_tweet$users,
        errors = init_tweet$errors,
        meta = dplyr::bind_rows(
          tibble::tibble(conversation_id = convo_id, timestamp = ts),
          init_tweet$meta
        )
      )

    # search for tweets

    # endpoint description
    endpoint_desc <- paste0("GET 2/tweets/search/", endpoint)

    query_url <-
      search_url(endpoint, convo_id, start_time, end_time, max_results)
    req_header <- req_auth_header(token)
    resp <- httr::GET(query_url, req_header)

    # check rate-limit
    if (resp$status == 429) {
      warning(paste0("twitter api rate-limit reached at ", Sys.time()), call. = FALSE)
      reset <- resp$headers$`x-rate-limit-reset`
      if (retry_on_limit & !is.null(reset)) {
        rl_status <- resp_rate_limit(resp$headers, endpoint_desc, TRUE)

        # repeat request after reset
        if (rl_status) {
          resp <- httr::GET(query_url, req_header)
        } else {
          next_token <- NULL
        }

      } else {
        rl_status <- resp_rate_limit(resp$headers, endpoint_desc, FALSE)
        next_token <- NULL
      }
    }

    if (resp$status == 200) {
      resp_data <- resp_content(resp)
      # resp_meta(resp_data$meta)

      results$tweets <-
        dplyr::bind_rows(results$tweets, resp_data$tweets)
      results$users <-
        dplyr::bind_rows(results$users, resp_data$users)
      results$errors <-
        dplyr::bind_rows(results$errors, resp_data$errors)
      results$meta <- dplyr::bind_rows(results$meta, resp_data$meta)

      next_token <- resp_data$meta[["next_token"]]
    } else {
      warning(
        paste0(
          "twitter api response status (",
          endpoint_desc,
          "): ",
          resp$status,
          "\n",
          "conversation_id: ",
          convo_id,
          ", next_token: -"
        )
        ,
        call. = FALSE
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
            warning(
              paste0(
                "reached max_total tweets: ",
                result_tally,
                "\n",
                "conversation_id: ",
                convo_id,
                ", next_token: ",
                next_token
              )
              ,
              call. = FALSE
            )
            break
          }
        }
      }

      # ensure only 1 request per second full-archive search
      if (endpoint == "all" &
          req_ts == as.integer(as.POSIXct(Sys.time()))) {
        Sys.sleep(1)
      }

      query_url_next <- paste0(query_url, "&next_token=", next_token)
      resp <- httr::GET(query_url_next, req_header)

      # check rate-limit
      if (resp$status == 429) {
        warning(paste0("twitter api rate-limit reached at ", Sys.time()), call. = FALSE)
        reset <- resp$headers$`x-rate-limit-reset`
        if (retry_on_limit & !is.null(reset)) {
          rl_status <- resp_rate_limit(resp$headers, endpoint_desc, TRUE)

          # repeat request after reset
          if (rl_status) {
            resp <- httr::GET(query_url_next, req_header)
          } else {
            next_token <- NULL
          }

        } else {
          rl_status <- resp_rate_limit(resp$headers, endpoint_desc, FALSE)
          next_token <- NULL
        }
      }

      if (resp$status == 200) {
        resp_data <- resp_content(resp)
        # resp_meta(resp_data$meta)

        results$tweets <-
          dplyr::bind_rows(results$tweets, resp_data$tweets)
        results$users <-
          dplyr::bind_rows(results$users, resp_data$users)
        results$errors <-
          dplyr::bind_rows(results$errors, resp_data$errors)
        results$meta <-
          dplyr::bind_rows(results$meta, resp_data$meta)

        next_token <- resp_data$meta[["next_token"]]
      } else {
        warning(
          paste0(
            "twitter api response status (",
            endpoint_desc,
            "): ",
            resp$status,
            "\n",
            "conversation_id: ",
            convo_id,
            ", next_token: ",
            next_token
          )
          ,
          call. = FALSE
        )
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
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       flatten = TRUE)
  }, error = function(e) {
    message("JSON content error: ", e)
    return(NULL)
  })

  if (!is.null(content)) {
    # tweets <- tibble::as_tibble(unnest_ref_tweets(content$data)) %>%
    tweets <-
      tibble::as_tibble(content$data) %>%
      dplyr::mutate(includes = "FALSE", timestamp = ts)

    if (!is.null(content$includes$tweets)) {
      # incl_tweets <-tibble::as_tibble(unnest_ref_tweets(content$includes$tweets)) %>%
      incl_tweets <-
        tibble::as_tibble(content$includes$tweets) %>%
        dplyr::mutate(includes = "TRUE", timestamp = ts)

      # there is duplication between a search results for conversation_id tweets and
      # referenced_tweet objects in the includes section

      # this is because tweets as seen in conversation threads are referenced_tweets (replied_to, quoted),
      # includes will also contain conversation starter tweet and any quoted tweets not part of conversation

      # find obs that occur in incl_tweets but not in tweets
      incl_tweets <-
        dplyr::anti_join(incl_tweets, tweets, by = "id")

      tweets <- dplyr::bind_rows(tweets, incl_tweets)
    }

    users <-
      tibble::as_tibble(content$includes$users) %>% dplyr::mutate(timestamp = ts)
    if (!is.null(content$errors)) {
      errors <-
        tibble::as_tibble(content$errors) %>% dplyr::mutate(timestamp = ts)
    }
    if (!is.null(content$meta)) {
      meta <-
        tibble::as_tibble(content$meta) %>% dplyr::mutate(timestamp = ts)
    }
  }

  list(
    tweets = tweets,
    users = users,
    errors = errors,
    meta = meta
  )
}

# reformat and remove duplicates
# attempts to retain most recent version if duplicated
clean_results <- function(results) {
  if (!is.null(results$tweets) && nrow(results$tweets)) {
    results$tweets <- results$tweets %>%
      dplyr::rename(tweet_id = .data$id) %>%
      dplyr::arrange(dplyr::desc(.data$timestamp)) %>%
      dplyr::distinct(.data$tweet_id, .keep_all = TRUE)
  }

  if (!is.null(results$users) && nrow(results$users)) {
    results$users <- results$users %>%
      dplyr::rename_with(~ paste0("profile.", .x)) %>%
      dplyr::rename(user_id = .data$profile.id,
                    timestamp = .data$profile.timestamp) %>%
      dplyr::arrange(dplyr::desc(
        .data$timestamp,
        as.integer(.data$profile.public_metrics.tweet_count)
      )) %>%
      dplyr::distinct(.data$user_id, .keep_all = TRUE)
  }

  results
}

# tweet fields to request
query_tweet_fields <- function() {
  tweet_fields <- paste0(
    c(
      "attachments",
      "author_id",
      "context_annotations",
      "conversation_id",
      "created_at",
      "entities",
      "geo",
      "in_reply_to_user_id",
      "lang",
      "possibly_sensitive",
      "public_metrics",
      "referenced_tweets",
      "reply_settings",
      "source",
      "withheld"
    ),
    collapse = ","
  )
  tweet_fields <- paste0("tweet.fields=", tweet_fields)
}

# tweet expansions to request
# expansions includes tweets, users
query_expansions <- function() {
  expansions <- paste0(
    c(
      "author_id",
      "in_reply_to_user_id", # may be redundant
      "referenced_tweets.id",
      "referenced_tweets.id.author_id"
    ),
    collapse = ","
  )
  expansions <- paste0("expansions=", expansions)
}

# tweet user fields to request
query_user_fields <- function() {
  user_fields <- paste0(
    c(
      "created_at",
      "description",
      "entities",
      "location",
      "pinned_tweet_id",
      "profile_image_url",
      "protected",
      "public_metrics",
      "url",
      "verified",
      "withheld"
    ),
    collapse = ","
  )
  user_fields <- paste0("user.fields=", user_fields)
}

# search query url
search_url <- function(endpoint, convo_id, start_time, end_time, max_results) {
  paste0(
    "https://api.twitter.com/2/tweets/search/",
    endpoint,
    "?query=conversation_id:",
    convo_id,
    "&",
    query_tweet_fields(),
    "&",
    query_expansions(),
    "&",
    query_user_fields(),
    ifelse(
      !is.null(start_time),
      paste0("&start_time=", start_time),
      ""
    ),
    ifelse(!is.null(end_time),
           paste0("&end_time=", end_time),
           ""),
    "&max_results=",
    max_results
  )
}
