#' Get conversation tweet counts
#'
#' Return the number of tweets for conversation ids.
#'
#' @param ids List. Conversation ids.
#' @param token List. Twitter API tokens.
#' @param endpoint Character string. Twitter API v2 search endpoint. Can be either "recent" for the last 7 days or "all"
#'   if users app has access to historical "full-archive" tweets. Default is "recent".
#' @param start_time Character string. Earliest tweet timestamp to return (UTC in ISO 8601 format). If NULL API will
#'   default to 30 days before end_time. Default is NULL.
#' @param end_time Character string. Latest tweet timestamp to return (UTC in ISO 8601 format). If NULL API will default
#'   to now - 30 seconds. Default is NULL.
#' @param granularity Character string. Granularity or period for tweet counts. Can be "day", "minute" or "hour". Default is "day".
#'
#' @return A dataframe of conversation ids and counts.
#' @export
#'
#' @examples
#' \dontrun{
#' # get tweet count for conversation thread over approximately 7 days
#' counts <-
#'   tcn_counts(
#'     ids = "xxxxxxxx",
#'     token = token,
#'     endpoint = "all",
#'     start_time = "2020-09-30T01:00:00Z",
#'     end_time = "2020-10-07T01:00:00Z",
#'     granularity = "day"
#'   )
#'
#' # total tweets per conversation id for period
#' counts$counts %>% dplyr::count(conversation_id, wt = tweet_count)
#' }
#'
tcn_counts <-
  function(ids = NULL,
           token = NULL,
           endpoint = "recent",
           start_time = NULL,
           end_time = NULL,
           granularity = "day") {

    # check params
    if (is.null(token$bearer) ||
        !is.character(token$bearer)) {
      stop("missing or invalid bearer token.")
    }

    if (is.null(ids) || !check_numeric(ids)) {
      stop("invalid id in tweet_ids.")
    }

    if (!is.null(start_time)) {
      if (!check_fmt_datetime(start_time)) {
        stop(
          "invalid start_time. string must be datetime in ISO 8601 format e.g 2021-05-01T00:00:00Z."
        )
      }
    }

    if (!is.null(end_time)) {
      if (!check_fmt_datetime(end_time)) {
        stop(
          "invalid end_time. string must be datetime in ISO 8601 format e.g 2021-05-01T00:00:00Z."
        )
      }
    }

    # search endpoints
    if (!endpoint %in% c("recent", "all")) {
      endpoint <- "recent"
    }

    results <-
      list(
        counts = NULL,
        errors = NULL,
        meta = NULL
      )

    for (convo_id in ids) {
      # endpoint description
      endpoint_desc <- paste0("GET 2/tweets/counts/", endpoint)

      query_url <-
        counts_url(endpoint, convo_id, start_time, end_time, granularity)
      req_header <- req_auth_header(token)
      resp <- httr::GET(query_url, req_header)

      if (resp$status == 200) {
        resp_data <- resp_content_counts(resp)

        data <- dplyr::mutate(resp_data$counts, conversation_id = convo_id, page = NA)

        results$counts <-
          dplyr::bind_rows(results$counts, data)
        results$errors <-
          dplyr::bind_rows(results$errors, resp_data$errors)
        results$meta <- dplyr::bind_rows(results$meta, resp_data$meta)

        next_token <- resp_data$meta[["next_token"]]
      } else if (resp$status == 429) {
        warning("rate-limit exceeded", call. = FALSE)
        rl_status <- resp_rate_limit(resp$headers, endpoint_desc, FALSE)
        next_token <- NULL
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

      req_ts <- 0

      # if more pages of results keep going
      while (!is.null(next_token)) {
        # ensure only 1 request per second full-archive search
        if (endpoint == "all" &
            req_ts == as.integer(as.POSIXct(Sys.time()))) {
          Sys.sleep(1)
        }

        url <- paste0(query_url, "&next_token=", next_token)
        resp <- httr::GET(url, req_header)

        if (resp$status == 200) {
          resp_data <- resp_content_counts(resp)

          data <- dplyr::mutate(resp_data$counts, conversation_id = convo_id, page = next_token)

          results$counts <-
            dplyr::bind_rows(results$counts, data)
          results$errors <-
            dplyr::bind_rows(results$errors, resp_data$errors)
          results$meta <- dplyr::bind_rows(results$meta, resp_data$meta)

          next_token <- resp_data$meta[["next_token"]]
        } else if (resp$status == 429) {
          warning("rate-limit exceeded", call. = FALSE)
          rl_status <- resp_rate_limit(resp$headers, endpoint_desc, FALSE)
          next_token <- NULL
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
    }

    results
  }

# extract data from response content
resp_content_counts <- function(resp) {
  ts <- as.integer(as.POSIXct(Sys.time()))
  counts <- errors <- meta <- NULL

  content <- tryCatch({
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       flatten = TRUE)
  }, error = function(e) {
    message("JSON content error: ", e)
    return(NULL)
  })

  if (!is.null(content)) {
    counts <-
      tibble::as_tibble(content$data) %>% dplyr::mutate(timestamp = ts)

    if (!is.null(content$meta)) {
      meta <-
        tibble::as_tibble(content$meta) %>% dplyr::mutate(timestamp = ts)
    }
  }

  list(
    counts = counts,
    errors = errors,
    meta = meta
  )
}

# counts query url
counts_url <- function(endpoint, convo_id, start_time, end_time, granularity) {
  paste0(
    "https://api.twitter.com/2/tweets/counts/",
    endpoint,
    "?query=conversation_id:",
    convo_id,
    ifelse(
      !is.null(start_time),
      paste0("&start_time=", start_time),
      ""
    ),
    ifelse(!is.null(end_time),
           paste0("&end_time=", end_time),
           ""),
    "&granularity=",
    granularity
  )
}
