#' Get threaded conversation tweets
#'
#' @param tweet_ids List. Tweet ids of any tweet that are part of the threaded conversations of interest. Also accepts
#' a list of tweet URLs or a mixed list.
#' @param token List. Twitter API tokens.
#' @param end_point Character string. Twitter API v2 search end-point. Can be either "recent" for the last 7 days or
#' "all" if users app has access to historical tweets. Default is "recent".
#' @param skip_list Character vector. List of tweet conversation IDs to skip searching if found. This list is
#' automatically appended with conversation_id's when collecting multiple conversation threads to prevent search
#' duplication.
#'
#' @return A dataframe of tweets.
#' @export
#'
#' @examples
#' \dontrun{
#' # get twitter conversation threads by tweet ids or urls
#' tweet_ids <- c("xxxxxxxx",
#'                "https://twitter.com/xxxxxxxx/status/xxxxxxxx")

#' tweets <- tcn_threads(tweet_ids, token)
#' }
#'
tcn_threads <- function(tweet_ids = NULL, token = NULL, end_point = "recent", skip_list = NULL) {
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

  # httr options
  saved_opts <- save_set_opts()
  on.exit(restore_opts(saved_opts), add = TRUE)

  # purrr::map_dfr(tweet_ids, ~get_thread(.x, token$bearer, end_point))

  res_df <- tibble::tibble()
  for (id in tweet_ids) {
    df <- get_thread(
      tweet_id = id,
      token = token$bearer,
      end_point = end_point,
      skip_list = skip_list)

    if (!is.null(df) && nrow(df) > 0) {
      skip_list <- union(skip_list, unique(unlist(df$conversation_id)))
      res_df <- dplyr::bind_rows(res_df, df)
    }
  }

  res_df
}

# get conversation
get_thread <- function(tweet_id = NULL, token = NULL, end_point = "recent", skip_list = NULL) {
  # create authorization header
  bearer_token <-
    httr::add_headers(Authorization = paste0("Bearer ", token))

  # search end-points
  # /2/tweets/search/all - historical
  # /2/tweets/search/recent - last 7 days
  if (!end_point %in% c("recent", "all")) {
    end_point <- "recent"
  }

  # get conversation_id from tweet requested by tweet_id
  url <-
    paste0("https://api.twitter.com/2/tweets/",
           tweet_id,
           "?tweet.fields=conversation_id")
  resp <- httr::GET(url, bearer_token)
  if (resp$status != 200) {
    warning(
      paste0(
        "twitter api response status: ",
        resp$status,
        ".\n",
        "tweet_id: ",
        tweet_id
      ),
      call. = FALSE
    )
    return(NULL)
  }

  # parse response
  resp_obj <- resp_data(resp)
  convo_id <- resp_obj$data$conversation_id

  if (is.null(convo_id)) {
    warning("failed to get tweet conversation_id.", call. = FALSE)
    return(NULL)
  }

  if (convo_id %in% skip_list) {
    warning("skipping tweet id as conversation_id in skip list.", call. = FALSE)
    return(NULL)
  }

  # tweet fields to collect for conversation tweets
  fields <- paste0(
    c(
      "conversation_id",
      "referenced_tweets",
      "in_reply_to_user_id",
      "author_id",
      "created_at",
      "source"
    ),
    collapse = ","
  )
  fields <- paste0("tweet.fields=", fields)

  # get conversation starter tweet as it is not returned by search
  url <-
    paste0("https://api.twitter.com/2/tweets/",
           convo_id,
           "?", fields
           )
  resp <- httr::GET(url, bearer_token)
  resp_obj <- resp_data(resp)
  if (resp$status != 200) {
    message(
      paste0(
        "twitter api response status (tweets): ",
        resp$status,
        ".\n",
        "conversation_id: ",
        convo_id
      )
    )
  }

  # create dataframe
  df_convo <- tibble::as_tibble(resp_obj$data)

  # initial search tweets
  search_url <-
    paste0(
      "https://api.twitter.com/2/tweets/search/",
      end_point,
      "?query=conversation_id:",
      convo_id,
      "&", fields,
      "&max_results=100"
    )

  # search tweets and add results to dataframe
  resp <- httr::GET(search_url, bearer_token)
  resp_obj <- resp_data(resp)

  if (resp$status == 200) {
    # check referenced_tweets
    df_convo <- ensure_ref_tweets_list(df_convo)
    resp_obj$data <- ensure_ref_tweets_list(resp_obj$data)

    df_convo <- dplyr::bind_rows(resp_obj$data, df_convo)
  } else {
    message(
      paste0(
        "twitter api response status (search): ",
        resp$status,
        ".\n",
        "conversation_id: ",
        convo_id,
        " next_token: -"
      )
    )
  }

  next_token <- resp_obj$meta$next_token

  # if more pages of results keep going
  while (!is.null(next_token)) {
    url <- paste0(search_url, "&next_token=", next_token)
    resp <- httr::GET(url, bearer_token)

    if (resp$status == 200) {
      resp_obj <- resp_data(resp)
      if (!is.null(resp_obj$data)) {
        df_convo <- dplyr::bind_rows(df_convo, resp_obj$data)
      }
      next_token <- resp_obj$meta$next_token
    } else {
      message(
        paste0(
          "twitter api response status (search): ",
          resp$status,
          ".\n",
          "conversation_id: ",
          convo_id,
          " next_token: ",
          next_token
        )
      )
    }
  }

  df_convo
}

# httr response json text into a dataframe
resp_data <- function(resp_obj) {
  # if errors print them and return
  if (length(httr::content(resp_obj)$error)) {
    err <-
      sapply(httr::content(resp_obj)$error, function(x)
        warning(x$message))
    return(NULL)
  }

  # return dataframe produced from content json
  resp_content <-
    httr::content(resp_obj, as = "text", encoding = "UTF-8")
  json_df <- tryCatch({
    jsonlite::fromJSON(resp_content)
  }, error = function(e) {
    message(e)
    return(NULL)
  })
}

# ensure referenced tweets column is a list
ensure_ref_tweets_list <- function(x) {
  if ("referenced_tweets" %in% colnames(x)) {
    if (class(x$referenced_tweets) == "data.frame") {
      x <- dplyr::mutate(x, referenced_tweets = list(.data$referenced_tweets))
    }
  }
  x
}
