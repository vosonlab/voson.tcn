#' Get conversation tweets
#'
#' @param token List. Twitter API tokens.
#' @param tweet_id Character string. Tweet id of any tweet that is part of the threaded conversation.
#'
#' @return A dataframe of tweets.
#' @export
#'
#' @examples
#' \dontrun{
#' # get twitter conversation by tweet id
#' tweets <- get_convo(token, "1309703812123226112")
#' }
#'
get_convo <- function(token, tweet_id = NULL) {
  # check params
  if (is.null(token$bearer) ||
      !is.character(token$bearer)) {
    stop("invalid bearer token.")
  }
  if (is.null(tweet_id) ||
      is.na(as.numeric(tweet_id))) {
    stop("invalid tweet_id.")
  }

  # httr options
  saved_opts <- save_set_opts()
  on.exit(restore_opts(saved_opts), add = TRUE)

  # create authorization header
  bearer_token <-
    httr::add_headers(Authorization = paste0("Bearer ", token$bearer))

  # get conversation_id from tweet requested by tweet_id
  url <-
    paste0("https://api.twitter.com/2/tweets/",
           tweet_id,
           "?tweet.fields=conversation_id")
  resp <- httr::GET(url, bearer_token)
  if (resp$status != 200) {
    stop(
      paste0(
        "twitter api response status: ",
        resp$status,
        ".\n",
        "tweet_id: ",
        tweet_id
      ),
      call. = FALSE
    )
  }

  # parse response
  resp_obj <- resp_data(resp)
  convo_id <- resp_obj$data$conversation_id

  if (is.null(convo_id)) {
    stop("failed to get tweet conversation_id.", call. = FALSE)
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

  # get conversation starter tweet as it is not returned by search
  url <-
    paste0("https://api.twitter.com/2/tweets/",
           convo_id,
           "?tweet.fields=",
           fields)
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
      "https://api.twitter.com/2/tweets/search/recent?query=conversation_id:",
      convo_id,
      "&tweet.fields=",
      fields,
      "&max_results=100"
    )

  # search tweets and add results to dataframe
  resp <- httr::GET(search_url, bearer_token)
  resp_obj <- resp_data(resp)

  if (resp$status == 200) {
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
