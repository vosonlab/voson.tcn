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

  tweets_df <- users_df <- tibble::tibble()
  for (id in tweet_ids) {
    df <- get_thread(
      tweet_id = id,
      token = token$bearer,
      end_point = end_point,
      skip_list = skip_list)

    if (!is.null(df$tweets) && nrow(df$tweets) > 0) {
      skip_list <- union(skip_list,
                         unlist(df$tweets %>% dplyr::filter(.data$includes == "FALSE") %>%
                                dplyr::select(.data$conversation_id) %>%
                                dplyr::distinct()))
      tweets_df <- dplyr::bind_rows(tweets_df, df$tweets)
      users_df <- dplyr::bind_rows(users_df, df$users)
    }
  }

  if (nrow(tweets_df) > 0) {
    tweets_df <- tweets_df %>%
      dplyr::distinct(.data$tweet_id, .data$ref_tweet_type, .keep_all = TRUE) %>%
      tidyr::chop(c(.data$public_metrics)) %>%
      tidyr::unnest_wider(.data$public_metrics, names_sep = ".") %>%
      tidyr::unnest(dplyr::starts_with("public_metrics"))
  }

  if (nrow(users_df) > 0) {
    users_df <- users_df %>%
      dplyr::rename_with(~ paste0("profile.", .x)) %>%
      dplyr::rename(user_id = .data$profile.id) %>%
      tidyr::chop(c(.data$profile.public_metrics)) %>%
      tidyr::unnest_wider(.data$profile.public_metrics, names_sep = ".") %>%
      tidyr::unnest(dplyr::starts_with("profile.public_metrics")) %>%
      dplyr::arrange(dplyr::desc(as.numeric(.data$profile.public_metrics.tweet_count))) %>%
      dplyr::distinct(.data$user_id, .keep_all = TRUE)
  }

  list(tweets = tweets_df, users = users_df)
}

# get conversation
get_thread <- function(tweet_id = NULL, token = NULL, end_point = "recent", skip_list = NULL) {
  # create authorization header
  bearer_token <-
    httr::add_headers(Authorization = paste0("Bearer ", token))

  # search end-points
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
  tweet_fields <- paste0(
    c(
      "conversation_id",
      "author_id",
      "created_at",
      "source",
      "public_metrics",
      "in_reply_to_user_id",
      "referenced_tweets"
    ),
    collapse = ","
  )
  tweet_fields <- paste0("tweet.fields=", tweet_fields)

  # expansions includes tweets, users
  expansions <- paste0(
    c(
      "author_id",
      "in_reply_to_user_id",
      "referenced_tweets.id",
      "referenced_tweets.id.author_id"
    ),
    collapse = ","
  )
  expansions <- paste0("expansions=", expansions)

  user_fields <- paste0(
    c(
      "created_at",
      "description",
      "location",
      "profile_image_url",
      "public_metrics",
      "verified"
    ),
    collapse = ","
  )
  user_fields <- paste0("user.fields=", user_fields)

  # initial search tweets
  search_url <-
    paste0(
      "https://api.twitter.com/2/tweets/search/",
      end_point,
      "?query=conversation_id:",
      convo_id,
      "&", tweet_fields,
      "&", expansions,
      "&", user_fields,
      "&max_results=100"
    )

  # search tweets and add results to dataframe
  resp <- httr::GET(search_url, bearer_token)
  resp_obj <- resp_data(resp)

  if (resp$status == 200) {
    tweets <-
      tibble::as_tibble(unnest_ref_tweets(resp_obj$data)) %>% dplyr::mutate(includes = "FALSE")
    tweets_incl <-
      tibble::as_tibble(unnest_ref_tweets(resp_obj$includes$tweets)) %>% dplyr::mutate(includes = "TRUE")

    # there is duplication between a search for conversation_id tweets and returning referenced tweet objects
    # in includes as replied to tweets as seen in conversations are referenced tweets
    # includes will contain conversation starter tweet and any quoted tweets not part of conversation
    tweets_incl <- dplyr::anti_join(tweets_incl, tweets, by = c("tweet_id" = "tweet_id"))

    df_convo <- dplyr::bind_rows(tweets, tweets_incl)
    df_users <- tibble::as_tibble(resp_obj$includes$users)
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
        tweets <-
          tibble::as_tibble(unnest_ref_tweets(resp_obj$data)) %>% dplyr::mutate(includes = "FALSE")
        tweets_incl <-
          tibble::as_tibble(unnest_ref_tweets(resp_obj$includes$tweets)) %>% dplyr::mutate(includes = "TRUE")

        tweets_incl <- dplyr::anti_join(tweets_incl, tweets, by = c("tweet_id" = "tweet_id"))

        df_convo <- dplyr::bind_rows(df_convo, tweets, tweets_incl)
        df_users <- dplyr::bind_rows(df_users, tibble::as_tibble(resp_obj$includes$users))
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

  list(tweets = df_convo, users = df_users)
}

# httr response json text into a dataframe
resp_data <- function(resp_obj) {
  # if errors print them and return
  if (length(httr::content(resp_obj)$error)) {
    err <-
      sapply(httr::content(resp_obj)$error,
             function(x) {
               if (!is.null(x$message)) { warning(x$message) }
             })

    res_count <- httr::content(resp_obj)$meta$result_count
    if (is.null(res_count) || (as.numeric(res_count) < 1)) {
      return(NULL)
    }
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

# unnest referenced tweets column
unnest_ref_tweets <- function(x) {
  if ("referenced_tweets" %in% colnames(x)) {
    if (class(x$referenced_tweets) == "data.frame") {
      x <- dplyr::mutate(x, referenced_tweets = list(.data$referenced_tweets))
    }
    x <- x %>%
      dplyr::rename(tweet_id = .data$id) %>%
      tidyr::unnest(cols = c("referenced_tweets"), keep_empty = TRUE) %>%
      dplyr::rename(ref_tweet_id = .data$id, ref_tweet_type = .data$type)
  } else {
    if (!is.null(x)) {
      x <- x %>% dplyr::rename(tweet_id = .data$id)
    }
  }
  x
}
