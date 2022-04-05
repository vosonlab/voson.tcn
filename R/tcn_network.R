#' Generate network from conversation tweets
#'
#' Creates the nodes and edges for a Twitter conversation network. An "activity" type network with tweets as nodes, or
#' an "actor" type with users as nodes can be created.
#'
#' @param data Named list. Dataframes of threaded conversation tweets and users collected by get_threads function.
#' @param type Character string. Type of network to generate, either "actor" or "activity". Default is "actor".
#'
#' @return Named list of dataframes for network nodes and edges.
#' @export
#'
#' @examples
#' \dontrun{
#' # generate twitter conversation network
#' network <- tcn_network(tweets, "activity")
#'
#' # network nodes and edges
#' network$nodes
#' network$edges
#' }
#'
tcn_network <- function(data = NULL, type = "actor") {
  if (is.null(data$tweets) || !is.data.frame(data$tweets)) {
    stop("invalid input dataframe.")
  }

  if (nrow(data$tweets) < 1) {
    stop("input dataframe is empty.")
  }

  if (!type %in% c("activity", "actor")) {
    stop("network type not supported.")
  }

  # unnest referenced tweets
  unnested_refs <- data$tweets %>%
    dplyr::select(.data$tweet_id,
                  .data$author_id,
                  .data$referenced_tweets) %>%
    unnest_ref_tweets() %>%
    dplyr::distinct()

  if (type == "activity") {
    edges <- unnested_refs %>%
      dplyr::select(
        from = .data$tweet_id,
        to = .data$ref_tweet_id,
        type = .data$ref_tweet_type
      ) %>%
      dplyr::filter(!is.na(.data$to)) %>% # remove non-edges
      dplyr::mutate(
        type = dplyr::case_when(
          .data$type == "replied_to" ~ "reply",
          .data$type == "quoted" ~ "quote",
          TRUE ~ NA_character_
        )
      )

    nodes <-
      dplyr::bind_rows(
        unnested_refs %>% dplyr::select(.data$tweet_id),
        unnested_refs %>% dplyr::select(tweet_id = .data$ref_tweet_id)
      ) %>%
      dplyr::filter(!is.na(.data$tweet_id)) %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        data$tweets %>%
          dplyr::arrange(dplyr::desc(.data$timestamp)) %>%
          dplyr::distinct(.data$tweet_id, .keep_all = TRUE),
        by = "tweet_id"
      ) %>%
      dplyr::select(
        .data$tweet_id,
        .data$conversation_id,
        user_id = .data$author_id,
        .data$source,
        .data$created_at,
        .data$text,
        dplyr::starts_with("public_metrics")
      )

    if (!is.null(data$users) && nrow(data$users) > 0) {
      nodes <- nodes %>% dplyr::left_join(
        data$users %>%
          dplyr::select("user_id", "profile.name", "profile.username") %>%
          dplyr::distinct(.data$user_id, .keep_all = TRUE),
        by = c("user_id" = "user_id")
      )
    }

    nodes <- nodes %>% dplyr::relocate(.data$tweet_id)

  } else if (type == "actor") {
    # tweet_id, author_id, ref_tweet_id, ref_tweet_type
    # unnested_refs

    # list of tweet ids and their author ids
    tweet_authors <-
      data$tweets %>%
      dplyr::select(.data$tweet_id, to_user_id = .data$author_id) %>%
      dplyr::distinct()

    edges <- unnested_refs %>%
      dplyr::left_join(tweet_authors, by = c("ref_tweet_id" = "tweet_id")) %>%
      dplyr::select(
        from = .data$author_id,
        to = .data$to_user_id,
        type = .data$ref_tweet_type,
        .data$tweet_id
      ) %>%
      dplyr::left_join(
        data$tweets %>%
          dplyr::select(
            .data$tweet_id,
            .data$conversation_id,
            .data$created_at,
            .data$text
          )
        ,
        by = "tweet_id"
      ) %>%
      dplyr::mutate(
        to = dplyr::if_else(is.na(.data$to), .data$from, .data$to),
        type = dplyr::case_when(
          .data$type == "replied_to" ~ "reply",
          .data$type == "quoted" ~ "quote",
          TRUE ~ NA_character_
        )
      )

    nodes <- data$users %>%
      dplyr::relocate(.data$user_id)
  }

  list(nodes = nodes, edges = edges)
}

# referenced tweets will be either quoted or replied_to
unnest_ref_tweets <- function(df) {
  if ("referenced_tweets" %in% colnames(df)) {
    # can occur as a df if a single record, typically will be a list
    if (inherits(df$referenced_tweets, "data.frame")) {
      df <-
        dplyr::mutate(df, referenced_tweets = list(.data$referenced_tweets))
    }
    df <- df %>%
      tidyr::unnest(cols = c("referenced_tweets"),
                    keep_empty = TRUE) %>%
      dplyr::rename(ref_tweet_id = .data$id,
                    ref_tweet_type = .data$type)
  } else {
    if (!is.null(df)) {
      df <- df %>% dplyr::mutate(ref_tweet_id = NA, ref_tweet_type = NA)
    }
  }
  df
}
