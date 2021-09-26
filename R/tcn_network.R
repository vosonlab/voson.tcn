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
  if (is.null(data) || !is.data.frame(data$tweets)) {
    stop("invalid input dataframe.")
  }

  if (nrow(data$tweets) < 1) {
    stop("input dataframe is empty.")
  }

  if (!type %in% c("activity", "actor")) {
    stop("network type not supported.")
  }

  if (type == "activity") {
    # network edges
    edges <-
      data$tweets %>% dplyr::select(
        from = .data$tweet_id,
        to = .data$ref_tweet_id,
        type = .data$ref_tweet_type
      ) %>%
      dplyr::filter(!is.na(.data$to)) %>% # remove non-edges
      dplyr::mutate(
        type = dplyr::case_when(
          .data$type == "replied_to" ~ "reply",
          .data$type == "quoted" ~ "quote",
          .data$type == "retweeted" ~ "retweet",
          TRUE ~ NA_character_
        )
      )

    # network nodes
    nodes <-
      dplyr::bind_rows(
        data$tweets %>% dplyr::select(.data$tweet_id),
        data$tweets %>% dplyr::select(tweet_id = .data$ref_tweet_id)
      ) %>%
      dplyr::filter(!is.na(.data$tweet_id)) %>%
      dplyr::distinct() %>%
      dplyr::left_join( # may need to arrange
        data$tweets %>% dplyr::distinct(.data$tweet_id, .keep_all = TRUE), by = "tweet_id"
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

    tweet_authors <-
      data$tweets %>%
      dplyr::select(.data$tweet_id, to_user_id = .data$author_id) %>%
      dplyr::distinct()

    edges <- data$tweets %>%
      dplyr::left_join(tweet_authors, by = c("ref_tweet_id" = "tweet_id")) %>%
      dplyr::select(
        from = .data$author_id,
        to = .data$to_user_id,
        type = .data$ref_tweet_type,
        .data$tweet_id,
        .data$conversation_id,
        .data$created_at,
        .data$text
      ) %>%
      dplyr::mutate(
        to = dplyr::if_else(is.na(.data$to), .data$from, .data$to),
        type = dplyr::case_when(
          .data$type == "replied_to" ~ "reply",
          .data$type == "quoted" ~ "quote",
          .data$type == "retweeted" ~ "retweet",
          TRUE ~ NA_character_
        )
      )

    nodes <-
      data$users %>%
      dplyr::distinct(.data$user_id, .keep_all = TRUE) %>%
      dplyr::relocate(.data$user_id)
  }

  list(nodes = nodes, edges = edges)
}
