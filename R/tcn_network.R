#' Generate network from conversation tweets
#'
#' @param data Named list. Dataframes of threaded conversation tweets and users collected by get_threads function.
#' @param type Character string. Type of network to generate. Can be an "actor" network with users as nodes, or an
#' "activity" network with tweets as nodes. The edges are the relationships between the nodes. Default is "actor".
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
      dplyr::mutate(to = dplyr::if_else(is.na(.data$to), .data$from, .data$to))

    # network nodes
    nodes <-
      dplyr::bind_rows(
        edges %>% dplyr::select(tweet_id = .data$from),
        edges %>% dplyr::select(tweet_id = .data$to)
      ) %>%
      dplyr::left_join(data$tweets, by = "tweet_id") %>%
      dplyr::distinct(.data$tweet_id, .keep_all = TRUE) %>%
      dplyr::select(.data$tweet_id,
                    user_id = .data$author_id,
                    .data$source,
                    .data$created_at,
                    .data$text,
                    dplyr::starts_with("public_metrics"))

    # remove non-conversation self-loops
    edges <- edges %>% dplyr::filter((.data$from != .data$to) & !is.na(.data$type))

    if (!is.null(data$users) && nrow(data$users) > 0) {
      nodes <- nodes %>% dplyr::left_join(data$users %>%
                                          dplyr::select("user_id", "profile.name", "profile.username"),
                                          by = c("user_id" = "user_id"))
    }

  } else if (type == "actor") {
    edges <- data$tweets %>%
      dplyr::select(
        from = .data$author_id,
        to = .data$in_reply_to_user_id,
        type = .data$ref_tweet_type,
        .data$tweet_id,
        .data$created_at,
        .data$text
      ) %>%
      dplyr::mutate(
        to = dplyr::if_else(is.na(.data$to), .data$from, .data$to),
        type = dplyr::case_when(
          .data$type == "replied_to" ~ "reply",
          .data$type == "quoted" ~ "quote",
          is.na(.data$type) ~ "tweet",
          TRUE ~ "tweet"
        )
      )

    nodes <-
      dplyr::bind_rows(
        edges %>% dplyr::select(user_id = .data$from),
        edges %>% dplyr::select(user_id = .data$to)
      ) %>%
      dplyr::left_join(data$tweets, by = c("user_id" = "author_id")) %>%
      dplyr::distinct(.data$user_id, .keep_all = TRUE) %>%
      dplyr::select(.data$user_id, .data$source)

    if (!is.null(data$users) && nrow(data$users) > 0) {
      nodes <- nodes %>% dplyr::left_join(data$users, by = c("user_id" = "user_id"))
    }
  }

  list(nodes = nodes, edges = edges)
}
