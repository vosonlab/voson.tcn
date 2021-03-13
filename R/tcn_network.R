#' Generate network from conversation tweets
#'
#' @param threads_df Dataframe. Threaded conversation tweets collected by get_threads function.
#' @param type Character string. Type of network to generate. Default is "actor".
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
tcn_network <- function(threads_df = NULL, type = "actor") {
  if (is.null(threads_df) || !is.data.frame(threads_df)) {
    stop("invalid input dataframe.")
  }

  if (nrow(threads_df) < 1) {
    stop("input dataframe is empty.")
  }

  if (!type %in% c("activity", "actor")) {
    stop("network type not supported.")
  }

  # unnest referenced tweets
  threads_df %<>% dplyr::rename(tweet_id = .data$id)
  threads_df %<>% tidyr::unnest(cols = c("referenced_tweets"),
                                keep_empty = TRUE) %>%
    dplyr::rename(
      ref_tweet_id = .data$id,
      ref_tweet_type = .data$type
    )

  if (type == "activity") {
    # network edges
    edges <-
      threads_df %>% dplyr::select(
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
      dplyr::left_join(threads_df, by = "tweet_id") %>%
      dplyr::distinct(.data$tweet_id, .keep_all = TRUE) %>%
      dplyr::select(.data$tweet_id,
                    user_id = .data$author_id,
                    .data$source,
                    .data$created_at,
                    .data$text)

    # edges <- edges %>% dplyr::filter((.data$from != .data$to) & !is.na(.data$type))

  } else if (type == "actor") {
    # does not include quoted edges as quoted tweet user ids are not in the data,
    # so just has conversation replies at this stage
    edges <- threads_df %>%
      dplyr::filter(is.na(.data$ref_tweet_type) |
                      .data$ref_tweet_type != "quoted") %>%
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
        type = dplyr::if_else(is.na(.data$type), "tweet", .data$type)
      )

    nodes <-
      dplyr::bind_rows(
        edges %>% dplyr::select(user_id = .data$from),
        edges %>% dplyr::select(user_id = .data$to)
      ) %>%
      dplyr::left_join(threads_df, by = c("user_id" = "author_id")) %>%
      dplyr::distinct(.data$user_id, .keep_all = TRUE) %>%
      dplyr::select(.data$user_id, .data$source)
  }

  list(nodes = nodes, edges = edges)
}
