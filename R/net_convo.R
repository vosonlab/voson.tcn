#' Generate network from conversation tweets
#'
#' @param df_convo Dataframe. Conversation tweets collected by get_convo function.
#' @param type Character string. Type of network to generate. Default is activity.
#'
#' @return Named list of dataframes for network nodes and edges.
#' @export
#'
#' @examples
#' \dontrun{
#' # generate twitter conversation network
#' network <- net_convo(tweets, "actor")
#'
#' # network nodes and edges
#' network$nodes
#' network$edges
#' }
#'
net_convo <- function(df_convo = NULL, type = "activity") {
  if (is.null(df_convo) || !is.data.frame(df_convo)) {
    stop("invalid input dataframe.")
  }

  if (nrow(df_convo) < 1) {
    stop("input dataframe is empty.")
  }

  if (!type %in% c("activity", "actor")) {
    stop("network type not supported.")
  }

  # unnest referenced tweets
  df_convo %<>% dplyr::rename(tweet_id = .data$id)
  df_convo %<>% tidyr::unnest(cols = c("referenced_tweets"),
                              keep_empty = TRUE) %>%
    dplyr::rename(
      ref_tweet_id = .data$id,
      ref_tweet_type = .data$type
    )

  if (type == "activity") {
    # network edges
    edges <-
      df_convo %>% dplyr::select(
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
      dplyr::left_join(df_convo, by = "tweet_id") %>%
      dplyr::distinct(.data$tweet_id, .keep_all = TRUE) %>%
      dplyr::select(.data$tweet_id,
                    user_id = .data$author_id,
                    .data$source,
                    .data$created_at,
                    .data$text)

  } else if (type == "actor") {
    # does not include quoted edges as quoted tweet user ids are not in the data,
    # so just has conversation replies at this stage
    edges <- df_convo %>%
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
      dplyr::left_join(df_convo, by = c("user_id" = "author_id")) %>%
      dplyr::distinct(.data$user_id, .keep_all = TRUE) %>%
      dplyr::select(.data$user_id, .data$source)
  }

  list(nodes = nodes, edges = edges)
}
