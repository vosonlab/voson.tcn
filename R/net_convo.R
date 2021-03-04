#' Generate network from conversation tweets
#'
#' @param df_convo Dataframe. Conversation tweets collected by get_convo function.
#'
#' @return Named list of dataframes for network nodes and edges.
#' @export
#'
#' @examples
#' \dontrun{
#' # generate twitter conversation network
#' network <- net_convo(tweets)
#'
#' # network nodes and edges
#' network$nodes
#' network$edges
#' }
#'
net_convo <- function(df_convo = NULL) {
  if (is.null(df_convo) || !is.data.frame(df_convo)) {
    stop("invalid input dataframe.")
  }

  if (nrow(df_convo) < 1) {
    stop("input dataframe is empty.")
  }

  # unnest referenced tweets
  df_convo %<>% dplyr::rename(tweet_id = .data$id)
  df_convo %<>% tidyr::unnest(cols = c("referenced_tweets"),
                              keep_empty = TRUE) %>%
    dplyr::rename(
      ref_tweet_id = .data$id,
      ref_tweet_type = .data$type
    )

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

  list(nodes = nodes, edges = edges)
}
