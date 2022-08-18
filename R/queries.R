# tweet fields to request
query_tweet_fields <- function() {
  tweet_fields <- c(
    "attachments",
    "author_id",
    "context_annotations",
    "conversation_id",
    "created_at",
    "entities",
    "geo",
    "in_reply_to_user_id",
    "lang",
    "possibly_sensitive",
    "public_metrics",
    "reply_settings",
    "source",
    "withheld",
    "referenced_tweets"
  )

  tweet_fields <- paste0(tweet_fields, collapse = ",")
  tweet_fields <- paste0("tweet.fields=", tweet_fields)

  tweet_fields
}

# tweet expansions to request
# expansions includes tweets, users
query_expansions <- function(ref_tweets = TRUE) {
  expansions <- c(
    "author_id",
    "in_reply_to_user_id" # may be redundant
  )

  if (ref_tweets) {
    expansions <- c(expansions, "referenced_tweets.id", "referenced_tweets.id.author_id")
  }

  expansions <- paste0(expansions, collapse = ",")
  expansions <- paste0("expansions=", expansions)

  expansions
}

# tweet user fields to request
query_user_fields <- function() {
  user_fields <- paste0(
    c(
      "created_at",
      "description",
      "entities",
      "location",
      "pinned_tweet_id",
      "profile_image_url",
      "protected",
      "public_metrics",
      "url",
      "verified",
      "withheld"
    ),
    collapse = ","
  )
  user_fields <- paste0("user.fields=", user_fields)

  user_fields
}

# tweets query url
tweets_url <- function(ids, ref_tweets = FALSE) {
  paste0(
    "https://api.twitter.com/2/tweets",
    "?ids=",
    paste0(ids, collapse = ","),
    "&",
    query_tweet_fields(),
    "&",
    query_expansions(ref_tweets = ref_tweets),
    "&",
    query_user_fields()
  )
}

# search query url
search_url <- function(endpoint, convo_id, start_time, end_time, max_results) {
  q <- paste0(paste0("conversation_id:", convo_id), collapse = "%20OR%20")

  paste0(
    "https://api.twitter.com/2/tweets/search/",
    endpoint,
    # "?query=conversation_id:",
    # convo_id,
    "?query=",
    q,
    "&",
    query_tweet_fields(),
    "&",
    query_expansions(),
    "&",
    query_user_fields(),
    ifelse(
      !is.null(start_time),
      paste0("&start_time=", start_time),
      ""
    ),
    ifelse(!is.null(end_time),
           paste0("&end_time=", end_time),
           ""),
    ifelse(
      !is.null(max_results),
      paste0("&max_results=", max_results),
      ""
    )
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
