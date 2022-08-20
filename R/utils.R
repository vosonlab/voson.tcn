# get package version
get_version <- function() {
  if ("voson.tcn" %in% loadedNamespaces()) {
    return(utils::packageVersion("voson.tcn"))
  }
  "_"
}

# build user-agent string
get_ua <- function() {
  paste0("voson.tcn v", get_version(), " (R package)")
}

# httr request header
req_auth_header <- function(token) {
  httr::add_headers(.headers = c(
    Authorization = paste0("Bearer ", token),
    "User-Agent" = get_ua()
  ))
}

# extract tweet ids from tweet urls
ids_from_urls <- function(x) {
  if (is.null(x)) return(x)

  url_regex <- "^https://twitter\\.com/[A-Za-z0-9_]+?/status/(\\d+)/{0,1}$"
  x <- stringr::str_match(tolower(as.character(x)),
    paste0(url_regex, "|", "^(\\d+)$"))

  dplyr::coalesce(x[, 2], x[, 3])
}

# api rate-limit
resp_rate_limit <- function(headers, endpoint = NULL, sleep = FALSE) {
  reset <- as.numeric(headers$`x-rate-limit-reset`)
  message(
    paste0(
      ifelse(is.null(endpoint), "", paste0(endpoint, " ")),
      "remaining: ",
      headers$`x-rate-limit-remaining`,
      "/",
      headers$`x-rate-limit-limit`,
      " reset: ",
      as.POSIXct(reset, origin = "1970-01-01"),
      " (UNIX ",
      reset,
      ")"
    )
  )

  if (sleep) {
    wait_time <- reset - as.numeric(as.POSIXct(Sys.time()))

    # should be approx 15 mins, but if wait time > 30 mins signal exit
    if (wait_time <= 0 | wait_time > (15*60)*2) {
      return(FALSE)
    }

    message(paste0("waiting ", round((wait_time/60), digits = 1), " mins for reset"))
    Sys.sleep(wait_time + 2) # add 2 sec buffer
  }

  TRUE
}

# check ISO 8601 format string
check_fmt_datetime <- function(dt) {
  dt_pattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
  if (!stringr::str_detect(toupper(dt), dt_pattern)) {
    return(FALSE)
  }
  TRUE
}

# remove trailing .000Z from twitter datetime if present
rm_dt_tail <- function(x) {
  stringr::str_replace(x, "\\.000Z$", "Z")
}

# counts results list
lst_counts_results <- function(...) {
  list(counts = NULL, errors = NULL, meta = NULL, ...)
}

# tweets results list
lst_tweets_results <- function(...) {
  list(tweets = NULL, users = NULL, errors = NULL, meta = NULL, ...)
}

# append dataframes in tweet results list
add_tweets_results <- function(x, y) {
  x$tweets <- dplyr::bind_rows(x$tweets, y$tweets)
  x$users <- dplyr::bind_rows(x$users, y$users)
  x$errors <- dplyr::bind_rows(x$errors, y$errors)
  x$meta <- dplyr::bind_rows(x$meta, y$meta)
  x
}

# reformat results data and remove duplicates
# attempts to retain most recent version if duplicated
clean_results <- function(results) {
  if (!is.null(results$tweets) && nrow(results$tweets)) {
    results$tweets <- results$tweets |>
      dplyr::rename(tweet_id = .data$id) |>
      dplyr::arrange(dplyr::desc(.data$timestamp)) |>
      dplyr::distinct(.data$tweet_id, .keep_all = TRUE)
  }

  if (!is.null(results$users) && nrow(results$users)) {
    results$users <- results$users |>
      dplyr::rename_with(~ paste0("profile.", .x)) |>
      dplyr::rename(user_id = .data$profile.id,
                    timestamp = .data$profile.timestamp) |>
      dplyr::arrange(
        dplyr::desc(.data$timestamp),
        dplyr::desc(as.integer(.data$profile.public_metrics.tweet_count)) # imperfect
      ) |>
      dplyr::distinct(.data$user_id, .keep_all = TRUE)
  }

  results
}

# build conversation id search queries based on ids and endpoint
build_chunks <- function(ids, endpoint = "recent") {
  if (length(ids) == 1) return(paste0("conversation_id:", ids))

  q_size <- 512
  if (endpoint == "all") q_size <- 1024

  chunks <- c()
  qs <- ""
  n_chr_tally <- 0
  for (x in ids) {
    q <- paste0("conversation_id:", x)
    if ((n_chr_tally + nchar(q)) > q_size) {
      qs <- sub("\\sOR\\s$", "", qs)
      qs <- gsub("\\sOR\\s", "%20OR%20", qs)
      chunks <- c(chunks, qs)
      qs <- ""
      n_chr_tally <- 0
    } else {
      qs <- paste0(qs, q, " OR ")
      n_chr_tally <- nchar(qs)
    }
  }

  chunks
}

prog_bar <- function(n_iter, desc) {
  progress::progress_bar$new(
    format = paste0("[:bar] :percent ", desc),
    total = n_iter,
    complete = "=",
    incomplete = "-",
    current = ">",
    width = 60)
}

