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
ids_from_urls <- function(urls) {
  na.omit(unique(sapply(urls, function(x) {
    if (!is.na(suppressWarnings(as.numeric(x)))) {
      return(as.character(x))
    }
    path <-
      stringr::str_split(httr::parse_url(x)$path, "/", simplify = TRUE)
    id <- path[length(path)]
    ifelse(!is.na(suppressWarnings(as.numeric(id))), as.character(id), NA)
  }, USE.NAMES = FALSE)))
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

# ensure numbers
check_numeric <- function(i) {
  suppressWarnings({
    i <- as.numeric(i)
    if (is.na(all(i))) {
      return(FALSE)
    }
    all(i > 0)
  })
}

# check ISO 8601 format string
check_fmt_datetime <- function(dt) {
  dt_pattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"
  if (!stringr::str_detect(toupper(dt), dt_pattern)) {
    return(FALSE)
  }
  TRUE
}
