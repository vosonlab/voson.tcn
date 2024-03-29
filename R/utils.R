# get package version
get_version <- function() {
  if ("voson.tcn" %in% loadedNamespaces()) {
    return(utils::packageVersion("voson.tcn"))
  }
  "_"
}

# save and set some httr related options
save_set_opts <- function() {
  opts <- list()
  opts$encoding <- getOption("encoding")
  opts$HTTPUserAgent <- getOption("HTTPUserAgent")
  options(encoding = "UTF-8")
  options(HTTPUserAgent = paste0("voson.tcn v", get_version(), " (R package)"))
  opts
}

# restore saved httr related options
restore_opts <- function(opts) {
  options(encoding = opts$encoding)
  options(HTTPUserAgent = opts$HTTPUserAgent)
}

# httr request header
request_header <- function(token) {
  httr::add_headers(.headers = c(Authorization = paste0("Bearer ", token),
                                 "User-Agent" = paste0("voson.tcn v", get_version(), " (R package)")))
}

# extract tweet ids from tweet urls
ids_from_urls <- function(urls) {
  na.omit(unique(sapply(urls, function(x) {
    if (!is.na(suppressWarnings(as.numeric(x)))) {
      return(as.character(x))
    }
    path <- stringr::str_split(httr::parse_url(x)$path, "/", simplify = TRUE)
    id <- path[length(path)]
    ifelse(!is.na(suppressWarnings(as.numeric(id))), as.character(id), NA)
  }, USE.NAMES = FALSE)))
}
