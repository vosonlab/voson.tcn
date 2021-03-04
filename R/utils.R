# get package version
get_version <- function() {
  if ("rtconvo" %in% loadedNamespaces()) {
    return(utils::packageVersion("rtconvo"))
  }
  "_"
}

# save and set some httr related options
save_set_opts <- function() {
  opts <- list()
  opts$encoding <- getOption("encoding")
  opts$HTTPUserAgent <- getOption("HTTPUserAgent")
  options(encoding = "UTF-8")
  options(HTTPUserAgent = paste0("vosonTCN v", get_version(), " (R package)"))
  opts
}

# restore saved httr related options
restore_opts <- function(opts) {
  options(encoding = opts$encoding)
  options(HTTPUserAgent = opts$HTTPUserAgent)
}
