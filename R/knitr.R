
#' Initialize the asciicast knitr engine
#'
#' Call this function in your Rmd file, to enable creating asciinema
#' casts from code chunks.
#'
#' @export

init_knitr_engine <- function() {
  knitr::knit_engines$set("asciicast" = eng_asciicast)
  knitr::cache_engines$set("asciicast" = cache_eng_asciicast)
  deps <- htmlwidgets::getDependency("asciinema", "asciicast")
  knitr::knit_meta_add(deps)
}

eng_asciicast <- function(options) {
  options$echo <- FALSE

  if (!is.null(options$file)) {
    cast_file <- options$file
    if (!is.null(options$code)) {
      warning("asciicast file and code both given, code will be ignored")
    }
    options$code <- ""

  } else {
    cast_file <- tempfile()
    on.exit(unlink(cast_file), add = TRUE)
    writeLines(options$code %||% "", cast_file, useBytes = TRUE)
  }

  cast <- record(cast_file)

  if (options$cache > 0) cache_asciicast(cast, options$hash)

  knitr::knit_print(asciinema(cast), options = options)
}

cache_eng_asciicast <- function(options) {
  options$echo <- FALSE
  cast <- readRDS(paste0(options$hash, ".cast"))
  knitr::knit_print(asciinema(cast), options = options)
}

cache_asciicast <- function(cast, path) {
  saveRDS(cast, paste0(path, ".cast"))
}
