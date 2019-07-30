
#' Initialize the asciicast knitr engine
#'
#' Call this function in your Rmd file, to enable creating asciinema
#' casts from code chunks.
#'
#' @export

init_knitr_engine <- function() {
  knitr::knit_engines$set("asciicast" = eng_asciicast)
  knitr::cache_engines$set("asciicast" = cache_eng_asciicast)
  deps <- htmlwidgets::getDependency("asciinema_player", "asciicast")
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

  eng_asciicast_print(cast, options)
}

cache_eng_asciicast <- function(options) {
  options$echo <- FALSE
  cast <- readRDS(paste0(options$hash, ".cast"))
  eng_asciicast_print(cast, options)
}

eng_asciicast_print <- function(cast, options) {
  svg <-
    getOption("asciicast_knitr_svg", NULL) %||%
    Sys.getenv("ASCIICAST_KNITR_SVG", "")
  obj <- if (isTRUE(as.logical(svg))) {
    asciicast_knitr_svg(cast, options)
  } else {
    asciinema_player(cast)
  }

  knitr::knit_print(obj, options = options)
}

cache_asciicast <- function(cast, path) {
  saveRDS(cast, paste0(path, ".cast"))
}

asciicast_knitr_svg <- function(cast, options) {
  filename <- file.path(
    knitr::opts_chunk$get("fig.path"),
    paste0(options$label, ".svg"))
  mkdirp(dirname(filename))
  write_svg(cast, filename)
  extra = knitr::knit_hooks$get('plot')(filename, options)
  knitr::engine_output(options, options$code, '', extra)
}
