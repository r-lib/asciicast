
#' Initialize the asciicast knitr engine
#'
#' Call this function in your Rmd file, to enable creating asciinema
#' casts from code chunks.
#'
#' @param echo Whether to print the code of asciicast chunks.
#'
#' @export

init_knitr_engine <- function(echo = FALSE) {
  knitr::knit_engines$set("asciicast" = eng_asciicast)
  knitr::cache_engines$set("asciicast" = cache_eng_asciicast)
  deps <- htmlwidgets::getDependency("asciinema_player", "asciicast")
  knitr::knit_meta_add(deps)
  default_echo <- knitr::opts_chunk$get("echo")
  attr(default_echo, "asciicast") <- echo
  knitr::opts_chunk$set(echo = default_echo)
}

eng_asciicast <- function(options) {
  # If 'echo' was specified directly, then attr(options$echo, "asciicast")
  # will be NULL, and we use the directly specified value.
  # otherwise we use the asciicast default, which is FALSE
  options$echo <- attr(options$echo, "asciicast") %||% options$echo

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
  options$echo <- attr(options$echo, "asciicast") %||% options$echo
  cast <- readRDS(paste0(options$hash, ".cast"))
  eng_asciicast_print(cast, options)
}

eng_asciicast_print <- function(cast, options) {
  svg <-
    getOption("asciicast_knitr_svg", NULL) %||%
    Sys.getenv("ASCIICAST_KNITR_SVG", "")
  extra <- if (isTRUE(as.logical(svg))) {
    asciicast_knitr_svg(cast, options)
  } else {
    knitr::knit_print(asciinema_player(cast), options = options)
  }

  knitr::engine_output(options, options$code, '', extra)
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
  knitr::knit_hooks$get('plot')(filename, options)
}
