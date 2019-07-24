
#' @export

init_knitr_engine <- function() {
  knitr::knit_engines$set("asciicast" = eng_asciicast)
  deps <- htmlwidgets::getDependency("asciinema", "asciicast")
  knitr::knit_meta_add(deps)
}

eng_asciicast <- function(options) {
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

  options$echo <- FALSE
  cast <- record(cast_file)
  knitr::knit_print(asciinema(cast), options = options)
}
