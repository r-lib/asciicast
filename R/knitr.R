
#' @export

init_knitr_engine <- function() {
  knitr::knit_engines$set("asciicast" = eng_asciicast)
  deps <- htmlwidgets::getDependency("asciinema", "asciicast")
  knitr::knit_meta_add(deps)
}

eng_asciicast <- function(options) {
  options$echo <- FALSE
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(options$code, tmp, useBytes = TRUE)
  cast <- record(tmp)
  knitr::knit_print(asciinema(cast), options = options)
}
