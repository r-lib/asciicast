
#' Import an asciicast from an asciinema file
#'
#' @param json JSON asciinema file, version 2:
#'   <https://github.com/asciinema/asciinema/blob/master/doc/asciicast-v2.md>.
#' @return `asciicast` object.
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @family asciicast functions

read_cast <- function(json) {
  lines <- readLines(json)
  config <- rethrow(fromJSON(lines[1]), new_parse_error(json))
  if (!identical(as.integer(config$version), 2L)) {
    throw(new_parse_error(json))
  }

  nrec <- length(lines) - 1L
  output <- tibble::tibble(
    time = double(nrec),
    type = character(nrec),
    data = character(nrec))

  for (i in seq_along(lines)[-1]) {
    l <- rethrow(
      fromJSON(lines[i], simplifyVector = FALSE),
      new_parse_error(json, line = i))
    if (!is.numeric(l[[1]]) || !is.character(l[[2]]) || !is.character(l[[3]])) {
      throw(new_parse_error(json, line = i))
    }
    output[i, ] <- l
  }

  new_cast(config, output)
}

new_parse_error <- function(file, line = 1L) {
  msg <- paste0(
    "Parse error in ", file, ":", line, ".",
    if (line == 1L) " Only version 2 asciinema files are supported")
  cnd <- new_error(msg)
  class(cnd) <- c("asciinema_parse_error", class(cnd))
  cnd$file <- file
  cnd$line <- line
  cnd
}
