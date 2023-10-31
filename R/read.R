#' Import an asciicast from an asciicast JSON file
#'
#' @param json Path to JSON asciicast file, version 2:
#'   <https://github.com/asciinema/asciinema/blob/master/doc/asciicast-v2.md>.
#'   If a numeric id, then it is taken as a public <https://asciinema.org>
#'   recording id, that is downloaded. It can also be a URL of private
#'   <https://asciinema.org> link.
#' @return `asciicast` object.
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @family asciicast functions
#' @examplesIf interactive()
#' c1 <- read_cast("https://asciinema.org/a/uHQwIVpiZvu0Ioio8KYx6Uwlj.cast?dl=1")
#' play(c1)
#'
#' c2 <- read_cast(258660)
#' play(c2)
#'
#' @examplesIf interactive()
#' c3 <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
#' play(c3)
read_cast <- function(json) {
  if (is.numeric(json)) {
    anurl <- sprintf("https://asciinema.org/a/%d.cast?dl=1", json)
    con <- curl::curl(anurl)
    on.exit(close(con), add = TRUE)
    lines <- readLines(con)
  } else if (grepl("^https?://", json)) {
    con <- curl::curl(json)
    on.exit(close(con), add = TRUE)
    lines <- readLines(con)
  } else {
    lines <- readLines(json)
  }

  config <- chain_error(fromJSON(lines[1]), new_parse_error(json))
  if (!identical(as.integer(config$version), 2L)) {
    throw(new_parse_error(json))
  }

  nrec <- length(lines) - 1L
  output <- tibble::tibble(
    time = double(nrec),
    type = character(nrec),
    data = character(nrec)
  )

  for (i in seq_along(lines)[-1]) {
    l <- chain_error(
      fromJSON(lines[i], simplifyVector = FALSE),
      new_parse_error(json, line = i)
    )
    if (!is.numeric(l[[1]]) || !is.character(l[[2]]) || !is.character(l[[3]])) {
      throw(new_parse_error(json, line = i))
    }
    output[i - 1L, ] <- l
  }

  new_cast(config, output)
}

new_parse_error <- function(file, line = 1L) {
  msg <- paste0(
    "Parse error in ", file, ":", line, ".",
    if (line == 1L) " Only version 2 asciicast files are supported"
  )
  cnd <- new_error(msg)
  class(cnd) <- c("asciicast_parse_error", class(cnd))
  cnd$file <- file
  cnd$line <- line
  cnd
}
