as_json <- function(cast) {
  if (!inherits(cast, "asciicast")) {
    throw(cli::format_error(c(
      "{.var cast} must be an {.cls asciicast}, object.",
      i = "It is a {.type {cast}}."
    )))
  }
  con <- textConnection(NULL, "w", local = TRUE)
  on.exit(close(con), add = TRUE)
  write_json_con(cast, con)
  textConnectionValue(con)
}

write_json_con <- function(cast, con) {
  # Header
  cat(jsonlite::toJSON(cast$config, auto_unbox = TRUE), file = con)
  cat("\n", file = con)

  # data
  cat(
    sep = "", file = con,
    paste0(
      "[", cast$output$time, ", ",
      encode_str(cast$output$type), ", ",
      encode_str(cast$output$data), "]", "\n"
    )
  )
}

#' Write an ascii cast to file
#'
#' The file uses the asciinema file format, version 2:
#' <https://github.com/asciinema/asciinema/blob/master/doc/asciicast-v2.md>.
#'
#' @param cast `asciicast` object.
#' @param path Path to write to.
#'
#' @export
#' @family asciicast functions
#' @examplesIf !asciicast:::is_rcmd_check()
#' script <- system.file("examples", "hello.R", package = "asciicast")
#' cast <- record(script)
#' json <- tempfile(fileext = ".json")
#' write_json(cast, json)
#' \dontshow{
#' unlink(json, recursive = TRUE)
#' }
write_json <- function(cast, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  write_json_con(cast, con)
  invisible()
}
