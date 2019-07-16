
#' @importFrom zeallot %<-%
#' @export

record <- function(events, width = NULL, height = NULL,
                   title = NULL, timestamp = NULL, env = NULL,
                   idle_time_limit = NULL) {

  lines <- readLines(events)
  c(header, body) %<-% parse_header(lines)

  ## Default values for attributes
  config <- not_null(list(
    version = 2L,
    command = "R -q",
    width = width %||% header$width %||% 80L,
    height = height %||% header$height %||% 24L,
    title = title %||% header$title,
    timestamp = as.integer(timestamp %||% header$timestamp %||% Sys.time()),
    env = env %||%
      (if (!is.null(header$env)) eval(parse(text = header$env))) %||% 
      list(TERM = "xterm-256color", SHELL = "/bin/zsh"),
    idle_time_limit = idle_time_limit %||% header$idle_time_limit
  ))

  output <- record_commands(body)

  new_cast(config, output)
}

new_cast <- function(config, output) {
  structure(
    list(config = config, output = output),
    class = "asciicast")
}

#' @export

write_cast <- function(cast, path) {
  stopifnot(inherits(cast, "asciicast"))
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)

  # Header
  cat(jsonlite::toJSON(cast$config, auto_unbox = TRUE), file = con)
  cat("\n", file = con)

  # data
  cat(sep = "", file = con,
      paste0("[", cast$output$time, ", ",
             encode_str(cast$output$type), ", ",
             encode_str(cast$output$data), "]", "\n"))
  invisible()
}

encode_str <- function(x) {
  vapply(x, jsonlite::toJSON, character(1), auto_unbox = TRUE)
}

#' @export

play <- function(cast) {

}

#' @export

to_svg <- function(cast) {

}

parse_header <- function(lines) {
  cmdlines <- grep("^#'", lines)
  header <- parse_header_dcf(lines[cmdlines])
  body <- if (length(cmdlines)) lines[-cmdlines] else lines

  # Drop leading and trailing empty lines from body
  nonempty <- grep("^\\s*$", body, invert = TRUE)
  if (length(nonempty)) {
    first <- nonempty[1]
    last <- nonempty[length(nonempty)]
    body <- body[first:last]
  }

  list(header = header, body = body)
}

parse_header_dcf <- function(lines) {
  lines <- sub("^#'\\s*", "", lines)
  dcf <- as.list(read.dcf(textConnection(lines))[1, ])
  names(dcf) <- tolower(names(dcf))
  dcf
}
