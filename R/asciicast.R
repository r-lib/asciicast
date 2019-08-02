
#' Record an asciinema screencast
#'
#' @param script Path of an R script to record.
#' @param typing_speed Average typing speed, per keypress, in seconds.
#' @param empty_wait How long to wait for empty lines in the script file,
#'   in seconds.
#' @param cols Width of the terminal, in number of characters.
#' @param rows Height of the terminal, in number of characters.
#' @param title Title of the cast, this is included in the cast JSON file.
#' @param timestamp Time stamp of the recording, defaults to `Sys.time()`,
#'   this is included in the cast JSON file.
#' @param env Environment variables to include in the case JSON file.
#'   Defaults to `list(TERM = "xterm-256color", SHELL = "/bin/zsh")`.
#' @param idle_time_limit Time limit for the cast not printing anything,
#'   in seconds. By default there is no limit.
#' @param allow_errors Whether to cast errors properly. If this is set to
#'   `TRUE`, then asciicast overwrites the `"error"` option. Only change
#'   this if you know what you are doing.
#' @param timeout Idle timeout, in seconds If the R subprocess running
#'   the recording does not answer within this limit, it is killed and the
#'   recording stops. Update this for slow running code, that produces no
#'   output as it runs.
#' @param start_wait Delay at the beginning, in seconds.
#' @param end_wait Delay at the very end, in seconds.
#' @param record_env Environment variables to set for the R subprocess.
#' @param startup Quoted language object to run in the subprocess before
#'   starting the recording.
#' @param process A processx subprocess to run the cast in. By default a
#'   new subprocess is started. You can reuse a process by calling
#'   [asciicast_start_process()] first, and supplying the returned process
#'   here.
#'
#' @return An `asciicast` object, write this to
#'   file with [write_json()].
#'
#' @export
#' @family asciicast functions
#' @examples
#' script <- system.file("examples", "hello.R", package = "asciicast")
#' cast <- record(script)
#' cast

record <- function(script, typing_speed = NULL, empty_wait = NULL,
                   cols = NULL, rows = NULL, title = NULL, timestamp = NULL,
                   env = NULL, idle_time_limit = NULL, allow_errors = TRUE,
                   timeout = NULL, start_wait = NULL, end_wait = NULL,
                   record_env = NULL, startup = NULL, process = NULL) {

  lines <- readLines(script)
  parsed <- parse_header(lines)
  header <- parsed$header
  body <- parsed$body

  typing_speed <- as.numeric(get_param("typing_speed", 0.05, header))
  empty_wait <- as.numeric(get_param("empty_wait", 1L, header))
  start_wait <- as.numeric(get_param("start_wait", 2L, header))
  end_wait <- as.numeric(get_param("end_wait", 5L, header))
  timeout <- as.numeric(get_param("timeout", 10, header))
  if (is.null(record_env) && !is.null(header$record_env)) {
    record_env <- eval(parse(text = header$record_env))
  }

  ## Default values for attributes
  cols <- as.integer(get_param("cols", 80L, header))
  rows <- as.integer(get_param("rows", 24L, header))
  config <- not_null(list(
    version = 2L,
    command = "R -q",
    cols = cols,
    rows = rows,
    width = cols,
    height = rows,
    title = get_param("title", config = header),
    timestamp = as.integer(get_param("timestamp", Sys.time(), header)),
    env = env %||%
      (if (!is.null(header$env)) eval(parse(text = header$env))) %||%
      list(TERM = "xterm-256color", SHELL = "/bin/zsh"),
    idle_time_limit = get_param("idle_time_limit", config = header)
  ))

  header[names(config)] <- config

  startup <- if (!is.null(header$startup)) str2lang(header$startup)

  output <- record_commands(body, typing_speed, timeout, empty_wait,
                            allow_errors, start_wait, end_wait, record_env,
                            startup, process)

  new_cast(header, output)
}

new_cast <- function(config, output) {
  structure(
    list(config = config, output = output),
    class = "asciicast")
}

#' @export

print.asciicast <- function(x, ...) {
  cat("<asciicast>\n")
  cat("<config>\n")
  config <- jsonlite::toJSON(x$config, pretty = TRUE, auto_unbox = TRUE)
  config <- strsplit(config, "\n", fixed = TRUE)[[1]]
  config <- utils::tail(utils::head(config, -1), -1)
  cat(config, sep = "\n")

  cat("\n<frames>\n")
  print(x$output)

  invisible(x)
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

write_json <- function(cast, path) {
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
  if (length(lines) == 0) return(list())
  dcf <- as.list(read.dcf(textConnection(lines))[1, ])
  names(dcf) <- tolower(names(dcf))
  dcf
}
