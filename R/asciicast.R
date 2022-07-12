
#' Record an asciinema screencast
#'
#' @param script Path of an R script to record. It can also be a readable
#'   R connection or URL, as it is passed to [base::readLines()]. It can also
#'   be a language object, which is deparsed, or a character vector with
#'   the source code itself.
#' @param typing_speed Average typing speed, per keypress, in seconds.
#' @param empty_wait How long to wait for empty lines in the script file,
#'   in seconds.
#' @param cols Width of the terminal, in number of characters.
#' @param rows Height of the terminal, in number of characters. If it the
#'   string `"auto"`, then it will be determined automatically, by including
#'   all output on the screen.
#' @param title Title of the cast, this is included in the cast JSON file.
#' @param timestamp Time stamp of the recording, defaults to `Sys.time()`,
#'   this is included in the cast JSON file.
#' @param env Environment variables to include in the case JSON file.
#'   Defaults to `list(TERM = "xterm-256color", SHELL = "/bin/zsh")`.
#' @param idle_time_limit Time limit for the cast not printing anything,
#'   in seconds. By default there is no limit.
#' @param timeout Idle timeout, in seconds If the R subprocess running
#'   the recording does not answer within this limit, it is killed and the
#'   recording stops. Update this for slow running code, that produces no
#'   output as it runs.
#' @param start_wait Delay at the beginning, in seconds.
#' @param end_wait Delay at the very end, in seconds.
#' @param record_env Environment variables to set for the R subprocess.
#' @param startup Quoted language object to run in the subprocess before
#'   starting the recording.
#' @param echo Whether to echo the input to the terminal. If `FALSE`, then
#'   only the output is shown.
#' @param speed Rescale the speed of the recorded cast with this factor.
#'   The delay of the first frame is kept constant.
#' @param process A processx subprocess to run the cast in. By default a
#'   new subprocess is started. You can reuse a process by calling
#'   [asciicast_start_process()] first, and supplying the returned process
#'   here.
#' @param interactive Whether to run R in interactive mode. This argument
#'   is ignored if `process` is specified. If `process` is `NULL` then
#'   it is passed to [asciicast_start_process()].
#' @param incomplete_error Whether to error on incomplete expressions.
#'   You might need to set this to `FALSE` for R code that does keyboard
#'   input, e.g. in `browser()`. The default is `TRUE`.
#' @inheritParams asciicast_start_process
#' @return An `asciicast` object, write this to
#'   file with [write_json()].
#'
#' @export
#' @family asciicast functions
#' @examplesIf interactive()
#' script <- system.file("examples", "hello.R", package = "asciicast")
#' cast <- record(script)
#' play(cast)

record <- function(script, typing_speed = NULL, empty_wait = NULL,
                   cols = NULL, rows = NULL, title = NULL, timestamp = NULL,
                   env = NULL, idle_time_limit = NULL,
                   timeout = NULL, start_wait = NULL, end_wait = NULL,
                   record_env = NULL, startup = NULL, echo = TRUE,
                   speed = NULL, process = NULL, interactive = TRUE,
                   locales = get_locales(), options = asciicast_options(),
                   incomplete_error = NULL) {

  lines <- if (is.language(script)) {
    deparse(script)
  } else if (inherits(script, "connection")) {
    readLines(script)
  } else if (is.character(script) && length(script) == 1 &&
             file_exists_safe(script)) {
    readLines(script)
  } else {
    unlist(strsplit(as.character(script), "\n", fixed = TRUE))
  }

  parsed <- parse_header(lines)
  header <- parsed$header
  body <- parsed$body

  typing_speed <- as.numeric(get_param("typing_speed", 0.05, header))
  empty_wait <- as.numeric(get_param("empty_wait", 1L, header))
  start_wait <- as.numeric(get_param("start_wait", 0L, header))
  end_wait <- as.numeric(get_param("end_wait", 5L, header))
  timeout <- as.numeric(get_param("timeout", 10, header))
  speed <- as.numeric(get_param("speed", 1.0, header))
  incomplete_error <- as.logical(get_param("incomplete_error", TRUE, header))
  if (is.null(record_env) && !is.null(header$record_env)) {
    record_env <- eval(parse(text = header$record_env))
  }

  ## Default values for attributes
  cols <- get_param("cols", 80L, header)
  rows <- get_param("rows", 24L, header)
  config <- not_null(list(
    version = 2L,
    command = "R -q",
    title = get_param("title", config = header),
    timestamp = as.integer(get_param("timestamp", Sys.time(), header)),
    env = env %||%
      (if (!is.null(header$env)) eval(parse(text = header$env))) %||%
      list(TERM = "xterm-256color", SHELL = "/bin/zsh"),
    idle_time_limit = get_param("idle_time_limit", config = header)
  ))

  header[names(config)] <- config

  if (is.null(startup) && !is.null(header$startup)) {
    startup <- str2lang(header$startup)
  }

  output <- record_embedded(body, typing_speed, timeout, empty_wait,
                            start_wait, end_wait, record_env,
                            startup, echo, speed, process, interactive,
                            locales, options, incomplete_error)

  if (rows == "auto") {
    plain <- cli::ansi_strip(paste0(
      output$data[output$type == "o"],
      collapse = ""
    ))

    # The show/hide cursor sequences
    plain <- gsub("\033[?25h", "", plain, fixed = TRUE)
    plain <- gsub("\033[?25l", "", plain, fixed = TRUE)

    # Count the \n characters. If the last character is not \n then
    # we also want to print the last incomplete line
    chrs <- charToRaw(plain)
    rows <- sum(chrs == charToRaw("\n"))
    if (length(chrs) == 0 || chrs[length(chrs)] != charToRaw("\n")) {
      rows <- rows + 1L
    }
  }

  header$rows <- header$height <- as.integer(rows)
  header$cols <- header$width <- as.integer(cols)

  new_cast(header, output)
}

file_exists_safe <- function(x) {
  tryCatch(
    file.exists(x),
    error = function(e) FALSE
  )
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
