debug_levels <- c(
  fatal = 0,
  error = 1,
  warn = 2,
  info = 3,
  debug = 4,
  trace = 5
)

get_debug_level <- function() {
  lvl <- tolower(Sys.getenv("ASCIICAST_DEBUG_LEVEL", "fatal"))
  if (!lvl %in% names(debug_levels)) {
    throw(cli::format_error(c(
      "Invalid asciicast debug level in {.env ASCIICAST_DEBUG_LEVEL env
       var: {.code lvl}.",
      i = "It must be one of {.code {names(debug_levels)}}."
    )))
  }
}

debug <- function(level, ..., .envir = parent.frame()) {
  current <- get_debug_level()
  if (debug_levels[current] >= debug_levels[level]) {
    cli::cli_alert(..., .envir = .envir)
  }
}

show_line <- function(line) {
  pl <- parse_line(line)
  if (pl$type == "o") {
    cat(pl$value)
  }
}
