#' Create a Cast Object from Character Vector
#'
#' This function takes a character vector (typically with ANSI color codes)
#' and converts it to a cast object printing one line per second.
#'
#' @param x A character vector containing terminal output with ANSI color codes.
#'
#' @return A cast object
#' @export
#'
#' @examples
#' colored_text <- c("\033[31mRed text\033[0m", "\033[32mGreen text\033[0m")
#' cast_obj <- cast(colored_text)
cast <- function(x) {
  x <- unlist(strsplit(x, "\r?\n"))

  if (length(x) == 0) {
    x <- ""
  }

  # Create x data frame
  x_df <- tibble::tibble(time = seq_along(x), type = "o", data = paste0(x, "\r\n"))

  # Calculate dimensions
  height <- length(x)
  width <- max(cli::ansi_nchar(x), 1)

  # Bogus metadata
  command <- "cat"
  term <- "xterm-256color"
  shell <- "/bin/zsh"

  # Create configuration
  config <- list(
    version = 2L,
    command = command,
    timestamp = get_cast_time(),
    env = list(TERM = term, SHELL = shell),
    height = height,
    rows = height,
    width = width,
    cols = width
  )

  # Create cast object
  new_cast(config, x_df)
}

# For mocking
get_cast_time <- function() {
  as.integer(Sys.time())
}

#' Capture Expression Output and Create a Cast Object
#'
#' This function captures the console output from evaluating an R expression
#' and returns a cast object.
#'
#' @param expr An expression to evaluate and capture the output from
#' @param num_colors The number of colors to use in the terminal (default: 256)
#'
#' @return A cast object
#' @export
#'
#' @examples
#' cast_obj <- capture_cast(tibble::tibble(a = 1:3))
capture_cast <- function(
  expr,
  num_colors = 256
) {
  # Placing this inside capture.output() doesn't reset the option
  withr::local_options(cli.num_colors = num_colors)

  # Capture output
  output <- capture.output({
    force(expr)
  })

  # Create cast object from output
  cast(output)
}

#' Create and Save an SVG of Expression Output
#'
#' This function wraps [capture_cast()], [write_svg()] and [testthat::expect_snapshot_file()]
#' for use in snapshot tests.
#' It captures the output of an expression, creates a cast object,
#' and registers it as a snapshot in SVG format with a name derived from the expression.
#'
#' @param dir Directory to save the SVG file (default: `tempdir()`)
#' @inheritParams capture_cast
#' @inheritParams write_svg
#'
#' @return The value of `expr`, invisibly.
#'   This function is normally called for its side effects.
#' @export
#'
#' @examples
#' svg_path <- expect_snapshot_cast(print(tibble::tibble(a = 1:3)))
expect_snapshot_cast <- function(
  expr,
  ...,
  dir = tempdir(),
  num_colors = 256,
  window = FALSE,
  start_at = NULL,
  end_at = NULL,
  at = "end",
  cursor = FALSE,
  rows = NULL,
  cols = NULL,
  padding = NULL,
  padding_x = NULL,
  padding_y = NULL,
  omit_last_line = TRUE,
  theme = NULL,
  show = FALSE,
  name = NULL,
  cran = FALSE,
  compare = NULL,
  transform = NULL,
  variant = NULL
) {
  if (!isTRUE(omit_last_line)) {
    cli::cli_abort("Setting {.arg omit_last_line} to a value other than {.val TRUE} is not supported.")
  }

  # Create a hash from the expression to use in the filename
  expr_text <- paste(deparse(substitute(expr)), collapse = "")
  hash <- rlang::hash(expr_text)
  file_name <- paste0("asciicast_", hash, ".svg")
  path <- file.path(dir, file_name)

  # Capture the output as a cast object
  cast_obj <- capture_cast(expr, num_colors = num_colors)

  # Write the cast to an SVG file with all arguments explicitly passed
  write_svg(
    cast = cast_obj,
    path = path,
    window = window,
    start_at = start_at,
    end_at = end_at,
    at = at,
    cursor = cursor,
    rows = rows,
    cols = cols,
    padding = padding,
    padding_x = padding_x,
    padding_y = padding_y,
    omit_last_line = omit_last_line,
    theme = theme,
    show = show
  )

  on.exit(unlink(path))

  if (is.null(name)) {
    name <- file_name
  }

  testthat::expect_snapshot_file(
    path,
    name = name,
    cran = cran,
    compare = compare,
    transform = transform,
    variant = variant
  )

  invisible(expr)
}
