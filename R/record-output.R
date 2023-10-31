#' Record output of an R script and return it as a character vector
#'
#' This function uses [record()] internally, but instead of creating
#' an ascii cast, it just returns the output of the code in a character
#' vector.
#'
#' @param script The code to record, passed to [record()].
#' @param echo Whether to include the input in the return value.
#' @param prompt Whether to include the R prompt in the return value.
#' @param stdout Whether to include the standard output in the return
#'   value.
#' @param stderr Whether to include the standard error in the return
#'   value.
#' @param ... Addiitonal arguments are passed to [record()]. (You cannot
#'   use `typing_speed` and `echo`, though, because these are used
#'   internally by `record_output()`.
#' @return Character vector of output (plus input if `echo`, plus
#'   prompt if `prompt`), as it would appear on a terminal.
#'
#' See [record()] for additional options.
#'
#' @export

record_output <- function(script, echo = FALSE, prompt = echo, stdout = TRUE,
                          stderr = TRUE, ...) {
  cast <- record(
    script,
    typing_speed = 0,
    echo = TRUE,
    ...
  )

  data <- cast$output

  keep_echo <- if (echo) {
    which(data$type == "rlib" & data$data == "type: input") + 1L
  }
  keep_prompt <- if (prompt) {
    which(data$type == "rlib" & data$data == "type: prompt") + 1L
  }
  keep_stdout <- if (stdout) {
    which(data$type == "rlib" & data$data == "type: stdout") + 1L
  }
  keep_stderr <- if (stderr) {
    which(data$type == "rlib" & data$data == "type: stderr") + 1L
  }

  keep <- sort(c(keep_echo, keep_prompt, keep_stdout, keep_stderr))

  strsplit(
    paste(data$data[keep], collapse = ""),
    "\r?\n"
  )[[1]]
}
