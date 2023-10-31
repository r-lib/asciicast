#' testthat snapshot test with asciicast
#'
#' This function is very similar to [testthat::expect_snapshot_output()],
#' but it runs the code in an asciciast subprocess, using [record_output()].
#'
#' THe `Code` part of the snapshot is always the same, but the
#' `Output` part shows the code, assuming `echo = TRUE` (the default).
#'
#' @param ... Code to run (unnamed arguments) and arguments to pass to
#'   [record_output()] (named arguments). The code is evaluated in a new
#'   asciicast subprocess. Their output is returned and used in a testthat
#'   snapshot test.
#' @param interactive Whether to use an interactive R process to evaluate
#'   the code.
#' @param echo Whether to echo the code in the subprocess before running
#'   it.
#' @param startup Expression to evaluate in the subprocess before
#'   recording the snapshot. By default it loads and attaches the calling
#'   package, including its internal functions.
#' @param transform Passed to [testthat::expect_snapshot()].
#' @param variant Passed to [testthat::expect_snapshot()].
#'
#' @export
#' @examplesIf !asciicast:::is_rcmd_check()
#' Sys.getpid()
#' testthat::local_edition(3)
#' expect_snapshot_r_process(Sys.getpid())
expect_snapshot_r_process <- function(..., interactive = TRUE, echo = TRUE,
                                      startup = NULL, transform = NULL,
                                      variant = NULL) {
  # errors.R assumes non-interactive in testthat, but we don't want that
  withr::local_envvar(TESTTHAT = NA_character_)
  dots <- eval(substitute(alist(...)))
  nms <- names(dots)
  if (all(nms == "")) {
    code_pos <- rep(TRUE, length(dots))
  } else {
    code_pos <- nms == ""
  }
  code <- unlist(lapply(dots[code_pos], deparse))
  args <- dots[!code_pos]

  if (is.null(startup)) {
    startup <- substitute(
      do.call("attach", list(asNamespace(pkg), name = paste0("package:", pkg))),
      list(pkg = environmentName(topenv(parent.frame())))
    )
  }
  rec_args <- list(
    interactive = interactive,
    echo = echo,
    startup = startup,
    record_env = c("R_CLI_HIDE_CURSOR" = "false")
  )

  record_output <- record_output
  output <- do.call(
    "record_output",
    c(list(code), args, rec_args),
    quote = TRUE
  )

  r_process <- function() writeLines(output)

  testthat::expect_snapshot(
    r_process(),
    transform = transform,
    variant = variant
  )
}
