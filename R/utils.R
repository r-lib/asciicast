
`%||%` <- function(l, r) if (is.null(l)) r else l

not_null <- function(x) {
  x[! vapply(x, is.null, logical(1))]
}

is_empty_line <- function(x) {
  grepl("^\\s*$", x)
}

encode_str <- function(x) {
  vapply(x, jsonlite::toJSON, character(1), auto_unbox = TRUE)
}

str_trim <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}

mkdirp <- function(x) {
  dir.create(x, showWarnings = FALSE, recursive = TRUE)
}

get_param <- function(x, default = NULL,
                      config = parent.frame()$cast$config) {
  x <- tolower(x)
  env <- parent.frame()
  env[[x]] %||%
    config[[x]] %||%
    getOption(paste0("asciicast_", x)) %||%
    default
}

modify_list <- function(l, upd) {
  l[names(upd)] <- upd
  l
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, FUN.VALUE = character(1), ...)
}

map_dbl <- function(.x, .f, ...) {
  vapply(.x, .f, FUN.VALUE = double(1), ...)
}

messagex <- function (..., domain = NULL, appendLF = TRUE) {
  args <- list(...)
  msg  <- .makeMessage(..., domain = domain, appendLF = appendLF)
  call <- sys.call()
  cond <- simpleMessage(msg, call)

  defaultHandler <- function(c) {
    output <- if (is_interactive()) stdout() else stderr()
    cat(conditionMessage(c), file = output, sep = "")
  }

  withRestarts({
    signalCondition(cond)
    defaultHandler(cond)
  }, muffleMessage = function() NULL)
  invisible()
}

is_interactive <- function() {
  opt <- getOption("rlib_interactive")
  if (isTRUE(opt)) {
    TRUE
  } else if (identical(opt, FALSE)) {
    FALSE
  } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
    FALSE
  } else if (tolower(getOption("rstudio.notebook.executing", "false")) == "true") {
    FALSE
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    interactive()
  }
}
