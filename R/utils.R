`%||%` <- function(l, r) if (is.null(l)) r else l

not_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

encode_str <- function(x) {
  vapply(x, jsonlite::toJSON, character(1), auto_unbox = TRUE)
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

is_windows <- function() {
  .Platform$OS.type == "windows"
}

is_macos <- function() {
  Sys.info()[["sysname"]] == "Darwin"
}

is_linux <- function() {
  Sys.info()[["sysname"]] == "Linux"
}

dir_exists <- function(path) {
  utils::file_test("-d", path)
}

try_silently <- function(expr) {
  try(expr, silent = TRUE)
}

#' @importFrom cli cli_process_start cli_process_done

with_cli_process <- function(msg, expr, ...) {
  proc <- cli_process_start(msg, ...)
  ret <- withVisible(expr)
  cli_process_done(proc)
  if (ret$visible) ret$value else invisible(ret$value)
}

file_ext <- function(x) {
  pos <- regexpr("(\\.[[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

na_omit <- function(x) {
  x[!is.na(x)]
}

if (getRversion() < "3.6.0") {
  str2lang <- function(x) {
    parse(text = x, keep.source = FALSE)[[1]]
  }
}

is_rcmd_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}
