
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
