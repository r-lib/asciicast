
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
