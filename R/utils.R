
`%||%` <- function(l, r) if (is.null(l)) r else l

not_null <- function(x) {
  x[! vapply(x, is.null, logical(1))]
}
