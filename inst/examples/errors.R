#' End_wait: 20
# Demonstrate that errors are handled well

library("not-this-really")

traceback()

callr::r(function() library("another-failure"))

.Last.error
