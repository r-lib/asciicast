#' End_wait: 20
# Demonstrate that errors are handled well

# Base R error
library("not-this-really")
traceback()

# callr errors are saved to `.Last.error`, including a stack trace
callr::r(function() library("another-failure"))
.Last.error
