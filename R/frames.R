
load_svg_term <- function() {
  check_svg_support()

  ct <- v8(c("global", "window", "document"))
  ct$assign("setTimeout", JS("function(callback, after) { callback(); }"))
  ct$assign("clearTimeout", JS("function(timer) { }"))
  jsfile <- gzfile(system.file("svg-term.js.gz", package = "asciicast"))
  on.exit(close(jsfile), add = TRUE)
  ct$source(jsfile)

  ct
}

#' Extract / create complete screen frames from an ascii cast
#'
#' @param cast aciicast object
#' @param height Number of rows. (`NA` for default.)
#' @param width Number of columns. (`NA` for default.)
#' @return `asciicast_frames` object.
#'
#' @keywords internal

load_frames <- function(cast, height = NA, width = NA) {

  # Start with this, because it is faster
  json <- paste(as_json(cast), collapse = "\n")

  ct <- load_svg_term()
  ldjs <- system.file("load-cast.js", package = "asciicast")
  if (ldjs == "") stop("Internal error, cannot find 'load-cast.js'")
  ct$source(ldjs)

  ct$assign("json", json)
  options <- list(fps = 15, height = height, width = width)
  structure(
    ct$call("get_cast", json, options),
    class = "aciicast_frames"
  )
}
