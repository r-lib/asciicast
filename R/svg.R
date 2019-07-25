
#' Create animated SVG from an asciicast
#'
#' @param cast `asciicast` object.
#' @param path Path to SVG file to create.
#' @param window Render with window decorations.
#' @param from Lower range of timeline to render in seconds.
#' @param to Upper range of timeline to render in seconds.
#' @param at Timestamp of frame to render in seconds.
#' @param cursor Enable cursor rendering.
#' @param height Height in lines.
#' @param width Width in columns.
#' @param padding Distance between text and image bounds.
#' @param padding_x Distance between text and image bounds on x axis.
#' @param padding_y Distance between text and image bounds on y axis.
#'
#' @export
#' @family rsciinema functions
#' @importFrom V8 v8 JS

write_svg <- function(cast, path, window = TRUE, from = NULL, to = NULL,
                      at = NULL, cursor = TRUE, height = NULL, width = NULL,
                      padding = NULL, padding_x = NULL, padding_y = NULL) {

  if (!is.null(from)) from <- from * 1000
  if (!is.null(to)) to <- to * 1000
  if (!is.null(at)) at <- at * 1000

  ct <- v8(c("global", "window", "document"))
  ct$assign("setTimeout", JS("function(callback, after) { callback(); }"))
  ct$assign("clearTimeout", JS("function(timer) { }"))
  jsfile <- gzfile(system.file("svg-term.js.gz", package = "rsciinema"))
  on.exit(close(jsfile), add = TRUE)
  ct$source(jsfile)

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  write_json(cast, tmp)

  json <- readChar(tmp, nchars = file.size(tmp))

  options <- not_null(list(
    window = window, from = from, to = to, at = at, cursor = cursor,
    paddingX = padding_x %||% padding, paddingY = padding_y %||% padding,
    height = height, width = width))

  svg <- ct$call("svgterm.render", json, options)
  cat(svg, file = path)

  invisible()
}

#' Play asciinema cast as an SVG image in the default browser
#'
#' Uses [write_svg()] to create an SVG image for a cast, in a temporary
#' file, and then previews a minimal HTML file with the SVG image,
#' in the default browser.
#'
#' @param cast `asciicast` object
#' @param ... Additional arguments are passed to [write_svg()].
#' @return The path of the temporary SVG file, invisibly.
#' 
#' @export
#' @family rsciinema functions

play <- function(cast, ...) {
  tmpsvg <- tempfile(fileext = ".svg")
  tmphtml <- tempfile(fileext = ".html")

  write_svg(cast, path = tmpsvg, ...)

  cat(sep = "", file = tmphtml,
      "<html><body><img src=\"", basename(tmpsvg), "\"></body></html>")

  utils::browseURL(tmphtml)
  invisible(tmpsvg)
}
