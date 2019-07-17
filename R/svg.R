
#' Create animated SVG from an ascii cast
#'
#' @param cast `asciicast` object.
#' @param path Path to SVG file to create.
#' @param window Render with window decorations.
#' @param from Lower range of timeline to render in ms.
#' @param to Upper range of timeline to render in ms.
#' @param at Timestamp of frame to render in ms.
#' @param cursor Enable cursor rendering.
#' @param height Height in lines.
#' @param width Width in columns.
#' @param padding Distance between text and image bounds.
#' @param padding_x Distance between text and image bounds on x axis.
#' @param padding_y Distance between text and image bounds on y axis.
#'
#' @export
#' @importFrom V8 v8 JS

write_svg <- function(cast, path, window = FALSE, from = NULL, to = NULL,
                      at = NULL, cursor = TRUE, height = NULL, width = NULL,
                      padding = NULL, padding_x = NULL, padding_y = NULL) {

  ct <- v8(c("global", "window", "document"))
  ct$assign("setTimeout", JS("function(callback, after) { callback(); }"))
  ct$assign("clearTimeout", JS("function(timer) { }"))
  jsfile <- gzfile(system.file("svg-term.js.gz", package = "asciicast"))
  on.exit(close(jsfile), add = TRUE)
  ct$source(jsfile)

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  write_json(cast, tmp)

  json <- readChar(tmp, nchars = file.size(tmp))

  options <- not_null(list(
    window = window, from = from, to = to, at = at, cursor = cursor,
    paddingx = paddingx %||% padding, paddingy = paddingy %||% padding,
    height = height, width = width))

  svg <- ct$call("svgterm.render", json, options)
  cat(svg, file = path)

  invisible()
}
