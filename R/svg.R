
#' Create animated SVG from an asciicast
#'
#' @param cast `asciicast` object.
#' @param path Path to SVG file to create.
#' @param window Render with window decorations.
#' @param start_at Lower range of timeline to render in seconds.
#' @param end_at Upper range of timeline to render in seconds.
#' @param at Timestamp of single frame to render, in seconds. Alternatively
#'   it can be `"end"`, to take a snapshot at the end of the cast, after
#'   all output is done.
#' @param cursor Enable cursor rendering.
#' @param rows Height in lines.
#' @param cols Width in columns.
#' @param padding Distance between text and image bounds.
#' @param padding_x Distance between text and image bounds on x axis.
#' @param padding_y Distance between text and image bounds on y axis.
#' @param omit_last_line Whether to omit the last line of the cast. This
#'   often just the prompt, and sometimes it is not worth showing.
#'
#' @export
#' @family asciicast functions
#' @importFrom V8 v8 JS

write_svg <- function(cast, path, window = NULL, start_at = NULL, end_at = NULL,
                      at = NULL, cursor = NULL, rows = NULL, cols = NULL,
                      padding = NULL, padding_x = NULL, padding_y = NULL,
                      omit_last_line = NULL) {

  ct <- v8(c("global", "window", "document"))
  ct$assign("setTimeout", JS("function(callback, after) { callback(); }"))
  ct$assign("clearTimeout", JS("function(timer) { }"))
  jsfile <- gzfile(system.file("svg-term.js.gz", package = "asciicast"))
  on.exit(close(jsfile), add = TRUE)
  ct$source(jsfile)

  window <- as.logical(get_param("window", TRUE))
  start_at <- get_param("start_at")
  end_at <- get_param("end_at")
  at <- get_param("at")
  if (identical(at, "end")) at <- utils::tail(cast$output$time, 1)
  cursor <- as.logical(get_param("cursor"))
  rows <- get_param("rows") %||% cast$config$height
  cols <- get_param("cols") %||% cast$config$width
  padding <- get_param("padding")
  padding_x <- get_param("padding_x") %||% padding
  padding_y <- get_param("padding_y") %||% padding
  if (!is.null(padding_x)) padding_x <- as.numeric(padding_x)
  if (!is.null(padding_y)) padding_y <- as.numeric(padding_y)

  omit_last_line <- as.logical(get_param("omit_last_line", FALSE))

  if (!is.null(start_at)) start_at <- start_at * 1000
  if (!is.null(end_at)) end_at <- end_at * 1000
  if (!is.null(at)) at <- at * 1000

  if (omit_last_line) cast <- remove_last_line(cast)

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  write_json(cast, tmp)
  json <- readChar(tmp, nchars = file.size(tmp))

  options <- not_null(list(
    window = window, from = start_at, to = end_at, at = at, cursor = cursor,
    paddingX = padding_x, paddingY = padding_y, height = rows, width = cols))

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
#' @family asciicast functions

play <- function(cast, ...) {
  tmpsvg <- tempfile(fileext = ".svg")
  tmphtml <- tempfile(fileext = ".html")

  write_svg(cast, path = tmpsvg, ...)

  cat(sep = "", file = tmphtml,
      "<html><body><img src=\"", basename(tmpsvg), "\"></body></html>")

  utils::browseURL(tmphtml)
  invisible(tmpsvg)
}

remove_last_line <- function(cast) {
  last <- utils::tail(which(
    grepl("\n", cast$output$data, fixed = TRUE) &
    cast$output$type == "o"), 1)
  if (!length(last)) return(cast)

  cast$output$data[last] <- sub("\r?\n[^\n]*$", "", cast$output$data[last])
  toremove <- last < seq_len(nrow(cast$output)) & cast$output$type == "o"
  cast$output$data[toremove] <- ""

  cast
}
