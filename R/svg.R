
#' Create animated SVG from an asciicast
#'
#' @param cast `asciicast` object.
#' @param path Path to the SVG file to create.
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
#' @param theme A named list to override the default theme
#'   (see [default_theme()]).
#'
#' @export
#' @family SVG functions
#' @importFrom V8 v8 JS
#' @examplesIf !asciicast:::is_rcmd_check()
#' cast <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
#' svg_file <- tempfile(fileext = ".svg")
#' write_svg(cast, svg_file)
#' \dontshow{unlink(svg_file, recursive = TRUE)}

write_svg <- function(cast, path, window = NULL, start_at = NULL, end_at = NULL,
                      at = NULL, cursor = NULL, rows = NULL, cols = NULL,
                      padding = NULL, padding_x = NULL, padding_y = NULL,
                      omit_last_line = NULL, theme = NULL) {

  ct <- load_svg_term()

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

  theme <- rename_theme(interpret_theme(theme))

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  write_json(cast, tmp)
  json <- readChar(tmp, nchars = file.size(tmp))

  options <- not_null(list(
    window = window, from = start_at, to = end_at, at = at, cursor = cursor,
    paddingX = padding_x, paddingY = padding_y, height = rows, width = cols,
    theme = theme))

  svg <- ct$call("svgterm.render", json, options)
  svg <- gsub(
    "<text",
    paste0("<text font-size=\"", theme$fontSize, "\""),
    svg,
    fixed = TRUE
  )
  cat(svg, file = path)

  invisible()
}

is_svg_supported <- function() {
  nodever <- V8::engine_info()$version
  major <- as.numeric(strsplit(nodever, ".", fixed = TRUE)[[1]][[1]])
  !is.na(major) && major >= 6
}

check_svg_support <- function() {
  if (! is_svg_supported()) {
    stop("Writing SVG files needs a more recent Node library, see the ",
         "documentation of the V8 package, e.g. ",
         "https://github.com/jeroen/v8#readme")
  }
}

rename_theme <- function(theme) {
  recode <- c(
    "black" = "0",
    "red" = "1",
    "green" = "2",
    "yellow" = "3",
    "blue" = "4",
    "magenta" = "5",
    "cyan" = "6",
    "white" = "7",
    "light_black" = "8",
    "light_red" = "9",
    "light_green" = "10",
    "light_yellow" = "11",
    "light_blue" = "12",
    "light_magenta" = "13",
    "light_cyan" = "14",
    "light_white" = "15",
    "background" = "background",
    "bold" = "bold",
    "cursor" = "cursor",
    "text" = "text",
    "font_size" = "fontSize",
    "line_height" = "lineHeight",
    "font_family" = "fontFamily"
  )
  diff <- names(theme) %in% names(recode)
  names(theme)[diff] <- recode[names(theme)[diff]]
  theme
}

#' The default asciicast theme
#'
#' Currently only used for [write_svg()]
#'
#' @return A named list.
#'
#' @family SVG functions
#' @export
#' @examplesIf !asciicast:::is_rcmd_check()
#' cast <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
#' svg_file <- tempfile(fileext = ".svg")
#' mytheme <- modifyList(default_theme(), list(cursor = c(255, 0, 0)))
#' write_svg(cast, svg_file, theme = mytheme)
#' \dontshow{unlink(svg_file, recursive = TRUE)}

default_theme <- function() {
  list(
    "black"         = c(40, 45, 53),
    "red"           = c(232, 131, 136),
    "green"         = c(168, 204, 140),
    "yellow"        = c(219, 171, 121),
    "blue"          = c(113, 190, 242),
    "magenta"       = c(210, 144, 228),
    "cyan"          = c(102, 194, 205),
    "white"         = c(185, 191, 202),
    "light_black"   = c(111, 119, 131),
    "light_red"     = c(232, 131, 136),
    "light_green"   = c(168, 204, 140),
    "light_yellow"  = c(219, 171, 121),
    "light_blue"    = c(115, 190, 243),
    "light_magenta" = c(210, 144, 227),
    "light_cyan"    = c(102, 194, 205),
    "light_white"   = c(255, 255, 255),
    "background"    = c(40, 45, 53),
    "bold"          = c(185, 192, 203),
    "cursor"        = c(111, 118, 131),
    "text"          = c(185, 192, 203),
    "font_size"     = 1.67,
    "line_height"   = 1.3,
    "font_family"   = paste(sep = ",",
       "'Fira Code'", "Monaco", "Consolas", "Menlo",
       "'Bitstream Vera Sans Mono'", "'Powerline Symbols'", "monospace")
  )
}

themes <- list(
  asciinema = list(
    black         = c(grDevices::col2rgb("#000000")),
    red           = c(grDevices::col2rgb("#dd3c69")),
    green         = c(grDevices::col2rgb("#4ebf22")),
    yellow        = c(grDevices::col2rgb("#ddaf3c")),
    blue          = c(grDevices::col2rgb("#26b0d7")),
    magenta       = c(grDevices::col2rgb("#b954e1")),
    cyan          = c(grDevices::col2rgb("#54e1b9")),
    white         = c(grDevices::col2rgb("#d9d9d9")),
    light_black   = c(grDevices::col2rgb("#4d4d4d")),
    light_red     = c(grDevices::col2rgb("#dd3c69")),
    light_green   = c(grDevices::col2rgb("#4ebf22")),
    light_yellow  = c(grDevices::col2rgb("#ddaf3c")),
    light_blue    = c(grDevices::col2rgb("#26b0d7")),
    light_magenta = c(grDevices::col2rgb("#b954e1")),
    light_cyan    = c(grDevices::col2rgb("#54e1b9")),
    light_white   = c(grDevices::col2rgb("#ffffff")),
    background    = c(grDevices::col2rgb("#cccccc")),
    cursor        = c(grDevices::col2rgb("#121314")),
    bold          = c(grDevices::col2rgb("#121314")),
    text          = c(grDevices::col2rgb("#121314"))
  ),

  tango = list(
    black         = c(grDevices::col2rgb("#000000")),
    red           = c(grDevices::col2rgb("#cc0000")),
    green         = c(grDevices::col2rgb("#4e9a06")),
    yellow        = c(grDevices::col2rgb("#c4a000")),
    blue          = c(grDevices::col2rgb("#3465a4")),
    magenta       = c(grDevices::col2rgb("#75507b")),
    cyan          = c(grDevices::col2rgb("#06989a")),
    white         = c(grDevices::col2rgb("#d3d7cf")),
    light_black   = c(grDevices::col2rgb("#555753")),
    light_red     = c(grDevices::col2rgb("#ef2929")),
    light_green   = c(grDevices::col2rgb("#8ae234")),
    light_yellow  = c(grDevices::col2rgb("#fce94f")),
    light_blue    = c(grDevices::col2rgb("#729fcf")),
    light_magenta = c(grDevices::col2rgb("#ad7fa8")),
    light_cyan    = c(grDevices::col2rgb("#34e2e2")),
    light_white   = c(grDevices::col2rgb("#eeeeec")),
    background    = c(grDevices::col2rgb("#121314")),
    cursor        = c(grDevices::col2rgb("#cccccc")),
    bold          = c(grDevices::col2rgb("#cccccc")),
    text          = c(grDevices::col2rgb("#cccccc"))
  ),

  "solarized-dark" = list(
    black         = c(grDevices::col2rgb("#073642")),
    red           = c(grDevices::col2rgb("#dc322f")),
    green         = c(grDevices::col2rgb("#859900")),
    yellow        = c(grDevices::col2rgb("#b58900")),
    blue          = c(grDevices::col2rgb("#268bd2")),
    magenta       = c(grDevices::col2rgb("#d33682")),
    cyan          = c(grDevices::col2rgb("#2aa198")),
    white         = c(grDevices::col2rgb("#eee8d5")),
    light_black   = c(grDevices::col2rgb("#002b36")),
    light_red     = c(grDevices::col2rgb("#cb4b16")),
    light_green   = c(grDevices::col2rgb("#586e75")),
    light_yellow  = c(grDevices::col2rgb("#657b83")),
    light_blue    = c(grDevices::col2rgb("#839496")),
    light_magenta = c(grDevices::col2rgb("#6c71c4")),
    light_cyan    = c(grDevices::col2rgb("#93a1a1")),
    light_white   = c(grDevices::col2rgb("#fdf6e3")),
    background    = c(grDevices::col2rgb("#002b36")),
    cursor        = c(grDevices::col2rgb("#839496")),
    bold          = c(grDevices::col2rgb("#839496")),
    text          = c(grDevices::col2rgb("#839496"))
  ),

  "solarized-light" = list(
    black         = c(grDevices::col2rgb("#073642")),
    red           = c(grDevices::col2rgb("#dc322f")),
    green         = c(grDevices::col2rgb("#859900")),
    yellow        = c(grDevices::col2rgb("#b58900")),
    blue          = c(grDevices::col2rgb("#268bd2")),
    magenta       = c(grDevices::col2rgb("#d33682")),
    cyan          = c(grDevices::col2rgb("#2aa198")),
    white         = c(grDevices::col2rgb("#eee8d5")),
    light_black   = c(grDevices::col2rgb("#002b36")),
    light_red     = c(grDevices::col2rgb("#cb4b16")),
    light_green   = c(grDevices::col2rgb("#586e75")),
    light_yellow  = c(grDevices::col2rgb("#657c83")),
    light_blue    = c(grDevices::col2rgb("#839496")),
    light_magenta = c(grDevices::col2rgb("#6c71c4")),
    light_cyan    = c(grDevices::col2rgb("#93a1a1")),
    light_white   = c(grDevices::col2rgb("#fdf6e3")),
    background    = c(grDevices::col2rgb("#fdf6e3")),
    cursor        = c(grDevices::col2rgb("#657b83")),
    bold          = c(grDevices::col2rgb("#657b83")),
    text          = c(grDevices::col2rgb("#657b83"))
  ),

  seti = list(
    black         = c(grDevices::col2rgb("#323232")),
    red           = c(grDevices::col2rgb("#c22832")),
    green         = c(grDevices::col2rgb("#8ec43d")),
    yellow        = c(grDevices::col2rgb("#e0c64f")),
    blue          = c(grDevices::col2rgb("#43a5d5")),
    magenta       = c(grDevices::col2rgb("#8b57b5")),
    cyan          = c(grDevices::col2rgb("#8ec43d")),
    white         = c(grDevices::col2rgb("#eeeeee")),
    light_black   = c(grDevices::col2rgb("#323232")),
    light_red     = c(grDevices::col2rgb("#c22832")),
    light_green   = c(grDevices::col2rgb("#8ec43d")),
    light_yellow  = c(grDevices::col2rgb("#e0c64f")),
    light_blue    = c(grDevices::col2rgb("#43a5d5")),
    light_magenta = c(grDevices::col2rgb("#8b57b5")),
    light_cyan    = c(grDevices::col2rgb("#8ec43d")),
    light_white   = c(grDevices::col2rgb("#ffffff")),
    background    = c(grDevices::col2rgb("#111213")),
    cursor        = c(grDevices::col2rgb("#cacecd")),
    bold          = c(grDevices::col2rgb("#cacecd")),
    text          = c(grDevices::col2rgb("#cacecd"))
  ),

  monokai = list(
    black         = c(grDevices::col2rgb("#272822")),
    red           = c(grDevices::col2rgb("#f92672")),
    green         = c(grDevices::col2rgb("#a6e22e")),
    yellow        = c(grDevices::col2rgb("#f4bf75")),
    blue          = c(grDevices::col2rgb("#66d9ef")),
    magenta       = c(grDevices::col2rgb("#ae81ff")),
    cyan          = c(grDevices::col2rgb("#a1efe4")),
    white         = c(grDevices::col2rgb("#f8f8f2")),
    light_black   = c(grDevices::col2rgb("#75715e")),
    light_red     = c(grDevices::col2rgb("#f92672")),
    light_green   = c(grDevices::col2rgb("#a6e22e")),
    light_yellow  = c(grDevices::col2rgb("#f4bf75")),
    light_blue    = c(grDevices::col2rgb("#66d9ef")),
    light_magenta = c(grDevices::col2rgb("#ae81ff")),
    light_cyan    = c(grDevices::col2rgb("#a1efe4")),
    light_white   = c(grDevices::col2rgb("#f9f8f5")),
    background    = c(grDevices::col2rgb("#272822")),
    cursor        = c(grDevices::col2rgb("#f8f8f2")),
    bold          = c(grDevices::col2rgb("#f8f8f2")),
    text          = c(grDevices::col2rgb("#f8f8f2"))
  ),

  "github-light" = list(
    black         = c(grDevices::col2rgb("#073642")),
    red           = c(grDevices::col2rgb("#dc322f")),
    green         = c(grDevices::col2rgb("#859900")),
    yellow        = c(grDevices::col2rgb("#b58900")),
    blue          = c(grDevices::col2rgb("#268bd2")),
    magenta       = c(grDevices::col2rgb("#d33682")),
    cyan          = c(grDevices::col2rgb("#2aa198")),
    white         = c(grDevices::col2rgb("#eee8d5")),
    light_black   = c(grDevices::col2rgb("#002b36")),
    light_red     = c(grDevices::col2rgb("#cb4b16")),
    light_green   = c(grDevices::col2rgb("#586e75")),
    light_yellow  = c(grDevices::col2rgb("#657c83")),
    light_blue    = c(grDevices::col2rgb("#839496")),
    light_magenta = c(grDevices::col2rgb("#6c71c4")),
    light_cyan    = c(grDevices::col2rgb("#93a1a1")),
    light_white   = c(grDevices::col2rgb("#fdf6e3")),
    background    = c(grDevices::col2rgb("#f7f7f7")),
    cursor        = c(grDevices::col2rgb("#657b83")),
    bold          = c(grDevices::col2rgb("#657b83")),
    text          = c(grDevices::col2rgb("#657b83"))
  ),

  "pkgdown" = list(
    black         = c(grDevices::col2rgb("#073642")),
    red           = c(grDevices::col2rgb("#dc322f")),
    green         = c(grDevices::col2rgb("#859900")),
    yellow        = c(grDevices::col2rgb("#b58900")),
    blue          = c(grDevices::col2rgb("#268bd2")),
    magenta       = c(grDevices::col2rgb("#d33682")),
    cyan          = c(grDevices::col2rgb("#2aa198")),
    white         = c(grDevices::col2rgb("#eee8d5")),
    light_black   = c(grDevices::col2rgb("#002b36")),
    light_red     = c(grDevices::col2rgb("#cb4b16")),
    light_green   = c(grDevices::col2rgb("#586e75")),
    light_yellow  = c(grDevices::col2rgb("#657c83")),
    light_blue    = c(grDevices::col2rgb("#839496")),
    light_magenta = c(grDevices::col2rgb("#6c71c4")),
    light_cyan    = c(grDevices::col2rgb("#93a1a1")),
    light_white   = c(grDevices::col2rgb("#fdf6e3")),
    background    = c(grDevices::col2rgb("#f1f3f5")),
    cursor        = c(grDevices::col2rgb("#172431")),
    bold          = c(grDevices::col2rgb("#172431")),
    text          = c(grDevices::col2rgb("#172431"))
  )

)

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
#' @family SVG functions
#' @examplesIf interactive()
#' cast <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
#' play(cast)

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
