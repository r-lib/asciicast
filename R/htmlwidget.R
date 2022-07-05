
#' asciinema player HTML widget
#'
#' You can use this widget in Rmd files or Shiny applications, the
#' same way as [other HTML widgets](http://www.htmlwidgets.org/).
#'
#' @param cast `asciicast` object.
#' @param start_at Where to start the playback from, in seconds.
#' @param rows Number of rows, defaults to the number of rows in the
#'   recording, or 24 if not specified in the cast.
#' @param cols Number of columns, defaults to the number columns in the
#'   recording, or 80 if not specified in the cast.
#' @param autoplay Whether to start playing the cast automatically.
#' @param loop Whether to loop the playback.
#' @param speed Whether to play slower or faster. 1 is normal speed.
#' @param title If specified, it overrides the title in the recording.
#' @param author Author, displayed in the titlebar in fullscreen mode.
#' @param author_url URL of the author's homepage/profile. Author name
#'   (author above) is linked to this URL.
#' @param author_img_url URL of the author's image, displayed in the
#'   titlebar in fullscreen mode.
#' @param poster_text if not `NULL`, used as the text of the poster
#'   (preview).
#' @param poster_frame Which frame to use for the preview. A number means
#'   seconds. Defaults to the last frame. This is only used if `poster_text`
#'   is `NULL`.
#' @param font_size Size of terminal font. Possible values: small, medium,
#'   big, any css `font-size` value (e.g. 15px).
#' @param theme Theme.
#' @param idle_time_limit Time limit for the cast not printing anything,
#'   in seconds. By default there is no limit.
#' @param html_height HTML height of the widget.
#' @param html_width HTML width of the widget.
#' @param element_id HTML id of the widget's element. If `NULL`, then the
#'   id is generated randomly.
#'
#' @export
#' @examplesIf interactive()
#' cast <- read_cast(system.file("examples", "hello.cast", package = "asciicast"))
#' asciinema_player(cast)

asciinema_player <- function(cast, start_at = 0, rows = NULL, cols = NULL,
                             autoplay = NULL, loop = NULL, speed = NULL,
                             title = NULL, author = NULL, author_url = NULL,
                             author_img_url = NULL, poster_text = NULL,
                             poster_frame = NULL, font_size = NULL,
                             theme = NULL, idle_time_limit = NULL,
                             html_height = NULL, html_width = NULL,
                             element_id = NULL) {

  rows <- rows %||% cast$config$rows %||% cast$config$height %||% 24
  cols <- cols %||% cast$config$cols %||% cast$config$width %||% 80
  title <- title %||% cast$config$title %||% ""
  poster_frame <- poster_frame %||% cast$config$poster_frame %||% ""
  poster_text <- poster_text %||% cast$config$poster_text %||% title
  theme <- theme %||% cast$config$theme %||% "asciinema"
  idle_time_limit <- idle_time_limit %||% cast$config$idle_time_limit

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  write_json(cast, tmp)
  src <- paste0(
    "data:application/json;base64,",
    jsonlite::base64_enc(readBin(tmp, what = "raw", n = file.size(tmp))))

  last_frame <- utils::tail(c(0, cast$output$time), 1)
  htmlwidgets::createWidget(
    name = "asciinema_player",
    list(
      src = src, cols = cols, rows = rows,
      autoplay = autoplay %||% cast$config$autoplay,
      loop = loop %||% cast$config$loop,
      start_at = start_at,
      speed = speed %||% cast$config$speed %||% 1,
      poster = poster(poster_text, poster_frame, last_frame),
      theme = theme,
      font_size = font_size %||% cast$config$font_size %||% NULL,
      title = title,
      author = author %||% "",
      author_url = author_url %||% "",
      author_img_url = author_img_url %||% ""),
    width = html_width,
    height = html_height,
    package = "asciicast",
    elementId = element_id,
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.suppress = TRUE,
      knitr.figure = FALSE,
      browser.fill = TRUE,
      browser.padding = 20,
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = "100%")
  )
}

poster <- function(poster_text = NULL, poster_frame = NULL, secs = 0 ) {
  seconds <- if (identical(poster_frame, "")) {
    secs

  } else {
    poster_frame
  }

  if (!identical(poster_text, "")) {
    sprintf("data:text/plain,%s", poster_text)

  } else {
    sprintf("npt:%f", seconds)
  }
}
