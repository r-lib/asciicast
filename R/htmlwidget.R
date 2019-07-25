
#' asciinema player HTML widget
#'
#' @param cast `asciicast` object.
#' @param from Where to start the playback from, in seconds.
#' @param height Number of rows, defaults to the number of rows in the
#'   recording, or 24 if not specified in the cast.
#' @param width Number of columns, defaults to the number columns in the
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
#' @param poster_frame If not `NULL`, used as the image of the poster
#'   (preview).
#' @param font_size Size of terminal font. Possible values: small, medium,
#'   big, any css `font-size` value (e.g. 15px).
#' @param theme Theme.
#' @param html_height HTML height of the widget.
#' @param html_width HTML width of the widget.
#' @param element_id HTML id of the widget's element. If `NULL`, then the
#'   id is generated randomly.
#'
#' @export

asciinema_player <- function(cast, from = 0, height = NULL, width = NULL,
                             autoplay = FALSE, loop = FALSE, speed = 1,
                             title = NULL, author = NULL, author_url = NULL,
                             author_img_url = NULL, poster_text = title,
                             poster_frame = "", font_size = "small",
                             theme = "asciinema", html_height = NULL,
                             html_width = NULL, element_id = NULL) {

  height <- height %||% as.numeric(cast$config$height) %||% 24
  width <- width %||% as.numeric(cast$config$width) %||% 80
  title <- title %||% cast$config$title %||% ""

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  write_json(cast, tmp)
  src <- paste0(
    "data:application/json;base64,",
    jsonlite::base64_enc(readBin(tmp, what = "raw", n = file.size(tmp))))

  htmlwidgets::createWidget(
    name = "asciinema_player",
    list(
      src = src, cols = width, rows = height,
      autoplay = autoplay, loop = loop,
      start_at = from,
      speed = speed,
      poster = poster(poster_text, poster_frame, from),
      theme = theme,
      font_size = font_size,
      title = title,
      author = author %||% "",
      author_url = author_url %||% "",
      author_img_url = author_img_url %||% ""),
    width = html_width,
    height = html_height,
    package = "rsciinema",
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
    as.numeric(seconds(poster_frame))
  }

  if (!identical(poster_text, "")) {
    sprintf("data:text/plain,{%s}", poster_text)

  } else {
    sprintf("npt:{%d}", seconds)
  }
}
