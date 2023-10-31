#' Export ascii screencast to animated GIF file
#'
#' @param cast `asciicast` object.
#' @param path Path to GIF file to create.
#' @param show Whether to show the GIF on the screen, in the viewer pane
#'   in RStudio, or using the image viewer in the magick package.
#'   By default it only show the image in RStudio.
#' @param cols If not `NULL`, _clip_ terminal width to this number of
#'   columns.
#' @param rows If not `NULL`, _clip_ terminal height to this number of
#'   rows.
#' @param theme Theme. Currently supported themes: asciinema, tango,
#'   solarized-dark, solarized-light, monokai. Defaults to the theme
#'   specified in the cast, or asciiname if not specified.
#' @param scale Image scale / pixel density.
#' @param speed Playback speed. Higher number means faster.
#' @param max_colors Maximum number of colors in the GIF. This is
#'   currently per frame.
#' @param loop How many times to loop the animation. Zero means infinite
#'   loop.
#' @param end_wait Number of seconds to wait at the end, before looping.
#' @param optimize Whether to try to create smaller GIF files. This might
#'   be slow for casts with many frames.
#' @return `path`, invisibly.
#'
#' @importFrom processx process
#' @importFrom jsonlite toJSON
#' @importFrom magick image_read image_write image_animate image_quantize
#'   image_display
#' @importFrom cli cli_status cli_status_update cli_status_clear
#' @export

write_gif <- function(cast, path, show = NULL, cols = NULL,
                      rows = NULL, theme = NULL, scale = 2.0, speed = 1.0,
                      max_colors = 256, loop = 0, end_wait = 10,
                      optimize = TRUE) {
  with_cli_process("Finding phantom.js", {
    phexe <- find_phantom()
    if (is.null(phexe)) throw(cli::format_error("No phantom.js, exiting."))
  })

  frames <- load_frames(cast)
  ts <- vapply(frames$frames, "[[", double(1), 1)
  delays <- as.integer(c(diff(ts) / speed, end_wait) * 100)
  screens <- lapply(frames$frames, function(f) f[[2]]$screen["lines"])

  sin <- tempfile()
  on.exit(unlink(sin), add = TRUE)
  sincon <- file(sin, open = "w")
  on.exit(try_silently(close(sincon)), add = TRUE)

  dir.create(pngdir <- tempfile())
  on.exit(unlink(pngdir, recursive = TRUE), add = TRUE)
  png_files <- sprintf(file.path(pngdir, "snap-%i.png"), seq_along(screens))

  lapply(seq_along(screens), function(idx) {
    scr <- screens[[idx]]
    # auto_unbox is for the single strings for each span (line piece)
    json <- toJSON(scr, auto_unbox = TRUE)
    cat(json, file = sincon, sep = "\n")
    cat(png_files[[idx]], file = sincon, sep = "\n")
  })

  close(sincon)

  rndr_js <- system.file("renderer.js", package = "asciicast")
  rndr_html <- system.file(
    "page",
    "asciicast2gif.html",
    package = "asciicast"
  )
  if (identical(rows, "auto")) {
    rows <- sum(unlist(strsplit(cast$output$data, "")) == "\n")
  }
  # without this, windows fails, because it takes c: as the protocol
  if (is_windows()) {
    rndr_html <- paste0("file:///", rndr_html) # nocovif !is_windows()
  }
  args <- c(
    rndr_js, rndr_html, cols %||% frames$width,
    rows %||% frames$height,
    theme %||% cast$config$theme %||% "asciinema", scale
  )

  status <- cli_status("{.alert-info Creating {length(screens)} snapshot{?s}}")
  env <- c(Sys.getenv(), c("PHJS_DEBUG" = "1"))
  phjs <- process$new(
    phexe, args,
    stdin = sin, stdout = "|", stderr = "|", env = env,
    poll_connection = TRUE
  )

  err <- character()
  while (TRUE) {
    out <- phjs$read_output_lines()
    err <- c(err, phjs$read_error_lines())
    str <- grep("^Starting frame ", out, value = TRUE)
    lapply(sub("^Starting frame ", "", str), function(i) {
      cli_status_update(status, "{.alert-info Creating snapshot {i}}")
    })
    pr <- phjs$poll_io(500)
    if (pr[["process"]] == "ready") break
  }

  phjs$wait(3000)
  phjs$kill()

  if (phjs$get_exit_status() != 0) {
    cnd <- new_error("phantom.js failed, see `$stderr` for standard error")
    cnd$stderr <- err
    throw(cnd)
  }

  cli_status_clear(
    status,
    result = "done",
    msg_done = "{.alert-success Creating {length(screens)} snapshot{?s} ... done}"
  )

  msg <- paste(if (optimize) "Optimizing" else "Creating", "GIF frames")
  with_cli_process(msg, {
    imgs <- image_read(png_files)
    anim <- image_animate(
      imgs,
      optimize = optimize, loop = loop, delay = delays
    )
  })

  if (optimize) {
    with_cli_process("Optimizing GIF colors", {
      anim <- image_quantize(anim, max = max_colors)
    })
  }

  with_cli_process("Writing GIF output", image_write(anim, path))

  show <- show %||% is_rstudio()
  if (isTRUE(show)) {
    if (is_rstudio()) {
      view_image_in_rstudio(path)
    } else {
      tryCatch(image_display(anim), interrupt = function() NULL)
    }
  }

  invisible(path)
}
