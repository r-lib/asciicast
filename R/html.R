
#' Create a HTML snapshot of an asciicast
#'
#' @param cast `asciicast` object.
#' @param path Path to the HTML file to create.
#' @param at When to take the snapshot, defaults to the end of the cast
#'   (`"end"`). Can also be a number, in seconds.
#' @param omit_last_line Whether to omit the last line of the cast. This
#'   often just the prompt, and sometimes it is not worth showing.
#' @param prefix Prefix to add to the beginning to every line. E.g.
#'   `#> ` is usually added to knitr output.
#' @param theme A theme name to use, or a a named list to override the
#'   default theme (see [default_theme()]).
#' @param details Whether to put the output in a `<details>` tag.
#' @param summary Summary of the `<details>` tag, ignored if `details` is
#'   FALSE.
#'
#' @export

write_html <- function(cast, path, at = "end", omit_last_line = NULL,
                       prefix = "", theme = NULL, details = FALSE,
                       summary = "See output") {

  omit_last_line <- as.logical(get_param("omit_last_line", TRUE))
  if (omit_last_line) cast <- remove_last_line(cast)

  if (identical(at, "end")) at <- utils::tail(cast$output$time, 1)

  frames <- load_frames(
    cast,
    height = cast$config$height,
    width = cast$config$width
  )
  ts <- vapply(frames$frames, "[[", double(1), 1)
  which_frame <- utils::tail(which(ts <= at), 1)
  lines <- frames$frames[[which_frame]][[2]]$screen$lines

  theme <- to_html_theme(interpret_theme(theme))

  lines <- c(
    if (details) c("<details><summary>", summary, "</summary>"),
    paste0("<div class=\"asciicast\" style=\"", theme[["div.asciicast"]], "\"><pre>"),
    vapply(lines, format_html_line, character(1), prefix = prefix, theme = theme),
    "</pre></div>",
    if (details) "</details>"
  )

  cat(lines, file = path, sep = "\n")

  invisible()
}

interpret_theme <- function(theme) {
  theme <- theme %||%
    getOption("asciicast_theme") %||%
    if (Sys.getenv("IN_PKGDOWN") == "true") "pkgdown"

  if (is.character(theme) && length(theme) == 1) {
    if (!theme %in% names(themes)) {
      throw(new_error("Unknown theme: ", theme))
    }
    theme <- themes[[theme]]
  }
  modify_list(default_theme(), theme)
}

col_to_css <- function(x) {
  sprintf(
    "color: %s",
    grDevices::rgb(x[[1]] / 255, x[[2]] / 255, x[[3]] / 255)
  )
}

bg_to_css <- function(x) {
  sprintf(
    "background-color: %s",
    grDevices::rgb(x[[1]] / 255, x[[2]] / 255, x[[3]] / 255)
  )
}

col_to_rgb <- function(x) {
  grDevices::rgb(x[[1]] / 255, x[[2]] / 255, x[[3]] / 255)
}

to_html_theme <- function(theme) {
  cli_theme <- cli::ansi_html_style()
  cli_theme[] <- sub("^\\{[ ]+", "", cli_theme)
  cli_theme[] <- sub("[ ]+\\}$", "", cli_theme)

  cli_theme[[".ansi-color-0"]] <- col_to_css(theme$black)
  cli_theme[[".ansi-color-1"]] <- col_to_css(theme$red)
  cli_theme[[".ansi-color-2"]] <- col_to_css(theme$green)
  cli_theme[[".ansi-color-3"]] <- col_to_css(theme$yellow)
  cli_theme[[".ansi-color-4"]] <- col_to_css(theme$blue)
  cli_theme[[".ansi-color-5"]] <- col_to_css(theme$magenta)
  cli_theme[[".ansi-color-6"]] <- col_to_css(theme$cyan)
  cli_theme[[".ansi-color-7"]] <- col_to_css(theme$white)
  cli_theme[[".ansi-color-8"]] <- col_to_css(theme$light_black)
  cli_theme[[".ansi-color-9"]] <- col_to_css(theme$light_red)
  cli_theme[[".ansi-color-10"]] <- col_to_css(theme$light_green)
  cli_theme[[".ansi-color-11"]] <- col_to_css(theme$light_yellow)
  cli_theme[[".ansi-color-12"]] <- col_to_css(theme$light_blue)
  cli_theme[[".ansi-color-13"]] <- col_to_css(theme$light_magenta)
  cli_theme[[".ansi-color-14"]] <- col_to_css(theme$light_cyan)
  cli_theme[[".ansi-color-15"]] <- col_to_css(theme$light_white)

  cli_theme[[".ansi-bg-color-0"]] <- bg_to_css(theme$black)
  cli_theme[[".ansi-bg-color-1"]] <- bg_to_css(theme$red)
  cli_theme[[".ansi-bg-color-2"]] <- bg_to_css(theme$green)
  cli_theme[[".ansi-bg-color-3"]] <- bg_to_css(theme$yellow)
  cli_theme[[".ansi-bg-color-4"]] <- bg_to_css(theme$blue)
  cli_theme[[".ansi-bg-color-5"]] <- bg_to_css(theme$magenta)
  cli_theme[[".ansi-bg-color-6"]] <- bg_to_css(theme$cyan)
  cli_theme[[".ansi-bg-color-7"]] <- bg_to_css(theme$white)
  cli_theme[[".ansi-bg-color-8"]] <- bg_to_css(theme$light_black)
  cli_theme[[".ansi-bg-color-9"]] <- bg_to_css(theme$light_red)
  cli_theme[[".ansi-bg-color-10"]] <- bg_to_css(theme$light_green)
  cli_theme[[".ansi-bg-color-11"]] <- bg_to_css(theme$light_yellow)
  cli_theme[[".ansi-bg-color-12"]] <- bg_to_css(theme$light_blue)
  cli_theme[[".ansi-bg-color-13"]] <- bg_to_css(theme$light_magenta)
  cli_theme[[".ansi-bg-color-14"]] <- bg_to_css(theme$light_cyan)
  cli_theme[[".ansi-bg-color-15"]] <- bg_to_css(theme$light_white)

  cli_theme[["div.asciicast"]] <- sprintf(
    "color: %s;font-family: %s;line-height: %f",
    col_to_rgb(theme$text),
    theme$font_family,
    theme$line_height
  )

  cli_theme
}

format_html_line <- function(
  line,
  theme = to_html_theme(interpret_theme(NULL)),
  prefix = "") {

  out <- paste(unlist(
    lapply(line, format_html_piece, theme = theme)
  ), collapse = "")

  if (nchar(prefix) != 0) {
    out <- paste0(prefix, out)
  }

  out
}

format_html_piece <- function(pc, theme) {
  txt <- escape_html(pc[[1]])
  mkp <- lapply(
    seq_along(pc[[2]]),
    function(i) create_markup(names(pc[[2]])[i], pc[[2]][[i]], theme = theme)
  )

  classes <- na_omit(map_chr(mkp, function(x) x$class %||% NA_character_))
  styles <- na_omit(map_chr(mkp, function(x) x$style %||% NA_character_))

  paste0(
    if (length(classes) + length(styles)) {
      "<span"
    },
    if (length(classes)) {
      paste0(" class=\"", paste(classes, collapse = " "), "\"")
    },
    if (length(styles)) {
      paste0(" style=\"", paste(styles, collapse = ";"), "\"")
    },
    if (length(classes) + length(styles)) {
      ">"
    },
    txt,
    if (length(classes) + length(styles)) {
      "</span>"
    }
  )
}

create_markup_fg <- function(col, theme) {
  if (length(col) == 1) {
    list(style = theme[[paste0(".ansi-color-", col)]])
  } else if (length(col) == 3) {
    list(style = paste(
      "color:",
      grDevices::rgb(col[[1]] / 255, col[[2]] / 255, col[[3]] / 255)
    ))
  } else {
    warning("Unknown color markup: ", format(col))
    NULL
  }
}

create_markup_bg <- function(col, theme) {
  if (length(col) == 1) {
    list(style = theme[[paste0(".ansi-bg-color-", col)]])
  } else if (length(col) == 3) {
    list(style = paste(
      "background-color:",
      grDevices::rgb(col[[1]] / 255, col[[2]] / 255, col[[3]] / 255)
    ))
  } else {
    warning("Unknown color markup: ", format(col))
    NULL
  }
}

create_markup <- function(nm, vl, theme) {
  switch(
    nm,
    "fg" = create_markup_fg(vl, theme),
    "bg" = create_markup_bg(vl, theme),
    "bold" = if (vl) list(style = theme[[".ansi-bold"]]),
    "italic" = if (vl) list(style = theme[[".ansi-italic"]]),
    "underline" = if (vl) list(style = theme[[".ansi-underline"]]),
    # blink?
    # hide/hidden?
    # crossedout/strikethrough?
    # links?
    NULL
  )
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("%", "&#37;", x, fixed = TRUE)
  x
}
