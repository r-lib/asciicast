
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

  cast$output <- cast$output[cast$output$type == "o", ]
  if (!identical(at, "end")) {
    cast$output <- cast$output[cast$output$time <= at, ]
  }

  height <- cast$config$height
  width <- cast$config$width
  screen <- cli::vt_output(cast$output$data, width = width , height = height)

  theme <- to_html_theme(interpret_theme(theme))

  lines <- lapply(seq_len(max(screen$lineno)), function(l) {
    format_html_line(screen[screen$lineno == l, ], prefix = prefix, theme = theme)
  })

  alllines <- c(
    if (details) c("<details><summary>", summary, "</summary>"),
    paste0("<div class=\"asciicast\" style=\"", theme[["div.asciicast"]], "\"><pre>"),
    unlist(lines),
    "</pre></div>",
    if (details) "</details>"
  )

  cat(alllines, file = path, sep = "\n")

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
  segments,
  theme = to_html_theme(interpret_theme(NULL)),
  prefix = "") {

  out <- paste(unlist(
    lapply(seq_len(nrow(segments)), function(i) {
      format_html_piece(segments[i, ], theme = theme)
    })
  ), collapse = "")

  if (nchar(prefix) != 0) {
    out <- paste0(prefix, out)
  }

  out
}

format_html_piece <- function(pc, theme) {
  style <- paste0("",
    if (pc$bold) theme[[".ansi-bold"]],
    if (pc$italic) theme[[".ansi-italic"]],
    if (pc$underline) theme[[".ansi-underline"]],
    if (!is.na(pc$color)) create_markup_fg(pc$color, theme),
    if (!is.na(pc$background_color)) create_markup_bg(pc$background_color, theme)
  )
  paste0(
    if (nchar(style)) paste0("<span style=\"", style, "\">"),
    if (!is.na(pc$link)) paste0("<a href=\"", escape_html(pc$link), "\">"),
    escape_html(pc$segment),
    if (!is.na(pc$link)) "</a>",
    if (nchar(style)) "</span>"
  )
}

create_markup_fg <- function(col, theme) {
  if (substr(col, 1, 1) != "#") {
    paste0(theme[[paste0(".ansi-color-", col)]], ";")
  } else {
    paste0("color:", col, ";")
  }
}

create_markup_bg <- function(col, theme) {
  if (substr(col, 1, 1) != "#") {
    paste0(theme[[paste0(".ansi-bg-color-", col)]], ";")
  } else {
    paste0("color:", col, ";")
  }
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("%", "&#37;", x, fixed = TRUE)
  x
}
