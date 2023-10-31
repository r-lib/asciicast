test_that("no markup", {
  withr::local_options(asciicast_typing_speed = 0)
  cast <- record(quote({
    options(cli.num_colors = 8)
    cat(paste("pre", "nothing", "post"))
  }), echo = FALSE, rows = 2)
  tmp <- tempfile("ac-html-", fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  write_html(cast, tmp)
  expect_snapshot_file(tmp, name = "html-no-markup.html")
})

test_that("8 colors", {
  withr::local_options(asciicast_typing_speed = 0)
  cast <- record(quote({
    options(cli.num_colors = 8)
    cat(paste(
      "pre", cli::col_yellow("yellow"), cli::bg_yellow(" yellow"), "post"
    ))
  }), echo = FALSE, rows = 2)
  tmp <- tempfile("ac-html-", fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  write_html(cast, tmp)
  expect_snapshot_file(tmp, name = "html-8-colors.html")
})

test_that("styles", {
  withr::local_options(asciicast_typing_speed = 0)
  cast <- record(quote({
    options(cli.num_colors = 8)
    cat(paste(
      "pre", cli::style_bold("bold"), cli::style_italic("italic"),
      cli::style_underline("underline"), "post"
    ))
  }), echo = FALSE, rows = 2)
  tmp <- tempfile("ac-html-", fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  write_html(cast, tmp)
  expect_snapshot_file(tmp, name = "html-styles.html")
})

test_that("256 colors", {
  withr::local_options(asciicast_typing_speed = 0)
  cast <- record(quote({
    options(cli.num_colors = 256)
    mycol <- cli::make_ansi_style("orange")
    cat(paste("pre", mycol("orange"), "post"))
  }), echo = FALSE, rows = 2)
  tmp <- tempfile("ac-html-", fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  write_html(cast, tmp)
  expect_snapshot_file(tmp, name = "html-256-colors.html")
})

test_that("unknown theme", {
  cast <- record("ls()")
  expect_error(
    write_html(cast, tempfile(), theme = basename(tempfile())),
    "Unknown theme"
  )
})

test_that("prefix", {
  withr::local_options(asciicast_typing_speed = 0)
  cast <- record("cat('foobar')", echo = FALSE, rows = 2)
  tmp <- tempfile("ac-html-", fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  write_html(cast, tmp, prefix = "#> ")
  expect_snapshot_file(tmp, name = "html-prefix.html")
})

test_that("true color", {
  mode <- if (l10n_info()[["UTF-8"]]) "utf8" else "ascii"
  cast <- record(quote(
    withr::with_options(
      list(cli.num_colors = cli::truecolor),
      cli::ansi_palette_show("dichro", colors = cli::truecolor)
    )
  ), cols = 200, rows = "auto")
  tmp <- tempfile("ac-html-", fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  write_html(cast, tmp)
  expect_snapshot_file(tmp, name = "html-truecolor.html", variant = mode)
})

test_that("create_markup_{fg,bg}", {
  theme <- to_html_theme(interpret_theme(NULL))
  expect_snapshot({
    create_markup_fg("4", theme = theme)
    create_markup_fg("12", theme = theme)
    create_markup_fg("#010203", theme = theme)
  })
  expect_snapshot({
    create_markup_bg("4", theme = theme)
    create_markup_bg("12", theme = theme)
    create_markup_bg("#010203", theme = thene)
  })
})

test_that("hyperlink", {
  withr::local_options(asciicast_typing_speed = 0)
  cast <- record(quote({
    options(cli.num_colors = 256, cli.hyperlink = TRUE)
    st_from_bel <- function(x) {
      gsub("\007", "\033\\", x, fixed = TRUE)
    }
    cat(paste(
      "pre",
      st_from_bel(
        cli::style_hyperlink("text", "https://example.com")
      ),
      "post"
    ))
  }), echo = FALSE, rows = 2)
  tmp <- tempfile("ac-html-", fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  write_html(cast, tmp)
  expect_snapshot_file(tmp, name = "html-hyperlink.html")
})
