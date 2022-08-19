
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

test_that("full html", {
  withr::local_options(asciicast_typing_speed = 0)
  cast <- record(quote({
    options(cli.num_colors = 256)
    mycol <- cli::make_ansi_style("orange")
    cat(paste("pre", mycol("orange"), "post"))
  }), echo = FALSE, rows = 2)
  tmp <- tempfile("ac-html-", fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  write_html(cast, tmp, full = TRUE, theme = "asciinema")
  expect_snapshot_file(tmp, name = "html-full.html")
})

test_that("unknown theme", {
  cast <- record("ls()")
  expect_error(
    write_html(cast, tempfile(), theme = basename(tempfile()), full_html = TRUE),
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
  cast <- record(quote(
    cli::ansi_palette_show("dichro", colors = cli::truecolor)
  ))
  tmp <- tempfile("ac-html-", fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  write_html(cast, tmp)
  expect_snapshot_file(tmp, name = "html-truecolor.html")
})
