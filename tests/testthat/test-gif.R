test_that("write_gif", {
  skip_on_ci()

  if (!is_windows() && !is_macos() && !is_linux()) {
    skip("Unsupported OS")
  }
  if (is_linux() && R.Version()$arch != "x86_64") {
    skip("Unsupported OS")
  }
  if (is.null(suppressMessages(find_phantom()))) {
    install_phantomjs()
  }
  if (is.null(suppressMessages(find_phantom()))) {
    skip("Could not install phantom.js")
  }

  withr::local_options(cli.ansi = FALSE)

  code <- paste(
    sep = "\n",
    "# comment",
    "letters",
    "message('hi there')",
    ""
  )

  cast <- record(textConnection(code), typing_speed = 0, rows = "auto")
  cast$output$time <- seq_along(cast$output$time) / 10

  gif <- tempfile(fileext = ".gif")
  on.exit(unlink(gif), add = TRUE)
  expect_snapshot(
    write_gif(cast, gif, show = FALSE, rows = "auto")
  )

  mgif <- magick::image_read(gif)
  expect_true(
    all(magick::image_info(mgif)$format == "GIF")
  )

  rs <- NULL
  fake(write_gif, "is_rstudio", TRUE)
  fake(write_gif, "view_image_in_rstudio", function(path) rs <<- path)
  suppressMessages(
    write_gif(cast, gif, show = TRUE)
  )
  expect_false(is.null(rs))

  rs <- NULL
  fake(write_gif, "is_rstudio", FALSE)
  fake(write_gif, "image_display", function(anim) rs <<- TRUE)
  suppressMessages(
    write_gif(cast, gif, show = TRUE)
  )
  expect_true(rs)
})

test_that("write_gif errors", {
  skip_on_ci()
  withr::local_options(cli.dynamic = FALSE, cli.ansi = FALSE)
  fake(write_gif, "find_phantom", NULL)
  expect_snapshot(error = TRUE, suppressMessages(write_gif()))

  fake(
    write_gif,
    "find_phantom",
    asNamespace("processx")$get_tool("px")
  )
  cast <- record(textConnection("1+1\n"))
  gif <- tempfile(fileext = ".gif")
  on.exit(unlink(gif), add = TRUE)
  expect_snapshot(
    error = TRUE,
    write_gif(cast, gif),
    # this is kind of random, maybe 3, maybe 4 frames
    transform = function(x) sub("4", "3", x)
  )
})
