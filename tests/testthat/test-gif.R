
test_that("write_gif", {
  if (is.null(suppressMessages(find_phantom()))) {
    install_phantomjs()
  }

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

  mockery::stub(write_gif, "is_rstudio", TRUE)
  rs <- NULL
  mockery::stub(write_gif, "view_image_in_rstudio", function(path) rs <<- path)
  suppressMessages(
    write_gif(cast, gif, show = TRUE)
  )
  expect_false(is.null(rs))

  mockery::stub(write_gif, "is_rstudio", FALSE)
  rs <- NULL
  mockery::stub(write_gif, "image_display", function(anim) rs <<- TRUE)
  suppressMessages(
    write_gif(cast, gif, show = TRUE)
  )
  expect_true(rs)
  
})

test_that("write_gif errors", {
  mockery::stub(write_gif, "find_phantom", NULL)
  expect_error(
    suppressMessages(write_gif()),
    "No phantom.js, exiting"
  )
})
