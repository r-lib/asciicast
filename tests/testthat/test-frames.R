test_that("load_svg_term", {
  ct <- load_svg_term()
  expect_s3_class(ct, "V8")
})

test_that("load_frames", {
  code <- paste(
    sep = "\n",
    "# comment",
    "letters",
    "message('hi there')",
    ""
  )

  cast <- record(textConnection(code), typing_speed = 0, rows = "auto")
  cast$output$time <- seq_along(cast$output$time) / 100
  cast$config$timestamp <- 1656941462
  frames <- load_frames(cast)

  expect_snapshot(
    sapply(frames$frames[[14]][[2]]$screen$lines, function(x) x[[1]][[1]])
  )

  mockery::stub(load_frames, "system.file", "")
  expect_error(
    load_frames(cast),
    "cannot find 'load-cast.js'"
  )
})
