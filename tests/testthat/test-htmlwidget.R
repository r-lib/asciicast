test_that("asciinema_player", {
  code <- paste(
    sep = "\n",
    "# comment",
    "letters",
    "message('hi there')",
    ""
  )

  cast <- record(textConnection(code), typing_speed = 0, rows = "auto")
  cast$output$time <- seq_along(cast$output$time) / 10

  wdgt <- asciinema_player(cast, poster_frame = 0.5)
  expect_s3_class(wdgt, "asciinema_player")
  expect_s3_class(wdgt, "htmlwidget")

  wdgt <- asciinema_player(cast, poster_text = "Look at this")
  expect_s3_class(wdgt, "asciinema_player")
  expect_s3_class(wdgt, "htmlwidget")
})
