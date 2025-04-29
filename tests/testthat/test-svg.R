test_that("write_svg", {
  withr::local_options(asciicast_typing_speed = 0)

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  hello <- system.file(package = "asciicast", "examples", "hello.R")
  cast <- record(hello, interactive = FALSE)

  svg <- file.path(tmp, "hello.svg")
  write_svg(
    cast,
    svg,
    at = "end",
    padding = 5,
    omit_last_line = TRUE,
    rows = "auto"
  )
  expect_snapshot_file(svg)

  svg1 <- file.path(tmp, "hello1.svg")
  cast$output$time <- seq_along(cast$output$time) / 100
  write_svg(
    cast,
    svg1,
    start_at = min(cast$output$time),
    end_at = max(cast$output$time)
  )
  expect_snapshot_file(svg1)
})

test_that("themes", {
  withr::local_options(asciicast_typing_speed = 0)

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  hello <- system.file(package = "asciicast", "examples", "hello.R")
  cast <- record(hello, interactive = FALSE)

  svg <- file.path(tmp, "hello2.svg")
  write_svg(cast, svg, theme = "solarized-light", at = "end")
  expect_snapshot_file(svg)
})

test_that("write_svg errors", {
  withr::local_options(asciicast_typing_speed = 0)

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  hello <- system.file(package = "asciicast", "examples", "hello.R")
  cast <- record(hello, interactive = FALSE)

  svg <- file.path(tmp, "foobar.svg")
  expect_snapshot(error = TRUE, write_svg(cast, svg, theme = "foobarxx"))

  fake(check_svg_support, "is_svg_supported", FALSE)
  expect_snapshot(error = TRUE, check_svg_support())
})

test_that("play", {
  path <- NULL
  on.exit(unlink(path), add = TRUE)

  hello <- system.file(package = "asciicast", "examples", "hello.R")
  cast <- record(hello, interactive = FALSE)

  fake(play, "play_svg", function(x, ...) path <<- x)
  play(cast, at = "end")

  expect_false(is.null(path))
})

test_that("remove_last_line edge case", {
  cast <- record(textConnection("1"), interactive = FALSE)
  cast$output <- cast$output[cast$output$type != "o", ]

  expect_equal(remove_last_line(cast), cast)
})
