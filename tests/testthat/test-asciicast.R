
test_that("record", {
  withr::local_options(asciicast_typing_speed = 0)
  hello <- system.file(package = "asciicast", "examples", "hello.R")
  cast <- record(hello, interactive = FALSE)
  expect_snapshot(cast$output$data)
})

test_that("env vars in header", {
  code <- paste(
    sep = "\n",
    "#' record_env: c(FOO = 'bar', SHELL = NA_character_)",
    "#' typing_speed: 0",
    "Sys.getenv('FOO')",
    "Sys.getenv('SHELL', NA_character_)",
    ""
  )

  cast <- record(textConnection(code), echo = FALSE)
  expect_snapshot(cast$output$data)
})

test_that("startup option", {
  cast <- record(
    quote(foo),
    echo = FALSE,
    startup = quote(foo <- 112)
  )
  expect_snapshot(cast$output$data)
})

test_that("startup in header", {
  code <- paste(
    sep = "\n",
    "#' startup: foo <- 112",
    "foo",
    ""
  )
  cast <- record(code, echo = FALSE)
  expect_snapshot(cast$output$data)
})

test_that("automatic row numbers", {
  cast <- record(textConnection("# comment\n1+1\n"), rows = "auto")
  expect_equal(cast$config$rows, 3L)

  cast <- record(textConnection("cat('foobar')"), rows = "auto")
  expect_equal(cast$config$rows, 2L)
})

test_that("print.asciicast", {
  cast <- record(textConnection("# comment\n1+1\n"))
  cast$output$time <- seq_along(cast$output$time) / 100
  cast$config$timestamp <- 1656941462
  expect_snapshot(cast)
})
