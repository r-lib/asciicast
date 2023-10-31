test_that("read_cast from file", {
  code <- paste(
    sep = "\n",
    "# comment",
    "1+1",
    ""
  )

  cast <- record(textConnection(code))

  json <- tempfile(fileext = ".json")
  on.exit(unlink(json), add = TRUE)
  write_json(cast, json)

  cast2 <- read_cast(json)
  expect_equal(cast, cast2)
})

test_that("read cast from asciinema.org & from URL", {
  cast <- read_cast(258660)
  cast2 <- read_cast(
    "https://asciinema.org/a/uHQwIVpiZvu0Ioio8KYx6Uwlj.cast?dl=1"
  )
  expect_equal(cast, cast2)
  json <- tempfile(fileext = ".json")
  on.exit(unlink(json), add = TRUE)
  write_json(cast, json)
})

test_that("errors", {
  v1 <- test_path("fixtures", "v1.json")
  expect_error(
    read_cast(v1),
    "Only version 2 asciicast files are supported"
  )

  badver <- test_path("fixtures", "badver.json")
  expect_error(
    read_cast(badver),
    "Only version 2 asciicast files are supported"
  )

  bad <- test_path("fixtures", "bad.json")
  expect_error(
    read_cast(bad),
    class = "asciicast_parse_error"
  )
})
