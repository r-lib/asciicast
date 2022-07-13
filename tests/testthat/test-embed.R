
test_that("record", {
  withr::local_options(asciicast_typing_speed = 0)
  hello <- system.file(package = "asciicast", "examples", "hello.R")
  cast <- record(hello, interactive = FALSE)
  expect_snapshot(cast$output$data)
})

test_that("errors", {
  withr::local_options(asciicast_typing_speed = 0)
  cast1 <- record(textConnection("foo12313\nbarsdsdfsdf\n"))
  expect_snapshot(cast1$output$data)
})

test_that("R quits", {
  withr::local_options(asciicast_typing_speed = 0)
  cast <- record(textConnection("quit('no')\n"))
  expect_snapshot(cast$output$data)
})

test_that("R crashes", {
  withr::local_options(asciicast_typing_speed = 0)
  cast <- record(textConnection("callr:::crash()\n"), interactive = FALSE)
  expect_snapshot(cast$output$data, variant = os_arch())
})

test_that("incomplete expression", {
  withr::local_options(asciicast_typing_speed = 0)
  expect_error(
    record(textConnection("1 + (\n")),
    "Incomplete asciicast expression"
  )
})

test_that("incomplete expression allowed", {
  withr::local_options(asciicast_typing_speed = 0)
  expect_silent(
    record(textConnection("1 + (\n"), incomplete_error = FALSE)
  )
})

test_that("timeout", {
  withr::local_options(asciicast_typing_speed = 0)
  expect_error(
    record(textConnection("Sys.sleep(1)\n"), timeout = 0.1),
    "asciicast timeout after line"
  )
})

test_that("echo = FALSE", {
  withr::local_options(asciicast_typing_speed = 0)
  hello <- system.file(package = "asciicast", "examples", "hello.R")
  cast <- record(hello, interactive = FALSE, echo = FALSE)
  expect_snapshot(cast$output$data)
})

test_that("speed", {
  hello <- system.file(package = "asciicast", "examples", "hello.R")
  cast1 <- record(hello)
  cast2 <- record(hello, speed = 10)
  expect_true(
    utils::tail(cast2$output$time, 1) < utils::tail(cast1$output$time, 1) /2
  )
})

test_that("subprocess fails", {
  mockery::stub(asciicast_start_process, "processx::poll", list("timeout"))
  expect_error(
    asciicast_start_process(),
    "subprocess did not connect back"
  )
})

test_that("startup crashes", {
  expect_error(
    asciicast_start_process(
      startup = quote(callr:::crash()),
      interactive = FALSE
    ),
    "asciicast process exited while running"
  )
})

test_that("cannot send input, buffer is full", {
  skip_on_os("windows") # TODO
  expect_error(
    record(textConnection(strrep("1 + ", 100000))),
    "Cannot send input, buffer is full"
  )
})

test_that("shift", {
  expect_equal(shift(character()), character())
  expect_equal(shift("a"), "")
  expect_equal(shift(letters), c(letters[-1], ""))
})

test_that("add_empty_wait", {
  withr::local_options(asciicast_typing_speed = 0)
  cast1 <- record(textConnection("1+1\n\n2+2\n"), empty_wait = 0)
  cast2 <- record(textConnection("1+1\n\n2+2\n"), empty_wait = 5)
  expect_true(
    utils::tail(cast1$output$time, 1) < utils::tail(cast2$output$time, 1) - 3
  )
})

test_that("adjust_typing_speed", {
  withr::local_options(asciicast_typing_speed = 0)
  cast1 <- record(textConnection("1+1\n\n2+2\n"), empty_wait = 0)
  data <- cast1$output

  data1 <- adjust_typing_speed(data, 0.05)
  data2 <- adjust_typing_speed(data, 0.5)
  expect_true(
    utils::tail(data1$time, 1) < utils::tail(data2$time, 1) - 1
  )

  empty <- data[integer(), ]
  expect_equal(adjust_typing_speed(empty, 0.05), empty)
})

test_that("find_rem error", {
  mockery::stub(find_rem, "system.file", "")
  expect_error(
    find_rem(),
    "Cannot find embedded R executable"
  )
})

test_that("forced pause", {
  cast <- record(c(
    "#! --",
    "1 + 1",
    "#! --",
    "2 + 2"
  ))
  cmds <- grep("^type:", cast$output$data, value=TRUE)
  expect_snapshot(cmds)
})

test_that("edge case with no wait", {
  cast <- record(c(
    "#! --",
    "1 + 1"
  ), end_wait = 0)
  cmds <- grep("^type:", cast$output$data, value=TRUE)
  expect_snapshot(cmds)
})
