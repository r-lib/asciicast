test_that("record_output", {
  code <- c(
    "1:50",
    "message('hi there!')"
  )

  out1 <- record_output(code)
  expect_snapshot(out1)

  out2 <- record_output(code, stdout = FALSE)
  expect_snapshot(out2)

  out3 <- record_output(code, stderr = FALSE)
  expect_snapshot(out3)

  out4 <- record_output(code, echo = TRUE)
  expect_snapshot(out4)
})
