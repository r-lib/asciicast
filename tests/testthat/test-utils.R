
test_that("with_cli_process", {
  expect_snapshot(
    with_cli_process("I am practicing, mom!", 1:10)
  )
})
