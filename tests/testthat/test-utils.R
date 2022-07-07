
test_that("with_cli_process", {
  withr::local_options(cli.ansi = FALSE)
  expect_snapshot(
    with_cli_process("I am practicing, mom!", 1:10)
  )
})
