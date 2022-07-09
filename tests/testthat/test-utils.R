
test_that("with_cli_process", {
  withr::local_options(cli.ansi = FALSE)
  expect_snapshot(
    with_cli_process("I am practicing, mom!", 1:10)
  )
})

test_that("is_rcmd_check", {
  withr::local_envvar(NOT_CRAN = "true")
  expect_false(is_rcmd_check())

  withr::local_envvar(
    NOT_CRAN = NA_character_,
    "_R_CHECK_PACKAGE_NAME_" = "asciicast"
  )
  expect_true(is_rcmd_check())

  withr::local_envvar(
    NOT_CRAN = NA_character_,
    "_R_CHECK_PACKAGE_NAME_" = NA_character_
  )
  expect_false(is_rcmd_check())
})
