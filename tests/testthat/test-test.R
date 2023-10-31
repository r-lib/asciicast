test_that("expect_snapshot_r_process", {
  if (packageVersion("cli") < "3.4.1.9000") skip("Needs newer cli")
  testthat::local_edition(3)
  expect_snapshot_r_process(cat("'4.2.2'"))
  expect_snapshot_r_process(1 + "")
  expect_snapshot_r_process(cat("\033[31m\033[1mboldred\033[22m\033[39m"))
})
