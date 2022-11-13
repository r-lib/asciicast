
test_that("expect_snapshot_r_process", {
  testthat::local_edition(3)
  expect_snapshot_r_process(getRversion())
  expect_snapshot_r_process(1 + "")
  expect_snapshot_r_process(cat(cli::col_red(cli::style_bold("boldred"))))
})
