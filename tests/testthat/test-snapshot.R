test_that("cast()", {
  local_mocked_bindings(
    get_cast_time = function() {
      1746043110L
    }
  )

  expect_snapshot(cast(c("\033[32mabc\033[39m", "d")))

  expect_identical(cast(c("abc", "d")), cast("abc\nd"))

  expect_identical(cast(character()), cast(""))
})

test_that("capture_cast()", {
  local_mocked_bindings(
    get_cast_time = function() {
      1746043110L
    }
  )

  expect_snapshot(capture_cast(cli::col_green("abc")))
})

test_that("expect_snapshot_cast()", {
  expect_snapshot_cast(cli::col_green("abc"))
})
