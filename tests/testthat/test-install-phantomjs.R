
# We don't test the download currently. It does work, and will work as
# long as the files are there on GH. It is also tested in write_gif,
# if phantom is not installed.
return()

test_that("install_phantomjs", {
  if (!is_windows() && !is_macos() && !is_linux()) {
    skip("Unsupported OS")
  }
  if (is_linux() && R.Version()$arch != "x86_64") {
    skip("Unsupported OS")
  }
  suppressMessages(install_phantomjs(
    baseURL = 'https://github.com/wch/webshot/releases/download/v0.3.1'
  ))
  expect_true(TRUE)
  expect_true(file.exists(find_phantom()))
})

test_that("install_phantomjs errors", {
  mockery::stub(install_phantomjs, "is_windows", FALSE)
  mockery::stub(install_phantomjs, "is_macos", FALSE)
  mockery::stub(install_phantomjs, "is_linux", FALSE)
  expect_message(
    install_phantomjs(),
    "this platform is not supported"
  )
})
