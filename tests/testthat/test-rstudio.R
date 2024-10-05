test_that("is_rstudio", {
  expect_false(is_rstudio())

  local_mocked_bindings(loadedNamespaces = function() "rstudioapi")
  local_mocked_bindings(isAvailable = function() TRUE, .package = "rstudioapi")
  expect_true(is_rstudio())
})

test_that("view_image_in_rstudio", {
  dir.create(tmp <- tempfile())
  path2 <- file.path(tmp, "out.html")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- NULL
  on.exit(unlink(path), add = TRUE)

  local_mocked_bindings(
    viewer = function(x) path <<- x, .package = "rstudioapi"
  )

  view_image_in_rstudio(tempdir())
  expect_false(is.null(path))

  file.copy(path, path2)
  expect_snapshot_file(
    path2,
    transform = function(x) {
      sub('asciicast-preview-[^"]*"', 'asciicast-preview-xxx"', x)
    },
    variant = os_type()
  )
})
