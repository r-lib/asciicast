
test_that("basics", {
  
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  opath <- test_path("fixtures", "test-1.Rmd")
  tpath <- file.path(tmp, basename(opath))
  file.copy(opath, tpath)
  rmarkdown::render(tpath, quiet = TRUE)

  mdpath <- sub(".Rmd$", ".md", tpath)
  expect_snapshot_file(mdpath)

  svgs <- sort(dir(
    file.path(tmp, "test-1_files", "figure-gfm"),
    pattern = "[.]svg$",
    full.names = TRUE
  ))

  for (svg in svgs) {
    expect_snapshot_file(svg)
  }

  processed <- file.path(
    tmp,
    "test-1_files",
    "figure-gfm",
    "5-process.svg-processed"
  )
  expect_true(file.exists(processed))
})

test_that("options are set temporarily", {
  withr::local_options(asciicast_end_wait = 0)
  
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  opath <- test_path("fixtures", "test-1.Rmd")
  tpath <- file.path(tmp, basename(opath))
  file.copy(opath, tpath)
  rmarkdown::render(tpath, quiet = TRUE)

  mdpath <- sub(".Rmd$", ".md", tpath)
  expect_snapshot_file(mdpath)

  expect_equal(
    getOption("asciicast_end_wait"),
    0
  )
})

test_that("crash", {
  withr::local_options(asciicast_end_wait = 0)
  
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  opath <- test_path("fixtures", "test-2.Rmd")
  tpath <- file.path(tmp, basename(opath))
  file.copy(opath, tpath)
  expect_error(
    suppressMessages(rmarkdown::render(tpath, quiet = TRUE)),
    "asciicast subprocess crashed"
  )
})

test_that("caching", {
  withr::local_options(asciicast_end_wait = 0)
  
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  opath <- test_path("fixtures", "test-3.Rmd")
  tpath <- file.path(tmp, basename(opath))
  file.copy(opath, tpath)

  suppressMessages(rmarkdown::render(tpath, quiet = TRUE))
  svg <- file.path(tmp, "test-3_files", "figure-gfm", "1-cached.svg")
  expect_true(file.exists(svg))
  hash <- cli::hash_file_sha256(svg)

  rmarkdown::render(tpath, quiet = TRUE)
  expect_true(file.exists(svg))
  expect_equal(
    hash,
    cli::hash_file_sha256(svg)
  )
})

test_that("cpp11", {
  
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  opath <- test_path("fixtures", "test-4.Rmd")
  tpath <- file.path(tmp, basename(opath))
  file.copy(opath, tpath)
  rmarkdown::render(tpath, quiet = TRUE)

  mdpath <- sub(".Rmd$", ".md", tpath)
  expect_snapshot_file(mdpath)

  svgs <- sort(dir(
    file.path(tmp, "test-4_files", "figure-gfm"),
    pattern = "[.]svg$",
    full.names = TRUE
  ))

  for (svg in svgs) {
    expect_snapshot_file(svg)
  }
})
