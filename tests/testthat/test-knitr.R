
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

test_that("eng_asciicast_output_type", {
  withr::local_options(asciicast_knitr_output = "html")
  expect_equal(eng_asciicast_output_type(), "html")

  withr::local_options(asciicast_knitr_output = NULL, asciicast_at = "all")
  withr::local_envvar(IN_PKGDOWN = "true")
  expect_equal(eng_asciicast_output_type(), "svg")

  withr::local_options(asciicast_at = "end")
  expect_equal(eng_asciicast_output_type(), "html")

  withr::local_options(asciicast_knitr_output = NULL, asciicast_at = NULL)
  withr::local_envvar(IN_PKGDOWN = NA_character_)
  withr::local_options(asciicast_knitr_svg = TRUE)
  expect_equal(eng_asciicast_output_type(), "svg")

  withr::local_options(asciicast_knitr_svg = FALSE)
  expect_equal(eng_asciicast_output_type(), "svg")
})

test_that("html output", {
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  opath <- test_path("fixtures", "test-5.Rmd")
  tpath <- file.path(tmp, basename(opath))
  file.copy(opath, tpath)
  rmarkdown::render(tpath, quiet = TRUE)

  mdpath <- sub(".Rmd$", ".md", tpath)
  expect_snapshot_file(mdpath)
})

test_that("html output with style", {
  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  opath <- test_path("fixtures", "test-6.Rmd")
  tpath <- file.path(tmp, basename(opath))
  file.copy(opath, tpath)
  rmarkdown::render(tpath, quiet = TRUE)

  mdpath <- sub(".Rmd$", ".md", tpath)
  expect_snapshot_file(mdpath)
})
