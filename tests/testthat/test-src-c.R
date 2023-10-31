test_that("Unicode output", {
  if (!l10n_info()$`UTF-8`) {
    skip("Needs UTF-8 system")
  }

  withr::local_options(asciicast_typing_speed = 0)

  code <- quote({
    cat("\u0100\u2500\U1F600")
  })

  cast <- record(code)
  expect_true(any(grepl("\u0100\u2500\U1F600", cast$output$data)))
})

test_that("Invalid UTF-8", {
  if (!l10n_info()$`UTF-8`) {
    skip("Needs UTF-8 system")
  }

  withr::local_options(asciicast_typing_speed = 0)

  cast <- record(textConnection(c(
    "cat(rawToChar(as.raw(c(226, 148))))",
    'cast("nope")'
  )))

  expect_false(any(grepl("nope", cast$output$data)))
})

test_that("Special characters go through JSON", {
  if (!l10n_info()$`UTF-8`) {
    skip("Needs UTF-8 system")
  }

  withr::local_options(asciicast_typing_speed = 0)

  # just backslash it seems..., plus some control characters
  code <- quote({
    cat("foo\\bar\fbaz")
  })

  cast <- record(textConnection(deparse(code)))
  expect_true(any(grepl("foo\\bar\fbaz", cast$output$data, fixed = TRUE)))
})

test_that("usage", {
  if (!has_embedded()) {
    skip("Needs embedded R")
  }
  rem <- find_rem()
  env <- setup_env()
  out <- processx::run(rem, env = env, error_on_status = FALSE)
  expect_equal(out$status, 5)
  expect_match(out$stderr, "Usage:.*rem.*-i")
})
