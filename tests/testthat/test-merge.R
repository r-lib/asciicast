test_that("merge", {
  withr::local_options(asciicast_typing_speed = 0)

  dir.create(tmp <- tempfile())
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  code1 <- paste(
    sep = "\n",
    "# comment",
    "1:100",
    ""
  )

  code2 <- paste(
    sep = "\n",
    "# comment",
    "letters",
    ""
  )

  cast1 <- record(textConnection(code1))
  cast2 <- record(textConnection(code2))

  json <- file.path(tmp, "cast1.json")
  write_json(cast1, json)

  cast <- merge_casts(
    cast1,
    pause(10),
    clear_screen(),
    json,
    pause(10),
    cast2
  )

  svg <- file.path(tmp, paste0("merge-", 1:3, ".svg"))
  on.exit(unlink(svg), add = TRUE)

  write_svg(cast, svg[1], at = 5)
  write_svg(cast, svg[2], at = 15)
  write_svg(cast, svg[3], at = "end")

  expect_snapshot_file(svg[1])
  expect_snapshot_file(svg[2])
  expect_snapshot_file(svg[3])
})

test_that("merge error", {
  expect_error(
    merge_casts(pause(5), clear_screen()),
    "need to include at least one cast"
  )
})
