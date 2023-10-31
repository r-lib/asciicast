withr::defer(
  {
    try_silently(close(attr(.knitr_asciicast_process, "sock")))
    try_silently(.knitr_asciicast_process$kill())
  },
  teardown_env()
)
