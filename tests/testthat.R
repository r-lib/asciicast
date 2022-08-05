# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(asciicast)

if (Sys.getenv("NOT_CRAN") == "true") {
  test_check("asciicast", reporter = "summary")
  if (.Platform$OS.type != "windows" && asciicast:::has_embedded()) {
    Sys.setenv(R_ASCIICAST_EMBEDDED = "false")
    test_check("asciicast", reporter = "summary")
  }
}
